library(tidyverse)
library(httr2)
library(jsonlite)
library(here)

# ── Data ──────────────────────────────────────────────────────────────────────
tiny_tib <-
  here::here("data", "coke_pepsi_reddit_trimmed.csv") |>
  read_csv() |>
  mutate(original_row_number = row_number())


# ══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# Runtime note: actual speed depends on your OpenAI rate-limit tier, model,
# average text length, and retry volume. At tier-1 (~500 RPM for gpt-4o-mini)
# with MAX_CONCURRENT = 20, 52k rows typically completes in ~2 hours.
# Raising MAX_CONCURRENT and/or upgrading your tier will speed this up.
# Keep MAX_CONCURRENT * avg_seconds_per_req / 60 <= REQUESTS_PER_MIN.
# ══════════════════════════════════════════════════════════════════════════════
MODEL             <- "gpt-4o-mini"  # classification model; swap freely
MAX_CONCURRENT    <- 4              # parallel in-flight HTTP requests
MAX_CHARS         <- 750           # truncate input text to this many chars
MAX_OUTPUT_TOKENS <- 50             # boolean + short quote needs <30 tokens
MAX_RETRIES       <- 5              # attempts per request (429 / 5xx only)
REQ_TIMEOUT_SEC   <- 30            # wall-clock timeout per individual request
CHECKPOINT_DIR    <- "checkpoints"
CHECKPOINT_EVERY  <- 500           # write partial results every N rows
OUTPUT_PATH       <- "results/classified.rds"

# ── JSON schema — strict mode guarantees machine-readable output ──────────────
complaint_schema <- list(
  type = "json_schema",
  json_schema = list(
    name   = "complaint_response",
    strict = TRUE,
    schema = list(
      type                 = "object",
      properties           = list(
        is_complaint      = list(type = "boolean"),
        complaint_subject = list(type = "string")
      ),
      required             = list("is_complaint", "complaint_subject"),
      additionalProperties = FALSE
    )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# MODULE: build_prompt
# Beverage name is injected per row. Prompt is kept compact because it
# is transmitted 52k times — every saved token reduces cost directly.
# API choice: Chat Completions is used rather than the Responses API because
# this is a stateless, single-turn classification task with no tool use or
# conversation history. For that pattern, Chat Completions is the correct
# and fully-supported choice.
# ══════════════════════════════════════════════════════════════════════════════
build_prompt <- function(group) {
  bev <- if (group == "treatment") "Coke Zero" else "Pepsi Zero/Max"
  str_c(
    "Classify whether the text complains about ", bev, ". ",
    "Return true only if it criticizes ", bev, "'s taste, quality, formula, ",
    "ingredients, carbonation, sweetness, aftertaste, or drinking experience. ",
    "Do not count complaints about other products, price, ads, availability, ",
    "packaging, delivery, or the company generally. ",
    "If true, complaint_subject = exact 3-8 word quote from the text. ",
    'If false, complaint_subject = "".'
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# MODULE: build_request
# Constructs a fully configured httr2 request object for one row.
# req_throttle limits the initiation rate across all parallel slots.
# req_retry handles 429 and 5xx with exponential backoff + jitter;
# permanent 4xx (except 429) are not retried.
# ══════════════════════════════════════════════════════════════════════════════
build_request <- function(row) {
  text <- str_c(replace_na(row$title, ""), " ", replace_na(row$text, "")) |>
    str_squish() |>
    str_trunc(MAX_CHARS)

  request("https://api.openai.com/v1/chat/completions") |>
    req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
    req_body_json(list(
      model           = MODEL,
      temperature     = 0,
      max_tokens      = MAX_OUTPUT_TOKENS,
      response_format = complaint_schema,
      messages        = list(
        list(role = "system", content = build_prompt(row$group)),
        list(role = "user",   content = text)
      )
    )) |>
    req_timeout(REQ_TIMEOUT_SEC) |>
    req_retry(
      max_tries    = MAX_RETRIES,
      is_transient = \(resp) resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L),
      backoff      = \(attempt) min(2^attempt + runif(1, 0, 1), 15),
      after        = \(resp) NA
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# MODULE: parse_response
# Returns a named list with full per-row status metadata.
# On JSON parse failure, returns status = "parse_error" with raw_response
# preserved so the caller can retry the same request once.
# ══════════════════════════════════════════════════════════════════════════════
parse_response <- function(resp, original_row_number, attempt_count = 1L) {
  base <- list(
    original_row_number = original_row_number,
    is_complaint        = NA,
    complaint_subject   = NA_character_,
    raw_response        = NA_character_,
    status              = NA_character_,
    error_message       = NA_character_,
    attempt_count       = attempt_count
  )

  # Network failures, timeouts, and non-retried HTTP errors arrive as
  # error objects (not responses) when on_error = "continue"
  if (inherits(resp, "error")) {
    return(modifyList(base, list(
      status        = "api_error",
      error_message = conditionMessage(resp)
    )))
  }

  raw <- tryCatch(
    resp |> resp_body_json() |> _$choices[[1]]$message$content,
    error = \(e) NULL
  )

  if (is.null(raw)) {
    return(modifyList(base, list(
      status        = "api_error",
      error_message = "Empty or malformed API response"
    )))
  }

  parsed <- tryCatch(fromJSON(raw), error = \(e) NULL)

  if (is.null(parsed)) {
    return(modifyList(base, list(
      raw_response  = raw,
      status        = "parse_error",
      error_message = "JSON parse failed"
    )))
  }

  modifyList(base, list(
    is_complaint      = isTRUE(parsed$is_complaint),
    complaint_subject = parsed$complaint_subject %||% "",
    raw_response      = raw,
    status            = "success"
  ))
}

# ══════════════════════════════════════════════════════════════════════════════
# MODULE: checkpoint helpers
# Checkpoints are time-stamped RDS files. resume_from_checkpoint() loads
# the most recent one so a restarted run skips already-classified rows.
# ══════════════════════════════════════════════════════════════════════════════
save_checkpoint <- function(results_list, dir = CHECKPOINT_DIR) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(dir, sprintf("ckpt_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S")))
  saveRDS(bind_rows(results_list), path)
  invisible(path)
}

resume_from_checkpoint <- function(dir = CHECKPOINT_DIR) {
  files <- list.files(dir, pattern = "^ckpt_.*\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    message("No checkpoint found — starting fresh.")
    return(NULL)
  }
  latest <- files[which.max(file.mtime(files))]
  message("Resuming from: ", latest)
  readRDS(latest)
}

# ══════════════════════════════════════════════════════════════════════════════
# MODULE: classify_rows_parallel
# Primary runtime path. Uses httr2::req_perform_parallel(), which drives
# concurrent HTTP requests through curl's multi interface — no additional
# R processes are spawned. Concurrency is controlled by splitting work into
# mini-batches of max_concurrent rows; each mini-batch runs fully in parallel
# before the next starts.
#
# Processing is chunked at CHECKPOINT_EVERY rows so partial progress is
# written to disk continuously. A single JSON parse failure per row triggers
# one sequential retry before the row is marked parse_error.
# ══════════════════════════════════════════════════════════════════════════════
classify_rows_parallel <- function(df,
                                   max_concurrent = MAX_CONCURRENT,
                                   checkpoint_dir = CHECKPOINT_DIR) {
  n           <- nrow(df)
  chunk_idxs  <- split(seq_len(n), ceiling(seq_len(n) / CHECKPOINT_EVERY))
  all_results <- list()
  t_start     <- Sys.time()
  error_count <- 0L

  for (ci in seq_along(chunk_idxs)) {
    idx      <- chunk_idxs[[ci]]
    chunk_df <- df[idx, ]
    reqs     <- map(seq_len(nrow(chunk_df)), \(i) build_request(chunk_df[i, ]))

    # Split into mini-batches of max_concurrent so no more than that many
    # requests are in-flight simultaneously. req_perform_parallel runs each
    # mini-batch concurrently; we move to the next only when it finishes.
    mini_batches  <- split(seq_along(reqs), ceiling(seq_along(reqs) / max_concurrent))
    chunk_results <- list()

    for (mb in mini_batches) {
      mb_resps   <- req_perform_parallel(reqs[mb], on_error = "return")
      mb_results <- map2(mb_resps, chunk_df$original_row_number[mb], parse_response)
      chunk_results <- c(chunk_results, mb_results)
      Sys.sleep(0.25)
    }

    # One sequential retry for rows where the model returned malformed JSON
    parse_fail_idx <- which(map_chr(chunk_results, "status") == "parse_error")
    for (fi in parse_fail_idx) {
      retry_resp <- tryCatch(req_perform(reqs[[fi]]), error = \(e) e)
      chunk_results[[fi]] <- parse_response(
        retry_resp,
        chunk_df$original_row_number[fi],
        attempt_count = 2L
      )
    }

    all_results  <- c(all_results, chunk_results)
    error_count  <- error_count + sum(map_chr(chunk_results, "status") != "success")

    # ── Throughput log ────────────────────────────────────────────────────────
    elapsed  <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    done     <- length(all_results)
    rate     <- done / max(elapsed, 1)
    eta      <- Sys.time() + (n - done) / max(rate, 1e-3)
    message(sprintf("[%s] %d/%d rows | %.1f rows/sec | ETA %s | errors: %d",
                    format(Sys.time(), "%H:%M:%S"), done, n, rate,
                    format(eta, "%H:%M:%S"), error_count))

    # Checkpoint after every chunk so an interruption loses at most
    # CHECKPOINT_EVERY rows of work
    save_checkpoint(all_results, checkpoint_dir)
  }

  bind_rows(all_results)
}

# ══════════════════════════════════════════════════════════════════════════════
# MODULE: run_validation_sample
# Stratified sample across group × prefilter_pass before committing to 52k.
# Inspect the output before uncommenting the full run below.
# ══════════════════════════════════════════════════════════════════════════════
run_validation_sample <- function(df, n = 300) {
  df_pf <- prefilter_rows(df, use_prefilter = TRUE)

  sample_df <- df_pf |>
    group_by(group, prefilter_pass) |>
    slice_sample(n = ceiling(n / 4), replace = FALSE) |>
    ungroup() |>
    slice_sample(n = min(n, nrow(df_pf)))

  message(sprintf("Validation: classifying %d rows ...", nrow(sample_df)))
  results <- classify_rows_parallel(
    sample_df,
    checkpoint_dir = file.path(CHECKPOINT_DIR, "validation")
  )

  out <- "results/validation_sample.rds"
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  saveRDS(results, out)
  message("Validation results saved to: ", out)
  results
}

# ══════════════════════════════════════════════════════════════════════════════
# MODULE: attach_results
# Joins classification output to the original tibble by original_row_number.
# Always uses an explicit key join — never relies on list position.
# ══════════════════════════════════════════════════════════════════════════════
attach_results <- function(df, results_df) {
  df |>
    left_join(
      results_df |>
        select(original_row_number, is_complaint,
               complaint_subject, status, error_message),
      by = "original_row_number"
    ) |>
    rename(
      classification_status = status,
      classification_error  = error_message
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# MAIN EXECUTION
# ══════════════════════════════════════════════════════════════════════════════
t_main <- Sys.time()

# 1. Resume from checkpoint — drop rows already successfully classified
df_to_send <- tiny_tib
prior <- resume_from_checkpoint()
if (!is.null(prior)) {
  done_rows  <- prior |> filter(status == "success") |> pull(original_row_number)
  df_to_send <- filter(df_to_send, !original_row_number %in% done_rows)
  message(sprintf("Resuming: %d done, %d remaining",
                  length(done_rows), nrow(df_to_send)))
}

# 2. Full parallel classification
results_new <- classify_rows_parallel(df_to_send)

# Merge with prior checkpoint; deduplicate in case a chunk was checkpointed
# mid-run and the same rows were reprocessed on restart
results_final <- if (!is.null(prior)) {
  bind_rows(prior, results_new) |> distinct(original_row_number, .keep_all = TRUE)
} else {
  results_new
}

# 6. Join back to original tibble by row number
tiny_tib <- attach_results(tiny_tib, results_final)

# 7. Save outputs
dir.create(dirname(OUTPUT_PATH), showWarnings = FALSE, recursive = TRUE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)
saveRDS(tiny_tib, OUTPUT_PATH)
write_csv(
  select(tiny_tib, -any_of(c("raw_response", "attempt_count", "prefilter_pass"))),
  str_replace(OUTPUT_PATH, "\\.rds$", ".csv")
)
write_csv(tiny_tib, here::here("data", "coke_pepsi_reddit_extracted.csv"))
message("Saved: ", OUTPUT_PATH)

# Sanity-check summary
n_total     <- nrow(tiny_tib)
n_complaint <- sum(tiny_tib$is_complaint == TRUE,  na.rm = TRUE)
share_comp  <- n_complaint / n_total
n_subj      <- sum(nchar(replace_na(tiny_tib$complaint_subject, "")) > 0)

cat(sprintf(paste(
  "\n── Extraction Sanity Check ─────────────────",
  "  Total rows:              %d",
  "  Complaints (is_complaint = TRUE): %d",
  "  Share complaints:        %.1f%%",
  "  Non-empty subject:       %d",
  "────────────────────────────────────────────\n",
  sep = "\n"),
  n_total, n_complaint, share_comp * 100, n_subj
))

# 8. Rows that still need a retry pass
retry_df <- results_final |> filter(status %in% c("api_error", "parse_error"))
if (nrow(retry_df) > 0) {
  saveRDS(retry_df, "results/retry_needed.rds")
  message(nrow(retry_df), " rows need retry — see results/retry_needed.rds")
}

# 9. Summary
elapsed_min <- as.numeric(difftime(Sys.time(), t_main, units = "mins"))
n_success   <- sum(tiny_tib$classification_status == "success",           na.rm = TRUE)
n_filtered  <- sum(tiny_tib$classification_status == "skipped_prefilter", na.rm = TRUE)
n_api_err   <- sum(tiny_tib$classification_status == "api_error",         na.rm = TRUE)
n_parse_err <- sum(tiny_tib$classification_status == "parse_error",       na.rm = TRUE)

message(sprintf(paste(
  "\n── Classification Summary ──────────────────",
  "  Total rows:         %d",
  "  Successful:         %d",
  "  Skipped (filter):   %d",
  "  API errors:         %d",
  "  Parse errors:       %d",
  "  Elapsed:            %.1f min  |  %.1f rows/min",
  "────────────────────────────────────────────",
  sep = "\n"),
  nrow(tiny_tib), n_success, n_filtered, n_api_err, n_parse_err,
  elapsed_min, n_success / max(elapsed_min, 1e-3)
))

# 10. Sample output — not all 52k
cat("\n── 20-row sample of successful classifications ──\n")
tiny_tib |>
  filter(classification_status == "success") |>
  select(text, group, is_complaint, complaint_subject) |>
  slice_sample(n = 20) |>
  print()

# 11. Clean up checkpoints to prevent stale data from affecting future runs
unlink(CHECKPOINT_DIR, recursive = TRUE)
message("Checkpoints cleared.")

# ══════════════════════════════════════════════════════════════════════════════
# OPTIONAL FALLBACK: Batch API
# Use when an overnight run is acceptable and 50% cost savings is the priority.
# Tradeoff: 1–24h completion window; no live progress after submission.
# Set run_batch_fallback <- TRUE to activate.
# ══════════════════════════════════════════════════════════════════════════════
run_batch_fallback <- FALSE

if (run_batch_fallback) {

  build_batch_jsonl <- function(df) {
    seq_len(nrow(df)) |>
      map_chr(\(i) toJSON(list(
        custom_id = paste0("row-", df$original_row_number[i]),
        method    = "POST",
        url       = "/v1/chat/completions",
        body      = list(
          model            = MODEL,
          temperature      = 0,
          max_tokens       = MAX_OUTPUT_TOKENS,
          response_format  = complaint_schema,
          messages         = list(
            list(role = "system", content = build_prompt(df$group[i])),
            list(role = "user",   content = df$text[i] |> str_squish() |> str_trunc(MAX_CHARS))
          )
        )
      ), auto_unbox = TRUE)) |>
      paste(collapse = "\n")
  }

  upload_batch_file <- function(jsonl_content) {
    tmp <- tempfile(fileext = ".jsonl")
    writeLines(jsonl_content, tmp, useBytes = TRUE)
    on.exit(unlink(tmp))
    request("https://api.openai.com/v1/files") |>
      req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
      req_body_multipart(purpose = "batch",
                         file    = curl::form_file(tmp, type = "application/jsonl")) |>
      req_perform() |> resp_body_json() |> _$id
  }

  submit_batch <- function(file_id) {
    request("https://api.openai.com/v1/batches") |>
      req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
      req_body_json(list(input_file_id     = file_id,
                         endpoint          = "/v1/chat/completions",
                         completion_window = "24h")) |>
      req_perform() |> resp_body_json() |> _$id
  }

  poll_batch <- function(batch_id, interval = 30) {
    repeat {
      obj <- request(paste0("https://api.openai.com/v1/batches/", batch_id)) |>
        req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
        req_perform() |> resp_body_json()
      message(sprintf("[%s] %s — %d/%d completed", format(Sys.time(), "%H:%M:%S"),
                      obj$status, obj$request_counts$completed %||% 0L,
                      obj$request_counts$total %||% 0L))
      if (obj$status == "completed") return(obj$output_file_id)
      if (obj$status %in% c("failed", "expired", "cancelled")) stop("Batch ", obj$status)
      Sys.sleep(interval)
    }
  }

  parse_batch_output <- function(output_file_id) {
    lines <- request(paste0("https://api.openai.com/v1/files/", output_file_id, "/content")) |>
      req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
      req_perform() |> resp_body_string() |>
      str_split_1("\n") |> keep(\(x) nchar(x) > 0)
    map(lines, \(line) {
      obj    <- fromJSON(line, simplifyVector = FALSE)
      orn    <- as.integer(str_remove(obj$custom_id, "row-"))
      parsed <- tryCatch(fromJSON(obj$response$body$choices[[1]]$message$content), error = \(e) NULL)
      list(
        original_row_number = orn,
        is_complaint        = isTRUE(parsed$is_complaint),
        complaint_subject   = parsed$complaint_subject %||% "",
        status              = if (is.null(parsed)) "parse_error" else "success",
        error_message       = NA_character_
      )
    }) |> bind_rows()
  }

  # Run in ≤ 50k chunks (Batch API per-submission limit)
  chunk_idxs <- split(seq_len(nrow(df_to_send)),
                      ceiling(seq_len(nrow(df_to_send)) / 50000))
  batch_results <- map_dfr(seq_along(chunk_idxs), \(k) {
    rows <- chunk_idxs[[k]]
    message(sprintf("Batch %d/%d: %d rows", k, length(chunk_idxs), length(rows)))
    file_id  <- build_batch_jsonl(df_to_send[rows, ]) |> upload_batch_file()
    batch_id <- submit_batch(file_id)
    message("Batch ID: ", batch_id)
    parse_batch_output(poll_batch(batch_id))
  })

  tiny_tib <- attach_results(tiny_tib, batch_results)
}


# Exercise 8

