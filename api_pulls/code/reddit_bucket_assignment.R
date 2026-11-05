library(tidyverse)
library(httr2)
library(jsonlite)
library(here)

# ══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# ══════════════════════════════════════════════════════════════════════════════
MAX_CONCURRENT  <- 8
MAX_RETRIES     <- 5
REQ_TIMEOUT_SEC <- 30
CHECKPOINT_PATH <- here("data", "coke_pepsi_reddit_bucket_assignments_checkpoint.csv")
OUTPUT_PATH     <- here("data", "coke_pepsi_reddit_bucketed.csv")
LOG_EVERY       <- 100
CKPT_EVERY      <- 500

# ── Guards ─────────────────────────────────────────────────────────────────────
extracted_path <- here("data", "coke_pepsi_reddit_extracted.csv")
buckets_path   <- here("data", "coke_pepsi_reddit_buckets_meta.json")

if (!file.exists(extracted_path)) stop("Missing input: ", extracted_path)
if (!file.exists(buckets_path))   stop("Missing input: ", buckets_path)

api_key <- Sys.getenv("OPENAI_API_KEY")
if (nchar(api_key) == 0) stop("OPENAI_API_KEY environment variable is not set.")

# ── Load inputs ────────────────────────────────────────────────────────────────
message("Loading extracted data...")
extracted <- read_csv(extracted_path, show_col_types = FALSE)

missing_cols <- setdiff(c("group", "is_complaint", "complaint_subject"), names(extracted))
if (length(missing_cols) > 0)
  stop("Missing columns in extracted CSV: ", paste(missing_cols, collapse = ", "))

message("Loading bucket metadata...")
bucket_meta <- tryCatch(
  fromJSON(buckets_path, simplifyVector = FALSE),
  error = \(e) stop("Failed to parse bucket metadata: ", conditionMessage(e))
)

if (is.null(bucket_meta$buckets) || length(bucket_meta$buckets) == 0)
  stop("Bucket metadata JSON is missing or has no buckets.")

bucket_names <- map_chr(bucket_meta$buckets, "bucket_name")
bucket_defs  <- map_chr(bucket_meta$buckets, "short_definition")
message(sprintf("Loaded %d buckets.", length(bucket_names)))

# ── Extract unique phrases ─────────────────────────────────────────────────────
all_phrases <- extracted |>
  filter(group == "treatment", is_complaint == TRUE) |>
  pull(complaint_subject) |>
  str_trim() |>
  discard(\(x) is.na(x) | x == "") |>
  unique()

if (length(all_phrases) == 0)
  stop("No complaint subjects found after filtering. Check your input data.")

message(sprintf("Unique complaint phrases: %s", format(length(all_phrases), big.mark = ",")))

# ── Resume from checkpoint ─────────────────────────────────────────────────────
if (file.exists(CHECKPOINT_PATH)) {
  prior_ckpt   <- read_csv(CHECKPOINT_PATH, show_col_types = FALSE)
  done_phrases <- prior_ckpt |> filter(!is.na(complaint_bucket)) |> pull(complaint_subject)
  phrases_todo <- setdiff(all_phrases, done_phrases)
  message(sprintf("Resuming: %s done, %s remaining",
                  format(length(done_phrases), big.mark = ","),
                  format(length(phrases_todo), big.mark = ",")))
} else {
  prior_ckpt   <- tibble(complaint_subject = character(), complaint_bucket = character())
  phrases_todo <- all_phrases
  message("No checkpoint found — starting fresh.")
}

# ── Schema and prompt ──────────────────────────────────────────────────────────
assignment_schema <- list(
  type = "json_schema",
  json_schema = list(
    name   = "bucket_assignment",
    strict = TRUE,
    schema = list(
      type       = "object",
      properties = list(
        complaint_bucket = list(
          type = "string",
          enum = as.list(bucket_names)
        )
      ),
      required             = list("complaint_bucket"),
      additionalProperties = FALSE
    )
  )
)

system_prompt <- paste0(
  "You classify Coke Zero complaint phrases into exactly one of the following buckets:\n\n",
  paste(map2_chr(bucket_names, bucket_defs, \(n, d) sprintf("- %s: %s", n, d)),
        collapse = "\n"),
  "\n\nAssign the single best-matching bucket. If a phrase could fit multiple buckets, ",
  "choose the most specific and thematically dominant one."
)

# ── Request builder ────────────────────────────────────────────────────────────
`%||%` <- function(x, y) if (!is.null(x)) x else y

build_request <- function(phrase) {
  request("https://api.openai.com/v1/chat/completions") |>
    req_auth_bearer_token(api_key) |>
    req_body_json(list(
      model            = "gpt-5.4-mini",
      reasoning_effort = "low",
      response_format  = assignment_schema,
      messages         = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = phrase)
      )
    )) |>
    req_timeout(REQ_TIMEOUT_SEC) |>
    req_retry(
      max_tries    = MAX_RETRIES,
      is_transient = \(resp) resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L),
      backoff      = \(attempt) min(2^attempt + runif(1, 0, 1), 30),
      after        = \(resp) NA
    )
}

parse_assignment <- function(resp, phrase) {
  if (inherits(resp, "error")) {
    warning(sprintf("API error for phrase '%s': %s", phrase, conditionMessage(resp)))
    return(tibble(complaint_subject = phrase, complaint_bucket = NA_character_))
  }

  raw <- tryCatch(
    resp |> resp_body_json() |> _$choices[[1]]$message$content,
    error = \(e) NULL
  )
  if (is.null(raw))
    return(tibble(complaint_subject = phrase, complaint_bucket = NA_character_))

  parsed <- tryCatch(fromJSON(raw), error = \(e) NULL)
  bucket <- parsed$complaint_bucket %||% NULL

  if (is.null(bucket) || !bucket %in% bucket_names) {
    warning(sprintf("Invalid bucket '%s' for phrase: %s", bucket %||% "NULL", phrase))
    return(tibble(complaint_subject = phrase, complaint_bucket = NA_character_))
  }

  tibble(complaint_subject = phrase, complaint_bucket = bucket)
}

# ══════════════════════════════════════════════════════════════════════════════
# MAIN: classify in parallel mini-batches
# ══════════════════════════════════════════════════════════════════════════════
n_todo      <- length(phrases_todo)
results     <- list()
t_start     <- Sys.time()
batch_idxs  <- split(seq_len(n_todo), ceiling(seq_len(n_todo) / MAX_CONCURRENT))

message(sprintf("Classifying %s phrases with MAX_CONCURRENT = %d ...",
                format(n_todo, big.mark = ","), MAX_CONCURRENT))

for (bi in seq_along(batch_idxs)) {
  idx   <- batch_idxs[[bi]]
  batch <- phrases_todo[idx]
  reqs  <- map(batch, build_request)

  resps         <- req_perform_parallel(reqs, on_error = "return")
  batch_results <- map2(resps, batch, parse_assignment)
  results       <- c(results, batch_results)

  done <- length(results)

  # Progress log
  if (done %% LOG_EVERY < MAX_CONCURRENT || bi == length(batch_idxs)) {
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    rate    <- done / max(elapsed, 1)
    eta     <- Sys.time() + (n_todo - done) / max(rate, 1e-3)
    message(sprintf("[%s] %s/%s phrases | %.1f/sec | ETA %s",
                    format(Sys.time(), "%H:%M:%S"),
                    format(done,   big.mark = ","),
                    format(n_todo, big.mark = ","),
                    rate,
                    format(eta, "%H:%M:%S")))
  }

  # Checkpoint
  if (done %% CKPT_EVERY < MAX_CONCURRENT || bi == length(batch_idxs)) {
    bind_rows(prior_ckpt, bind_rows(results)) |>
      distinct(complaint_subject, .keep_all = TRUE) |>
      write_csv(CHECKPOINT_PATH)
  }

  Sys.sleep(0.1)
}

# ── Build phrase lookup ────────────────────────────────────────────────────────
phrase_lookup <- bind_rows(prior_ckpt, bind_rows(results)) |>
  distinct(complaint_subject, .keep_all = TRUE)

n_assigned <- sum(!is.na(phrase_lookup$complaint_bucket))
message(sprintf("\nDone. %s / %s phrases assigned to a bucket.",
                format(n_assigned, big.mark = ","),
                format(nrow(phrase_lookup), big.mark = ",")))

# ── Join back to full dataset and save ────────────────────────────────────────
bucketed <- extracted |>
  left_join(phrase_lookup, by = "complaint_subject")

write_csv(bucketed, OUTPUT_PATH)
message("Saved: ", OUTPUT_PATH)

# ── Summary ────────────────────────────────────────────────────────────────────
n_total      <- nrow(bucketed)
n_complaints <- sum(bucketed$group == "treatment" & isTRUE(bucketed$is_complaint), na.rm = TRUE)
n_bucketed   <- sum(!is.na(bucketed$complaint_bucket))

cat(sprintf("\n── Summary ──────────────────────────────────────────\n"))
cat(sprintf("Total rows:              %s\n", format(n_total,      big.mark = ",")))
cat(sprintf("Coke Zero complaints:    %s\n", format(n_complaints, big.mark = ",")))
cat(sprintf("Non-missing bucket:      %s\n", format(n_bucketed,   big.mark = ",")))
cat("\nCount by complaint_bucket (descending):\n")
bucketed |>
  filter(!is.na(complaint_bucket)) |>
  count(complaint_bucket, sort = TRUE) |>
  print(n = Inf)
