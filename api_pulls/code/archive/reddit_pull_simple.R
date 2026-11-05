library(httr2)
library(jsonlite)
library(dplyr)
library(readr)

`%||%` <- function(x, y) if (!is.null(x)) x else y

output_path <- "C:/Users/johnm/OneDrive/Documents/Claude/Projects/AIML - 451 Work/api_pulls/data/coke_pepsi_reddit_all.csv"

start_time <- Sys.time()

windows <- list(
  `2021_reformulation` = list(after = as.integer(as.POSIXct("2021-06-01", tz = "UTC")),
                               before = as.integer(as.POSIXct("2021-09-01", tz = "UTC")))
)

terms <- list(
  treatment = c("coca cola zero"),
  control   = c("pepsimax")
)

BASE_URL   <- "https://api.pullpush.io"
PAGE_SIZE  <- 100
TRANSIENT  <- c(429L, 500L, 502L, 503L, 504L)
BACKOFFS   <- c(10, 20, 40, 80, 160, 300)

fetch_page <- function(endpoint, q, after, before) {
  req <- request(paste0(BASE_URL, endpoint)) |>
    req_url_query(q = q, after = after, before = before,
                  size = PAGE_SIZE, sort = "asc", sort_type = "created_utc") |>
    req_timeout(30)

  for (attempt in seq_along(BACKOFFS)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(resp, "error")) {
      status <- NA_integer_
      msg    <- conditionMessage(resp)
    } else {
      status <- resp_status(resp)
      msg    <- NULL
    }

    if (!inherits(resp, "error") && status == 200L) {
      body <- resp_body_string(resp)
      parsed <- fromJSON(body, simplifyVector = FALSE)
      items  <- if (is.list(parsed$data)) parsed$data else list()
      return(items)
    }

    if (!inherits(resp, "error") && !(status %in% TRANSIENT)) {
      warning(sprintf("Non-transient HTTP %d for q=%s — skipping page.", status, q))
      return(NULL)
    }

    sleep_sec <- BACKOFFS[min(attempt, length(BACKOFFS))]
    if (!is.na(status) && status == 429L) {
      cat(sprintf("  [retry %d/6] 429 rate-limit — sleeping %ds\n", attempt, sleep_sec))
    } else if (!is.na(status)) {
      cat(sprintf("  [retry %d/6] HTTP %d — sleeping %ds\n", attempt, status, sleep_sec))
    } else {
      cat(sprintf("  [retry %d/6] request error (%s) — sleeping %ds\n", attempt, msg, sleep_sec))
    }
    Sys.sleep(sleep_sec)
  }

  warning(sprintf("All retries exhausted for q=%s — skipping page.", q))
  NULL
}

extract_post <- function(item, term, group, window_name) {
  tibble(
    id          = as.character(item$id %||% NA),
    type        = "post",
    subreddit   = as.character(item$subreddit %||% NA),
    author      = as.character(item$author %||% NA),
    title       = as.character(item$title %||% ""),
    text        = as.character(item$selftext %||% ""),
    score       = as.integer(item$score %||% NA),
    comments    = as.integer(item$num_comments %||% 0L),
    created_utc = as.integer(item$created_utc %||% NA),
    term        = term,
    group       = group,
    window      = window_name
  )
}

pull_endpoint <- function(endpoint, extractor, q, after_start, before_end,
                          term, group, window_name) {
  rows  <- list()
  after <- after_start

  repeat {
    items <- fetch_page(endpoint, q, after, before_end)

    if (is.null(items) || length(items) == 0L) break

    rows <- c(rows, lapply(items, extractor, term = term,
                             group = group, window_name = window_name))

    last_utc <- items[[length(items)]]$created_utc
    if (is.null(last_utc) || as.integer(last_utc) <= after) break
    after <- as.integer(last_utc)

    Sys.sleep(0.3)
  }

  total <- length(rows)
  cat(sprintf("    -> %d items\n", total))

  if (total == 0L) return(tibble())
  bind_rows(rows)
}

all_data <- list()
window_names <- names(windows)

for (wi in seq_along(window_names)) {
  wname   <- window_names[wi]
  wparams <- windows[[wname]]
  cat(sprintf("\n=== Window: %s ===\n", wname))

  for (grp in names(terms)) {
    for (term in terms[[grp]]) {
      cat(sprintf("  [%s | %s | posts]\n", wname, term))
      df <- pull_endpoint(
        endpoint    = "/reddit/search/submission/",
        extractor   = extract_post,
        q           = term,
        after_start = wparams$after,
        before_end  = wparams$before,
        term        = term,
        group       = grp,
        window_name = wname
      )
      all_data[[length(all_data) + 1L]] <- df
    }
  }
}

non_empty <- Filter(function(df) nrow(df) > 0L, all_data)
if (length(non_empty) == 0L) stop("All queries returned zero results — nothing to write.")

combined <- bind_rows(non_empty) |>
  distinct(id, .keep_all = TRUE)

write_csv(combined, output_path)
cat(sprintf("\nWrote %d deduplicated rows to:\n  %s\n", nrow(combined), output_path))

cat("\nCount summary (window x group):\n")
combined |>
  count(window, group) |>
  as.data.frame() |>
  print()

elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nTotal wall-clock time: %.1f minutes\n", as.numeric(elapsed)))
