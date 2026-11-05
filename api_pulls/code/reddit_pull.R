library(httr2)
library(jsonlite)
library(dplyr)
library(readr)

output_path <- "C:/Users/johnm/OneDrive/Documents/Claude/Projects/AIML - 451 Work/api_pulls/data/coke_pepsi_reddit_full_pull.csv"

start_time <- Sys.time()

# --- Study design ---------------------------------------------------------
# Two event windows capture distinct Coke Zero brand moments; Pepsi Zero/Max
# serves as the control because it had no major changes in either period.
# Unix timestamps are required by the PullPush API's after/before parameters.

windows <- list(
  `2017_rebrand`      = list(after = as.integer(as.POSIXct("2017-01-01", tz = "UTC")),
                              before = as.integer(as.POSIXct("2018-01-01", tz = "UTC"))),
  `2021_reformulation` = list(after = as.integer(as.POSIXct("2021-01-01", tz = "UTC")),
                               before = as.integer(as.POSIXct("2022-01-01", tz = "UTC")))
)

# Multiple spellings are included because users write them inconsistently;
# each variant is queried separately and deduplicated at the end.
terms <- list(
  treatment = c("coke zero", "cokezero", "coca cola zero"),
  control   = c("pepsi zero", "pepsizero", "pepsi max", "pepsimax")
)

# --- API constants --------------------------------------------------------
BASE_URL   <- "https://api.pullpush.io"
PAGE_SIZE  <- 100                               # max items per request
TRANSIENT  <- c(429L, 500L, 502L, 503L, 504L)  # HTTP codes worth retrying
BACKOFFS   <- c(30, 60, 120, 240, 480, 600)     # seconds to wait on each retry attempt

# Fetch one page of results from a PullPush endpoint.
# Returns a list of raw item objects, or NULL if the request ultimately fails.
# Sorting ascending by created_utc lets us paginate by advancing `after`
# to the timestamp of the last item on each page.
fetch_page <- function(endpoint, q, after, before) {
  req <- request(paste0(BASE_URL, endpoint)) |>
    req_url_query(q = q, after = after, before = before,
                  size = PAGE_SIZE, sort = "asc", sort_type = "created_utc") |>
    req_timeout(30)

  for (attempt in seq_along(BACKOFFS)) {
    # Capture network errors as values so we can handle them uniformly below
    resp <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(resp, "error")) {
      status <- NA_integer_
      msg    <- conditionMessage(resp)
    } else {
      status <- resp_status(resp)
      msg    <- NULL
    }

    # Success: parse and return the data array
    if (!inherits(resp, "error") && status == 200L) {
      body   <- resp_body_string(resp)
      parsed <- fromJSON(body, simplifyVector = FALSE)
      items  <- if (is.list(parsed$data)) parsed$data else list()
      return(items)
    }

    # Non-transient error (e.g. 404): retrying won't help, skip this page
    if (!inherits(resp, "error") && !(status %in% TRANSIENT)) {
      warning(sprintf("Non-transient HTTP %d for q=%s — skipping page.", status, q))
      return(NULL)
    }

    # Transient error or network failure: wait before retrying
    sleep_sec <- BACKOFFS[min(attempt, length(BACKOFFS))]
    Sys.sleep(sleep_sec)
  }

  warning(sprintf("All retries exhausted for q=%s — skipping page.", q))
  NULL
}

# Convert a raw submission (post) object into a one-row tibble.
# selftext holds the post body; title is blank for comments so we use "" here.
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

# Convert a raw comment object into a one-row tibble.
# Comments have no title and no child-comment count in this API response.
extract_comment <- function(item, term, group, window_name) {
  tibble(
    id          = as.character(item$id %||% NA),
    type        = "comment",
    subreddit   = as.character(item$subreddit %||% NA),
    author      = as.character(item$author %||% NA),
    title       = "",
    text        = as.character(item$body %||% ""),
    score       = as.integer(item$score %||% NA),
    comments    = 0L,
    created_utc = as.integer(item$created_utc %||% NA),
    term        = term,
    group       = group,
    window      = window_name
  )
}

# NULL-coalescing helper: return x if non-NULL, otherwise y
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Page through an endpoint until no new results are returned.
# Pagination works by advancing `after` to the last item's timestamp on
# each page; the loop exits when the API returns an empty page or repeats
# the same cursor (which would cause an infinite loop).
pull_endpoint <- function(endpoint, extractor, q, after_start, before_end,
                          term, group, window_name) {
  rows  <- list()
  after <- after_start

  repeat {
    items <- fetch_page(endpoint, q, after, before_end)

    if (is.null(items) || length(items) == 0L) break

    rows <- c(rows, lapply(items, extractor, term = term,
                           group = group, window_name = window_name))

    # Advance the cursor; stop if the API returns a stale or missing timestamp
    last_utc <- items[[length(items)]]$created_utc
    if (is.null(last_utc) || as.integer(last_utc) <= after) break
    after <- as.integer(last_utc)

    Sys.sleep(0.5)  # reduced from 1.5s; rate-limit backoff handles spikes
  }

  if (length(rows) == 0L) return(tibble())
  bind_rows(rows)
}

# Execute one job spec and return a tibble of collected rows.
run_job <- function(job) {
  extractor_fn <- if (job$label == "posts") extract_post else extract_comment
  df <- pull_endpoint(
    endpoint    = job$endpoint,
    extractor   = extractor_fn,
    q           = job$term,
    after_start = job$after,
    before_end  = job$before,
    term        = job$term,
    group       = job$group,
    window_name = job$window
  )
  cat(sprintf("  done [%s | %s | %s] -> %d rows\n", job$window, job$term, job$label, nrow(df)))
  df
}

# --- Build flat job list --------------------------------------------------
# Each combination of window × group × term × content type becomes one job.
# Flattening first makes it straightforward to distribute across workers.

endpoints <- list(
  list(endpoint = "/reddit/search/submission/", label = "posts"),
  list(endpoint = "/reddit/search/comment/",    label = "comments")
)

jobs <- list()
for (wname in names(windows)) {
  wparams <- windows[[wname]]
  for (grp in names(terms)) {
    for (term in terms[[grp]]) {
      for (ep in endpoints) {
        jobs[[length(jobs) + 1L]] <- list(
          window   = wname,
          after    = wparams$after,
          before   = wparams$before,
          group    = grp,
          term     = term,
          endpoint = ep$endpoint,
          label    = ep$label
        )
      }
    }
  }
}

cat(sprintf("Running %d jobs sequentially...\n", length(jobs)))

# --- Sequential execution -------------------------------------------------
all_data <- lapply(jobs, run_job)

# --- Output ---------------------------------------------------------------
# Deduplicate on (id, type) because the same post/comment can match multiple
# search terms (e.g. a post mentioning both "coke zero" and "cokezero").

combined <- bind_rows(all_data) |>
  distinct(id, type, .keep_all = TRUE)

write_csv(combined, output_path)
cat(sprintf("\nWrote %d deduplicated rows to:\n  %s\n", nrow(combined), output_path))

cat("\nCount summary (window x group x type):\n")
combined |>
  count(window, group, type) |>
  as.data.frame() |>
  print()

elapsed <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nTotal wall-clock time: %.1f minutes\n", as.numeric(elapsed)))
