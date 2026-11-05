# Reddit API Data Pull
# Requires: httr2, dplyr, jsonlite
# Setup: Create a Reddit app at https://www.reddit.com/prefs/apps
#   - Type: "script"
#   - Copy the client_id (under app name) and client_secret

library(httr2)
library(dplyr)
library(jsonlite)

# ── Credentials ──────────────────────────────────────────────────────────────
# Set these as environment variables (recommended) or replace directly
REDDIT_CLIENT_ID     <- Sys.getenv("REDDIT_CLIENT_ID",     unset = "YOUR_CLIENT_ID")
REDDIT_CLIENT_SECRET <- Sys.getenv("REDDIT_CLIENT_SECRET", unset = "YOUR_CLIENT_SECRET")
REDDIT_USER_AGENT    <- Sys.getenv("REDDIT_USER_AGENT",    unset = "r:reddit_research:v1.0 (by u/YOUR_USERNAME)")

# ── Authentication ────────────────────────────────────────────────────────────
get_reddit_token <- function(client_id, client_secret, user_agent) {
  resp <- request("https://www.reddit.com/api/v1/access_token") |>
    req_auth_basic(client_id, client_secret) |>
    req_headers(`User-Agent` = user_agent) |>
    req_body_form(grant_type = "client_credentials") |>
    req_perform()

  token <- resp |> resp_body_json()
  token$access_token
}

# ── Core fetch function ───────────────────────────────────────────────────────
# subreddit        : subreddit name without "r/" (e.g. "AskReddit")
# sort             : "hot", "new", "top", "rising"
# time             : for "top" only — "hour","day","week","month","year","all"
# n_posts          : total posts to retrieve (will paginate automatically, max 100/request)
# include_comments : if TRUE, fetches comments for every post and returns a named list
# comments_per_post: max comments to fetch per post (only used when include_comments = TRUE)
fetch_subreddit_posts <- function(subreddit,
                                  sort             = "hot",
                                  time             = "week",
                                  n_posts          = 100,
                                  include_comments = FALSE,
                                  comments_per_post = 200,
                                  token            = NULL,
                                  user_agent       = REDDIT_USER_AGENT) {
  if (is.null(token)) {
    token <- get_reddit_token(REDDIT_CLIENT_ID, REDDIT_CLIENT_SECRET, user_agent)
  }

  all_posts <- list()
  after     <- NULL
  fetched   <- 0

  repeat {
    batch_limit <- min(100, n_posts - fetched)
    if (batch_limit <= 0) break

    req <- request(glue::glue("https://oauth.reddit.com/r/{subreddit}/{sort}")) |>
      req_auth_bearer_token(token) |>
      req_headers(`User-Agent` = user_agent) |>
      req_url_query(
        limit  = batch_limit,
        t      = time,
        after  = after
      ) |>
      req_throttle(rate = 60 / 60)  # respect Reddit's 60 req/min limit

    resp <- req |> req_perform()
    data <- resp |> resp_body_json()

    posts <- data$data$children
    if (length(posts) == 0) break

    all_posts <- c(all_posts, posts)
    fetched   <- fetched + length(posts)
    after     <- data$data$after

    if (is.null(after) || fetched >= n_posts) break
  }

  posts_df <- parse_posts(all_posts, subreddit)

  if (!include_comments) return(posts_df)

  message("  Fetching comments for ", nrow(posts_df), " posts in r/", subreddit, " ...")
  comments_df <- purrr::map_dfr(posts_df$post_id, function(id) {
    fetch_post_comments(subreddit, id,
                        token      = token,
                        user_agent = user_agent,
                        limit      = comments_per_post)
  })

  list(posts = posts_df, comments = comments_df)
}

# ── Parser ────────────────────────────────────────────────────────────────────
parse_posts <- function(posts, subreddit) {
  if (length(posts) == 0) return(tibble())

  purrr::map_dfr(posts, function(p) {
    d <- p$data
    tibble(
      subreddit       = subreddit,
      post_id         = d$id %||% NA_character_,
      title           = d$title %||% NA_character_,
      author          = d$author %||% NA_character_,
      score           = d$score %||% NA_integer_,
      upvote_ratio    = d$upvote_ratio %||% NA_real_,
      num_comments    = d$num_comments %||% NA_integer_,
      url             = d$url %||% NA_character_,
      selftext        = d$selftext %||% NA_character_,
      is_self         = d$is_self %||% NA,
      created_utc     = as.POSIXct(d$created_utc %||% NA_real_,
                                   origin = "1970-01-01", tz = "UTC"),
      flair           = d$link_flair_text %||% NA_character_,
      permalink       = paste0("https://reddit.com", d$permalink %||% "")
    )
  })
}

# ── Fetch comments for a post ─────────────────────────────────────────────────
fetch_post_comments <- function(subreddit, post_id,
                                token      = NULL,
                                user_agent = REDDIT_USER_AGENT,
                                limit      = 200) {
  if (is.null(token)) {
    token <- get_reddit_token(REDDIT_CLIENT_ID, REDDIT_CLIENT_SECRET, user_agent)
  }

  resp <- request(glue::glue("https://oauth.reddit.com/r/{subreddit}/comments/{post_id}")) |>
    req_auth_bearer_token(token) |>
    req_headers(`User-Agent` = user_agent) |>
    req_url_query(limit = limit) |>
    req_perform()

  data <- resp |> resp_body_json()
  comments_raw <- data[[2]]$data$children

  purrr::map_dfr(comments_raw, function(c) {
    if (c$kind != "t1") return(NULL)
    d <- c$data
    tibble(
      post_id     = post_id,
      comment_id  = d$id %||% NA_character_,
      author      = d$author %||% NA_character_,
      body        = d$body %||% NA_character_,
      score       = d$score %||% NA_integer_,
      created_utc = as.POSIXct(d$created_utc %||% NA_real_,
                                origin = "1970-01-01", tz = "UTC")
    )
  })
}

# ── Convenience wrapper: pull multiple subreddits ─────────────────────────────
# Returns a flat data frame of posts when include_comments = FALSE.
# Returns a named list with $posts and $comments (both combined across subreddits)
# when include_comments = TRUE.
fetch_multiple_subreddits <- function(subreddits,
                                      include_comments  = FALSE,
                                      comments_per_post = 200,
                                      ...) {
  token <- get_reddit_token(REDDIT_CLIENT_ID, REDDIT_CLIENT_SECRET, REDDIT_USER_AGENT)

  results <- purrr::map(subreddits, function(sr) {
    message("Fetching r/", sr, " ...")
    fetch_subreddit_posts(sr,
                          include_comments  = include_comments,
                          comments_per_post = comments_per_post,
                          token             = token,
                          ...)
  })

  if (!include_comments) {
    return(purrr::map_dfr(results, identity))
  }

  list(
    posts    = purrr::map_dfr(results, "posts"),
    comments = purrr::map_dfr(results, "comments")
  )
}

# ── Save helper ───────────────────────────────────────────────────────────────
save_reddit_data <- function(df, prefix = "reddit", dir = "reddit_data") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  fname <- file.path(dir, paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  write.csv(df, fname, row.names = FALSE)
  message("Saved to: ", fname)
  invisible(fname)
}

# ══════════════════════════════════════════════════════════════════════════════
# PullPush.io — Historical Reddit Search
# No credentials required. Covers submissions and comments from 2005–present.
# API docs: https://pullpush.io
# ══════════════════════════════════════════════════════════════════════════════

PP_BASE_URL  <- "https://api.pullpush.io"
PP_PAGE_SIZE <- 100
PP_TRANSIENT <- c(429L, 500L, 502L, 503L, 504L)
PP_BACKOFFS  <- c(30, 60, 120, 240, 480, 600)

# Coerce a date argument to a Unix integer timestamp.
# Accepts: integer/numeric (passed through), Date, POSIXct, or "YYYY-MM-DD" string.
.as_utc_int <- function(x) {
  if (is.null(x))      return(NULL)
  if (is.numeric(x))   return(as.integer(x))
  if (inherits(x, "POSIXct")) return(as.integer(x))
  if (inherits(x, "Date"))    return(as.integer(as.POSIXct(as.character(x), tz = "UTC")))
  if (is.character(x)) return(as.integer(as.POSIXct(x, tz = "UTC")))
  stop("Unrecognised date type: ", class(x))
}

# Internal: fetch one page from a PullPush endpoint with retry/backoff.
.pp_fetch_page <- function(endpoint, query_params) {
  req <- request(paste0(PP_BASE_URL, endpoint)) |>
    req_url_query(!!!query_params) |>
    req_timeout(30)

  for (attempt in seq_along(PP_BACKOFFS)) {
    resp <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(resp, "error")) {
      status <- NA_integer_
      msg    <- conditionMessage(resp)
    } else {
      status <- resp_status(resp)
      msg    <- NULL
    }

    if (!inherits(resp, "error") && status == 200L) {
      parsed <- resp_body_string(resp) |> fromJSON(simplifyVector = FALSE)
      return(if (is.list(parsed$data)) parsed$data else list())
    }

    if (!inherits(resp, "error") && !(status %in% PP_TRANSIENT)) {
      warning(sprintf("Non-transient HTTP %d — skipping page.", status))
      return(NULL)
    }

    sleep_sec <- PP_BACKOFFS[min(attempt, length(PP_BACKOFFS))]
    if (is.na(status)) {
      message(sprintf("  [retry %d/6] request error (%s) — sleeping %ds", attempt, msg, sleep_sec))
    } else {
      message(sprintf("  [retry %d/6] HTTP %d — sleeping %ds", attempt, status, sleep_sec))
    }
    Sys.sleep(sleep_sec)
  }

  warning("All retries exhausted — skipping page.")
  NULL
}

# Internal: paginate through one PullPush endpoint for a single keyword.
.pp_pull_endpoint <- function(endpoint, q, after, before, subreddit, max_items) {
  rows   <- list()
  cursor <- after

  repeat {
    if (!is.null(max_items) && length(rows) >= max_items) break

    params <- list(
      q             = q,
      after         = cursor,
      before        = before,
      size          = PP_PAGE_SIZE,
      sort          = "asc",
      sort_type     = "created_utc"
    )
    if (!is.null(subreddit)) params$subreddit <- subreddit

    items <- .pp_fetch_page(endpoint, params)
    if (is.null(items) || length(items) == 0L) break

    rows     <- c(rows, items)
    last_utc <- as.integer(items[[length(items)]]$created_utc)
    if (is.null(last_utc) || last_utc <= cursor) break
    cursor <- last_utc

    Sys.sleep(1.5)
  }

  if (!is.null(max_items)) rows <- rows[seq_len(min(length(rows), max_items))]
  rows
}

# Internal: parse a list of raw submission items into a tibble.
.pp_parse_posts <- function(items, term, group) {
  if (length(items) == 0L) return(tibble())
  purrr::map_dfr(items, function(it) {
    tibble(
      type        = "post",
      id          = as.character(it$id          %||% NA_character_),
      subreddit   = as.character(it$subreddit   %||% NA_character_),
      author      = as.character(it$author      %||% NA_character_),
      title       = as.character(it$title       %||% ""),
      text        = as.character(it$selftext     %||% ""),
      score       = as.integer(it$score         %||% NA_integer_),
      num_comments= as.integer(it$num_comments  %||% 0L),
      url         = as.character(it$url         %||% NA_character_),
      permalink   = paste0("https://reddit.com", it$permalink %||% ""),
      created_utc = as.POSIXct(as.integer(it$created_utc %||% NA_real_),
                               origin = "1970-01-01", tz = "UTC"),
      term        = term,
      group       = group
    )
  })
}

# Internal: parse a list of raw comment items into a tibble.
.pp_parse_comments <- function(items, term, group) {
  if (length(items) == 0L) return(tibble())
  purrr::map_dfr(items, function(it) {
    tibble(
      type        = "comment",
      id          = as.character(it$id        %||% NA_character_),
      subreddit   = as.character(it$subreddit %||% NA_character_),
      author      = as.character(it$author    %||% NA_character_),
      title       = "",
      text        = as.character(it$body      %||% ""),
      score       = as.integer(it$score       %||% NA_integer_),
      num_comments= 0L,
      url         = NA_character_,
      permalink   = paste0("https://reddit.com", it$permalink %||% ""),
      created_utc = as.POSIXct(as.integer(it$created_utc %||% NA_real_),
                               origin = "1970-01-01", tz = "UTC"),
      term        = term,
      group       = group
    )
  })
}

# ── fetch_pullpush() ──────────────────────────────────────────────────────────
# Search historical Reddit data via PullPush.io.
#
# Parameters
#   terms      : character vector of search keywords (each queried separately)
#   after      : start of date range — Date / POSIXct / "YYYY-MM-DD" / Unix int
#   before     : end of date range  — same formats as above
#   type       : "posts", "comments", or "both" (default)
#   subreddit  : optional subreddit to restrict the search (no "r/")
#   groups     : optional named list mapping group labels to term subsets;
#                if NULL all terms get group = NA_character_
#   max_items  : optional per-term cap on items fetched (NULL = no cap)
#   dedup      : remove duplicate ids within each type (default TRUE)
#
# Returns a tibble when type is "posts" or "comments"; a named list
# with $posts and $comments when type = "both".
fetch_pullpush <- function(terms,
                           after,
                           before,
                           type      = "both",
                           subreddit = NULL,
                           groups    = NULL,
                           max_items = NULL,
                           dedup     = TRUE) {
  type  <- match.arg(type, c("both", "posts", "comments"))
  after  <- .as_utc_int(after)
  before <- .as_utc_int(before)

  # Build a lookup from term → group label
  term_group <- if (!is.null(groups)) {
    unlist(lapply(names(groups), function(g) setNames(rep(g, length(groups[[g]])), groups[[g]])))
  } else {
    setNames(rep(NA_character_, length(terms)), terms)
  }

  endpoints <- list()
  if (type %in% c("both", "posts"))
    endpoints[["posts"]]    <- list(ep = "/reddit/search/submission/", parser = .pp_parse_posts)
  if (type %in% c("both", "comments"))
    endpoints[["comments"]] <- list(ep = "/reddit/search/comment/",    parser = .pp_parse_comments)

  all_posts    <- list()
  all_comments <- list()

  for (term in terms) {
    grp <- unname(term_group[term])
    if (is.na(grp)) grp <- NA_character_

    for (ep_name in names(endpoints)) {
      ep     <- endpoints[[ep_name]]
      message(sprintf("  [pullpush | %s | %s]", term, ep_name))
      items  <- .pp_pull_endpoint(ep$ep, term, after, before, subreddit, max_items)
      df     <- ep$parser(items, term = term, group = grp)
      message(sprintf("    -> %d items", nrow(df)))

      if (ep_name == "posts")    all_posts[[length(all_posts) + 1L]]       <- df
      if (ep_name == "comments") all_comments[[length(all_comments) + 1L]] <- df
    }
  }

  combine <- function(lst) {
    out <- bind_rows(lst)
    if (dedup && nrow(out) > 0) out <- distinct(out, id, type, .keep_all = TRUE)
    out
  }

  if (type == "posts")    return(combine(all_posts))
  if (type == "comments") return(combine(all_comments))

  list(posts = combine(all_posts), comments = combine(all_comments))
}

# ── Example usage ─────────────────────────────────────────────────────────────
if (FALSE) {
  # ── Posts only (no comments) ──────────────────────────────────────────────
  posts <- fetch_subreddit_posts(
    subreddit        = "AskReddit",
    sort             = "top",
    time             = "week",
    n_posts          = 200,
    include_comments = FALSE
  )
  save_reddit_data(posts, prefix = "askreddit_posts")

  # ── Posts + comments for a single subreddit ───────────────────────────────
  data <- fetch_subreddit_posts(
    subreddit         = "AskReddit",
    sort              = "top",
    time              = "week",
    n_posts           = 50,
    include_comments  = TRUE,
    comments_per_post = 100        # cap comments per post
  )
  # data$posts    — data frame of posts
  # data$comments — data frame of all comments, joined by post_id
  save_reddit_data(data$posts,    prefix = "askreddit_posts")
  save_reddit_data(data$comments, prefix = "askreddit_comments")

  # ── Multiple subreddits, posts only ──────────────────────────────────────
  multi_posts <- fetch_multiple_subreddits(
    subreddits       = c("AskReddit", "todayilearned", "worldnews"),
    sort             = "new",
    n_posts          = 100,
    include_comments = FALSE
  )
  save_reddit_data(multi_posts, prefix = "multi_subreddits")

  # ── Multiple subreddits, posts + comments ─────────────────────────────────
  multi_data <- fetch_multiple_subreddits(
    subreddits        = c("AskReddit", "todayilearned"),
    sort              = "top",
    time              = "month",
    n_posts           = 50,
    include_comments  = TRUE,
    comments_per_post = 50
  )
  save_reddit_data(multi_data$posts,    prefix = "multi_posts")
  save_reddit_data(multi_data$comments, prefix = "multi_comments")

  # ── PullPush: keyword search, posts + comments, date range ────────────────
  hist_data <- fetch_pullpush(
    terms  = c("coke zero", "cokezero"),
    after  = "2017-01-01",
    before = "2018-01-01",
    type   = "both"
  )
  save_reddit_data(hist_data$posts,    prefix = "pp_posts")
  save_reddit_data(hist_data$comments, prefix = "pp_comments")

  # ── PullPush: treatment/control groups, posts only, subreddit filter ──────
  hist_posts <- fetch_pullpush(
    terms     = c("coke zero", "cokezero", "pepsi zero", "pepsi max"),
    after     = "2021-01-01",
    before    = "2022-01-01",
    type      = "posts",
    subreddit = "soda",
    groups    = list(
      treatment = c("coke zero", "cokezero"),
      control   = c("pepsi zero", "pepsi max")
    ),
    max_items = 500
  )
  save_reddit_data(hist_posts, prefix = "pp_soda_posts")

  # ── PullPush: comments only, Unix timestamp input ─────────────────────────
  hist_comments <- fetch_pullpush(
    terms  = c("coca cola zero"),
    after  = as.POSIXct("2020-06-01", tz = "UTC"),
    before = as.POSIXct("2020-12-31", tz = "UTC"),
    type   = "comments"
  )
  save_reddit_data(hist_comments, prefix = "pp_comments_2020")
}
