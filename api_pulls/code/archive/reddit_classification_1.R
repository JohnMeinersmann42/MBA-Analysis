library(tidyverse)
library(httr2)
library(jsonlite)

# ── Data: all three files (15 rows) ──────────────────────────────────────────
combined <- bind_rows(
  read_csv("data/improvements.csv"),
  read_csv("data/suggested_improvements.csv"),
  read_csv("data/suggested_changes-JohnM.csv")
) |>
  mutate(bucket = case_when(
    str_detect(suggested_changes, regex(
      "concurren|parallel|furrr|future_map|multisession|async.?IO|sustained throughput",
      ignore_case = TRUE)) ~ "Concurrency & Speed",
    str_detect(suggested_changes, regex(
      "increment.*checkpoint|checkpoint.*disk|resume.*logic|write partial|recoverable pipeline",
      ignore_case = TRUE)) ~ "Checkpointing & Resume",
    str_detect(suggested_changes, regex(
      "system message|input token|output token|cap.*token|token cost|shorten.{0,20}prompt|reduce input",
      ignore_case = TRUE)) ~ "Prompt & Token Optimization",
    str_detect(suggested_changes, regex(
      "progress.?bar|cost estimate|prompt_tokens|validate.*parsed|deduplic",
      ignore_case = TRUE)) ~ "Monitoring & Validation",
    TRUE ~ "Error Handling & Retries"
  )) |>
  select(suggested_changes, team_names, bucket)

# ── pick_best: one API call per bucket ───────────────────────────────────────
# Builds a strict enum schema from the exact suggestion strings in the bucket
# so the model can only return a value that is already in the list.
pick_best <- function(bucket_name, suggestions) {
  schema <- list(
    type = "json_schema",
    json_schema = list(
      name   = "best_recommendation",
      strict = TRUE,
      schema = list(
        type       = "object",
        properties = list(
          best_suggestion = list(
            type = "string",
            enum = as.list(suggestions)   # as.list() forces JSON array even for length-1
          )
        ),
        required             = list("best_suggestion"),
        additionalProperties = FALSE
      )
    )
  )

  prompt <- str_c(
    "We are building an industrial-scale API classification pipeline that must process ",
    "approximately 52,000 Reddit posts by sending each one to OpenAI's chat completions endpoint. ",
    "The key engineering concerns are: (1) wall-clock runtime — we need to finish in under one hour, ",
    "meaning roughly 15 completed rows per second sustained; (2) network timeouts and hung connections ",
    "that can stall the pipeline mid-run; (3) parallel request management so we can send many requests ",
    "concurrently without exceeding rate limits or crashing the session; (4) robustness to mid-run ",
    "failures — a crash at row 40,000 should not lose all prior work; (5) wasted API credit from ",
    "unnecessary retries or inefficient prompts.\n\n",
    "The following ", length(suggestions), " suggestion(s) all relate to the theme '", bucket_name, "'. ",
    "Select the single suggestion that would most directly help us address these concerns at scale:\n\n",
    str_c(seq_along(suggestions), ". ", suggestions, collapse = "\n\n")
  )

  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
    req_body_json(list(
      model            = "gpt-5",
      reasoning_effort = "high",
      response_format  = schema,
      messages         = list(
        list(role = "user", content = prompt)
      )
    )) |>
    req_perform()

  resp |>
    resp_body_json() |>
    _$choices[[1]]$message$content |>
    fromJSON() |>
    _$best_suggestion
}

# ── One call per bucket; join best back as a logical column ───────────────────
best_by_bucket <- combined |>
  group_by(bucket) |>
  summarise(best = pick_best(bucket[1], suggested_changes), .groups = "drop")

combined <- combined |>
  left_join(best_by_bucket, by = "bucket") |>
  mutate(best_recommendation = suggested_changes == best) |>
  select(suggested_changes, team_names, bucket, best_recommendation)

combined 
