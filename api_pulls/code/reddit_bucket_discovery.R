library(tidyverse)
library(httr2)
library(jsonlite)
library(here)

# ── Guards ─────────────────────────────────────────────────────────────────────
input_path  <- here("data", "coke_pepsi_reddit_extracted.csv")
output_path <- here("data", "coke_pepsi_reddit_buckets_meta.json")

if (!file.exists(input_path))
  stop("Input file not found: ", input_path)

api_key <- Sys.getenv("OPENAI_API_KEY")
if (nchar(api_key) == 0)
  stop("OPENAI_API_KEY environment variable is not set.")

# ── Load and filter ────────────────────────────────────────────────────────────
message("Loading ", input_path, " ...")
extracted <- read_csv(input_path, show_col_types = FALSE)

missing_cols <- setdiff(c("group", "is_complaint", "complaint_subject"), names(extracted))
if (length(missing_cols) > 0)
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))

phrases <- extracted |>
  filter(group == "treatment", is_complaint == TRUE) |>
  pull(complaint_subject) |>
  str_trim() |>
  discard(\(x) is.na(x) | x == "") |>
  unique()

if (length(phrases) == 0)
  stop("No complaint subjects found after filtering. Check your input data.")

message(sprintf("Unique complaint_subject phrases: %s", format(length(phrases), big.mark = ",")))

# ── JSON schema ────────────────────────────────────────────────────────────────
bucket_schema <- list(
  type = "json_schema",
  json_schema = list(
    name   = "bucket_response",
    strict = TRUE,
    schema = list(
      type       = "object",
      properties = list(
        buckets = list(
          type  = "array",
          items = list(
            type                 = "object",
            properties           = list(
              bucket_name      = list(type = "string"),
              short_definition = list(type = "string")
            ),
            required             = list("bucket_name", "short_definition"),
            additionalProperties = FALSE
          )
        )
      ),
      required             = list("buckets"),
      additionalProperties = FALSE
    )
  )
)

# ── Prompt ─────────────────────────────────────────────────────────────────────
system_prompt <- paste(
  "You are an expert in consumer sentiment analysis.",
  "Your task is to propose approximately 16 complaint bucket categories",
  "that cover the provided Coke Zero complaint phrases.",
  "",
  "Rules for buckets:",
  "- Each bucket must substantively and thematically group multiple raw terms.",
  "- Buckets must be mutually exclusive and clearly distinct from one another.",
  "- Avoid buckets that combine distinct issues (e.g. 'housing and employment issues').",
  "- Avoid vague catch-all buckets like 'miscellaneous', 'singleton', 'general', 'unclear', or 'other'.",
  "- Prefer descriptive, specific, directional bucket names.",
  "- It is better to make a bucket slightly too specific than too vague.",
  "- Buckets should be useful headline categories for an executive deck.",
  "",
  "Return only the bucket schema — do not classify individual phrases.",
  sep = "\n"
)

user_message <- paste0(
  "Here are the unique Coke Zero complaint phrases (one per line):\n\n",
  paste(phrases, collapse = "\n"),
  "\n\nPropose approximately 16 mutually exclusive complaint buckets."
)

# ── API call ───────────────────────────────────────────────────────────────────
message("Sending ", format(length(phrases), big.mark = ","),
        " phrases to gpt-5.5 (reasoning_effort: high) ...")

resp <- tryCatch(
  request("https://api.openai.com/v1/chat/completions") |>
    req_auth_bearer_token(api_key) |>
    req_body_json(list(
      model            = "gpt-5.5",
      reasoning_effort = "high",
      response_format  = bucket_schema,
      messages         = list(
        list(role = "system", content = system_prompt),
        list(role = "user",   content = user_message)
      )
    )) |>
    req_timeout(300) |>
    req_perform(),
  error = \(e) stop("API request failed: ", conditionMessage(e))
)

if (resp_status(resp) != 200L)
  stop(sprintf("API returned HTTP %d:\n%s", resp_status(resp), resp_body_string(resp)))

# ── Parse response ─────────────────────────────────────────────────────────────
raw_content <- tryCatch(
  resp |> resp_body_json() |> _$choices[[1]]$message$content,
  error = \(e) stop("Failed to extract content from API response: ", conditionMessage(e))
)

parsed <- tryCatch(
  fromJSON(raw_content, simplifyVector = FALSE),
  error = \(e) stop("Failed to parse JSON from model response. Raw:\n", raw_content)
)

buckets <- parsed$buckets
if (is.null(buckets) || length(buckets) == 0)
  stop("Model response contained no buckets. Raw response:\n", raw_content)

message(sprintf("Model returned %d buckets:", length(buckets)))
for (b in buckets)
  message(sprintf("  • %-40s %s", b$bucket_name, b$short_definition))

# ── Save output ────────────────────────────────────────────────────────────────
output <- list(
  model                          = "gpt-5.5",
  reasoning_effort               = "high",
  source_file                    = "data/coke_pepsi_reddit_extracted.csv",
  filters                        = list(group = "treatment", is_complaint = TRUE),
  unique_complaint_subject_count = length(phrases),
  created_at                     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  buckets                        = buckets
)

dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
write(toJSON(output, pretty = TRUE, auto_unbox = TRUE), output_path)
message("Saved: ", output_path)
