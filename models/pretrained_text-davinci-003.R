library(tidyverse)
library(httr2)
library(glue)

# Read in training data ---------------------------------------------------

train_df <- read_rds(here::here("data", "training_clean.rds"))

# Identify articles discussing protest events -----------------------------

gpt_identify_protests <- function(body) {

  prompt <- glue("Does the following text mention a protest event? {body}")

  req <- request("https://api.openai.com/v1/completions")

  resp <- req |>
    req_headers("Content-Type" = "application/json", "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))) |>
    req_body_json(
      list(
        "model" = "text-davinci-003",
        "prompt" = prompt,
        "max_tokens" = 3,
        "temperature" = 0
      )
    ) |>
    req_perform()

  print(glue("Completing: {prompt}"))

  print(glue("Status: {resp_status(resp)}"))

  label <- str_remove_all(resp_body_json(resp)$choices[[1]]$text, "\\n")

  return(label)

}

labelled_df <- train_df |>
  mutate(resp = map_chr(body, gpt_identify_protests))

labelled_df
