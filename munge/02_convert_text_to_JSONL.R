library(tidyverse)
library(jsonlite)

# Read in cleaned text data -----------------------------------------------

train_df <- read_rds(here::here("data", "training_clean.rds"))

# Prepare formatted prompt data -------------------------------------------
# More information can be found here: https://platform.openai.com/docs/guides/fine-tuning/preparing-your-dataset

rect_df <- train_df |>
  mutate(
    prompt = paste(body, "\n\n###\n\n"),
    completion = if_else(protest == 1, " yes\n", " no\n"),
    est_n_tokens = (nchar(prompt) + nchar(completion)) / 4
  ) |>
  filter(est_n_tokens <= 2048) |>
  select(prompt, completion)

rect_df <- rect_df |> slice(1:10)

json_df <- toJSON(x = rect_df, dataframe = "rows")
json_df

# Save JSON object for model ----------------------------------------------

writeLines(json_df, here::here("data", "training_cleaned.json"))
