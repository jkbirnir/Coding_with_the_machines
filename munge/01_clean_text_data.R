library(tidyverse)
library(textcat)

# Read in raw text data ---------------------------------------------------

train_raw <- read_csv(here::here("data-raw", "training_data_labelled.csv")) |>
  janitor::clean_names() |>
  select(id, title, body, protest)


# Create useful stop phrases list -----------------------------------------

stop_phrases <- c(
  "The Indian Express is now on Telegram. Click here to join our channel (@indianexpress) and stay updated with the latest headlines For all the latest City Others News, download Indian Express App.",
  "Click here to join our official telegram channel (@nationalherald) and stay updated with the latest headlines",
  "Track Latest News Live on NDTV.com and get news updates from India and around the world.",
  "Watch Live News: Follow Us:",
  "................................ Advertisement ................................"
)

# Clean raw text data -----------------------------------------------------

train_df <- train_raw |>
  mutate(
    body = str_remove_all(body, paste(stop_phrases, collapse = "|")),
    body = str_trim(body),
    est_n_tokens = nchar(body) / 4,
    lang = textcat(body)
  ) |>
  filter(
    # Filter out articles missing bodies
    est_n_tokens > 50,
    # Only keep English language articles
    lang == "english"
  ) |>
  drop_na(body, protest)

# Save cleaned data for model ---------------------------------------------

write_rds(train_df, here::here("data", "training_clean.rds"))
