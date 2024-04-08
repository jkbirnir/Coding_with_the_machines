library(tidyverse)
library(here)
library(textcat)
library(janitor)


# Read in raw text data ---------------------------------------------------
set.seed(123)
data <- read.csv(here("data","texas_coded_data.csv")) |>
    janitor::clean_names() |>
    select(id, title, body, protest)


# Create useful stop phrases list -----------------------------------------

stop_phrases <- c(
    "The Indian Express is now on Telegram. Click here to join our channel (@indianexpress) and stay updated with the latest headlines For all the latest City Others News, download Indian Express App.",
    "Click here to join our official telegram channel (@nationalherald) and stay updated with the latest headlines",
    "Track Latest News Live on NDTV.com and get news updates from India and around the world.",
    "Watch Live News: Follow Us:",
    "................................ Advertisement ................................",
    "Powered by"
)

# Clean raw text data -----------------------------------------------------

train_df <- data |>
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

table(train_df$protest) #Now, we have 425 not protests and 384 protests

#Now, we create data for GPT analysis

#Since we do not want to have a lot of tokens in our sample data in order to minimize cost,
#let's look at the average token we have per article. We will consider the mean number of tokens 
#and the median number of tokens. We will filter the data based on whichever is lower. If mean is lower,
#we will select data that has tokens that are lower than or equal to the mean number of token. If the 
#median is lower, we will use median to filter the data

summary(train_df$est_n_tokens) #median is 832 and mean is 1084. We will go with the median.

#Let's filter the data

#filter the data
positive_protest <- train_df |>
    filter(protest=="1" & est_n_tokens <=832)

index <- sample(nrow(positive_protest), size = 50, replace = F)

positive_protest <- positive_protest[index,] |>
    drop_na()

#do the same for the 0s
negative_protest <- train_df |>
    filter(protest!="1" & est_n_tokens <=832)

index1 <- sample(nrow(negative_protest), size = 50, replace = F)

negative_protest <- negative_protest[index1,] |>
    drop_na()

#Combine the data

final_data_gpt <- rbind.data.frame(positive_protest, negative_protest)
table(final_data_gpt$protest) #50 1s and 50 0s
sum(final_data_gpt$est_n_tokens)# we have a total of 50526 number of tokens

# save data as a csv
# Henry did this for running on BERT too

protest_short <- bind_rows(negative_protest, positive_protest)
protest_short <- protest_short %>% select(protest, body) %>% 
  mutate(labels = protest, text = body) %>% select(labels, body)

train_df <- train_df %>% select(protest, body) %>% 
  mutate(labels = protest, text = body) %>% select(labels, body)

train_df <- anti_join(train_df,protest_short)

rio::export(protest_short, here("data-final","100_short_articles_testing.csv"))
rio::export(train_df, here("data-final","training_data_for_short_article_testing.csv"))
#The total number of prompts we have is 50526. Our prompt is about 80 words. 
#When we add the tokens on our end and calculate the computational cost, the cost
#comes out to be about $1.60. GPT may calculate this differently. 

# Save cleaned data for model ---------------------------------------------

save(final_data_gpt, file = "final_sample_gpt.RData")

# Try this on downsampled data

data <- read.csv(here("data","data_downsampled.csv")) |>
  janitor::clean_names()


# Create useful stop phrases list -----------------------------------------

stop_phrases <- c(
  "The Indian Express is now on Telegram. Click here to join our channel (@indianexpress) and stay updated with the latest headlines For all the latest City Others News, download Indian Express App.",
  "Click here to join our official telegram channel (@nationalherald) and stay updated with the latest headlines",
  "Track Latest News Live on NDTV.com and get news updates from India and around the world.",
  "Watch Live News: Follow Us:",
  "................................ Advertisement ................................",
  "Powered by"
)

# Clean raw text data -----------------------------------------------------

train_df <- data |>
  mutate(
    body = str_remove_all(text, paste(stop_phrases, collapse = "|")),
    body = str_trim(text),
    est_n_tokens = nchar(text) / 4,
    lang = textcat(text)
  ) |>
  filter(
    # Filter out articles missing bodies
    est_n_tokens > 50,
    # Only keep English language articles
    lang == "english"
  ) |>
  drop_na(text, labels)

table(train_df$labels) #Now, we have 425 not protests and 384 protests

#Now, we create data for GPT analysis

#Since we do not want to have a lot of tokens in our sample data in order to minimize cost,
#let's look at the average token we have per article. We will consider the mean number of tokens 
#and the median number of tokens. We will filter the data based on whichever is lower. If mean is lower,
#we will select data that has tokens that are lower than or equal to the mean number of token. If the 
#median is lower, we will use median to filter the data

summary(train_df$est_n_tokens) #median is 832 and mean is 1084. We will go with the median.

#Let's filter the data

#filter the data
positive_protest <- train_df |>
  filter(labels=="1" & est_n_tokens <=832) %>% 
  drop_na()

#do the same for the 0s
negative_protest <- train_df |>
  filter(labels!="1" & est_n_tokens <=832) %>% 
  drop_na() %>% 
  sample_n(.,size = 199, replace = FALSE)

feb_1_data_downsampled_cleaned <- rbind(positive_protest, negative_protest)

export(feb_1_data_downsampled_cleaned, here("oja_gpt", "feb_1_data_downsampled_cleaned.csv"))
       