# newsmap on smaller data set
rm(list = ls())
options(scipen = 6, digits = 4)

## Load Packages ####

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}
here()

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret, forecast, xts, cvAUC, httr2, jsonlite)

## Load Functions ####
getwd()
source(here("code", "functions.R"))

## Load Raw Data ####

# stopwords
stopwords <- import(here("data-raw", "stopwords.rds"))

# load data
load(here("data-final", "final_merged_data_master.RData"))
# 
# data <- final_merged_data_master[,c("body", "Protest")]
# data_isprotest <- data[data$Protest == 1,]
# data_isnot_protest <- data[data$Protest == 0,]
# data_isnot_protest_sample <- sample_n(data_isnot_protest, size = 460)
# data_downsample <- rbind(data_isprotest, data_isnot_protest_sample)
data_downsample <- rio::import(here("indices","jan28_indices", "data_downsampled.csv"))


# Create an empty list to store the train-test sets
data_list <- list()
test_list <- list()


# Split the data into k folds
#folds <- createFolds(data_downsample$Protest, k = k, list = TRUE)

# Read the folds into R
n <- 5
train_indices_list <- list()
test_indices_list <- list()
#predictions_list <- list()
#reference_list <- list()
# predictions_bert_list <- list()
for (i in 0:(n-1)) {
  train_indices_list[[i+1]] <- as.vector(t(read.csv(here("indices","jan28_indices",paste0("train_indices_fold_", i, ".csv")), header = FALSE)))
  train_indices_list[[i+1]] <- train_indices_list[[i+1]] + 1
  test_indices_list[[i+1]] <- as.vector(t(read.csv(here("indices","jan28_indices",paste0("test_indices_fold_", i, ".csv")), header = FALSE)))
  test_indices_list[[i+1]] <- test_indices_list[[i+1]] + 1
}

# For each fold, create a train-test set and add it to the list
for (i in 1:5) {
  # Get the indices for the training set
  train_indices <- train_indices_list[[i]]
  test_indices <- test_indices_list[[i]]
  # Create the training set
  train_set <- data_downsample[train_indices, ]
  
  # Create the test set
  test_set <- data_downsample[test_indices, ]
  test_list[i][[1]] <- test_set
}
##############################################################################################################
test_set_1 <- data.frame(test_list[1])
test_set_2 <- data.frame(test_list[2])
test_set_3 <- data.frame(test_list[3])
test_set_4 <- data.frame(test_list[4])
test_set_5 <- data.frame(test_list[5])

######Now starting the GPT analysis
Sys.getenv("OPENAI_API_KEY")
#########
library(httr)
library(glue)
library(jsonlite)
library(tibble)

######### 

article_classification <- function(article_body) {
  print(sprintf("Article body length before: %s", nchar(article_body)))
  article_body <- substr(article_body, 1, 4000) # extract the first 8000 characters which is about 8000/4=2000 tokens
  print(sprintf("Article body length after: %s", nchar(article_body)))
  
  # Create your prompt
  article_prompt <- glue::glue("Identify with 'yes' or 'no' whether the following article (delimited in XML tags) mentions a protest event.: <article> {article_body} </article>")
  
  # Build your request
  req <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers("Content-Type" = "application/json",
                "Authorization" = paste("Bearer", 
                                        Sys.getenv("OPENAI_API_KEY"))) |>
    req_body_json(
      list(
        "model" = "gpt-4",
        "messages" = list(
          list(
            "role" = "system",
            "content" = "You are a helpful assistant that annotates data for a living."
          ),
          list(
            "role" = "user",
            "content" = article_prompt
          )
        ),
        "temperature" = 0,
        "max_tokens" = 1
      )
    ) |>
    req_retry(max_tries = 3, backoff = ~ 5)
  
  # Perform your request
  resp <- req_perform(req, verbosity = 0)
  
  # Clean up the response
  pred <- resp_body_json(resp)$choices[[1]]$message$content
  
  print("*** Sleeping for 5s")
  Sys.sleep(5)
  
  # Save the response
  df <- tibble(
    body = article_body,
    gpt_pred = pred
  )
  
  return(df)
}

stop_phrases <- c(
  "The Indian Express is now on Telegram. Click here to join our channel (@indianexpress) and stay updated with the latest headlines For all the latest City Others News, download Indian Express App.",
  "Click here to join our official telegram channel (@nationalherald) and stay updated with the latest headlines",
  "Track Latest News Live on NDTV.com and get news updates from India and around the world.",
  "Watch Live News: Follow Us:",
  "................................ Advertisement ................................",
  "Powered by"
)
store_df <- list()
labelled_articles <- list()
for (i in seq_along(test_list)) {
  current_df <- data.frame(test_list[i])
  
  # Apply the article_classification function to each row
  results <- map_df(1:nrow(current_df), ~{
    row <- slice(current_df, .x)
    text <- pull(row, text)
    article_classification(text)
  })
  
  # Append the results to the labelled_articles list
  labelled_articles[[i]] <- results
}

first_fold_pred0 <- cbind.data.frame(test_set_1, labelled_articles)
save(first_fold_pred0, file = "first_fold_pred0_first.RData")
 
