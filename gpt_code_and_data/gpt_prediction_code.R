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
  
  #train <- corpus(train_set, text_field = "text")
  
  #test <- corpus(test_set, text_field = "text")
  
  #tokstrain <- clean_corpus(train, stopwords)
  
  #tokstrain <- tokens_compound(tokstrain, pattern = compounds)
  
  #dfmtrain <- dfm(tokstrain)
  
  #test <- corpus(test_set, text_field = "text")
  
  #tokstest <- clean_corpus(test, stopwords)
  
  #tokstest <- tokens_compound(tokstest, pattern = compounds)
  
  #dfmtest <- dfm(tokstest)

  #trainLookup <- dfm_lookup(dfmtrain, dictionary = postprocDict)
  
  #model <- textmodel_newsmap(dfmtrain, trainLookup)
  
  #preds <- predict(model, newdata = dfmtest, confidence = TRUE)
  
  #preds <- as_tibble(preds)
  
  #preds <- transform(preds, class_fixed = ifelse(confidence.fit < 0.5, 0, class))
  
  #preds <- transform(preds, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))
  
  #preds <- preds %>% mutate(class_fixed = as.factor(class_fixed))
  
  #prediction <- preds[ , 3]
  
  #prediction <- as.factor(prediction)
  
  #prediction <- if_else(is.na(prediction), "0", prediction)
  
  #prediction <- as.factor(prediction)
  
  #reference <- as.factor(test_set$labels)
  
  #results <- caret::confusionMatrix(prediction, reference, positive = "1")
  
  #classResults <- results[["byClass"]]
  
  #models[[i]] <- model
  #results_k[[i]] <- results
  data_list[i][[1]] <- train_set
  test_list[i][[1]] <- test_set
  #predictions_list[[i]] <- prediction
  #reference_list[[i]] <- reference
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
  article_prompt <- glue::glue("Classify protest events based on contextual cues. Consider keywords like protest, demonstration, rally, strike, march, and sit in. Protests could be violent or symbolic forms of resistance. Examine contextual information such as location, participants (groups, organizations, activists, advocacy groups, specific communities), event date, and time. Check for motivations, demands, grievances, and the presence of law enforcement when large groups gather for a cause. Use 'yes' or 'no' to indicate if the article describes a protest event.: <article> {article_body} </article>")
  
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
  current_df <- data.frame(test_list[i])|>
    mutate(
      text = str_remove_all(text, paste(stop_phrases, collapse = "|")),
      text = str_trim(text),
      est_n_tokens = nchar(text) / 4)
  store_df[[i]] <- current_df
  
  # Apply the article_classification function to each row
  results <- map_df(1:nrow(current_df), ~{
    row <- slice(current_df, .x)
    text <- pull(row, text)
    article_classification(text)
  })
  
  # Append the results to the labelled_articles list
  labelled_articles[[i]] <- results
}
 
#try without the loop for the first set of prediction
#test_set_1
labelled_articles <- map(
  1:182, 
  ~ test_set_1 |>
    slice(.x) |>
    pull(text) |>
    article_classification()
) |> 
  bind_rows()
first_fold_pred0 <- cbind.data.frame(test_set_1, labelled_articles)
save(first_fold_pred0, file = "first_fold_pred0_first.RData")
#test_set_2
labelled_articles2 <- map(
  1:182, 
  ~ test_set_2 |>
    slice(.x) |>
    pull(text) |>
    article_classification()
) |> 
  bind_rows()
first_fold_pred1 <- cbind.data.frame(test_set_2, labelled_articles2)
save(first_fold_pred1, file = "first_fold_pred1_first.RData")

#test_set_3
labelled_articles3 <- map(
  1:182, 
  ~ test_set_3 |>
    slice(.x) |>
    pull(text) |>
    article_classification()
) |> 
  bind_rows()
first_fold_pred2 <- cbind.data.frame(test_set_3, labelled_articles3)
save(first_fold_pred2, file = "first_fold_pred2_first.RData")

#test_set_4

labelled_articles4 <- map(
  1:181, 
  ~ test_set_4 |>
    slice(.x) |>
    pull(text) |>
    article_classification()
) |> 
  bind_rows()
first_fold_pred3 <- cbind.data.frame(test_set_4, labelled_articles4)
save(first_fold_pred3, file = "first_fold_pred3_first.RData")

#test_set_5
labelled_articles5 <- map(
  1:181, 
  ~ test_set_5 |>
    slice(.x) |>
    pull(text) |>
    article_classification()
) |> 
  bind_rows()
first_fold_pred4 <- cbind.data.frame(test_set_5, labelled_articles5)
save(first_fold_pred4, file = "first_fold_pred4_first.RData")


#######################################################################################################

