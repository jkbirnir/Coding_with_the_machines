#Load the required packages

library(tidyverse)
library(httr2)
library(dplyr)
library(caret)
library(jsonlite)
library(caret)

#Load the clean data with 394 observations

cleanest_data_engineered_prompt <- read.csv("feb_2_downsampled_cleaned.csv")

table(cleanest_data_engineered_prompt$labels)
#We have 192 protests and 189 not protests
#Let's make sure that we have equal number

set.seed(123)

negative_protest <- cleanest_data_engineered_prompt|>
  filter(labels=="0")

positive_protest <- cleanest_data_engineered_prompt|>
  filter(labels=="1")

index <- sample(nrow(positive_protest), size = 189, replace = F)
positive_protest <- positive_protest[index,]|>
  drop_na(body)

#merge it to a new dataset for analysis

cleanest_data_balanced_merged <- rbind.data.frame(negative_protest, positive_protest)
#check whether your data is balanced
table(cleanest_data_balanced_merged$labels) #Yes, it is!


#Now, let's run a gpt model multiple times to measure model performance
Sys.setenv("OPENAI_API_KEY"="XXXXXXXXXXXXXXXXXXXXXXXX")
Sys.getenv("OPENAI_API_KEY")

#Engineered prompt

cleanest_data_balanced_merged$body_proper <- stri_trans_general(cleanest_data_balanced_merged$body, "latin-ascii")
cleanest_data_balanced_merged <- cleanest_data_balanced_merged|>
  select(labels, body_proper, text)

cleanest_data_balanced_merged<- cleanest_data_balanced_merged|>
  rename("body" = "body_proper",
         "text_uncleaned" = "text")
save(cleanest_data_balanced_merged, file ="cleanest_data_balanced_merged_cleantext378.RData")


article_classification <- function(article_body) {

  
  # Create your prompt
  article_prompt <- glue::glue("Classify protest events based on contextual cues. Consider keywords like protest,  demonstration,  rally,  strike, march, and sit in. Protests could be violent or symbolic forms of resistance. Examine contextual information such as location, participants (groups, organizations, activists, advocacy groups, specific communities), event date, and time. Check for motivations, demands, grievances, and the presence of law enforcement when large groups gather for a cause. Use `yes' or `no' to indicate if the following article describes a protest event.: <article> {article_body} </article>")
  
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
  
  print("*** Sleeping for 2s")
  Sys.sleep(5)
  
  # Save the response
  df <- tibble(
    body = article_body,
    gpt_pred_engineered = pred
  )
  
  return(df)
}

labelled_articles <- map(
  1:378, 
  ~ cleanest_data_balanced_merged |>
    slice(.x) |>
    pull(body_proper) |>
    article_classification()
) |> 
  bind_rows()

engineered_clean_prediction <- labelled_articles

###########Now, unengineered prompt

article_classification <- function(article_body) {
  
  
  # Create your prompt
  article_prompt <- glue::glue("Identify with `yes' or `no' whether the following article (delimited in XML tags) mentions a protest event: <article>{article_body}</article>")
  
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
  
  print("*** Sleeping for 2s")
  Sys.sleep(5)
  
  # Save the response
  df <- tibble(
    body = article_body,
    gpt_pred_unengineered = pred
  )
  
  return(df)
}

labelled_articles_unengineered <- map(
  1:378, 
  ~ cleanest_data_balanced_merged |>
    slice(.x) |>
    pull(body_proper) |>
    article_classification()
) |> 
  bind_rows()
unengineered_clean_prediction <- labelled_articles_unengineered


#####Let's combine the dataset for plots

combined_clean_prediction <- cbind.data.frame(cleanest_data_balanced_merged$body, cleanest_data_balanced_merged$labels,
                                              engineered_clean_prediction$gpt_pred_engineered, unengineered_clean_prediction$gpt_pred_unengineered)

##########Now let's make confusion matrix

combined_clean_prediction$gpt_binary_engineered <- as.factor(ifelse(combined_clean_prediction$`engineered_clean_prediction$gpt_pred_engineered`=="Yes", 1, 0))
combined_clean_prediction$labels <- as.factor(combined_clean_prediction$`cleanest_data_balanced_merged$labels`)
combined_clean_prediction$gpt_binary_unengineered <- as.factor(ifelse(combined_clean_prediction$`unengineered_clean_prediction$gpt_pred_unengineered`=="Yes", 1, 0))
combined_clean_prediction$body <- combined_clean_prediction$`cleanest_data_balanced_merged$body`
combined_clean_gpt_prediction_feb2 <- combined_clean_prediction |>
  select(body, labels, gpt_binary_engineered, gpt_binary_unengineered)

save(combined_clean_gpt_prediction_feb2, file = "combined_clean_feb2_gptpred_engineered_unengineered.RData")


cm_engineered <- confusionMatrix(combined_clean_gpt_prediction_feb2$gpt_binary_engineered, reference = combined_clean_gpt_prediction_feb2$labels)

matrix_engineered_clean378 <- data.frame(cm_engineered$byClass)
colnames(matrix_engineered_clean378) <- "Scores"


cm_unengineered <- confusionMatrix(combined_clean_gpt_prediction_feb2$gpt_binary_unengineered, reference = combined_clean_gpt_prediction_feb2$labels)

matrix_unengineered_clean378 <- data.frame(cm_unengineered$byClass)
colnames(matrix_unengineered_clean378) <- "Scores"
cm_unengineered$byClass

write.csv(matrix_engineered_clean378, file = "cm_engineered_clean378_feb2.csv")
write.csv(matrix_unengineered_clean378, file = "cm_unengineered_clean378_feb2.csv")

