library(tidyverse)
library(jsonlite)
library(glue)
#install.packages("httr2")
library(httr2)
library(readr)
library(purrr)



# Read in cleaned text data -----------------------------------------------


train_df <- load("final_sample_gpt.RData")


#----------------------------------------------------------------------------------------#

#Creating a function for automation #####

article_classification <- function(article_body) {
  
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
        "temperature" = 2,
        "max_tokens" = 1
      )
    ) |>
    req_retry(max_tries = 3)
  
  # Perform your request
  resp <- req_perform(req)
  
  Sys.sleep(0.4)
  
  # Clean up the response
  pred <- resp_body_json(resp)$choices[[1]]$message$content
  
  Sys.sleep(0.1)
  
  # Save the response
  df <- tibble(
    body = article_body,
    gpt_pred = pred
  )
  
  return(df)
  
}

labelled_articles <- map(
  1:100, 
  ~ final_data_gpt |>
    slice(.x) |>
    pull(body) |>
    article_classification()
) |> 
  bind_rows()



first_pred2 <- labelled_articles |>
  rename("gpt_pred1_temp2" = "gpt_pred")

save(first_pred2, file = "gptpred_first_2.RData")

labelled_articles2 <- map(
  1:100, 
  ~ final_data_gpt |>
    slice(.x) |>
    pull(body) |>
    article_classification()
) |> 
  bind_rows()



second_pred2 <- labelled_articles2 |>
  rename("gpt_pred2_temp2" = "gpt_pred")



save(second_pred2, file = "gptpred_second_2.RData")

labelled_articles3 <- map(
  1:100, 
  ~ final_data_gpt |>
    slice(.x) |>
    pull(body) |>
    article_classification()
) |> 
  bind_rows()



third_pred2 <- labelled_articles3 |>
  rename("gpt_pred3_temp2" = "gpt_pred")



save(third_pred2, file = "gptpred_third_2.RData")


#------------------------------------------------------------------------------------------------#



#------------------------------------------------------------------------###########  
