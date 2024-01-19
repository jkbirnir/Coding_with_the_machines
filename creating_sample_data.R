###Creating a balanced dataset for GPT model
library("tidyverse")
#set seed

set.seed(123)

#load the data

coded_data <- read.csv("texas_coded_data.csv")

#filter the data
positive_protest <- coded_data |>
  filter(Protest=="1")

index <- sample(nrow(positive_protest), size = 170, replace = F)

positive_protest <- positive_protest[index,] |>
  drop_na()

#do the same for the 0s
negative_protest <- coded_data |>
  filter(Protest!="1")

index1 <- sample(nrow(negative_protest), size = 170, replace = F)

negative_protest <- negative_protest[index,] |>
  drop_na()

#combine the dataframes

balanced_data_gpt <- rbind.data.frame(positive_protest, negative_protest)

#let's check whether the protest variable is balanced:

table(balanced_data_gpt$Protest)
#it is!

#now, let's save the dataset

balanced_data_gpt <- save(balanced_data_gpt, file = "coded_data_for_gpt.RData")



