# newsmap on smaller data set
rm(list = ls())
options(scipen = 6, digits = 4)

## Load Packages ####

if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
}

pacman::p_load(rio, tidyverse, here, quanteda, newsmap, caret, forecast, xts, cvAUC)

## Load Functions ####

source(here("code","functions.R"))

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
data_downsample <- rio::import(here("data-final","data_downsampled.csv"))

final_merged_data_master %>% filter(body %in% data_downsample$text)
compounds <- c(
  "african american*", "black lives matter", "black american*", "black *man", "black *men",
  "verbal opposition", "opposition speech", "codemn speech*", "public letter*",
  "court action", "sign petition*", "destruct property", "property destruct*", "traffic block*"
) # creating set of multiword tokens

# create 5 cvs
postprocDict <- dictionary(file = here("dictionaries", "single_prot_dict.yaml"), format = "YAML")

# Set the seed for reproducibility
set.seed(123)

# Create an empty list to store the train-test sets
data_list <- list()
models <- list()
results_k <- list()

# Split the data into k folds
#folds <- createFolds(data_downsample$Protest, k = k, list = TRUE)

# Read the folds into R
n <- 5
train_indices_list <- list()
test_indices_list <- list()
predictions_list <- list()
reference_list <- list()
predictions_bert_list <- list()
for (i in 0:(n-1)) {
  train_indices_list[[i+1]] <- as.vector(t(read.csv(here("indices", paste0("train_indices_fold_", i, ".csv")), header = FALSE)))
  train_indices_list[[i+1]] <- train_indices_list[[i+1]] + 1
  test_indices_list[[i+1]] <- as.vector(t(read.csv(here("indices", paste0("test_indices_fold_", i, ".csv")), header = FALSE)))
  test_indices_list[[i+1]] <- test_indices_list[[i+1]] + 1
  predictions_bert_list[i+1] <- as.vector((read.csv(here("indices", paste0("predictions_fold_",i+1,".csv")), header = TRUE)))
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
  
  train <- corpus(train_set, text_field = "text")
  
  test <- corpus(test_set, text_field = "text")
  
  tokstrain <- clean_corpus(train, stopwords)
  
  tokstrain <- tokens_compound(tokstrain, pattern = compounds)
  
  dfmtrain <- dfm(tokstrain)
  
  test <- corpus(test_set, text_field = "text")
  
  tokstest <- clean_corpus(test, stopwords)
  
  tokstest <- tokens_compound(tokstest, pattern = compounds)
  
  dfmtest <- dfm(tokstest)

  trainLookup <- dfm_lookup(dfmtrain, dictionary = postprocDict)
  
  model <- textmodel_newsmap(dfmtrain, trainLookup)
  
  preds <- predict(model, newdata = dfmtest, confidence = TRUE)
  
  preds <- as_tibble(preds)
  
  preds <- transform(preds, class_fixed = ifelse(confidence.fit < 0.5, 0, class))
  
  preds <- transform(preds, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))
  
  preds <- preds %>% mutate(class_fixed = as.factor(class_fixed))
  
  prediction <- preds[ , 3]
  
  prediction <- as.factor(prediction)
  
  prediction <- if_else(is.na(prediction), "0", prediction)
  
  prediction <- as.factor(prediction)
  
  reference <- as.factor(test_set$labels)
  
  results <- caret::confusionMatrix(prediction, reference, positive = "1")
  
  classResults <- results[["byClass"]]
  
  models[[i]] <- model
  results_k[[i]] <- results
  data_list[i][[1]] <- train_set
  data_list[i][[2]] <- test_set
  predictions_list[[i]] <- prediction
  reference_list[[i]] <- reference
}

prec <- c()
rec <- c()
f1 <- c()
for (i in 1:length(results_k)) {
  prec <- c(prec, results_k[[i]]$byClass[["Precision"]])
  rec <- c(rec, results_k[[i]]$byClass[["Recall"]])
  f1 <- c(f1, results_k[[i]]$byClass[["F1"]])
}

round(mean(prec),3)
round(mean(rec),3)
round(mean(f1),3)

#The BERT results were generated in the BERT_AMAR_analysis.ipynb file
#For convenience, results were copied here
#For reference as to how performance metrics were generated, see above file

bertprec <- c(0.8217, 0.8717, 0.7817, 0.7674, 0.7015)
bertrec <- c(0.80769, 0.8571, 0.7637, 0.7403, 0.6576)
bertf1 <- c(0.8056, 0.8558, 0.7600, 0.7332, 0.6385)

round(mean(bertprec),3)
round(mean(bertrec),3)
round(mean(bertf1),3)

predictions_list[[1]]
reference_list[[1]]
library(pROC)
plot(roc(reference_list[[1]], as.numeric(predictions_list[[1]])), col = "red")
for (i in seq_along(predictions_list)[-1]) {
  lines(roc(reference_list[[i]], as.numeric(predictions_list[[i]])), col = i)
}

for (i in 1:5) {
  predictions_list[[i]] <- as.numeric(predictions_list[[i]])
  reference_list[[i]] <- as.numeric(reference_list[[i]])
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

load(here("gpt_code_and_data",
          "gpt_final_datafold",
          "fold_1_merged_gptpred.RData"))
load(here("gpt_code_and_data",
          "gpt_final_datafold",
          "fold_2_merged_gptpred.RData"))
load(here("gpt_code_and_data",
          "gpt_final_datafold",
          "fold_3_merged_gptpred.RData"))
load(here("gpt_code_and_data",
          "gpt_final_datafold",
          "fold_4_merged_gptpred.RData"))
load(here("gpt_code_and_data",
          "gpt_final_datafold",
          "fold_5_merged_gptpred.RData"))

predictions_gpt_list <- list()

fold_1_merged$gpt_modal <- apply(fold_1_merged[, c("gpt_pred", "gpt_pred1", "gpt_pred3")], 1, Mode)
fold_2_merged$gpt_modal <- apply(fold_2_merged[, c("gpt_pred", "gpt_pred1", "gpt_pred3")], 1, Mode)
fold3_merged$gpt_modal <- apply(fold3_merged[, c("gpt_pred", "gpt_pred1", "gpt_pred3")], 1, Mode)
fold4_merged$gpt_modal <- apply(fold4_merged[, c("gpt_pred", "gpt_pred1", "gpt_pred3")], 1, Mode)
fold5_merged$gpt_modal <- apply(fold5_merged[, c("gpt_pred", "gpt_pred1", "gpt_pred3")], 1, Mode)
gpt_preds <- list(fold_1_merged, fold_2_merged, fold3_merged, fold4_merged, fold5_merged)


for (i in 1:5){
  prediction_data <- gpt_preds[[i]]
  prediction_data$gpt_modal <- apply(prediction_data[, c("gpt_pred", "gpt_pred1", "gpt_pred3")], 1, Mode)
  prediction_data <- prediction_data %>% mutate(gpt_modal = if_else(gpt_modal == "Yes", 1, 0))
  predictions_gpt_list[[i]] <- as.vector(prediction_data$gpt_modal)
}
gpt_preds$gpt_modal <- apply(prediction_data[, c("gpt_pred", "gpt_pred1", "gpt_pred3")], 1, Mode)

xval <- list(predictions_list, reference_list)
out <- cvAUC(xval[[1]], xval[[2]])
xvalbert <- list(predictions_bert_list, reference_list)
outbert <- cvAUC(xvalbert[[1]], xvalbert[[2]])
xvalgpt <- list(predictions_gpt_list, reference_list)
outgpt <- cvAUC(xvalgpt[[1]], xvalgpt[[2]])

par(mfrow = c(1,3))
plot(out$perf, col = "black", lty = 3, main = "", sub = "Newsmap")
plot(out$perf, col ="red", avg = "vertical", add = TRUE)
plot(outbert$perf, col = "black", lty = 3, main = "", sub = "BERT")
plot(outbert$perf, col ="red", avg = "vertical", add = TRUE)
plot(outgpt$perf, col = "black", lty = 3, main = "", sub = "GPT-4")
plot(outgpt$perf, col ="red", avg = "vertical", add = TRUE)
mtext("5-Fold CV AUC", side = 3, outer = TRUE, line = -2, cex = 1.5)
dev.off()

## graphs and analysis on 100 articles
# load the pre-cleaned test data
load(here("gpt_code_and_data",
          "unengineered_gpt_sample100.RData"))

load(here("gpt_code_and_data",
          "final_sample_gpt.RData"))
# Read in raw text data ---------------------------------------------------
data <- read.csv(here("data","texas_coded_data.csv")) |>
  janitor::clean_names() |>
  select(id, title, body, protest)

train_df <- anti_join(data, final_data_gpt)

# Load BERT predictions
bert_short_predictions <- rio::import(here("indices", "predictions_short_articles.csv"))

# Load BERT answers
bert_short_answers <- rio::import(here("data-final", "100_short_articles_testing.csv"))

gpt_short_predictions <- ifelse(unengineered_sample_prediction_merged$gpt_pred == "Yes", 1, 0)

# Newsmap

train <- corpus(train_df, text_field = "body")

test <- corpus(final_data_gpt, text_field = "body")

tokstrain <- clean_corpus(train, stopwords)

tokstrain <- tokens_compound(tokstrain, pattern = compounds)

dfmtrain <- dfm(tokstrain)

tokstest <- clean_corpus(test, stopwords)

tokstest <- tokens_compound(tokstest, pattern = compounds)

dfmtest <- dfm(tokstest)

trainLookup <- dfm_lookup(dfmtrain, dictionary = postprocDict)

model <- textmodel_newsmap(dfmtrain, trainLookup)

preds <- predict(model, newdata = dfmtest, confidence = TRUE)

preds <- as_tibble(preds)

preds <- transform(preds, class_fixed = ifelse(confidence.fit < 0.5, 0, class))

preds <- transform(preds, class_fixed = ifelse(class_fixed == 2, 0, class_fixed))

preds <- preds %>% mutate(class_fixed = as.factor(class_fixed))

prediction <- preds[ , 3]

prediction <- as.factor(prediction)

prediction <- if_else(is.na(prediction), "0", prediction)

prediction <- as.factor(prediction)

reference <- as.factor(final_data_gpt$protest)

results <- caret::confusionMatrix(prediction, reference, positive = "1")

classResults <- results[["byClass"]]

# cleaned data auc

newsmap_list <- list(as.numeric(prediction), final_data_gpt$protest)

s_newsmap_out <- cvAUC(newsmap_list[[1]],newsmap_list[[2]])

bert_list <- list(bert_short_predictions, bert_short_answers$labels)

s_bert_out <- cvAUC(bert_list[[1]], bert_list[[2]])

gpt_list <- list(gpt_short_predictions, unengineered_sample_prediction_merged$protest)
tt <- confusionMatrix(as.factor(gpt_list[[1]]), as.factor(gpt_list[[2]]), positive = "1")
tt$byClass
s_gpt_out <- cvAUC(gpt_list[[1]], gpt_list[[2]])

par(mfrow = c(1,3))
plot(s_newsmap_out$perf, col = "red", sub = "Newsmap")
plot(s_bert_out$perf, col ="red", sub = "BERT")
plot(s_gpt_out$perf, col ="red", sub = "GPT-4")
mtext("AUC on 100 Cleaned articles", side = 3, outer = TRUE, line = -2, cex = 1.5)
dev.off()

# Final GPT 

load(here("gpt_code_and_data",
          "combined_clean_feb2_gptpred_engineered_unengineered.RData"))
final_gpt_preds_engineered <- ifelse(combined_clean_gpt_prediction_feb2$gpt_binary_engineered == 1, 1, 0)
final_gpt_preds_basic <- ifelse(combined_clean_gpt_prediction_feb2$gpt_binary_unengineered== 1, 1, 0)

final_gpt_list1 <- list(final_gpt_preds_engineered, combined_clean_gpt_prediction_feb2$labels)
final_gpt_list2 <- list(final_gpt_preds_basic, combined_clean_gpt_prediction_feb2$labels)

f_gpt_out1 <- cvAUC(final_gpt_list1[[1]],final_gpt_list1[[2]])
f_gpt_out2 <- cvAUC(final_gpt_list2[[1]],final_gpt_list2[[2]])
par(mfrow = c(1,2))
plot(f_gpt_out1$perf, col ="red", sub = "Engineered prompt", main = "")
plot(f_gpt_out2$perf, col ="red", sub = "Basic prompt", main = "")
mtext("GPT-4 AUC on 378 cleaned articles", side = 3, outer = TRUE, line = -2, cex = 1.5)
dev.off()
