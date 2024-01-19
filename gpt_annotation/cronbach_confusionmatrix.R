### This R script was created to conduct four analytical activities


######Calculating Cronbach's alpha for GPT predictions at 0 temp and 2 temp 
###### Finding the mode for predictions at 0 temp and 2 temp
###### Merging with the original sample dataset
###make confusion matrix to compare human annotation and gpt annotation

#load the data for 0 temp

first_0temp <- load("gptpred_first_0.RData")
first_0temp <- labelled_articles
second_0temo <- load("gptpred_second_0.RData")
third_0temp <- load("gptpred_third_0.RData")

####Now we calculate cronbach's alpha for gpt prediction at 0 temperature


first_model <- first_0temp |>
  select(gpt_pred)

second_model <- second_pred |>
  select(gpt_pred2)

third_model <- third_pred |>
  select(gpt_pred3)

combined_pred <- cbind.data.frame(first_model, second_model, third_model)

####Load the package for cronbach's alpha
#install.packages("ltm")
library(ltm)
cronbach.alpha(combined_pred, CI=T) #The Cronbach alpha is 1. 

###Let's do the same for the gpt predictions at 2 temperature

first_model2 <- first_pred2[,2]
second_model2 <- second_pred2[,2]
third_model2 <- third_pred2[,2]


combined_pred2 <- cbind.data.frame(first_model2, second_model2, third_model2)
cronbach.alpha(combined_pred2, CI=T)  #Cronbach alpha is 0.972

your_dataset <- data.frame(col1 = c("yes", "no", "yes", "no", "yes", "yes"),
                             col2 = c("no", "no", "yes", "no", "yes", "yes"),
                             col3 = c("no", "no", "yes", "yes", "no", "no"))
####Create final gpt prediction for both temperatures using the modal value at each temp
library(dplyr)

# Mode function to calculate the mode for character variables
library(dplyr)
library(purrr)

# Mode function to calculate the mode for character variables
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Assuming your dataset is named 'your_dataset'
library(dplyr)
library(purrr)

# Mode function to calculate the mode for character variables
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

combined_pred$final_prediction0 <- apply(combined_pred[, c("gpt_pred", "gpt_pred2", "gpt_pred3")], 1, Mode)

combined_pred2$final_prediction2 <- apply(combined_pred2[, c("gpt_pred1_temp2", "gpt_pred2_temp2", "gpt_pred3_temp2")], 1, Mode)

gpt_pred0 <- (combined_pred[, 4])
gpt_pred2 <- combined_pred2[,4]

final_prediction = cbind.data.frame(gpt_pred0, gpt_pred2)

#merge this with the original sample data

final_data_gptpred <- cbind.data.frame(final_data_gpt, final_prediction)

save(final_data_gptpred, file = "final_data_gptpred.RData")


#####Confusion matrix

predicted_value0 <- final_data_gptpred[,7]

predicted_value0 <- as.factor(ifelse(predicted_value0=="Yes", 1,0))

class(predicted_value0)

predicted_value2 <- final_data_gptpred[,8]

predicted_value2 <- as.factor(ifelse(predicted_value2=="Yes", 1,0))

class(predicted_value2)

#install the required package

#install.packages("caret")
library(caret)
#convert protest into a factor level variable
final_data_gptpred$protest <- as.factor(final_data_gptpred$protest)

matrix_temp0 <- confusionMatrix(data=predicted_value0, reference = final_data_gptpred$protest)

tn_temp0 = 47 
fp_temp0 = 10
fn_temp0 = 3
tp_temp0 = 40

precision_temp0 = tp_temp0/(tp_temp0+fp_temp0)
precision_temp0 #0.8

recall_temp0 = tp_temp0/(tp_temp0+fn_temp0)
recall_temp0 #0.93

f1_temp0 = 2*((precision_temp0*recall_temp0)/(precision_temp0+recall_temp0))
f1_temp0  #0.86

matrix_temp2 <- confusionMatrix(data=predicted_value2, reference = final_data_gptpred$protest)
matrix_temp2$table

tn_temp2 = 48
fp_temp2 = 10
fn_temp2 = 2
tp_temp2 = 40

precision_temp2 = tp_temp2/(tp_temp2+fp_temp2)
precision_temp2 #0.8

recall_temp2 = tp_temp2/(tp_temp2+fn_temp2)
recall_temp2 #0.95

f1_temp2 = 2*((precision_temp2*recall_temp2)/(precision_temp2+recall_temp2))
f1_temp2  #0.87























