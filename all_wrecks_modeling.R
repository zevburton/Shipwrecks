
# Modeling all the wrecks

# reading in data
library(readr)
data <- read.csv("sink.csv")
data <- data[,-1]
data$sank <- factor(data$sank)
data$sank <- factor(data$sank, levels = c(0, 1), labels = c("Not", "Sank"))
levels(data$sank) <- make.names(levels(data$sank), unique = TRUE)

library(glmnet)
library(tidyverse)
library(dplyr)
library(ISLR)
library(leaps)
library(caret)
library(mlbench)
library(pROC)
library(e1071)
library(cvms)
library(tibble) 
library(ggplot2)
library(ggimage)

# Split the data into training and test sets for use throughout this
set.seed(23)
train_index <- sample(nrow(data), nrow(data) * 0.7)
train <- data[train_index, ]
test <- data[-train_index, ]



# logistic -- doesn't work due to high multicolinearity
lrFit <- glm(sank ~ ., data = train, family = "binomial") # does not converge
predictions <- predict(lrFit, newdata = test, type = "response", allowRankDeficiency = TRUE)

# Compute accuracy for different thresholds
thresholds <- seq(0.01, 0.99, by = 0.001)
accuracy <- rep(0, length(thresholds))
for (i in 1:length(thresholds)) {
  binary_predictions <- ifelse(predictions >= thresholds[i], "Sank", "Not")
  confusion_matrix <- table(test$sank, binary_predictions)
  accuracy[i] <- sum(diag(confusion_matrix))/sum(confusion_matrix)
}

# Find the threshold with the highest accuracy
optimal_threshold <- thresholds[which.max(accuracy)]
print(paste0("Optimal threshold: ", round(optimal_threshold, 3)))

# Convert the predictions to binary using a threshold of 0.5
binary_predictions <- ifelse(predictions >= optimal_threshold, "Sank", "Not")

# Evaluate the performance of the model
confusion_matrix <- table(test$sank, binary_predictions)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
print(confusion_matrix) # It predicted every single row as Sank.

# Decision Tree
# Set up a grid of hyperparameters to tune for the decision tree model
dtGrid <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))

# Train the decision tree model and cross-validate
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
dtFit <- train(sank ~ ., data = train[,1:44], method = "rpart", trControl = ctrl, tuneGrid = dtGrid)

# Make predictions on the test set and calculate the AUC
pred_dt <- predict(dtFit, newdata = test, type = "prob")[, 2]
roc_dt <- roc(test$sank, pred_dt)

# Find the optimal threshold based on the maximum Youden's J statistic
J <- roc_dt$specificities + roc_dt$sensitivities - 1
opt_thresh_dt <- roc_dt$thresholds[which.max(J)]

# Make binary predictions using the optimal threshold
pred_dt_bin <- as.numeric(pred_dt > opt_thresh_dt)
pred_dt_bin <- ifelse(pred_dt_bin == 0, "Not", "Sank")

# Confusion Matrix
library(ggimage)

predicted <- pred_dt_bin
actual <- test$sank

# Create confusion matrix with predicted and actual numbers
conf_mat <- table(predicted, actual)

# Convert confusion matrix to data frame
conf_df <- as.data.frame(conf_mat)

names(conf_df) <- c("Predicted", "Actual", "Count")

accuracy <- (conf_df$Count[1] + conf_df$Count[4]) / sum(conf_df$Count) * 100
accuracy <- round(accuracy, 3)
conf_df$Count <- factor(conf_df$Count, 
                        levels = c(conf_df$Count[1], 
                                   conf_df$Count[2], 
                                   conf_df$Count[3],
                                   conf_df$Count[4]))

conf_df$Predicted <- factor(conf_df$Predicted, levels = c("Sank", "Not"))
conf_df$Actual <- factor(conf_df$Actual, levels = c("Not", "Sank"))

auc <- roc(ifelse(predicted == "Not", 0, 1), ifelse(actual == "Not", 0, 1))$auc

ggplot(data = conf_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 11, family = "Optima") +
  scale_fill_manual(values = c("#65749D", "#D9D9D9", "#D9D9D9", "#65749D")) +
  labs(title = "\nDecision Tree Confusion Matrix", 
       x = "Predicted", 
       y = "Actual", 
       fill = "", 
       subtitle = paste0("Accuracy: ", round(accuracy, 3), "%\nAUC: ", round(roc_dt$auc, 4))) +
  theme_transparent()  +
  theme(text = element_text(family = "Optima", size = 14),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = 'none') 

## Random Forest

train_sample <- sample_n(train, 20000)

# split data into train, dev, and test sets
trainIndex <- createDataPartition(train_sample$sank, p = 0.7, list = FALSE)
train_data <- train_sample[trainIndex, ]
test_dev_data <- train_sample[-trainIndex, ]

devIndex <- createDataPartition(test_dev_data$sank, p = 0.5, list = FALSE)
dev_data <- test_dev_data[devIndex, ]
test_data <- test_dev_data[-devIndex, ]

### ref test
# load libraries
library(randomForest)
library(caret)
library(purrr)

# define mtry and ntree values to try
mtry_values <- seq(2, 13, by = 2)
ntree_values <- seq(100, 500, by = 50)

# initialize matrices to store errors
train_errors <- matrix(NA, nrow = length(mtry_values), ncol = length(ntree_values))
dev_errors <- matrix(NA, nrow = length(mtry_values), ncol = length(ntree_values))

# set up progress bar
pb <- txtProgressBar(min = 0, max = length(mtry_values) * length(ntree_values), style = 3)

# loop over mtry and ntree values to train and evaluate models
counter <- 0
for (i in seq_along(mtry_values)) { 
  for (j in seq_along(ntree_values)) { 
    # train model
    rf_model <- randomForest(sank ~ ., 
                             data = train_data, 
                             mtry = mtry_values[i], 
                             ntree = ntree_values[j], 
                             do.trace = FALSE, 
                             importance = FALSE, 
                             proximity = FALSE, 
                             replace = TRUE)
    
    # calculate train error
    train_pred <- predict(rf_model, train_data)
    train_cm <- confusionMatrix(train_pred, train_data$sank)
    train_errors[i, j] <- train_cm$overall['Accuracy']
    
    # calculate dev error
    dev_pred <- predict(rf_model, dev_data)
    dev_cm <- confusionMatrix(dev_pred, dev_data$sank)
    dev_accuracy[i, j] <- dev_cm$overall['Accuracy'] 
    
    # update progress bar
    counter <- counter + 1
    setTxtProgressBar(pb, counter)
  } 
}

# remove progress bar
close(pb)

# find optimal parameters
max_dev_error <- max(dev_accuracy)
optimal_mtry <- mtry_values[which(dev_errors == max_dev_error, arr.ind = TRUE)[1]]
optimal_ntree <- ntree_values[which(dev_errors == max_dev_error, arr.ind = TRUE)[2]]

# print optimal parameters
cat(paste0("Optimal mtry: ", optimal_mtry, "\n")) # 12
cat(paste0("Optimal ntree: ", optimal_ntree, "\n")) # 250

## bringing it out to the entire testing dataset
rf_model <- randomForest(sank ~., 
                       data = train, 
                         mtry = 12,
                         ntree = 250)

test_predictions <- predict(rf_model, newdata = test)

dt_conf <- table(test_predictions, test$sank) %>% 
  as.data.frame() %>%
  as_tibble()

predicted <- test_predictions
actual <- test$sank

# Create confusion matrix with predicted and actual numbers
conf_mat <- table(predicted, actual)

# Convert confusion matrix to data frame
conf_df <- as.data.frame(conf_mat)

names(conf_df) <- c("Predicted", "Actual", "Count")

accuracy <- (conf_df$Count[1] + conf_df$Count[4]) / sum(conf_df$Count) * 100
accuracy <- round(accuracy, 3)
conf_df$Count <- factor(conf_df$Count, 
                        levels = c(conf_df$Count[1], 
                                   conf_df$Count[2], 
                                   conf_df$Count[3],
                                   conf_df$Count[4]))

conf_df$Predicted <- factor(conf_df$Predicted, levels = c("Sank", "Not"))
conf_df$Actual <- factor(conf_df$Actual, levels = c("Not", "Sank"))

auc <- roc(ifelse(test_predictions == "Not", 0, 1), ifelse(actual == "Not", 0, 1))$auc

ggplot(data = conf_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 11, family = "Optima") +
  scale_fill_manual(values = c("#65749D", "#D9D9D9", "#D9D9D9", "#65749D")) +
  labs(title = "\nRandom Forest Confusion Matrix", 
       x = "Predicted", 
       y = "Actual", 
       fill = "", 
       subtitle = paste0("Accuracy: ", accuracy, "%\nAUC: ", round(auc, 4))) +
  theme_transparent()  +
  theme(text = element_text(family = "Optima", size = 14),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = 'none') 

# SVM
library(caret)
# Set up a grid of hyperparameters to tune for the SVM Linear model
svmGrid <- expand.grid(C = seq(0.01, 0.1, by = 0.01))

# Set up cross-validation method with 3 folds
ctrl <- trainControl(method = "cv", number = 3)

# Train the SVM Linear model and cross-validate
svmFit <- train(sank ~ ., 
                data = train[,1:44], 
                method = "svmLinear", 
                trControl = ctrl, 
                tuneGrid = svmGrid,
                preProc = c("center", "scale"))

# Get predictions on the test data
svm_pred <- predict(svmFit, newdata = test[,1:44])

# Compute accuracy and confusion matrix
accuracy <- round(mean(svm_pred == test$sank),4) * 100

predicted <- svm_pred
actual <- test$sank

# Create confusion matrix with predicted and actual numbers
conf_mat <- table(predicted, actual)

# Convert confusion matrix to data frame
conf_df <- as.data.frame(conf_mat)

names(conf_df) <- c("Predicted", "Actual", "Count")

accuracy <- (conf_df$Count[1] + conf_df$Count[4]) / sum(conf_df$Count) * 100
accuracy <- round(accuracy, 3)
conf_df$Count <- factor(conf_df$Count, 
                        levels = c(conf_df$Count[1], 
                                   0, 
                                   conf_df$Count[3]))

conf_df$Predicted <- factor(conf_df$Predicted, levels = c("Sank", "Not"))
conf_df$Actual <- factor(conf_df$Actual, levels = c("Not", "Sank"))

predicted <- factor(svm_pred, levels = c("Not", "Sank"))
actual <- factor(test$sank, levels = c("Not", "Sank"))

auc <- roc(ifelse(predicted == "Not", 0, 1), ifelse(actual == "Not", 0, 1))$auc

ggplot(data = conf_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 11, family = "Optima") +
  scale_fill_manual(values = c("#65749D", "#D9D9D9", "#a1a1a1", "#D9D9D9")) +
  labs(title = "\nSVM Confusion Matrix", 
       x = "Predicted", 
       y = "Actual", 
       fill = "", 
       subtitle = paste0("Accuracy: ", round(accuracy,2), "%\nAUC: NA due to the SVM classifying all values as 'Not'")) +
  theme_transparent()  +
  theme(text = element_text(family = "Optima", size = 14),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = 'none')

# Naive Bayes
library(caret)
library(e1071)

# Set up a grid of hyperparameters to tune for the naive bayes model
nbGrid <- expand.grid(laplace = seq(0.01, 1, by = 0.01),
                      usekernel = c(FALSE, TRUE),
                      adjust = c(FALSE, TRUE))

# Train the naive bayes model and cross-validate
nbFit <- train(sank ~ .,
               data = train[,1:44],
               method = "naive_bayes",
               trControl = ctrl,
               tuneGrid = nbGrid)


# Get predictions on the test data
nb_pred <- predict(nbFit, newdata = test[,1:44])

# Compute accuracy and confusion matrix
nb_accuracy <- mean(nb_pred == test$sank)
nb_conf <- table(nb_pred, test$sank)

# Output results
print(paste0("Naive Bayes Accuracy: ", round(nb_accuracy, 4)))
print(nb_conf)

predicted <- nb_pred
actual <- test$sank

# Create confusion matrix with predicted and actual numbers
conf_mat <- table(predicted, actual)

# Convert confusion matrix to data frame
conf_df <- as.data.frame(conf_mat)

names(conf_df) <- c("Predicted", "Actual", "Count")

accuracy <- (conf_df$Count[1] + conf_df$Count[4]) / sum(conf_df$Count) * 100
accuracy <- round(accuracy, 3)
conf_df$Count <- factor(conf_df$Count, 
                        levels = c(conf_df$Count[1], 
                                   conf_df$Count[2], 
                                   conf_df$Count[3],
                                   conf_df$Count[4]))

conf_df$Predicted <- factor(conf_df$Predicted, levels = c("Sank", "Not"))
conf_df$Actual <- factor(conf_df$Actual, levels = c("Not", "Sank"))

actual <- factor(test$sank, levels = c("Not", "Sank"))

auc <- roc(ifelse(predicted == "Not", 0, 1), ifelse(actual == "Not", 0, 1))$auc

ggplot(data = conf_df, aes(x = Predicted, y = Actual, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 11, family = "Optima") +
  scale_fill_manual(values = c("#65749D", "#D9D9D9", "#D9D9D9", "#65749D")) +
  labs(title = "\nSVM Confusion Matrix", 
       x = "Predicted", 
       y = "Actual", 
       fill = "", 
       subtitle = paste0("Accuracy: ", round(accuracy,2), "%\nAUC: ", round(auc, 3))) +
  theme_transparent()  +
  theme(text = element_text(family = "Optima", size = 14),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = 'none')

