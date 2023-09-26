#Author Jean-Baptiste GOMEZ

######################################### 1. Load the data in your software. Assign the value 1 for a client that has subscribed to a
######################################### term deposit and 0 if he or she has not.

library(ggplot2)
library(dplyr)

my_path <- "C:/Users/gomez/OneDrive/Documents/Master 2 EBDS & MAG3/Master 2 EBDS/Machine learning et statistical learning/bank+marketing/bank/bank.csv"

df_bank = read.csv(my_path, sep=';')

unique_names <- unique(df_bank$education)
print(unique_names)

################ Encode 

df_bank$y= factor(df_bank$y, labels = c(0,1))
df_bank$education = factor(df_bank$education, labels = c(1:4))
df_bank$default= factor(df_bank$default, labels = c(0,1))
df_bank$job = factor(df_bank$job,labels = c(1:12))
df_bank$marital = factor(df_bank$marital, labels = c(1:3))
df_bank$housing = factor(df_bank$housing, labels = c(0,1))
df_bank$loan = factor(df_bank$loan, labels = c(0,1))
df_bank$contact = factor(df_bank$contact, labels = c(1:3))
df_bank$month = factor(df_bank$month ,labels = c(1:12))
df_bank$poutcome = factor(df_bank$poutcome, labels = c(1:4))

View(df_bank)

############################################# 2. Split your data into a training and a test datasets.

# Splitting dataset for training and testing purpose

set.seed(200)
training_set = df_bank[1:round(nrow(df_bank)*0.7),]
test_set = df_bank[round(nrow(df_bank)*0.7):nrow(df_bank),]

View(training_set)
View(test_set)

#another way
# library(caTools)
# split = sample.split(Y = dataset$y, SplitRatio = 0.70)
# train_set = subset(x= dataset, split == TRUE)
# test_set = subset(x= dataset, split == F)



################################################ 3. Using the appropriate routine, fit a logit model in the train set to predict the probability
################################################ of subscribing to a term deposit using the following predictors: duration, education, and
################################################ campaign.

logit_model = glm(formula = y ~ duration + education + campaign, family = binomial(), data = training_set)
summary(logit_model)





################################################ 4. In a table, store the true observed value in a first column, and the predicted probability
################################################ estimated by the logit model.

y_predic = predict(logit_model, type='response', newdata = test_set)

bank_predict <- data.frame(
  Observed = test_set$y,   # True observed values
  Predicted_Probability = y_predic  # Predicted probabilities
)

# Display the first few rows of the result table
View(bank_predict)




################################################# 5. Using a threshold value of of .5, assign a predicted class to each individual.

threshold<- .5
bank_predict$predict_class_05= ifelse(bank_predict$Predicted_Probability>threshold, 1,0)

# View(bank_predict)


################################################# 6. Compute a confusion matrix based on that threshold.

confus_mat =table(bank_predict$Observed, bank_predict$predict_class_05) 
confus_mat

# Notice : bank_predict$Observed = test_set$y



############################################### 7. Create a function that computes, given a value ?? , the true positive rate, the false positive
############################################### rate, the true negative rate, the false negative rate and the overall error of your predictions
############################################### based on ?? .

classification_metrics <- function(threshold, observed, predicted) {
  # Compute confusion matrix based on the threshold
  confusion_matrix <- table(observed, ifelse(predicted >= threshold, 1, 0))
  
  # Check if confusion matrix is of the expected size
  if (nrow(confusion_matrix) != 2 || ncol(confusion_matrix) != 2) {
    stop(paste("Confusion matrix is not of the expected size (2x2). Check class labels.", threshold))
  }
  # Extract confusion matrix values(matrice carrée 2x2)
  TP <- confusion_matrix[2, 2]  # True Positives
  FP <- confusion_matrix[1, 2]  # False Positives
  TN <- confusion_matrix[1, 1]  # True Negatives
  FN <- confusion_matrix[2, 1]  # False Negatives
  
  # Calculate classification metrics
  TPR <- TP / (TP + FN)  # True Positive Rate (Sensitivity or Recall)
  FPR <- FP / (FP + TN)  # False Positive Rate
  TNR <- TN / (TN + FP)  # True Negative Rate (Specificity)
  FNR <- FN / (TP + FN)  # False Negative Rate
  Error_overall <- (FP + FN) / sum(confusion_matrix)  # Overall Error Rate
  
  # Return the computed metrics
  metrics <- c(TPR, FPR, TNR, FNR, Error_overall)
  names(metrics) <- c("True_Positive_Rate", "False_Positive_Rate", "True_Negative_Rate", "False_Negative_Rate", "Overall_Error")
  return(metrics)
}

#check for threshold .5 
classification_metrics(0.5,bank_predict$Observed, bank_predict$Predicted_Probability)

##################################### 8. Apply this function to multiple values of ?? ranging from 0 to 1.###########################
threshold_values <- seq(0.1, 0.9, by = 0.1) # with 0 and 1 there are some problems 
# print(threshold_values)

all_results <- sapply(threshold_values, function(tau) {
  classification_metrics(threshold = tau, observed= bank_predict$Observed, predicted = bank_predict$Predicted_Probability)
})

# print(all_results)

column_names <- c("threshold_01", "threshold_02", "threshold_03", "threshold_04",
                  "threshold_05", "threshold_06", "threshold_07", "threshold_08", "threshold_09")

colnames(all_results) <- column_names
View(all_results)

########################################################  9. Plot the ROC curve.######################################################
# Install and load the pROC package if not already installed
if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC")
}

library(pROC)

# Create a vector of threshold values from 0 to 1
thresholds <- seq(0.1, 0.9, by = 0.1)

# Initialize vectors to store TPR and FPR values
TPR_values <- numeric(length(thresholds)) # ne prend que l'entier donc 0
FPR_values <- numeric(length(thresholds))
# print(TPR_values)

# Calculate TPR and FPR for each threshold
for (i in 1:length(thresholds)) {
  threshold <- thresholds[i]
  metrics <- classification_metrics(threshold, observed= bank_predict$Observed, predicted = bank_predict$Predicted_Probability)
  TPR_values[i] <- metrics["True_Positive_Rate"]
  FPR_values[i] <- metrics["False_Positive_Rate"]
}

# Create the ROC curve
roc_curve <- roc(bank_predict$Observed, bank_predict$Predicted_Probability)

# Plot the ROC curve

plot(roc_curve,legacy.axes = TRUE, print.auc = TRUE, print.auc.polygon = TRUE) #print.thres = thresholds
# abline(a = 0, b = 1, col = "blue", lty = 2)

####################################################### 10. Comment the results.########################################################

# A value of 0.818 indicates that the model is capable of making this distinction fairly accurately. 
# In other words, it can classify positive examples as positive and negative examples as negative in a large proportion of cases.
# The closer the AUC is to 1, the better the model performs. A value of 0.818 is generally considered a solid performance. 
# This means that, on average, the model has a good probability of correctly classifying a positive example more often than a negative one.
# The AUC assesses the trade-off between the model's sensitivity (true positive rate) and specificity (true negative rate). 
# A high AUC value generally indicates that the model achieves a good balance between detecting true positives and avoiding false positives.
# 
# 
# Une valeur de 0,818 indique que le modèle est capable de faire cette distinction de manière assez précise. 
# En d'autres termes, il peut classer les exemples positifs comme positifs et les exemples négatifs comme négatifs dans une grande proportion des cas.
# Plus l'AUC se rapproche de 1, plus le modèle est performant. Une valeur de 0,818 est généralement considérée comme une performance solide. 
# Cela signifie que, en moyenne, le modèle a une bonne probabilité de classer correctement un exemple positif plus souvent qu'un exemple négatif.
# L'AUC évalue le compromis entre la sensibilité (taux de vrais positifs) et la spécificité (taux de vrais négatifs) du modèle. 
# Une valeur élevée d'AUC indique généralement que le modèle atteint un bon équilibre entre la capacité à détecter les vrais positifs et à éviter les faux positifs.















