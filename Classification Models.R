# Classification Models

# Classification models predict categorical outcome variables

# Predicting product purchases
#Classification models are used for predicting categorical outcome variables.
#An example might be predicting whether a customer will purchase a product based 
#on the time they spent on a company website and their total website visits.
#In the dataset below, each row represents a customer and the outcome variable, 
#purchased, consists of two categories, yes and no. Plotting this data
#and coloring the points by the outcome variable reveals that
#customers who do purchase products tend to spend more time on the website.



#Instead of predicting numbers, classification algorithms produce non-overlapping 
#regions where the same categorical outcome is predicted for all combinations of predictor values.

# Loading Libraries----
library(tidymodels)
library(tidyverse)
library(ggplot2)

# Ingesting Data----
leads_df<- read.csv("Data/Social_Network_Ads.csv")
leads_df$Purchased <- as.factor(leads_df$Purchased)
# We have a tidy data with target variable "purchased" and other predictor variables-(gender, age, estimated salary)
# User Id is an identification key and might not be useful for analysis.

# Step 1 Data Rsampling ----

leads_split <- initial_split(leads_df,
                              prop = 0.75,
                              strata = Purchased)

leads_training <- leads_split %>% training()

leads_test <- leads_split %>% testing()



# Step 2 Logistic Model Specification----
logistic_model <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")


# Step 3 Model Fitting----

logistic_fit <-logistic_model%>%
  fit(Purchased ~ Age+EstimatedSalary,
      data = leads_training)


# Print model fit object
logistic_fit

#Printing a model fit object will display the estimated model coefficients.



# 3.1 Predicting Outcome categories ----
class_preds <-logistic_fit %>% 
  predict(new_data = leads_test,
          type = "class")

# 3.2 Predicting estimated probabilities----

prob_preds <-logistic_fit %>% 
  predict(new_data = leads_test,
          type = "prob")


# 3.3 Combining Results----

leads_results <-  leads_test %>% 
  select(Purchased) %>% 
  bind_cols(class_preds,prob_preds)


leads_results


# Confusion Matrix

leads_results %>% conf_mat(truth = Purchased,
                           estimate = .pred_class)

#Accuracy
leads_results %>% accuracy(truth = Purchased,
                           estimate = .pred_class)

#Sensitivity
leads_results %>% sens(truth = Purchased,
                       estimate = .pred_class)


# Specificity

yardstick::spec(leads_results,truth = Purchased, estimate = .pred_class)


#he specificity of your logistic regression model is 0.629, which is less than
# the sensitivity of 0.891. This indicates that your model is much better at 
#detecting customers who will Purchase their goods & service versus 
#the ones who will not.





#The yardstick package also provides the ability to create custom sets of
#model metrics. In cases where the cost of obtaining false negative errors
#is different from the cost of false positive errors, 
#it may be important to examine a specific set of performance metrics.


#Instead of calculating accuracy, sensitivity, and specificity separately,
#you can create your own metric function that calculates all three at the same time.

#The metric_set() function is used for creating custom metric functions in yardstick.


leads_metrics <-yardstick:: metric_set(accuracy, sens,specificity)

leads_results %>% leads_metrics(truth = Purchased,
                                estimate = .pred_class)


#Your results tibble contains all the necessary columns for calculating 
#classification metrics. Next, you'll use this tibble
#and the yardstick package to evaluate your model's performance.

leads_results %>% 
  conf_mat(truth = Purchased,
           estimate = .pred_class) %>%
  summary()


# Step 4 Visualizing Model Performance ----

# 4.1 Heatmap of Confusion Matrix ----
# Create a confusion matrix
conf_mat(leads_results,
         truth = Purchased ,
         estimate = .pred_class) %>% 
  # Create a heat map
  autoplot(type = "heatmap")

# 4.2 Mosaic Plot----
# Create a confusion matrix
conf_mat(leads_results,
         truth = Purchased,
         estimate = .pred_class) %>% 
  # Create a mosaic plot
  autoplot(type = "mosaic")


# 4.3 ROC Curve or Area Under the Curve ----

# ROC curves are used to visualize the performance of a classification model 
# across a range of probability thresholds.

# An ROC curve with the majority of points near the upper left corner 
# of the plot indicates that a classification model is able to correctly predict 
# both the positive and negative outcomes correctly across a wide range 
#of probability thresholds.

#The area under this curve provides a letter grade summary of model performance.



# threshold_df, which contains the sensitivity and specificity of your 
# classification model across the unique probability thresholds in leads_results.


# Calculate metrics across thresholds
threshold_df <- leads_results %>% 
  roc_curve(truth = Purchased, .pred_0)

# View results
threshold_df

# Plot ROC curve
threshold_df %>% 
  autoplot()

#The ROC curve shows that the logistic regression model performs better than a
# model that guesses at random (the dashed line in the plot)

# Calculate ROC AUC
roc_auc(leads_results,
        truth = Purchased, 
        .pred_0)


# 5 Automating the Modeling workflow- last_fit() ----

# Train model with last_fit()
leads_last_fit <- logistic_model %>% 
  last_fit(Purchased ~ Age+EstimatedSalary,
           split = leads_split)

# View test set metrics
leads_last_fit %>% 
  collect_metrics()

#Notice that you got the same area under the ROC curve as before, just with a lot less effort!

# Collect predictions
last_fit_results <- leads_last_fit %>% 
  collect_predictions()

# View results
last_fit_results

# Custom metrics function
last_fit_metrics <- metric_set(accuracy, sens,
                               specificity, roc_auc)


last_fit_metrics(last_fit_results,
                 truth = Purchased,
                 estimate = .pred_class,
                 .pred_0)