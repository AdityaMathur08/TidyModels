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

# Step 1 Data Resampling ----

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

#Your results tibble contains all the necessary columns for calculating 
#classification metrics. Next, you'll use this tibble
#and the yardstick package to evalute your model's performance.