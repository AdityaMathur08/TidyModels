library(tidymodels)

#TidyModels is a collection of Libraries/pachages for streamlined Machine Learning Workflow in R


home_sales<- read.csv("Data/train.csv")







# Step 1:Data Resampling -- rsample---- 

##  The rsample package is designed to create training and test datasets.
## Creating a test dataset is important for estimating how a trained model will likely perform on new data.
## It also guards against overfitting, where a model memorizes patterns that exist only in the training data 
## and performs poorly on new data.  

home_split <- initial_split(home_sales,
                      prop =0.70 ,
                      strata = SalePrice)
# here we are creating an rsample object, home_split using initial_split() 
#function that contains instructions for randomly splitting home_sales data into training and test data

# Create the training data
home_training <- home_split %>%
  training()

# Create the test data
home_test <- home_split %>% 
  testing()

# Check number of rows in each dataset
nrow(home_training)
nrow(home_test)



# 1.1 Distribution of outcome variable values----
#Stratifying by the outcome variable when generating training and test datasets
#ensures that the outcome variable values have a similar range in both datasets.


#Since the original data is split at random, 
#stratification avoids placing all the expensive homes in home_sales into the test dataset

#Let's Calculate summary statistics for the selling_price variable in the training
# and test datasets. for the home_training and home_test

# Distribution of selling_price in training data
home_training %>% 
  summarize(min_sell_price = min(SalePrice),
            max_sell_price = max(SalePrice),
            mean_sell_price = mean(SalePrice),
            sd_sell_price = sd(SalePrice))

# Distribution of selling_price in test data
home_test %>% 
  summarize(min_sell_price = min(SalePrice),
            max_sell_price = max(SalePrice),
            mean_sell_price = mean(SalePrice),
            sd_sell_price = sd(SalePrice))


# The minimum and maximum selling prices in both datasets are the same.
# The mean and standard deviation are also similar.
# Stratifying by the outcome variable ensures the model fitting process is performed on
# a representative sample of the original data.



# Prediction of Sales Price of Houses is a Regression Problem.

# Step 2: Model Formulas -- Parsnip----


# 2.1 Specify a linear regression model, linear_model----
linear_model <- linear_reg() %>% 
  # Set the model engine
  set_engine('lm') %>% 
  # Set the model mode
  set_mode('regression')

# 2.2 Train the model with the training data----
lm_fit <- linear_model %>% 
  fit(SalePrice ~LotArea+YrSold,
      data = home_training)

# Print lm_fit to view model information
lm_fit

# have defined your model with linear_reg() and 
#trained it to predict selling_price using Yrsold and LotArea. 
#Printing a parsnip model fit object displays useful model information,
#such as the training time, model formula used during training, and
#the estimated model parameters.


#The tidy() function automatically creates a tibble of estimated model parameters.
#Since LotArea has a positive estimated parameter,
#the selling price of homes increases with the square footage.
#Conversely, since YrSold has a negative estimated parameter,
#older homes tend to have lower selling prices.

# 2.3  Predict selling_price----
home_predictions <- predict(lm_fit,
                            new_data = home_test)

# View predicted selling prices
home_predictions

#2.4  Combine test data with predictions----
home_test_results <- home_test %>% 
  select(SalePrice,YrSold,LotArea) %>% 
  cbind(home_predictions)

# View results
home_test_results %>% head()


#have trained a linear regression model and used it to predict the selling prices of homes
#in the test dataset! 
#The model only used two predictor variables, 
#but the predicted values in the .pred column seem reasonable!


# Step 3: Evaluating Model Performance- Yardstick Package ----

# All Yardstick functions require a tibble with Model Results.
#Tibble must contain :  1. Column with true outcome variable  
#                       2. column with Model predictions(.pred)


# A common performance metric for regression models is the root mean squared error, or RMSE. 


# Caculate the RMSE metric
home_test_results %>% 
  rmse(truth =SalePrice , estimate = .pred)

# The RMSE metric indicates that the average prediction error
# for home selling prices is about $74587. 
# Not bad considering you only used home_age and sqft_living as predictor variables!

# Calculate the R squared metric
home_test_results %>% 
  rsq(truth =SalePrice , estimate = .pred)

#The R squared metric ranges from 0 to 1, 0 being the worst and 1 the best.
#Calculating the R squared value is only the first step in studying your model's predictions.
#Making an R squared plot is extremely important because it will uncover potential problems with your model, such as non-linear patterns or regions where your model is either over or under-predicting the outcome variable.


# Create an R squared plot of model performance
ggplot(home_test_results, aes(x = SalePrice, y = .pred)) +
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')




#machine learning pipeline and visualized the performance of your model.


# Define a linear regression model
linear_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

# Train linear_model with last_fit()
linear_fit <- linear_model %>% 
  last_fit(SalePrice ~ ., split = home_split)

# Collect predictions and view results
predictions_df <- linear_fit %>% collect_predictions()
predictions_df

# Make an R squared plot using predictions_df
ggplot(predictions_df, aes(x = SalePrice, y = .pred)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')