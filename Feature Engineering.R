# feature engineering, which is the process of transforming data to a format
#that is suitable for machine learning algorithms.

# accomplished with the recipes package. It is designed to help with all stages of feature engineering

# Loading Libraries ----
library(tidymodels)
#Ingesting Data----
telecom<- read.csv("Data/Dataset_Cellphone.csv")

# Churn is our Target variable and We would be classifying Chrun or not, So it should be Nominal
telecom$Churn <- as.factor(telecom$Churn)

telecom %>% head() %>% View()

#The first step in feature engineering is to specify a recipe object with the recipe() function
#and add data preprocessing steps with one or more step_*() functions.

#Step 1: Rsample ----

telecom_split <- initial_split(telecom,
                               prop = 0.75,
                               strata = Churn)

telecom_train <- telecom_split %>% training()

telecom_test <- telecom_split %>%  testing()


# Step 2: Feature Engineering -- recipie----

#first step in feature engineering is to specify a recipe object with the 
#recipe() function and add data preprocessing steps with one or more
#step_*() functions. Storing all of this information in a single recipe object
#makes it easier to manage complex feature engineering pipelines and transform new data sources.

# Step 2.1 Recipie Object ----
telecom_rec <- recipe(Churn ~ .,
                      data = telecom_train) %>% 
  step_log(DayMins,DayCalls, base = 10)


telecom_rec


# To find out How many numeric and nominal predictor variables are encoded in the telecom_rec

telecom_rec %>% summary()

# Step 2.2 Training recipie object with training  & Test data----

telecom_log_rec_prep <- telecom_rec %>% 
  prep(training = telecom_train)

telecom_log_rec_prep

telecom_log_rec_prep %>% 
  bake(new_data = NULL)

# Apply to test data
telecom_log_rec_prep %>% 
  bake(new_data = telecom_test)


# Numeric Predictors ----

# Correlation Matrix:
telecom_train %>% select_if(is.numeric) %>% 
  cor() %>% View()

# there is  correlation between Monthly Charge and data usage
##########################################################################################################################################################################################################################################################################################################################################
##  Correlated predictor variables provide redundant information and can negatively
## impact the model fitting process. When two variables are highly correlated, their 
## values change linearly with each other and hence provide the same information to 
## your machine learning algorithms. This phenomenon is know as multicollinearity.  ##
##########################################################################################################################################################################################################################################################################################################################################



# Plot correlated predictors
ggplot(telecom_train, aes(x = DataUsage, y = MonthlyCharge)) + 
  # Add points
  geom_point() +
# Add title
labs(title ="Monthly Charges vs. Average Data Usage",
     y = 'Monthly Charges ($)', x = 'Average Data Usage (GB)')

# Its good to remove the multicollinearity 

# Specify a recipe object
telecom_cor_rec <- recipe(Churn ~ .,
                          data = telecom_train) %>% 
  # Remove correlated variables
  step_corr(all_numeric(), threshold = 0.8)


# Train the recipe
telecom_cor_rec_prep <- telecom_cor_rec %>% 
  prep(training = telecom_train
       
       )


# Apply to training data
telecom_cor_rec_prep %>% 
  bake(new_data = NULL)

# Apply to test data
telecom_cor_rec_prep %>% 
  bake(new_data = telecom_test)


#################################################################################################################################################################################################################
##  The power of the recipes package is that you can include multiple preprocessing
## steps in a single recipe object. These steps will be carried out in the order
## they are entered with the step_*() functions  ##
##################################################################################################################################################################################################################

# Specify a recipe object
telecom_norm_rec <- recipe(Churn ~ .,
                           data = telecom_train) %>% 
  # Remove correlated variables
  step_corr(all_numeric(), threshold = 0.8) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric())

# Train the recipe
telecom_norm_rec_prep <- telecom_norm_rec %>% 
  prep(training = telecom_train)

# Apply to test data
telecom_norm_rec_prep %>% 
  bake(new_data = telecom_test)


# Nominal Predictors----