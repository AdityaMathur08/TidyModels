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

telecom_rec <- recipe(Churn ~ .,
                      data = telecom_train) %>% 
  step_log(DayMins, base = 10)


telecom_rec
