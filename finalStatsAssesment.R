library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)

#Exploratory Data Analysis

diabetes_data_old <- read.csv("D:/Data Mining/diabetes_prediction_dataset/diabetes_prediction_dataset.csv", encoding = "UTF-8")
diabetes_data = na.omit(diabetes_data_old)
str(diabetes_data)

#Loading the csv
diabetes_data <- read.csv("D:/Data Mining/diabetes_prediction_dataset/diabetes_prediction_dataset.csv", encoding = "UTF-8")

#dimension of data
dim(diabetes_data)

#count no of rows
nrow(diabetes_data)

#display the top 10 rows of data
head(diabetes_data, 10)

#exploring the data
str(diabetes_data)

#summary of data
summary(diabetes_data)
#END Data Description

#Start Data Preprocessing

#apply function to each column of data
missing_values <- sapply(diabetes_data, function(x) sum(is.na(x)))
print(missing_values)

#For numeric columns, you might impute missing values with the mean
diabetes_data$age[is.na(diabetes_data$age)] <- mean(diabetes_data$age, na.rm = TRUE)

#Handle Categorical Data
diabetes_data$gender <- as.factor(diabetes_data$gender)
diabetes_data$smoking_history <- as.factor(diabetes_data$smoking_history)
# Normalize numeric columns (if needed)
#diabetes_data <- scale(diabetes_data$bmi)
#diabetes_data <- scale(diabetes_data$HbA1c_level)
#diabetes_data <- scale(diabetes_data$blood_glucose_level)
#diabetes_data$bmi <- scale(diabetes_data$age)

#zero_variance_cols <- sapply(diabetes_data[, c("gender", "hypertension", "smoking_history")], function(x) var(x) == 0)

# Remove columns with zero variance
#diabetes_data <- diabetes_data[, !zero_variance_cols]
#handling ordinal data
diabetes_data$smoking_history <- factor(diabetes_data$smoking_history, ordered = TRUE, levels = c("never", "No Info","former", "current","not current","ever"))
print(diabetes_data)
diabetes_data[, c("age", "bmi", "HbA1c_level", "blood_glucose_level")] <- scale(diabetes_data[, c("age", "bmi", "HbA1c_level", "blood_glucose_level")])
print(diabetes_data)
#Handle Missing Values
diabetes_data <- na.omit(diabetes_data)


summary(diabetes_data)
 
#Result and experiment
 summary(diabetes_data)
 
 #Descriptive Analysis
head(diabetes_data) 

summary(diabetes_data$age)
summary(diabetes_data$blood_glucose_level)
summary(diabetes_data$bmi)

nrow(diabetes_data)

hist(diabetes_data$blood_glucose_level, main = "Blood Glucose Distribution", xlab = "Blood Glucose",ylab = "Number of people")
#coorelation analysis
#Q1
bloodglucose_correlation_result <- cor.test(diabetes_data$age, diabetes_data$blood_glucose_level)
print(bloodglucose_correlation_result) 
cor.test(diabetes_data$age, diabetes_data$blood_glucose_level, "two.sided", "pearson")

#plot(diabetes_data$age,diabetes_data$blood_glucose_level,col="blue")
#abline(lm(diabetes_data$blood_glucose_level~diabetes_data$age))


#just checking others
hba_correlation_result <- cor.test(diabetes_data$hypertension, diabetes_data$HbA1c_level)
print(hba_correlation_result)

#Q2
gld_correlation_result <- cor.test(diabetes_data$age, diabetes_data$HbA1c_level)
print(gld_correlation_result)

plot(diabetes_data$age,diabetes_data$blood_glucose_level,col="blue")
plot(diabetes_data$hypertension,diabetes_data$bmi,col="green")
plot(diabetes_data$blood_glucose_level,diabetes_data$diabetes,col="red")
#abline( lm(diabetes_data$blood_glucose_level~diabetes_data$age)  )


#Hypothesis testing
#Method one
#Q1
hypertension_group <- diabetes_data$blood_glucose_level[diabetes_data$hypertension == 1]
no_hypertension_group <- diabetes_data$blood_glucose_level[diabetes_data$hypertension == 0]

# Perform t-test
t_test_result <- t.test(hypertension_group, no_hypertension_group)
print(t_test_result)

#Method Two
# Boxplot
ggplot(diabetes_data, aes(x = as.factor(hypertension), y = blood_glucose_level, fill = as.factor(hypertension))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", color = "black") +
  labs(title = "Blood Glucose Levels by Hypertension Status",
       x = "Hypertension",
       y = "Blood Glucose Level") +
  theme_minimal()

ggplot(diabetes_data, aes(x = as.factor(hypertension), y = blood_glucose_level, fill = as.factor(hypertension))) +
  geom_boxplot() +
  labs(title = "Blood Glucose Levels by Hypertension Status",
       x = "Hypertension",
       y = "Blood Glucose Level") +
  theme_minimal()

# Perform t-test
t_test_result <- t.test(blood_glucose_level ~ hypertension, data = diabetes_data)

# Print the result
print(t_test_result)

#question 1 hyptothesis testing.

hypertension_group <- diabetes_data$blood_glucose_level[diabetes_data$hypertension == 1]
no_hypertension_group <- diabetes_data$blood_glucose_level[diabetes_data$hypertension == 0]

# Perform t-test
t_test_result <- t.test(hypertension_group, no_hypertension_group)

# Print the result
print(t_test_result)

group_data <- data.frame(
  Group = rep(c("Hypertension", "No Hypertension"), 
              times = c(length(hypertension_group), length(no_hypertension_group))),
  Blood_Glucose_Level = c(hypertension_group, no_hypertension_group)
)

library(ggplot2)
ggplot(group_data, aes(x = Group, y = Blood_Glucose_Level, fill = Group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Blood Glucose Levels by Hypertension Status",
       x = "Hypertension Status",
       y = "Blood Glucose Level") +
  theme_minimal()


#question 2 hyptothesis testing.
# Perform ANOVA
anova_result <- aov(diabetes_data$blood_glucose_level ~ diabetes_data$smoking_history)

# Print the result
print(anova_result)


# Log transform the blood glucose levels
diabetes_data$log_blood_glucose_level <- log(diabetes_data$blood_glucose_level)

# Rerun the ANOVA test after log transforming the blood glucose levels
aov_results_after_transformation <- aov(log_blood_glucose_level ~ smoking_history, data = diabetes_data)



ggplot(diabetes_data, aes(x = smoking_history, y = blood_glucose_level, fill = smoking_history)) +
  geom_boxplot() +
  labs(title = "Boxplot of Blood Glucose Levels by Smoking History",
       x = "Smoking History",
       y = "Blood Glucose Level") +
  theme_minimal()
sum(is.na(diabetes_data$blood_glucose_level))
diabetes_data <- na.omit(diabetes_data)
ggplot(diabetes_data, aes(x = smoking_history, y = blood_glucose_level, fill = smoking_history)) +
  geom_boxplot() +
  labs(title = "Boxplot of Blood Glucose Levels by Smoking History",
       x = "Smoking History",
       y = "Blood Glucose Level") +
  theme_minimal() +
  scale_x_discrete(limits = c("current", "ever", "former","never", "No Info","current"))



#Linear Regression
linear_model <- lm(HbA1c_level ~ age, data = diabetes_data)
summary(linear_model)

ggplot(diabetes_data, aes(x = age, y = HbA1c_level)) +
  geom_point() +  # Scatterplot of data points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear regression line
  labs(title = "Linear Regression: Age vs. HbA1c Level",
       x = "Age",
       y = "HbA1c Level") +
  theme_minimal()

smoking_history <- as.numeric(factor(diabetes_data$smoking_history))
gender <- as.numeric(factor(diabetes_data$gender))
multiple_linear_model <- lm(diabetes ~ age + bmi + HbA1c_level + hypertension, data = diabetes_data)
summary(multiple_linear_model)
summary(multiple_linear_model$coefficients)

residuals_plot <- ggplot(data = as.data.frame(multiple_linear_model$residuals), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "green") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

plot(multiple_linear_model)

