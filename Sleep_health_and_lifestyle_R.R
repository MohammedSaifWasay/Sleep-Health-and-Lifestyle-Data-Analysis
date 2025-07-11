#Mohammed Saif Wasay
#Northeastern University

cat("\014") # clears console
rm(list = ls()) # clears global environment try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session
library(pacman) 
p_load(tidyverse) 
library(dplyr)
p_load(psych)
library(psych)
p_load(janitor)

#Reading Data
lifestyle_data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")
lifestyle_data <- clean_names(lifestyle_data)

#view(lifestyle_data)

# Select numeric variables except person_id
numeric_vars <- lifestyle_data[, sapply(lifestyle_data, is.numeric)]
numeric_vars <- numeric_vars[, !colnames(numeric_vars) %in% c("person_id")]

#Descriptive Statistics of the whole sample
descriptive_stats <- psych::describe(numeric_vars)
descriptive_stats

#Descriptive statistics for Sleep Quality 4
desc_stats_quality_4 <- psych::describe(numeric_vars[numeric_vars$quality_of_sleep == 4, ])
desc_stats_quality_4

#Descriptive statistics for Sleep Quality 9
desc_stats_quality_9 <- psych::describe(numeric_vars[numeric_vars$quality_of_sleep == 9, ])
desc_stats_quality_9

#Finding Average age and standard deviation from its mean
mean_age <- mean(lifestyle_data$age)
sd_age <- sd(lifestyle_data$age)

#Finding Average Sleep Duration for the sample and standard deviation from its mean
mean_sleep_duration <- mean(lifestyle_data$sleep_duration)
sd_sleep_duration <- sd(lifestyle_data$sleep_duration)

#Finding Average Daily Steps for the sample and standard deviation from its mean
mean_daily_steps <- mean(lifestyle_data$daily_steps)
sd_daily_steps <- sd(lifestyle_data$daily_steps)

#Finding Average Stress Level for the sample and standard deviation from its mean
mean_stress_level <- mean(lifestyle_data$stress_level)
sd_stress_level <- sd(lifestyle_data$stress_level)

#Use the data as sample
sample_size <- length(lifestyle_data$age)  # Assuming the sample size is the same for all variables

# Creating a data frame for the table
table_data <- data.frame(
  Measure = rep(c("Mean", "Standard Deviation", "Sample Size (N)")),
  Variable = rep(c("Age", "Sleep Duration", "Daily Steps", "Stress Level"), each = 3),
  Value = c(mean_age, sd_age, sample_size, mean_sleep_duration, sd_sleep_duration, sample_size, mean_daily_steps, sd_daily_steps, sample_size, mean_stress_level, sd_stress_level, sample_size),
  Interpretation = c(
    paste("The average age in the sample is", round(mean_age, 2)),
    paste("The variation in age around the mean is approximately", round(sd_age, 2)),
    paste("The analysis is based on a sample of", sample_size, "individuals for 'Age'."),
    
    paste("The average sleep duration of the sample is", round(mean_sleep_duration, 2), "hours"),
    paste("The variation in sleep duration around the mean is approximately", round(sd_sleep_duration, 2), "hours"),
    paste("The analysis is based on a sample of", sample_size, "individuals for 'Sleep Duration'."),
    
    paste("The average daily steps count in the sample is", round(mean_daily_steps, 2)),
    paste("The variation in steps count around the mean is approximately", round(sd_daily_steps, 2)),
    paste("The analysis is based on a sample of", sample_size, "individuals for 'Daily Steps'."),
    
    paste("The average stress level in the sample is", round(mean_stress_level, 2)),
    paste("The variation in stress levels around the mean is approximately", round(sd_stress_level, 2)),
    paste("The analysis is based on a sample of", sample_size, "individuals for 'Stress Level'.")
  )
)

#Creating a clean table using kable
knitr::kable(table_data, format = "markdown", col.names = c("Measure", "Variable", "Value", "Interpretation"))

# Scatter chart representing Age and Sleep Duration of Individual
par(mfrow = c(1, 1))
plot(lifestyle_data$age, lifestyle_data$sleep_duration, main = "Scatter Chart of Sleep Duration vs Age", xlab = "Age", ylab = "Sleep Duration")
abline(h = mean(lifestyle_data$sleep_duration), col = "red", lty = 2)

# Scatter chart representing Age and Stress Level of Individual
par(mfrow = c(1, 1))
plot(lifestyle_data$age, lifestyle_data$stress_level, main = "Scatter Chart of Age and Stress Levels", xlab = "Age", ylab = "Stress Level")
abline(h = mean(lifestyle_data$stress_level), col = "red", lty = 2)

# Jitter chart for Age of Individual and Steps walked Daily
par(mfrow = c(1, 1))
jittered_age <- jitter(lifestyle_data$age)
jittered_steps <- jitter(lifestyle_data$daily_steps)
plot(jittered_age, jittered_steps, main = "Jitter Chart of Age and Daily Steps", xlab = "Age", ylab = "Daily Steps")
abline(h = mean(jittered_steps), col = "red", lty = 2)
abline(v = mean(jittered_age), col = "blue", lty = 2)

# Jitter chart for sleep duration and stress level of individual
par(mfrow = c(1, 1))
jittered_sleep <- jitter(lifestyle_data$sleep_duration)
jittered_stress <- jitter(lifestyle_data$stress_level)
plot(jittered_sleep, jittered_stress, main = "Jitter Chart of Sleep Duration and Stress Level", xlab = "Sleep Duration", ylab = "Stress Level")
abline(h = mean(jittered_stress), col = "red", lty = 2)
abline(v = mean(jittered_sleep), col = "blue", lty = 2)

# Jitter chart for quality of sleep and physical activity of individual
par(mfrow = c(1, 1))
jittered_sleep <- jitter(lifestyle_data$quality_of_sleep)
jittered_activity <- jitter(lifestyle_data$physical_activity_level)
plot(jittered_activity, jittered_sleep, main = "Jitter Chart with Lines", xlab = "Physical Activity", ylab = "Sleep Quality")
abline(h = mean(jittered_sleep), col = "red", lty = 2)
abline(v = mean(jittered_activity), col = "blue", lty = 2)

# Boxplot chart for daily steps as per gender
par(mfrow = c(1, 1))
boxplot(lifestyle_data$daily_steps ~ lifestyle_data$gender, main = "Boxplot Chart of Daily Steps for the Genders ", xlab = "Gender", ylab = "Daily Steps")

# Boxplot chart for stress level as per gender
par(mfrow = c(1, 1))
boxplot(lifestyle_data$stress_level ~ lifestyle_data$gender, main = "Boxplot Chart of Stress Levels for the Genders", xlab = "Gender", ylab = "Stress")


