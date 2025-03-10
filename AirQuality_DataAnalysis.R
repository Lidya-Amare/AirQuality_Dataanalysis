
# AIM: To predict hourly averaged Carbon Monoxide (CO) concentrations in an urban environment using pollutant concentrations, sensor responses, and environmental factors, and identify the most significant predictors contributing to CO levels."
#Data Structure
#	Type of Data:
#Time-Series Data: Observations are recorded hourly over one year (March 2004–February 2005).
#Single Spatial Unit: Data is collected at a road-level location in an Italian city.
#Characteristics:
#Rows (Instances): 9358 hourly observations.
#Columns (Variables): 15 variables, including the target (CO), predictors, and temporal details (Date, Time).
#Key Variables for the Project
#Dependent Variable (Target):
#CO (True hourly averaged CO concentration in mg/m³):This is the main pollutant to be predicted.
#Independent Variables (Predictors):
# 1. Pollutant Concentrations: NMHC, Benzene, NOx, NO2 (measured by reference analyzers).
# 2. Sensor Responses:PT08.S1, PT08.S2, PT08.S3, PT08.S4, PT08.S5 (responses from metal oxide sensors).
# 3. Environmental Variables:Temperature, Relative Humidity, Absolute Humidity.

# Clean environment 

rm(list=ls())

# Load Required Libraries
library(tidyverse)   # For data manipulation and visualization
library(corrplot) # For correlation heatmap
library(Metrics)    # For RMSE computation
library(estimatr)
library(modelsummary)


# Set working directory

setwd("C:\\Users\\amare\\OneDrive\\Desktop\\DEB\\Data analytics for economics and business\\Final_Project_Exam")

## Import raw data

data =  read.csv("C:\\Users\\amare\\OneDrive\\Desktop\\DEB\\Data analytics for economics and business\\Final_Project_Exam\\AirQualityUCI_Final.csv")

# Select only relevant columns
data = select(data, c(CO.GT., PT08.S1.CO.,C6H6.GT., NO2.GT., T, RH))

# Rename variables in the data set
colnames(data) <- c(
  "CO", "Sensor_CO", "Benzene", "NO2", 
  "Temperature", "Relative_Humidity")

data[data == -200] <- NA
str(data)
glimpse(data)
summary(data)

data <- na.omit(data)

hist(data$CO)
plot(data$Sensor_CO, data$CO)

plot(data$Benzene, data$CO)
hist(data$Benzene)

summary(data)

# Apply log transformation, handling negative Temperature by adding an offset
data_log <- data %>%
  mutate(
    log_CO = log(CO),
    log_Sensor_CO = log(Sensor_CO),
    log_Benzene = log(Benzene),
    log_NO2 = log(NO2),
    log_Temperature = log(Temperature - min(Temperature) + 1),  # Shift Temperature to positive values
    log_Humidity = log(Relative_Humidity)
  ) %>%
  select(log_CO, log_Sensor_CO, log_Benzene, log_NO2, log_Temperature, log_Humidity)  # Keep only log-transformed variables



## Histogram for selected predictors (NOx and Temperature)
ggplot(data_log, aes(x =log_NO2)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Distribution of NO2 Concentrations", x = "NO2 (µg/m³)", y = "Frequency")

ggplot(data_log, aes(x = log_Temperature)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Temperature", x = "Temperature (°C)", y = "Frequency")
plot(data_log$log_Temperature, data_log$log_CO)

## Scatter plots to explore relationships between CO and key predictors
hist(data_log$log_CO)
plot(data_log$log_Sensor_CO, data$log_CO)

plot(data_log$log_CO)
hist(data_log$log_Benzene)

# Summary statistics for log-transformed variables 
datasummary(
  log_CO + log_Sensor_CO + log_Benzene + log_NO2 + log_Temperature + log_Humidity ~ 
    Mean + Median + SD, data = data_log)

## Correlation heatmap of predictors
corr_matrix <- data_log %>%
  select(log_CO,log_Sensor_CO, log_Benzene, log_NO2, log_Temperature, log_Humidity) %>%
  cor(use = "complete.obs")

corrplot(corr_matrix, method = "circle", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Heatmap of Predictors", 
         mar = c(0, 0, 1, 0))


# Build the full multiple linear regression model
reg1_log <- lm(log_CO ~ log_Benzene + log_NO2 + 
                 log_Sensor_CO + log_Temperature + log_Humidity,
               data = data_log)

reg2_original <- lm(CO ~ Benzene + NO2 + 
                          Sensor_CO + Temperature + Relative_Humidity,
                        data = data)
# Summarize the model to review the coefficients, p-values, and R-squared
summary(reg1_log)
summary(reg2_original)




# Predictions using the log transformed data
predicted_CO <- predict(reg1_log, data = data_log)

# Root Mean Squared Error (RMSE) - Compute RMSE for model evaluation
rmse_value <- rmse(data_log$log_CO, predicted_CO)
cat("RMSE: ", rmse_value, "\n")

# Residual analysis
residuals <- residuals(reg1_log)

# Histogram of residuals to check for normality
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "darkred", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

# Visualization: Actual vs Predicted CO levels
plot1 <- ggplot(data_log, aes(x = log_CO, y = predicted_CO)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid",linewidth = 1) +
  labs(title = "Actual vs Predicted CO Levels", x = "Actual CO", y = "Predicted CO") +
  theme_minimal()

print(plot1)

# Residual plot
residuals <- resid(reg1_log)
plot2 <- ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) +
  labs(title = "Residuals Distribution", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Display plots
print(plot2)

# Variable Importance Visualization
importance <- summary(reg1_log)$coefficients[-1, 4] # Extract p-values
importance_df <- data.frame(Variable = names(importance), P_Value = importance)

plot3 <- ggplot(importance_df, aes(x = reorder(Variable, -P_Value), y = -log10(P_Value))) +
  geom_bar(stat = "identity", fill = "darkblue", alpha = 0.7) +
  labs(title = "Significance of Predictors", x = "Predictors", y = "-log10(P-value)") +
  theme_minimal() +
  coord_flip()
# Display plots
print(plot3)

# Variable Importance Visualization for Original Data
importance_original <- summary(reg2_original)$coefficients[-1, 4] # Extract p-values
importance_df <- data.frame(Variable = names(importance_original), P_Value = importance_original)

plot_original <- ggplot(importance_df, aes(x = reorder(Variable, -P_Value), y = -log10(P_Value))) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.7) +  # Changed color for distinction
  labs(title = "Significance of Predictors (Original Data)", x = "Predictors", y = "-log10(P-value)") +
  theme_minimal() +
  coord_flip()

# Display plot
print(plot_original)






# Save the Transformed Model for Future Use
saveRDS(reg1_log, "reg1_log.rds")






