# Week 2 L2:
# Introduction to Linear Regression
library(dplyr)
library(ggplot2)
# install.packages("hrbrthemes") # NOTE: CRAN version is 0.8.7
#  or
# remotes::install_git("https://codeberg.org/hrbrmstr/hrbrthemes.git")
library(hrbrthemes)
library(tidyverse)
library(viridis)


# change the dir where you store your files:
setwd("/Users/mniazy/Desktop/BINF W26/Week 2")
glucose_data <- read.csv("glucose_data.csv")
#In human metabolism, blood glucose levels are tightly regulated and influenced by dietary intake and physiological factors. A researcher is interested in understanding the relationship between blood glucose concentration and the time since the last meal in healthy adults.
#As part of this study, participants consume a standardized meal, and blood glucose levels are measured at different time points following the meal (ranging from 0 to 6 hours). The goal is to examine how blood glucose levels change as a function of time since food intake.
#The data obtained from this experiment are used to model the relationship between blood glucose concentration (response variable) and time since last meal (predictor variable) using simle linear regression.

# EDA:
str(glucose_data)

#There are just two numeric variables. The first column (Time since last meal (min) contains the information about the experimental time treatments, and the second column (Glucose levels (mg.dL) contain the glucose measurements.
# visualise the data next using A scatter plot so that we understand it more.
ggplot(glucose_data, aes(x = Time, y = Glucose)) +
  geom_point( color = "#b23a3a",
              size = 2.8,
              alpha = 0.9) 

# fitting a model and plot the linear regrssion 
ggplot(glucose_data, aes(x = Time, y = Glucose)) +
  # Data points
  geom_point(
    color = "#b23a3a",
    size = 2.8,
    alpha = 0.9
  ) +
  
  # Regression line
  geom_line(
    aes(y = Glucose),
    color = "navy",
    linewidth = 1.3
  ) +
  geom_smooth(method = "lm", se = FALSE)



#Remember, Glucose is the response variable and Time is the predictor variable, so they belong on the  
#y and x axes, respectively.

#With confidence interval
ggplot(glucose_data, aes(x = Time, y = Glucose)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)


model <- lm(Glucose ~ Time, data = glucose_data)
glucose_data$fitted <- fitted(model)

# what happen if we plot the fitted values after adding them to the data
ggplot(glucose_data, aes(x = Time, y = Glucose)) +
  
  # Residual lines (vertical distance to regression line)
  geom_segment(
    aes(x = Time, xend = Time,
        y = fitted, yend = Glucose),
    color = "grey70",
    linewidth = 0.6
  ) +
  
  # Data points
  geom_point(
    color = "#b23a3a",
    size = 2.8,
    alpha = 0.9
  ) +
  
  # Regression line
  geom_line(
    aes(y = fitted),
    color = "navy",
    linewidth = 1.3
  ) +
  
  labs(
    x = "Time",
    y = "Glucose level"
  ) +
  
  theme_classic(base_size = 14)

##########
# 1. First, let's get the true values (observed) and predicted values
glucose_data$fitted <- fitted(model)

# 2. Create box plot with true values and mean
# If Time has multiple observations per time point
ggplot(glucose_data, aes(x = factor(Time), y = Glucose)) +
  # Box plot showing distribution
  geom_boxplot(
    fill = "lightblue",
    alpha = 0.7,
    outlier.shape = NA  # Hide outliers since we'll show all points
  ) +
  
  # Individual true values (observed data points)
  geom_jitter(
    width = 0.2,  # Spread points slightly
    height = 0,
    aes(color = "True Values"),
    size = 2,
    alpha = 0.7
  ) +
  
  # Mean of observed values for each time point
  stat_summary(
    fun = mean,
    geom = "point",
    aes(shape = "Mean of Observed"),
    size = 4,
    color = "red",
    fill = "red"
  ) +
  
  # Predicted values from the model (fitted means)
  geom_point(
    aes(y = fitted, shape = "Fitted Values"),
    size = 3,
    color = "darkblue",
    alpha = 0.8
  ) +
  
  # Add regression line connecting fitted values
  geom_line(
    aes(y = fitted, group = 1),
    color = "navy",
    linewidth = 1,
    alpha = 0.7
  ) +
  
  # Customize shapes and colors
  scale_shape_manual(
    name = "",
    values = c("Mean of Observed" = 18, "Fitted Values" = 16)
  ) +
  
  scale_color_manual(
    name = "",
    values = c("True Values" = "#b23a3a")
  ) +
  
  labs(
    x = "Time",
    y = "Glucose level",
    title = "Glucose Levels: True Values, Means, and Model Predictions"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(t = -10)
  )

# now label the true and the observed values

ggplot(glucose_data, aes(x = factor(Time), y = Glucose)) +
  geom_boxplot(
    fill = "lightblue",
    alpha = 0.7
  ) +
  
  # True values
  geom_jitter(
    width = 0.15,
    height = 0,
    aes(color = "True Values"),
    size = 2
  ) +
  
  # Observed mean
  stat_summary(
    fun = mean,
    geom = "point",
    aes(shape = "Observed Mean"),
    size = 4,
    color = "red"
  ) +
  
  # Fitted values (model predictions)
  geom_point(
    aes(y = fitted, shape = "Model Estimation"),
    size = 3,
    color = "darkblue"
  ) +
  
  # Facet by group if you have multiple groups
  # facet_wrap(~ Group) +  # Uncomment if you have groups
  
  labs(
    x = "Time",
    y = "Glucose level"
  ) +
  
  scale_shape_manual(
    name = "",
    values = c("Observed Mean" = 17, "Model Prediction" = 16)
  ) +
  
  scale_color_manual(
    name = "",
    values = c("True Values" = "#666666")
  ) +
  
  theme_minimal(base_size = 14)
##
# Simple box plot with true values and mean
ggplot(glucose_data, aes(x = factor(Time), y = Glucose)) +
  # Box plot
  geom_boxplot(
    fill = "lightblue",
    alpha = 0.5,
    width = 0.6
  ) +
  
  # All individual points
  geom_jitter(
    width = 0.1,
    height = 0,
    color = "darkred",
    size = 2,
    alpha = 0.6
  ) +
  
  # Mean point
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 5,
    fill = "gold",
    color = "black"
  ) +
  
  # Add mean value as text
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(..y.., 1)),
    vjust = -2,
    size = 4
  ) +
  
  labs(
    x = "Time",
    y = "Glucose Level",
    title = "Box Plot of Glucose Levels with Mean"
  ) +
  
  theme_bw(base_size = 14)


######################################################

