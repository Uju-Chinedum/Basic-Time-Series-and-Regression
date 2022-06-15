                ###################                
                #### Libraries ####
                ###################
                
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
# install.packages("TSstudio")
library(TSstudio)
# install.packages("lubridate")
library(lubridate)
# install.packages(c("broom", "ggpubr"))
library(broom)
library(ggpubr)
                ###########################
                #### Data Manipulation ####
                ###########################
                
# Reading files
time <- read_csv("../Sir Divine/3/Basic-Time-Series-and-Regression/time.csv")
View(time)
X <- read_csv("../Sir Divine/3/Basic-Time-Series-and-Regression/X.csv")
View(X)
y <- read_csv("../Sir Divine/3/Basic-Time-Series-and-Regression/y.csv")
View(y)

# Merging the tables
df <- cbind(X, y) %>% cbind(time)
View(df)

class(df$time)
x <- as.POSIXct(strptime(df$time, format = "%S"))
df <- mutate(df, time = x)
View(df)

                #################
                #### Answers ####
                #################

# Time-Series plot
ts_plot(df)

# Distribution of input signal
hist(df$x1, main = "Distribution of input signals", xlab = "Input signal")

# Distribution of outpu signal
hist(df$y, main = "Distribution of output signals", xlab = "Output signal")

# Correlation and scatter plots
scatter.smooth(df$x1, df$y, xlab = "Input signal", ylab = "Output signal", 
               col = "red")
# plot(x1 ~ y, data = df, xlab = "Input signal", ylab = "Output signal", 
#      col = "red")

# Boxplots of ooutput and sound categories
boxplot(y ~ x2, df, xlab = "Category", ylab = "Output Signal", col = "purple")

# Regression
summary(lm(x1 ~ y, data = df))

ggplot(df, aes(x = df$x1, y = df$y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
