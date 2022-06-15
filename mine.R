                ###################                
                #### Libraries ####
                ###################
                
# install.packages("tidyverse")
library(tidyverse)
# install.packages("TSstudio")
library(TSstudio)

                ###########################
                #### Data Manipulation ####
                ###########################
                
# Reading files
time <- read_csv("../Sir Divine/3/time.csv")
View(time)
X <- read_csv("../Sir Divine/3/X.csv")
View(X)
y <- read_csv("../Sir Divine/3/y.csv")
View(y)

# Merging the tables
df <- cbind(X, y) %>% cbind(time)
View(df)

class(df$time)
x <- as.POSIXct(strptime(df$time, format = "%S"))
df <- mutate(df, time = x)
View(df)

# Time-Series plot
ts_plot(df)

# Distribution of input signal
hist(df$x1, main = "Distribution of input signals", xlab = "Input signal")

# Distribution of outpu signal
hist(df$y, main = "Distribution of output signals", xlab = "Output signal")

# Correlation and scatter plots
scatter.smooth(df$x1, df$y, xlab = "Input signal", ylab = "Output signal")
