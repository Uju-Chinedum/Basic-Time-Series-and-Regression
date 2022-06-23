                ###################                
                #### Libraries ####
                ###################
                
install.packages("tidyverse")
install.packages("gglot2")
install.packages("broom")
install.packages("ggpubr")
install.packages("TSstudio")
install.packages("lubridate")

library("tidyverse")
library(readr)
library(ggplot2)
library(TSstudio)
library(lubridate)
library(broom)
library(ggpubr)
                ###########################
                #### Data Manipulation ####
                ###########################
                
# Reading files
time <- read.csv("/Users/test/Desktop/Basic-Time-Series-and-Regression/time.csv")
View(time)
X <- read.csv("/Users/test/Desktop/Basic-Time-Series-and-Regression/X.csv")
View(X)
y <- read_csv("/Users/test/Desktop/Basic-Time-Series-and-Regression/y.csv")
View(y)

# Merging the tables
df1 <- cbind(X, y) %>% cbind(time)
View(df1)

#check for the class
class(df1$time)

x <- as.POSIXct(strptime(df1$time, format = "%S"))

df2 <- mutate(df1, time = x)
View(df2)

df2$date <- as.Date(df2$time)
#df2$date <- as.Date(data$all)
df2$time <- format(as.POSIXct(df2$time),
                    format = "%H:%M:%S")
View(df2)

                #################
                #### Answers ####
                #################
#ts_plot(USgas,
        #title = "US Monthly Natural Gas Consumption",
        #Xtitle = "Time",
        #Ytitle = "Billion Cubic Feet")
# Time-Series plot
ts_plot(df2,
        title = "Regression plots",
        Xtitle = "time",
        Ytitle = "x1")

plot(df1$time, df1$x1, type='o', col = "red", xlab = "time", ylab = "x1",
     main = "Line plots")

plot(df1$time, df1$x2, type="o", col ="red", xlab="time", ylab ="x2",
     main = "Time series plot time vs x2")

plot(time ~ x2, data = df1)

# Distribution of input signal
hist(df1$x1, main = "Distribution of input signals", xlab = "Input signal")

# Distribution of outpu signal
hist(df1$y, main = "Distribution of output signals", xlab = "Output signal")

# Correlation and scatter plots
scatter.smooth(df1$x1, df1$y, xlab = "Input signal", ylab = "Output signal", 
               col = "red")
# plot(x1 ~ y, data = df, xlab = "Input signal", ylab = "Output signal", 
#      col = "red")

# Boxplots of ooutput and sound categories
boxplot(y ~ x2, df1, xlab = "Category", ylab = "Output Signal", col = "purple")



### building regression models

#Y=β0+β1X+β2X2
#PolynomMod <- lm(Consumption ~ Price+I(Price^2), data=data)


#Model 1: y = θ1x13 + θ2x15 + θ3x2 + θbias + ε
#Model 2: y = θ1x1 + θ2x2 + θbias + ε
#Model 3: y = θ1x1 + θ2x12 + θ3x14 + θ4 x2 + θbias + ε
#Model 4: y = θ1x1 + θ2x12 + θ3x13 + θ4x15 + θ5x2 + θbias + ε
#Model 5: y = θ1x1 + θ2x13 + θ3x14 + θ4x2 + θbias + ε

#https://www.analyticsvidhya.com/blog/2021/11/building-an-end-to-end-polynomial-regression-model-in-r/
#https://statisticsglobe.com/fitting-polynomial-regression-model-in-r

len <- length(df2$x1)
len

#m1 = I*(x1**3) + I*(x1**5) + I*(x2) + I
plymod <- lm(y ~ I(x1**3) + I(x1**5) + I(x2), data=df2)
summary(plymod)
# m1 = 16.93575 + 5.17127(x1^3) + -0.53733(x1^5) + 2.20202(x2)
rss <- sum(resid(plymod) ** 2)
rss # 11825.42
var <- rss / (len - 1)
var

#m2  = I*(x1) + I*(x2) + I
plymod2 <- lm(y ~ (x1) + (x2), data=df2)
summary(plymod2)
# m2 = 16.6356 + 8.6291x1 + 2.8140x2
rss2 <- sum(resid(plymod2) ** 2)
rss2 # 11238.95
var2 <- rss2 / (len - 1)
var2

#m3 = I*(x1) + I*(x1**2) + I*(x1**4) + I*(x2) + I
plymod3 <- lm(y ~ I(x1) + I(x1 ** 2) + I(x1 ** 4) + I(x2), data = df2)
summary(plymod3)
# m3 = 10.1737 + 8.5522(x1) + 6.2469(x1^2) + -0.2830(x1^4) + 4.1599(x2)
rss3 <- sum(resid(plymod3) ** 2)
rss3 # 1636.168
var3 <- rss3 / (len - 1)
var3

#m4 = I*(x1) + I*(x1**2) + I*(x1**3) + I*(x1**5) + I*(x2) + I
plymod4 <- lm(y ~ I(x1) + I(x1 ** 2) + I(x1 ** 3) + I(x1 ** 5) + I(x2), 
               data = df2)
summary(polymod4)
# m4 = 10.85328 + 9.35397(x1) + 4.64696(x1^2) + -0.57792(x1^3) + 0.07480(x1^5) + 4.34322(x2)
rss4 <- sum(resid(plymod4) ** 2)
rss4 # 1902.063
var4 <- rss4 / (len - 1)
var4

#m5 = I*(x1) + I*(x1**3) + I*(x1**4) + I*(x2) + I
plymod5 <- lm(y ~ I(x1) + I(x1 ** 3) + I(x1 ** 4) + I(x2), data = df2)
summary(polymod5)
# m5 = 14.13173 + 8.07946(x1) + 0.31445(x1^3) + 0.55921(x1^4) + 3.96815(x2)
rss5 <- sum(resid(plymod5))
rss5 # -3.508305e-14
var5 <- rss5 / (len - 1)
var5

log_like <- function(n, var, RSS){
  loglike <- (-(n / 2)*log(2*pi)) - ((n / 2)*log(var)) - ((1 / 2*var) * RSS)
  return (loglike)
}

loglike <- log_like(len, var, rss)
loglike

loglike2 <- log_like(len, var2, rss2)
loglike2

loglike3 <- log_like(len, var3, rss3)
loglike3

loglike4 <- log_like(len, var4, rss4)
loglike4

loglike5 <- log_like(len, var5, rss5)
loglike5

(2 * 5) - (2 * loglike5)

aic <- AIC(plymod)
aic

aic2 <- AIC(plymod2)
aic2

aic3 <- AIC(plymod3)
aic3

aic4 <- AIC(plymod4)
aic4

aic5 <- AIC(plymod5)
aic5

all_aic <- c(aic, aic2, aic3, aic4, aic5)

bic <- BIC(plymod)
bic

bic2 <- BIC(plymod2)
bic2

bic3 <- BIC(plymod3)
bic3

bic4 <- BIC(plymod4)
bic4

bic5 <- BIC(plymod5)
bic5

all_bic <- c(bic, bic2, bic3, bic4, bic5) 

# Function that generates a qqplot based on models passed
plotqq <- function(model){
  return (qqplot(df2$y, resid(model)))
}
chk <- plotqq(plymod)
chk2 <- plotqq(plymod2)
chk3 <- plotqq(plymod3)
chk4 <- plotqq(plymod4)
chk5 <- plotqq(plymod5)

# Based on the plot, AIC and BIC, plymod3 is the best equation





# Least Square Method
LSM <- function(dfcol, model){
  X_std <- scale(dfcol) # Standardizing the x1 column
  p1 <- (X_std * df2$x1) ** -1
  p2 <- X_std * df2$y
  LSM <- p1 * p2
  LSM
}

# Residual Summ of Squares
fx <- df2$x1 * LSM
sq_diff <- (df2$y - fx) ** 2
RSS <- sum(sq_diff)
RSS



