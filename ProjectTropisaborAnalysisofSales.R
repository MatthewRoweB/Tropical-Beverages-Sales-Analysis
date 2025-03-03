## Tropisabor Sales Analysis Project
## Matthew Rowe

rm(list=ls())
graphics.off() 

# ----------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(magrittr)
Sales_Year <- read_excel("C:/Users/mattr/Downloads/Ventas Topisabor Hn (2).xlsx")
View(Sales_Year)

Sales_Year <- Sales_Year %>%
  rename(c('Year' = 'Año','Amount' = 'Monto $ Canadiense')) %>% 
  select(c('Year', 'Amount')) %>%
  na.omit()

model <- arima(Sales_Year$Amount, order=c(1,0,0))
acf(Sales_Year$Amount, main="Yearly Sales 2015 - 2024")  # Lag k Autocorrelation Plot

# -----------------------------------------------------------------------------------------------------------------------------------
# Time Series Analysis from 2019 - 2024 (Since Exportation Variable was included)

# Data cleansing 

Monthly_Sales_2024 <- read_excel("C:/Users/mattr/Downloads/Ventas Topisabor Hn (2).xlsx", sheet = 2)
Monthly_Sales_2023 <- read_excel("C:/Users/mattr/Downloads/Ventas Topisabor Hn (2).xlsx", sheet = 3)
Monthly_Sales_2022 <- read_excel("C:/Users/mattr/Downloads/Ventas Topisabor Hn (2).xlsx", sheet = 4)
Monthly_Sales_2021 <- read_excel("C:/Users/mattr/Downloads/Ventas Topisabor Hn (2).xlsx", sheet = 5)
Monthly_Sales_2019 <- read_excel("C:/Users/mattr/Downloads/Ventas Topisabor Hn (2).xlsx", sheet = 7)        # Skip Covid 2020 Dataset

monthly_sales_cleanser <- function(data) {
  data <- data %>%
    slice(4:15) %>%
    select(c(1,6)) %>%
    setNames(c('Months', 'Sales_in_$'))
  return(data)
}

list_sales_unclean <- list(Monthly_Sales_2019, Monthly_Sales_2021, Monthly_Sales_2022, Monthly_Sales_2023, Monthly_Sales_2024)
list_sales <- lapply(list_sales_unclean, monthly_sales_cleanser)

monthly_sales <- bind_rows(list_sales)
values = seq(from = as.Date("2019-01-31"), to = as.Date("2024-12-31"), by = 'month')[-(13:24)]   # Cut out Covid Dates
monthly_sales$Months <- values

library(ggplot2)

ts_sales <- ts(monthly_sales$`Sales_in_$`, frequency=12, start=1, end = numeric())
plot(ts_sales, main='Time Series for Monthly Sales 2019-2024')


# Perform Augmented Dickey-Fuller Test to check Stationarity of the Time Series Data

library(tseries)
adf.test(ts_sales)

# The test statistic and p-value come out as -2.2172 and 0.487 respectively. Since the p-value is greater than 0.05, we fail to
# reject the null hypothesis, implying that the time series is non-stationary. 

# ACF and PACF plots
acf(as.numeric(ts_sales))
pacf(as.numeric(ts_sales))

# Since PACF plot indicates there is strong partial correlation up to lag 3. So its best to use an AR(3) model. 

model = arima(as.numeric(ts_sales), order=c(3,0,0))
model$coef

eHat <- residuals(model)
yHat <- as.numeric(ts_sales)- eHat
plot(as.vector(yHat), as.vector(eHat), pch=19)
abline(0,0)     

qqnorm(eHat)
qqline(eHat)

# Ljung-Box Test to check for autocorrelation (or independence) among residuals. Ideally we want residuals to be independent
# as is a fundamental assumptions of AR(1) models.

Box.test(as.numeric(ts_sales), lag = 3, type = "Ljung")

# The p-value is much smaller than 0.01 thus we reject the null hypothesis. This implies that the time series does contain
# autocorrelation. In other words, the errors are not white noise, which is not good as one of the key assumptions for AR models
# is for the error to be of white noise (has no autocorrelation)

library(forecast)

model_forecast <- auto.arima(as.numeric(monthly_sales$`Sales_in_$`))
summary(model_forecast)

f<-forecast(model_forecast, level=c(95), h=1*3)
plot(f)

model_decompose <- decompose(as.numeric(ts_sales), type='additive')





## Sales Prediction Based on Units (By product and its respective client)

# PCA -> reduce dimensions, linear combination with largest variance
# FA -> Identify factors that relate with variables
# K-means clustering -> Anomaly Detection for Multivariate Data (Outliers)
# Multivariate Regression -> Sales Prediction
#                           -Forward/Backward Elimination for best model
#                           -Ridge Regression -> Variables have multicollinearity (not worried about dropping features)
#                           -Lasso Regression -> Variables have almost no multicollinearity (dropping features is preferable)

                            



# https://weatherandclimate.com/canada/ontario/niagara-falls/january-2018 link for precipitation, wind, wind gust, snow depth for
# Niagara Falls Prediction

# start simple: holt winters, ARIMA, add seasonality etc. OP, have you looked at seasonality, stationarity of the data yet?


# ------------------------------------------------------------------------------------------------------------------------------









































