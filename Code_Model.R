# !!! Run this code after Code_Data.R !!!

# All libraries we used
library(dplyr)
library(zoo)
library(xts)
library(tidyr)
library(magrittr)
library(ggplot2)
library(tseries)
library(forecast)
library(stargazer)

# Modelisation of the SARIMA

# Get suggestion of hyperparameters for PARIS with auto.arima
paris_ts19_auto_params <- forecast::auto.arima(paris_ts19)

# We are going to favor manual selection of hyperparameters with the help of a loop. 
# We use the AIC and BIC to select the best model for each airport
### IMPORTANT : loops take a long time to run, so you can directly import the best hyperparameters 
### for each airport by running the two lines below.

# Sourcing the file wich contains the functions to find the best parameters with regard to AIC and BIC
source("hyperparameters-selection.R")

# Loading the best hyperparameters for our model we previously found
load("hyperparameters.RData")

# Uncomment the code below if you want to test the loops :

### CODE TO UNCOMMENT TO TEST THE LOOPS ###

"
paris_ts19_aic_params <- find_aic_params(paris_ts19)
paris_ts19_bic_params <- find_bic_params(paris_ts19)

madrid_ts19_aic_params <- find_aic_params(madrid_ts19)
madrid_ts19_bic_params <- find_bic_params(madrid_ts19)

roma_ts19_aic_params <- find_aic_params(roma_ts19)
roma_ts19_bic_params <- find_bic_params(roma_ts19)

copenhagen_ts19_aic_params <- find_aic_params(copenhagen_ts19)
copenhagen_ts19_bic_params <- find_bic_params(copenhagen_ts19)

oslo_ts19_aic_params <- find_aic_params(oslo_ts19)
oslo_ts19_bic_params <- find_bic_params(oslo_ts19)
"

# Modeling the SARIMA

# For each airports, we do the difference on the AIC model only because using the Ljung Box test results,
# it suggests less autocorrelation in the residuals of the AIC model than the BIC model.

# Modeling for PARIS CDG

# AIC

# Setting the parameters
non_seasonal_order_paris_aic <- paris_ts19_aic_params$min_AIC_params[1:3]
seasonal_order_paris_aic <- paris_ts19_aic_params$min_AIC_params[4:6]

# Fitting the model and checking residuals
sarima_aic_paris <- Arima(paris_ts19, order=non_seasonal_order_paris_aic, seasonal=list(order=seasonal_order_paris_aic, period=12))
sarima_aic_paris_residuals <- checkresiduals(sarima_aic_paris)
p_value <- sarima_aic_paris_residuals$p.value
checkresiduals(sarima_aic_paris)

# Plot of the prediction of the 41 next months (end of the raw data)
forecasts_aic_paris <- forecast(sarima_aic_paris, h=41)
plot(forecasts_aic_paris)

# Information on the model
sarima_aic_paris

# Forecast the next 41 periods
forecasted_values_paris <- forecast(sarima_aic_paris, h=41)


# For actual data: Create a tibble/data frame with time and actual values
actual_data_paris <- tibble(
  time = as.Date(time(paris_ts)),
  Value = as.vector(paris_ts),
  Type = 'Actual'
)

actual_data_paris <- actual_data_paris[actual_data_paris$time <= as.Date("2023-05-01"), ]

# For forecasted data: Create a tibble/data frame with forecasted times and values
forecast_data_paris <- tibble(
  time = as.Date(time(forecasted_values_paris$mean)),
  Value = as.vector(forecasted_values_paris$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data_paris <- bind_rows(actual_data_paris, forecast_data_paris)

# Plot of the coparison between actual and forecasted values
ggplot(data = combined_data_paris, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Paris passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Our actual data on the forecast period
actual_data_subset_paris <- actual_data_paris[actual_data_paris$time >= as.Date("2020-01-01"),]

# Calculate the difference between forecasted and actual values
forecast_data_paris$diff <- forecast_data_paris$Value - actual_data_subset_paris$Value

# Plot of the difference
ggplot(data = forecast_data_paris, aes(x = time, y = diff, color = Type)) +
  geom_line() +
  labs(title = "Difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()

# Scaling the difference
forecast_data_paris$scalediff <- forecast_data_paris$diff / forecast_data_paris$Value

# Plot the scaled difference
ggplot(data = forecast_data_paris, aes(x = time, y = scalediff, color = Type)) +
  geom_line() +
  labs(title = "Scaled difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()



# Setting the parameters
non_seasonal_order_paris_bic <- paris_ts19_bic_params$min_BIC_params[1:3]
seasonal_order_paris_bic <- paris_ts19_bic_params$min_BIC_params[4:6]

# Fitting the model and checking residuals
sarima_bic_paris <- Arima(paris_ts19, order=non_seasonal_order_paris_bic, seasonal=list(order=seasonal_order_paris_bic, period=12))
checkresiduals(sarima_bic_paris)

# Plot of the prediction of the 12 next months
forecasts_bic_paris <- forecast(sarima_bic_paris, h=41)
plot(forecasts_bic_paris)

# Information on the model
sarima_bic_paris

# Modeling for MADRID

# AIC

# Setting the parameters
non_seasonal_order_madrid_aic <- madrid_ts19_aic_params$min_AIC_params[1:3]
seasonal_order_madrid_aic <- madrid_ts19_aic_params$min_AIC_params[4:6]

# Fitting the model and checking residuals
sarima_aic_madrid <- Arima(madrid_ts19, order=non_seasonal_order_madrid_aic, seasonal=list(order=seasonal_order_madrid_aic, period=12))
sarima_aic_madrid_residuals <- checkresiduals(sarima_aic_madrid)
p_value <- sarima_aic_madrid_residuals$p.value
checkresiduals(sarima_aic_madrid)

# Plot of the prediction of the 41 next months (end of the raw data)
forecasts_aic_madrid <- forecast(sarima_aic_madrid, h=41)
plot(forecasts_aic_madrid)

# Information on the model
sarima_aic_madrid

# Forecast the next 41 periods
forecasted_values_madrid <- forecast(sarima_aic_madrid, h=41)


# For actual data: Create a tibble/data frame with time and actual values
actual_data_madrid <- tibble(
  time = as.Date(time(madrid_ts)),
  Value = as.vector(madrid_ts),
  Type = 'Actual'
)

actual_data_madrid <- actual_data_madrid[actual_data_madrid$time <= as.Date("2023-05-01"), ]

# For forecasted data: Create a tibble/data frame with forecasted times and values
forecast_data_madrid <- tibble(
  time = as.Date(time(forecasted_values_madrid$mean)),
  Value = as.vector(forecasted_values_madrid$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data_madrid <- bind_rows(actual_data_madrid, forecast_data_madrid)

# Plot of the coparison between actual and forecasted values
ggplot(data = combined_data_madrid, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Madrid passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Our actual data on the forecast period
actual_data_subset_madrid <- actual_data_madrid[actual_data_madrid$time >= as.Date("2020-01-01"),]

# Calculate the difference between forecasted and actual values
forecast_data_madrid$diff <- forecast_data_madrid$Value - actual_data_subset_madrid$Value

# Plot of the difference
ggplot(data = forecast_data_madrid, aes(x = time, y = diff, color = Type)) +
  geom_line() +
  labs(title = "Difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()

# Scaling the difference
forecast_data_madrid$scalediff <- forecast_data_madrid$diff / forecast_data_madrid$Value

# Plot the scaled difference
ggplot(data = forecast_data_madrid, aes(x = time, y = scalediff, color = Type)) +
  geom_line() +
  labs(title = "Scaled difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()



# Setting the parameters
non_seasonal_order_madrid_bic <- madrid_ts19_bic_params$min_BIC_params[1:3]
seasonal_order_madrid_bic <- madrid_ts19_bic_params$min_BIC_params[4:6]

# Fitting the model and checking residuals
sarima_bic_madrid <- Arima(madrid_ts19, order=non_seasonal_order_madrid_bic, seasonal=list(order=seasonal_order_madrid_bic, period=12))
checkresiduals(sarima_bic_madrid)

# Plot of the prediction of the 12 next months
forecasts_bic_madrid <- forecast(sarima_bic_madrid, h=41)
plot(forecasts_bic_madrid)

# Information on the model
sarima_bic_madrid


# Modeling for ROMA

# AIC

# Setting the parameters
non_seasonal_order_roma_aic <- roma_ts19_aic_params$min_AIC_params[1:3]
seasonal_order_roma_aic <- roma_ts19_aic_params$min_AIC_params[4:6]

# Fitting the model and checking residuals
sarima_aic_roma <- Arima(roma_ts19, order=non_seasonal_order_roma_aic, seasonal=list(order=seasonal_order_roma_aic, period=12))
sarima_aic_roma_residuals <- checkresiduals(sarima_aic_roma)
p_value <- sarima_aic_roma_residuals$p.value
checkresiduals(sarima_aic_roma)

# Plot of the prediction of the 41 next months (end of the raw data)
forecasts_aic_roma <- forecast(sarima_aic_roma, h=41)
plot(forecasts_aic_roma)

# Information on the model
sarima_aic_roma

# Forecast the next 41 periods
forecasted_values_roma <- forecast(sarima_aic_roma, h=41)


# For actual data: Create a tibble/data frame with time and actual values
actual_data_roma <- tibble(
  time = as.Date(time(roma_ts)),
  Value = as.vector(roma_ts),
  Type = 'Actual'
)

actual_data_roma <- actual_data_roma[actual_data_roma$time <= as.Date("2023-05-01"), ]

# For forecasted data: Create a tibble/data frame with forecasted times and values
forecast_data_roma <- tibble(
  time = as.Date(time(forecasted_values_roma$mean)),
  Value = as.vector(forecasted_values_roma$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data_roma <- bind_rows(actual_data_roma, forecast_data_roma)

# Plot of the coparison between actual and forecasted values
ggplot(data = combined_data_roma, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Roma passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Our actual data on the forecast period
actual_data_subset_roma <- actual_data_roma[actual_data_roma$time >= as.Date("2020-01-01"),]

# Calculate the difference between forecasted and actual values
forecast_data_roma$diff <- forecast_data_roma$Value - actual_data_subset_roma$Value

# Plot of the difference
ggplot(data = forecast_data_roma, aes(x = time, y = diff, color = Type)) +
  geom_line() +
  labs(title = "Difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()

# Scaling the difference
forecast_data_roma$scalediff <- forecast_data_roma$diff / forecast_data_roma$Value

# Plot the scaled difference
ggplot(data = forecast_data_roma, aes(x = time, y = scalediff, color = Type)) +
  geom_line() +
  labs(title = "Scaled difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()



# Setting the parameters
non_seasonal_order_roma_bic <- roma_ts19_bic_params$min_BIC_params[1:3]
seasonal_order_roma_bic <- roma_ts19_bic_params$min_BIC_params[4:6]

# Fitting the model and checking residuals
sarima_bic_roma <- Arima(roma_ts19, order=non_seasonal_order_roma_bic, seasonal=list(order=seasonal_order_roma_bic, period=12))
checkresiduals(sarima_bic_roma)

# Plot of the prediction of the 12 next months
forecasts_bic_roma <- forecast(sarima_bic_roma, h=41)
plot(forecasts_bic_roma)

# Information on the model
sarima_bic_roma

# Modeling for COPENHAGEN

# AIC

# Setting the parameters
non_seasonal_order_copenhagen_aic <- copenhagen_ts19_aic_params$min_AIC_params[1:3]
seasonal_order_copenhagen_aic <- copenhagen_ts19_aic_params$min_AIC_params[4:6]

# Fitting the model and checking residuals
sarima_aic_copenhagen <- Arima(copenhagen_ts19, order=non_seasonal_order_copenhagen_aic, seasonal=list(order=seasonal_order_copenhagen_aic, period=12))
sarima_aic_copenhagen_residuals <- checkresiduals(sarima_aic_copenhagen)
p_value <- sarima_aic_copenhagen_residuals$p.value
checkresiduals(sarima_aic_copenhagen)

# Plot of the prediction of the 41 next months (end of the raw data)
forecasts_aic_copenhagen <- forecast(sarima_aic_copenhagen, h=41)
plot(forecasts_aic_copenhagen)

# Information on the model
sarima_aic_copenhagen

# Forecast the next 41 periods
forecasted_values_copenhagen <- forecast(sarima_aic_copenhagen, h=41)


# For actual data: Create a tibble/data frame with time and actual values
actual_data_copenhagen <- tibble(
  time = as.Date(time(copenhagen_ts)),
  Value = as.vector(copenhagen_ts),
  Type = 'Actual'
)

actual_data_copenhagen <- actual_data_copenhagen[actual_data_copenhagen$time <= as.Date("2023-05-01"), ]

# For forecasted data: Create a tibble/data frame with forecasted times and values
forecast_data_copenhagen <- tibble(
  time = as.Date(time(forecasted_values_copenhagen$mean)),
  Value = as.vector(forecasted_values_copenhagen$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data_copenhagen <- bind_rows(actual_data_copenhagen, forecast_data_copenhagen)

# Plot of the coparison between actual and forecasted values
ggplot(data = combined_data_copenhagen, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Copenhagen passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Our actual data on the forecast period
actual_data_subset_copenhagen <- actual_data_copenhagen[actual_data_copenhagen$time >= as.Date("2020-01-01"),]

# Calculate the difference between forecasted and actual values
forecast_data_copenhagen$diff <- forecast_data_copenhagen$Value - actual_data_subset_copenhagen$Value

# Plot of the difference
ggplot(data = forecast_data_copenhagen, aes(x = time, y = diff, color = Type)) +
  geom_line() +
  labs(title = "Difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()

# Scaling the difference
forecast_data_copenhagen$scalediff <- forecast_data_copenhagen$diff / forecast_data_copenhagen$Value

# Plot the scaled difference
ggplot(data = forecast_data_copenhagen, aes(x = time, y = scalediff, color = Type)) +
  geom_line() +
  labs(title = "Scaled difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()



# Setting the parameters
non_seasonal_order_copenhagen_bic <- copenhagen_ts19_bic_params$min_BIC_params[1:3]
seasonal_order_copenhagen_bic <- copenhagen_ts19_bic_params$min_BIC_params[4:6]

# Fitting the model and checking residuals
sarima_bic_copenhagen <- Arima(copenhagen_ts19, order=non_seasonal_order_copenhagen_bic, seasonal=list(order=seasonal_order_copenhagen_bic, period=12))
checkresiduals(sarima_bic_copenhagen)

# Plot of the prediction of the 12 next months
forecasts_bic_copenhagen <- forecast(sarima_bic_copenhagen, h=41)
plot(forecasts_bic_copenhagen)

# Information on the model
sarima_bic_copenhagen

# Modeling for OSLO

# AIC

# Setting the parameters
non_seasonal_order_oslo_aic <- oslo_ts19_aic_params$min_AIC_params[1:3]
seasonal_order_oslo_aic <- oslo_ts19_aic_params$min_AIC_params[4:6]

# Fitting the model and checking residuals
sarima_aic_oslo <- Arima(oslo_ts19, order=non_seasonal_order_oslo_aic, seasonal=list(order=seasonal_order_oslo_aic, period=12))
sarima_aic_oslo_residuals <- checkresiduals(sarima_aic_oslo)
p_value <- sarima_aic_oslo_residuals$p.value
checkresiduals(sarima_aic_oslo)

# Plot of the prediction of the 41 next months (end of the raw data)
forecasts_aic_oslo <- forecast(sarima_aic_oslo, h=41)
plot(forecasts_aic_oslo)

# Information on the model
sarima_aic_oslo

# Forecast the next 41 periods
forecasted_values_oslo <- forecast(sarima_aic_oslo, h=41)


# For actual data: Create a tibble/data frame with time and actual values
actual_data_oslo <- tibble(
  time = as.Date(time(oslo_ts)),
  Value = as.vector(oslo_ts),
  Type = 'Actual'
)

actual_data_oslo <- actual_data_oslo[actual_data_oslo$time <= as.Date("2023-05-01"), ]

# For forecasted data: Create a tibble/data frame with forecasted times and values
forecast_data_oslo <- tibble(
  time = as.Date(time(forecasted_values_oslo$mean)),
  Value = as.vector(forecasted_values_oslo$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data_oslo <- bind_rows(actual_data_oslo, forecast_data_oslo)

# Plot of the coparison between actual and forecasted values
ggplot(data = combined_data_oslo, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Oslo passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Our actual data on the forecast period
actual_data_subset_oslo <- actual_data_oslo[actual_data_oslo$time >= as.Date("2020-01-01"),]

# Calculate the difference between forecasted and actual values
forecast_data_oslo$diff <- forecast_data_oslo$Value - actual_data_subset_oslo$Value

# Plot of the difference
ggplot(data = forecast_data_oslo, aes(x = time, y = diff, color = Type)) +
  geom_line() +
  labs(title = "Difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()

# Scaling the difference
forecast_data_oslo$scalediff <- forecast_data_oslo$diff / forecast_data_oslo$Value

# Plot the scaled difference
ggplot(data = forecast_data_oslo, aes(x = time, y = scalediff, color = Type)) +
  geom_line() +
  labs(title = "Scaled difference between Forecasted and Actual Values", x = "Time", y = "Difference") +
  theme_minimal()



# Setting the parameters
non_seasonal_order_oslo_bic <- oslo_ts19_bic_params$min_BIC_params[1:3]
seasonal_order_oslo_bic <- oslo_ts19_bic_params$min_BIC_params[4:6]

# Fitting the model and checking residuals
sarima_bic_oslo <- Arima(oslo_ts19, order=non_seasonal_order_oslo_bic, seasonal=list(order=seasonal_order_oslo_bic, period=12))
checkresiduals(sarima_bic_oslo)

# Plot of the prediction of the 12 next months
forecasts_bic_oslo <- forecast(sarima_bic_oslo, h=41)
plot(forecasts_bic_oslo)

# Information on the model
sarima_bic_oslo

# SARIMAX

# Models estimation

# As the SARIMA, we have imported the best hyperparameters because yhe auto.arima function is long to run
# Uncomment the code below if you want to test the loops :

"
### CODE TO UNCOMMENT TO TEST THE LOOPS ###
sarimax_auto_paris <- auto.arima(paris_ts, xreg=paris_covid, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
sarimax_auto_madrid <- auto.arima(madrid_ts, xreg=madrid_covid, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
sarimax_auto_roma <- auto.arima(roma_ts, xreg=roma_covid, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
sarimax_auto_copenhagen <- auto.arima(copenhagen_ts, xreg=copenhagen_covid, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)
sarimax_auto_oslo <- auto.arima(oslo_ts, xreg=oslo_covid, seasonal=TRUE, stepwise = FALSE, approximation = FALSE)

# Here we save the parameters to avoid having to run again the functions auto.arima each time

paris_auto_params <- reorder_params(sarimax_auto_paris$arma)
madrid_auto_params <- reorder_params(sarimax_auto_madrid$arma)
roma_auto_params <- reorder_params(sarimax_auto_roma$arma)
copenhagen_auto_params <- reorder_params(sarimax_auto_copenhagen$arma)
oslo_auto_params <- reorder_params(sarimax_auto_oslo$arma)
"


# Models fitting
# Paris
sarimax_paris <- Arima(paris_ts, order=paris_auto_params[1:3], seasonal=paris_auto_params[4:6], xreg=paris_covid)

# Madrid
sarimax_madrid <- Arima(madrid_ts, order=madrid_auto_params[1:3], seasonal=madrid_auto_params[4:6], xreg=madrid_covid)

# Roma
sarimax_roma <- Arima(roma_ts, order=roma_auto_params[1:3], seasonal=roma_auto_params[4:6], xreg=roma_covid)

# Copenhagen
sarimax_copenhagen <- Arima(copenhagen_ts, order=copenhagen_auto_params[1:3], seasonal=copenhagen_auto_params[4:6], xreg=copenhagen_covid)

# Oslo
sarimax_oslo <- Arima(oslo_ts, order=oslo_auto_params[1:3], seasonal=oslo_auto_params[4:6], xreg=oslo_covid)

# Forecast for 2045
forecasted_values_sarimax <- forecast::forecast(sarimax_paris, xreg=paris_covid)
forecasted_values_sarima <- forecast::forecast(sarima_aic_paris, h=300)

# For actual data: Create a tibble/data frame with time and actual values
actual_data <- tibble(
  time = as.Date(time(paris_ts)),
  Value = as.vector(paris_ts),
  Type = 'Actual'
)

# For forecasted data: Create a tibble/data frame with forecasted times and values

# SARIMAX
forecast_data_sarimax <- tibble(
  time = as.Date(time(forecasted_values_sarimax$mean)),
  Value = as.vector(forecasted_values_sarimax$mean),
  Type = 'Forecast SARIMAX'
)

# SARIMA
forecast_data_sarima <- tibble(
  time = as.Date(time(forecasted_values_sarima$mean)),
  Value = as.vector(forecasted_values_sarima$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data <- bind_rows(actual_data, forecast_data_sarimax, forecast_data_sarima)

# Plot
ggplot(data = combined_data, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Charles de Gaulles passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Forecast for 2045
forecasted_values_sarimax <- forecast::forecast(sarimax_madrid, xreg=madrid_covid)
forecasted_values_sarima <- forecast::forecast(sarima_aic_madrid, h=300)
# For actual data: Create a tibble/data frame with time and actual values
actual_data <- tibble(
  time = as.Date(time(madrid_ts)),
  Value = as.vector(madrid_ts),
  Type = 'Actual'
)

# For forecasted data: Create a tibble/data frame with forecasted times and values

# SARIMAX
forecast_data_sarimax <- tibble(
  time = as.Date(time(forecasted_values_sarimax$mean)),
  Value = as.vector(forecasted_values_sarimax$mean),
  Type = 'Forecast SARIMAX'
)

# SARIMA
forecast_data_sarima <- tibble(
  time = as.Date(time(forecasted_values_sarima$mean)),
  Value = as.vector(forecasted_values_sarima$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data <- bind_rows(actual_data, forecast_data_sarimax, forecast_data_sarima)

# Plot
ggplot(data = combined_data, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Adolfo Suarez passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Forecast for 2045
forecasted_values_sarimax <- forecast::forecast(sarimax_roma, xreg=roma_covid)
forecasted_values_sarima <- forecast::forecast(sarima_aic_roma, h=300)
# For actual data: Create a tibble/data frame with time and actual values
actual_data <- tibble(
  time = as.Date(time(roma_ts)),
  Value = as.vector(roma_ts),
  Type = 'Actual'
)

# For forecasted data: Create a tibble/data frame with forecasted times and values

# SARIMAX
forecast_data_sarimax <- tibble(
  time = as.Date(time(forecasted_values_sarimax$mean)),
  Value = as.vector(forecasted_values_sarimax$mean),
  Type = 'Forecast SARIMAX'
)

# SARIMA
forecast_data_sarima <- tibble(
  time = as.Date(time(forecasted_values_sarima$mean)),
  Value = as.vector(forecasted_values_sarima$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data <- bind_rows(actual_data, forecast_data_sarimax, forecast_data_sarima)

# Plot using
ggplot(data = combined_data, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Fiumicino passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Forecast for 2045
forecasted_values_sarimax <- forecast::forecast(sarimax_copenhagen, xreg=copenhagen_covid)
forecasted_values_sarima <- forecast::forecast(sarima_aic_copenhagen)
# For actual data: Create a tibble/data frame with time and actual values
actual_data <- tibble(
  time = as.Date(time(copenhagen_ts)),
  Value = as.vector(copenhagen_ts),
  Type = 'Actual'
)

# For forecasted data: Create a tibble/data frame with forecasted times and values

# SARIMAX
forecast_data_sarimax <- tibble(
  time = as.Date(time(forecasted_values_sarimax$mean)),
  Value = as.vector(forecasted_values_sarimax$mean),
  Type = 'Forecast SARIMAX'
)

# SARIMA
forecast_data_sarima <- tibble(
  time = as.Date(time(forecasted_values_sarima$mean)),
  Value = as.vector(forecasted_values_sarima$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data <- bind_rows(actual_data, forecast_data_sarimax, forecast_data_sarima)

# Plot
ggplot(data = combined_data, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Kastrup passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Forecast for 2045
forecasted_values_sarimax <- forecast::forecast(sarimax_oslo, xreg=oslo_covid)
forecasted_values_sarima <- forecast::forecast(sarima_aic_oslo, h=300)
# For actual data: Create a tibble/data frame with time and actual values
actual_data <- tibble(
  time = as.Date(time(oslo_ts)),
  Value = as.vector(oslo_ts),
  Type = 'Actual'
)

# For forecasted data: Create a tibble/data frame with forecasted times and values

# SARIMAX
forecast_data_sarimax <- tibble(
  time = as.Date(time(forecasted_values_sarimax$mean)),
  Value = as.vector(forecasted_values_sarimax$mean),
  Type = 'Forecast SARIMAX'
)

# SARIMA
forecast_data_sarima <- tibble(
  time = as.Date(time(forecasted_values_sarima$mean)),
  Value = as.vector(forecasted_values_sarima$mean),
  Type = 'Forecast SARIMA'
)

# Combine actual and forecasted data
combined_data <- bind_rows(actual_data, forecast_data_sarimax, forecast_data_sarima)

# Plot
ggplot(data = combined_data, aes(x = time, y = Value, color = Type)) +
  geom_line() +
  labs(title = "Gardermoen passenger traffic forecast", x = "Time", y = "Value") +
  theme_minimal()

# Here we save all our parameters in a database file
save(paris_ts19_aic_params, paris_ts19_bic_params, paris_auto_params,
     madrid_ts19_aic_params, madrid_ts19_bic_params, madrid_auto_params,
     roma_ts19_aic_params, roma_ts19_bic_params, roma_auto_params,
     copenhagen_ts19_aic_params, copenhagen_ts19_bic_params, copenhagen_auto_params,
     oslo_ts19_aic_params, oslo_ts19_bic_params, oslo_auto_params,
     file = "hyperparameters.RData")



