library(stats)
library(dplyr)
library(ggplot2)
library(tseries)
library(forecast)

# ARIMA -------------------------------------------------------------------

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")
df<- df %>% subset(Rok!=2020)
month_names <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec",
                 "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad",
                 "Grudzień")

# Zamiana danych na szereg czasowy

ts_data <- ts(df$Wskaznik, start=c(2000, 1), frequency=12)

plot(ts_data)
summary(ts_data)

# Sprawdzenie załozeń ARIMA, czyli stacjonarność 
# (średnia, wariancja i autokorelacja są stałe w czasie)

adf.test(ts_data)

model_ARIMA <- auto.arima(ts_data)

# Wykresy rezyduów, sprawdzanie czy nie ma autokorelacji

checkresiduals(model_ARIMA)

# Prognoza na przyszłe 24 miesiace

forecast_values_ARIMA <- forecast(model_ARIMA, h=24)
plot(forecast_values_ARIMA)

# SARIMA ------------------------------------------------------------------

# Zamiana danych na szereg czasowy
ts_data <- ts(df$Wskaznik, start=c(2000, 1), frequency=12)

# Sprawdzenie załozeń SARIMA, czyli stacjonarność 
# (średnia, wariancja i autokorelacja są stałe w czasie)

adf.test(ts_data)

model_SARIMA <- auto.arima(ts_data, seasonal=TRUE)

# Wykresy rezyduów, sprawdzanie czy nie ma autokorelacji

checkresiduals(model_SARIMA)

# Prognoza na przyszłe 24 miesiace

forecast_values_SARIMA <- forecast(model_SARIMA, h=24)
plot(forecast_values_SARIMA)

