library(forecast)
library(dplyr)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")

# Funkcje błędu -----------------------------------------------------------

MAPE.error <- function(x, y) {
  result <- mean(abs((x-y)/x)) * 100
  return(result)
}

RMSE.error <- function(x, y) {
  result <- sqrt((sum(x - y)^2)/length(x))
  return(result)
}

# Z 2020 ------------------------------------------------------------------

# Podział danych ----------------------------------------------------------

df.konstr <- head(df$Wskaznik, n = 12 * 20) # czesc konstrukcyjna
df.test <- tail(df$Wskaznik, n = 12 * 2) # czesc testowa

# Metoda STL --------------------------------------------------------------

ts_stl <- ts(df$Wskaznik, frequency = 12)

# Obliczenie trendu, wahań sezonowych i reszt

model.ts_stl <- stl(ts_stl, s.window = "periodic")
autoplot(model.ts_stl, range.bars = FALSE)
ts_stl %>% mstl() %>% autoplot(ts_stl)

# Predykcja dla kolejnych dwóch lat wraz z granicami przedzialów ufności 
# dla poziomów ufności 80% i 95%

model.ts_stl.forecast <- forecast(model.ts_stl, h = 24) 

# Wykres danych wraz z przewidzianymi wartościami wskaźnika
# Ciemnoniebieskie pole to przedział ufnosci 80%, jasnoniebieskie - 95%

autoplot(model.ts_stl.forecast)
model.ts_stl.forecast$mean

ts_stl.predict <- predict(model.ts_stl.forecast, df.test)

# Tabela z danymi z ostatnich 2 lat zestawiona z przewidzianymi danymi 
# na następne 2 lata

ts_stl.test_vs_predicted <- data.frame(df.test, model.ts_stl.forecast$mean)

# wykres danych z ostatnich 2 lat (na czarno) vs przewidzianych danych
# na kolejne 2 lata (na czerwono)

matplot(ts_stl.test_vs_predicted, type = 'l', lty = 1:2, col = 1:2)

MAPE.error(ts_stl.test_vs_predicted$df.test, 
           ts_stl.test_vs_predicted$model.ts_stl.forecast.mean) # 3.323
RMSE.error(ts_stl.test_vs_predicted$df.test, 
           ts_stl.test_vs_predicted$model.ts_stl.forecast.mean) # 16.506

# Metoda TBATS ------------------------------------------------------------

ts_tbats <- msts(df$Wskaznik, seasonal.periods = 12)
model.ts_tbats <- tbats(ts_tbats)
plot(model.ts_tbats)

model.ts_tbats.forecast <- forecast(model.ts_tbats, h = 24)
plot(model.ts_tbats.forecast)
model.ts_tbats.forecast$mean

ts_tbats.predict <- predict(model.ts_tbats.forecast, df.test)
ts_tbats.test_vs_predicted <- data.frame(df.test, model.ts_tbats.forecast$mean)
matplot(ts_tbats.test_vs_predicted, type = 'l', lty = 1:2, col = 1:2)

MAPE.error(ts_tbats.test_vs_predicted$df.test, 
           ts_tbats.test_vs_predicted$model.ts_tbats.forecast.mean) # 2.085
RMSE.error(ts_tbats.test_vs_predicted$df.test, 
           ts_tbats.test_vs_predicted$model.ts_tbats.forecast.mean) # 6.855

# BEZ 2020 ----------------------------------------------------------------

# Podzial danych 2---------------------------------------------------------

df_no_2020
df_no_2020.konstr <- head(df_no_2020$Wskaznik, n = 12 * 19)
df_no_2020.test <- tail(df_no_2020$Wskaznik, n = 12 * 2)

# Metoda STL 2-------------------------------------------------------------

ts_stl2 <- ts(df_no_2020$Wskaznik, frequency = 12)
model.ts_stl2 <- stl(ts_stl2, s.window = "periodic")
autoplot(model.ts_stl2)

model.ts_stl2.forecast <- forecast(model.ts_stl2, h = 24)
autoplot(model.ts_stl2.forecast)
model.ts_stl2.forecast$mean

ts_stl2.predict <- predict(model.ts_stl2.forecast, df_no_2020.test)
ts_stl2.test_vs_predicted <- data.frame(
  df_no_2020.test, 
  model.ts_stl2.forecast$mean)
matplot(ts_stl2.test_vs_predicted, type = 'l', lty = 1:2, col = 1:2)

MAPE.error(ts_stl2.test_vs_predicted$df_no_2020.test, 
           ts_stl2.test_vs_predicted$model.ts_stl2.forecast.mean) # 2.209
RMSE.error(ts_stl2.test_vs_predicted$df_no_2020.test, 
           ts_stl2.test_vs_predicted$model.ts_stl2.forecast.mean) # 10.667

# Metoda TBATS 2-----------------------------------------------------------

ts_tbats2 <- msts(df_no_2020$Wskaznik, seasonal.periods = 12)
model.ts_tbats2 <- tbats(ts_tbats2)
plot(model.ts_tbats2)

model.ts_tbats2.forecast <- forecast(model.ts_tbats2, h = 24)
plot(model.ts_tbats2.forecast)
model.ts_tbats2.forecast$mean

ts_tbats2.predict <- predict(model.ts_tbats2.forecast, df_no_2020.test)
ts_tbats2.test_vs_predicted <- data.frame(
  df_no_2020.test, 
  model.ts_tbats2.forecast$mean)
matplot(ts_tbats2.test_vs_predicted, type = 'l', lty = 1:2, col = 1:2)

MAPE.error(ts_tbats2.test_vs_predicted$df_no_2020.test, 
           ts_tbats2.test_vs_predicted$model.ts_tbats2.forecast.mean) # 1.169
RMSE.error(ts_tbats2.test_vs_predicted$df_no_2020.test, 
           ts_tbats2.test_vs_predicted$model.ts_tbats2.forecast.mean) # 3.977

# TBATS ze srednia globalna za rok 2020 -----------------------------------

df_mean2020 <- df

df_mean2020[df_mean2020$Rok == 2020,]$Wskaznik <- 
  mean(df_mean2020[df_mean2020$Rok != 2020,]$Wskaznik)

ts_tbats3 <- msts(df_mean2020$Wskaznik, seasonal.periods = 12)
model.ts_tbats3 <- tbats(ts_tbats3)
plot(model.ts_tbats3)

model.ts_tbats3.forecast <- forecast(model.ts_tbats3, h = 24)
plot(model.ts_tbats3.forecast)
model.ts_tbats3.forecast$mean

ts_tbats3.predict <- predict(model.ts_tbats3.forecast, df_no_2020.test)
ts_tbats3.test_vs_predicted <- data.frame(
  df_no_2020.test, 
  model.ts_tbats3.forecast$mean
  )
matplot(ts_tbats3.test_vs_predicted, type = 'l', lty = 1:2, col = 1:2)

MAPE.error(ts_tbats3.test_vs_predicted$df_no_2020.test, 
           ts_tbats3.test_vs_predicted$model.ts_tbats3.forecast.mean) # 1.009
RMSE.error(ts_tbats3.test_vs_predicted$df_no_2020.test, 
           ts_tbats3.test_vs_predicted$model.ts_tbats3.forecast.mean) # 2.643

# TBATS ze srednia z lat 2015-2019 za rok 2020 -----------------------------------

df_mean2020.2 <- df

df_mean2020.2[df_mean2020.2$Rok == 2020,]$Wskaznik <- 
  mean(df_mean2020.2[df_mean2020.2$Rok %in% 2015:2019,]$Wskaznik)

ts_tbats4 <- msts(df_mean2020.2$Wskaznik, seasonal.periods = 12)
model.ts_tbats4 <- tbats(ts_tbats4)
plot(model.ts_tbats4)

model.ts_tbats4.forecast <- forecast(model.ts_tbats4, h = 24)
plot(model.ts_tbats4.forecast)
model.ts_tbats4.forecast$mean

ts_tbats4.predict <- predict(model.ts_tbats4.forecast, df_no_2020.test)
ts_tbats4.test_vs_predicted <- data.frame(
  df_no_2020.test, 
  model.ts_tbats4.forecast$mean)
matplot(ts_tbats4.test_vs_predicted, type = 'l', lty = 1:2, col = 1:2)

MAPE.error(ts_tbats4.test_vs_predicted$df_no_2020.test, 
           ts_tbats4.test_vs_predicted$model.ts_tbats4.forecast.mean) # 1.065
RMSE.error(ts_tbats4.test_vs_predicted$df_no_2020.test, 
           ts_tbats4.test_vs_predicted$model.ts_tbats4.forecast.mean) # 3.255

TBATS <- model.ts_tbats4.forecast$mean %>% as.data.frame()
TBATS
