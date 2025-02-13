library(stats)
library(dplyr)
library(ggplot2)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")
df_no_2020<- df %>% subset(Rok!=2020)
month_names <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec",
                 "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad",
                 "Grudzień")

# Regresja liniowa dla każdego miesiąca -----------------------------------

data <- df

# Funkcja do regresji i predykcji

predict_monthly <- function(data, month) {
  monthly_data <- data %>% filter(M.c == month)
  model <- lm(Wskaznik ~ Rok, data = monthly_data)
  future_years <- data.frame(Rok = c(2022, 2023), M.c = month)
  predictionsWith2020 <- predict(model, newdata = future_years)
  
  return(
    data.frame(Rok = future_years$Rok, 
                    M.c = future_years$M.c, 
                    Wskaznik = predictionsWith2020)
         )
}

# Aplikowanie funkcji dla każdego miesiąca

predictionsWith2020 <- lapply(1:12, function(m) predict_monthly(data, m))

# Łączenie wyników

predictionsWith2020_df <- do.call(rbind, predictionsWith2020)

# Łączenie oryginalnych danych z predykcjami

combined_data <- bind_rows(data, predictionsWith2020_df)

# Wykres

combined_data %>%
  ggplot(aes(x = M.c, y = Wskaznik, color = Rok)) +
  geom_line() +
  facet_wrap(~ Rok) +
  theme_light() +
  theme(
    legend.position = 'none', 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
    ) +
  labs(x = 'Month', y = 'Indicator')


# regresja liniowa bez 2020 -----------------------------------------------

data <- df_no_2020

# Funkcja do regresji i predykcji
predict_monthly <- function(data, month) {
  monthly_data <- data %>% filter(M.c == month)
  model <- lm(Wskaznik ~ Rok, data = monthly_data)
  future_years <- data.frame(Rok = c(2022, 2023), M.c = month)
  predictionsWith2020 <- predict(model, newdata = future_years)
  
  return(
    data.frame(Rok = future_years$Rok,
                    M.c = future_years$M.c, 
                    Wskaznik = predictionsWith2020)
    )
}

# Aplikowanie funkcji dla każdego miesiąca

predictionsWithout2020 <- lapply(1:12, function(m) predict_monthly(data, m))

# Łączenie wyników

predictionsWithout2020_df <- do.call(rbind, predictionsWithout2020)

# Łączenie oryginalnych danych z predykcjami

combined_data <- bind_rows(data, predictionsWithout2020_df)

# wykres

combined_data %>%
  ggplot(aes(x = M.c, y = Wskaznik, color = Rok)) +
  geom_line() +
  facet_wrap(~ Rok) +
  theme_light() +
  theme(
    legend.position = 'none', 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
    ) +
  labs(x = 'Month', y = 'Indicator')
