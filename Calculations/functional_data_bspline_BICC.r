library(dplyr)
library(fda)
library(ggplot2)
library(plotly)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")

df_list <- df %>%
  group_by(Rok) %>%
  group_split()

# Przygotowanie danych funkcjonalnych z danych dyskretnych ----------------

df_list <- lapply(df_list, function(x) x[, "Wskaznik", drop = FALSE])

# Parametry bazowe bspline

n_basis <- 6  # Ilość funkcji bazowych
range_val <- c(1, 12)

# Funkcja tworząca bazę bspline

create_bspline_basis <- function(data, n_basis, range_val) {
  Wskaznik <- as.numeric(data$Wskaznik)
  basis <- create.bspline.basis(rangeval = range_val, nbasis = n_basis)
  
  smooth_basis <- smooth.basis(argvals = 1:12, y = Wskaznik, fdParobj = basis)
  
  return(smooth_basis)
}

bspline_list <- lapply(df_list, create_bspline_basis, n_basis, range_val)

# Funkcja tworząca wykres bazy bspline

plot_bspline <- function(smooth_basis, year) {
  eval_points <- seq(1, 12, length.out = 100)
  
  smooth_values <- eval.fd(eval_points, smooth_basis$fd)
  
  plot_data <- data.frame(
    M.c = eval_points,
    Wskaznik = as.vector(smooth_values),
    Rok = year
  )
  
  return(plot_data)
}

# Dane do wykresu na każdy rok

plot_data_list <- lapply(unique(df$Rok), function(year) {
  year_df <- df[df$Rok == year, ]
  smooth_basis <- create_bspline_basis(year_df, n_basis, range_val)
  plot_bspline(smooth_basis, year)
})

plot_data <- do.call(rbind, plot_data_list)

ggplotly(
  ggplot(plot_data, aes(x = M.c, y = Wskaznik, color = factor(Rok))) +
    geom_line(size = 1) +
    labs(title = "B-spline Smoothed Data by Year", x = "Month", y = "Smoothed Values") +
    theme_minimal() +
    scale_color_discrete(name = "Year")
  )

# Bez 2020

plot_data2 <- plot_data[plot_data$Rok != 2020,]

ggplotly(ggplot(plot_data2, aes(x = M.c, y = Wskaznik, color = factor(Rok))) +
           geom_line(size = 1) +
           labs(title = "B-spline Smoothed Data by Year", x = "Month", y = "Smoothed Values") +
           theme_minimal() +
           scale_color_discrete(name = "Year"))

# Liniowa regresja dla funkcjonalnych danych (po miesiacach) --------------

months_fun <- seq(1, 12, length.out = 100)

predict_monthly <- function(data, month) {
  monthly_data <- data %>% filter(M.c == month)
  model <- lm(Wskaznik ~ Rok, data = monthly_data)
  future_years <- data.frame(Rok = c(2022, 2023), M.c = month)
  
  predictionsWith2020 <- predict(model, newdata = future_years)
  
  return(
    data.frame(
      Rok = future_years$Rok, 
      M.c = future_years$M.c, 
      Wskaznik = predictionsWith2020))
}

predicted_values <- lapply(months_fun, 
                           function(m) predict_monthly(plot_data2, m))
combined_fun_data <- bind_rows(plot_data2, predicted_values)

ggplotly(ggplot(combined_fun_data, 
                aes(x = M.c, y = Wskaznik, color = factor(Rok))) +
           geom_line(size = 1) +
           labs(title = "B-spline Smoothed Data by Year", 
                x = "Month", 
                y = "Smoothed Values") +
           theme_minimal() +
           scale_color_discrete(name = "Year"))
