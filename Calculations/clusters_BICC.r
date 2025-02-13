library(stats)
library(dplyr)
library(ggplot2)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")
df_no_2020<- df %>% subset(Rok!=2020)
month_names <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec",
                 "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad",
                 "Grudzień")

# Klastrowanie marzec-lipiec ----------------------------------------------

data <- df

# Funkcja do klastrowania i przygotowania danych dla każdego miesiąca

cluster_month <- function(month, data, k = 2) {
  month_data <- data %>%
    filter(M.c == month) %>%
    select(Rok, Wskaznik)
  
  clusters <- kmeans(month_data$Wskaznik, centers = k)
  
  month_data$Cluster <- as.factor(clusters$cluster)
  month_data$Month <- month
  
  return(month_data)
}

months <- c(3, 4, 5, 6, 7)
clustered_data <- do.call(rbind, lapply(months, cluster_month, data = data))
clustered_data$Month <- factor(clustered_data$Month, 
                               levels = months,
                               labels = month_names[months])

# Wizualizacja wyników klasteryzacji

ggplot(clustered_data, aes(x = Rok, y = Wskaznik, color = Cluster)) +
  geom_point(size = 2) +  
  labs(title = "Klasteryzacja wartości wskaźnika (Marzec - Lipiec, 2000-2021)",
       x = "Rok",
       y = "Wartość wskaźnika") +
  scale_x_continuous(breaks = seq(2000, 2021, by = 1)) +
  theme_minimal() +
  theme(
    panel.spacing = unit(2, "lines"),  
    axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
  facet_wrap(~ Month, scales = "free_y", ncol = 1)
