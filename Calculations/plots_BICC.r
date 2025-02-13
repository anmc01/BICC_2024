library(ggplot2)
library(plotly)
library(gapminder)
library(dplyr)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")

# Wykres calościowy -------------------------------------------------------

df$full_date <- as.Date(paste(df$Rok, df$M.c, "01", sep = "-"))

df %>%
  ggplot(aes(x = full_date, y = Wskaznik)) +
  geom_line() +
  labs(x = 'Time', y = 'Wskaznik') +
  scale_x_date()

# Sprawdzenie rozkładu wskaźnika

yr <- 2000
while (yr < 2022) {
  print(yr)
  print(shapiro.test((df %>% filter(Rok == yr))$Wskaznik)$p)
  yr <- yr + 1
}
shapiro.test(df$Wskaznik[df$Rok==2020])$p

# Tabela średnich i odchyleń wskaźników - średnie roczne

View(df %>%
  group_by(Rok) %>%
  summarise(mean_indicator = mean(Wskaznik),
            sd_indicator = sd(Wskaznik)))

# Wykres zmiany wskaźnikow

length(df[,1])
ggplotly(
  df %>%
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
  )
