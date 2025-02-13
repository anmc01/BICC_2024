library(tidyr)
library(dplyr)

df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")

# Przekształcenie danych na rok ~ miesiąc

df
df2 <- df %>%
  pivot_wider(names_from = M.c, values_from = Wskaznik)

df2 <- as.data.frame(df2)
rownames(df2) <- df2$Rok
df2$Rok <- NULL
df2

# ANOVA

df_anova <- aov(Wskaznik ~ factor(Rok) + factor(M.c), data = df)
summary(df_anova)

# Zmienne rok i miesiąc istotne dla wartości wskaźnika