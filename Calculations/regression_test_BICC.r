df <- read.csv('BICCtemp.txt', sep = ';', dec = ",")

# Regresja liniowa --------------------------------------------------------

model <- lm(Wskaznik ~ M.c, data = df)
summary(model)
anova(model)

# Sprawdzenie założeń modelu

plot(model)

predicted_values <- predict(model)
plot(
  df$Wskaznik, predicted_values, 
  xlab = "Zaobserwowany wskaźnik", ylab = "Przewidziany wskaźnik",
  main = "Zaobserwowane vs. Przewidziane (model regresji liniowej)"
)
abline(0, 1, col = "red")


# Regresja wielomianowa ---------------------------------------------------

polynomial_model <- lm(Wskaznik ~ poly(Rok, degree = 5), data = df)
summary(polynomial_model)
anova(polynomial_model)

# Sprawdzenie założeń modelu

plot(polynomial_model)

predicted_values_polynomial <- predict(polynomial_model)
plot(
  df$Wskaznik, predicted_values_polynomial, 
  xlab = "Zaobserwowany wskaźnik", ylab = "Przewidziany Wskaźnik", 
  main = "Zaobserwowane vs. Przewidziane (model regresji wielomianowej)"
)
abline(0, 1, col = "red")
