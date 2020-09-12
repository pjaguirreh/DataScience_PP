library(tidyverse)
library(tidymodels)
library(patchwork)
library(kknn)
library(AER)
library(ISLR)
library(boot)

# Correlación vs beta$
x <- rnorm(100)
y <- 2*x + 2 + rnorm(100, sd = 2)

cor(x, y)
cor(y, x)

lm(y ~ x) %>% tidy() %>% pull(estimate)
lm(x ~ y) %>% tidy() %>% pull(estimate)

cor(x, y)^2

lm(y ~ x) %>% glance() %>% select(r.squared)
lm(x ~ y) %>% glance() %>% select(r.squared)

# Estandarizar/Normalizar
modelo1 <- lm(mpg ~ cyl + disp + hp, data = mtcars)
modelo1_sc <- lm(mpg ~ scale(cyl) + scale(disp) + scale(hp), data = mtcars)

tidy(modelo1)
tidy(modelo1_sc)

modelo2 <- lm(mpg ~ cyl*disp*hp, data = mtcars)
modelo2_sc <- lm(mpg ~ scale(cyl)*scale(disp)*scale(hp), data = mtcars)

tidy(modelo2) %>% slice(1:4)
tidy(modelo2_sc) %>% slice(1:4)

# Métodos paramétricos
data("CASchools")
p1 <- ggplot(data = CASchools, aes(x = expenditure, y = read)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + theme_bw() 

p2 <- ggplot(data = CASchools, aes(x = income, y = read)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 2)) + theme_bw() 

p1 / p2

# Métodos no paramétricos
k1.1 <- kknn(read ~ expenditure, train = CASchools, test = CASchools, k = 1, kernel = "rectangular")
k1.2 <- kknn(read ~ income, train = CASchools, test = CASchools, k = 1, kernel = "rectangular")

pknn1.1 <- ggplot(data = CASchools, aes(x = expenditure, y = math)) +
  geom_line(aes(x = expenditure, y = k1.1$fitted.values), col = "blue") +
  geom_point() +  theme_bw()

pknn1.2 <- ggplot(data = CASchools, aes(x = income, y = read)) +
  geom_line(aes(x = income, y = k1.2$fitted.values), col = "blue") +
  geom_point() + theme_bw()

pknn1.1 / pknn1.2


k2.1 <- kknn(read ~ expenditure, train = CASchools, test = CASchools, k = 9, kernel = "rectangular")
k2.2 <- kknn(read ~ income, train = CASchools, test = CASchools, k = 9, kernel = "rectangular")

pknn2.1 <- ggplot(data = CASchools, aes(x = expenditure, y = math)) +
  geom_line(aes(x = expenditure, y = k2.1$fitted.values), col = "blue") +
  geom_point() + theme_bw()

pknn2.2 <- ggplot(data = CASchools, aes(x = income, y = read)) +
  geom_line(aes(x = income, y = k2.2$fitted.values), col = "blue") +
  geom_point() + theme_bw()

pknn2.1 / pknn2.2

# Sesgo vs Varianza
set.seed(1)
df <- data.frame()
for (i in 1:10){
  x <- rnorm(1000, 0, 1); e <- rnorm(1000, 0 , 2); y <- -x^2 + e
  df <- rbind(df, data.frame(y, x, n = as.factor(i)))
}

df %>% 
  filter(n == 1) %>% 
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.4) + theme_void()

# Modelo simple
df %>% 
  filter(n == 1) %>% 
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.4) +
  theme_void() +
  geom_hline(yintercept = mean(y), col = "red", size = 1.2) 

# Baja varianza - Alto sesgo
for (i in 1:10){
  p1 <- df %>% 
    ggplot(aes(x, y, color = n)) +
    geom_point(alpha = 0.1, size = 1.2) + theme_void() + 
    geom_hline(yintercept = mean(pull(filter(df, n == i), y)), col = i, alpha = 0.4)
}

p1 + theme(legend.position = "none")

# Modelo complejo
knn <- kknn(y ~ x, filter(df, n == 1), filter(df, n == 1), k = 2 , kernel = "rectangular")
df2 <- cbind(df, knn = knn$fitted.values)

df2 %>% 
  filter(n == 1) %>% 
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(x, knn), color = "red") +
  theme_void()

# Bajo sesgo - Alta varianza
p2 <- df %>% ggplot(aes(x, y, color = n)) + geom_point(alpha = 0.1) + theme_void()
for (i in 1:10){
  knn <- kknn(y ~ x, filter(df, n == i), filter(df, n == i), k = 2 , kernel = "rectangular")
  x_v <- pull(filter(df, n == i), x)
  data <- data.frame(x_v, knn = knn$fitted.values)
  p2 <- p2 + geom_line(aes(x_v, knn), color = i, data = data, alpha = 0.4)
}

p2 + theme(legend.position = "none")

# Modelo "intermedio"
df %>% 
  filter(n == 1) %>% 
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "red", se = FALSE, formula = y ~ poly(x, 2)) +
  theme_void()

# Balance
p3 <- df %>% ggplot(aes(x, y, color = n)) + geom_point(alpha = 0.1) + theme_void()
for (i in 1:10){
  p3 <- p3 + geom_smooth(color = i, se = FALSE, data = filter(df, n == i), alpha = 0.4)
}

p3 + theme(legend.position = "none")

# Ejemplo de predicción
set.seed(1)
split <- initial_split(data = Auto, prop = 0.7)
auto_train <- training(split)
auto_validation <- testing(split)

c(train= nrow(auto_train), test = nrow(auto_validation))

# ¿Evaluar dentro de la muestra?
funcion_modelos <- function(x){
  lm(mpg ~ poly(horsepower, x), data = auto_train)
}

modelos <- map(1:4, funcion_modelos)
modelos %>% map(glance) %>% map_dfc(pull, adj.r.squared) %>% round(3)

# Evaluar fuera de muestra
ecm <- function(x){
  mean((auto_validation$mpg - predict(x, auto_validation))^2)
}

map_dfc(modelos, ecm) %>% round(3)

# Train/Validation 10 veces
set.seed(1)
df <- data.frame()
for (i in 1:10){
  split <- initial_split(data = Auto, prop = 0.7)
  auto_train <- training(split)
  auto_validation <- testing(split)
  
  modelos <- map(1:4, funcion_modelos)
  
  x <- map_dfc(modelos, ecm) %>% mutate(samp = as.factor(i))
  
  df <- rbind(df, x)
}

df %>% 
  pivot_longer(1:4, names_to = "Potencia", values_to = "ECM") %>% 
  mutate(Potencia = as.numeric(str_remove(Potencia, "..."))) %>% 
  ggplot(aes(Potencia, ECM, color = samp)) +
  geom_line() + theme_minimal() + 
  theme(legend.position="none")

# LOOCV
cv.error <- rep(0,4)
for (i in 1:4){
  lm.fit_cv <- glm(mpg ~ poly(horsepower, i) , data = Auto)
  cv.error[i] <- cv.glm(Auto, lm.fit_cv)$delta[1]
}

data.frame(Potencia = 1:4, ECM = cv.error) %>% 
  ggplot(aes(Potencia, ECM)) +
  geom_line() +
  theme_minimal()

# K-fold Cross-Validation
set.seed(1)
cv.error.10 <- rep(0,4)
for (i in 1:4){
  lm.fit_cv10 <- glm(mpg ~ poly(horsepower, i) , data = Auto)
  cv.error.10[i] <- cv.glm(Auto, lm.fit_cv10, K = 10)$delta[1]
}

data.frame(Potencia = rep(1:4, 2), 
           ECM = c(cv.error, cv.error.10), 
           CV = c(rep("LOOOCV", 4), rep("K10", 4))) %>% 
  ggplot(aes(Potencia, ECM, color = CV)) +
  geom_line() +
  theme_minimal()