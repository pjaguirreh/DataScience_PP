library(tidyverse)
library(MASS)
library(rpart)
library(rpart.plot)

# Ejemplos árboles de decisión
set.seed(123)
Boston %>%
  sample_n(300) %>% 
  rpart(formula = medv ~ .,
        data = .,
        method = "anova") %>% 
  rpart.plot()

# Árboles de decisión en R
library(AmesHousing) # datos
library(rsample) # construcción de train/test 

## Preparar datos
datos_casas <- make_ames()
dim(datos_casas)

set.seed(123)
split <- initial_split(datos_casas , prop = .7)
datos_train <- training(split)
datos_test  <- testing(split)

dim(datos_train)
dim(datos_test)

## Estimar modelo
m1 <- rpart(
  formula = Sale_Price ~ .,
  data    = datos_train,
  method  = "anova") # "class" para clasificación

str(m1) #explorar objeto
m1
rpart.plot(m1)
plotcp(m1)
m1$cptable

## Otros hiperparámetros
m3 <- rpart(
  formula = Sale_Price ~ .,
  data    = datos_train,
  method  = "anova", 
  control = list(minsplit = 10, 
                 maxdepth = 2))

rpart.plot(m3)

## Elegir "mejor" hiperparámetros
hiper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

head(hiper_grid)
nrow(hiper_grid)

modelos <- list()

for (i in 1:nrow(hiper_grid)) {
  
  # definir hiperparametros de la iteración
  minsplit <- hiper_grid$minsplit[i]
  maxdepth <- hiper_grid$maxdepth[i]
  
  # entrenar modelos y guardar en la lista
  modelos[[i]] <- rpart(
    formula = Sale_Price ~ .,
    data    = datos_train,
    method  = "anova",
    control = list(minsplit = minsplit, 
                   maxdepth = maxdepth)
  )
}

# función para obtener el valor óptimo de cp
obtener_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# función para obtener el valor de error asociado
obtener_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

(mejores_valores <- hiper_grid %>%
    mutate(
      cp    = map_dbl(modelos, obtener_cp),
      error = map_dbl(modelos, obtener_min_error)
    ) %>%
    arrange(error) %>%
    slice(1))

arbol_optimo <- rpart(
  formula = Sale_Price ~ .,
  data    = datos_train,
  method  = "anova",
  control = list(minsplit = mejores_valores$minsplit, 
                 maxdepth = mejores_valores$maxdepth, 
                 cp = 0.01)
)

pred <- predict(arbol_optimo, newdata = datos_test)
sqrt(mean((pred - datos_test$Sale_Price)^2))

summary(datos_casas$Sale_Price)
