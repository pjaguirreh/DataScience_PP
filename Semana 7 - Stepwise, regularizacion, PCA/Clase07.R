library(dplyr)
library(ggplot2)

## Datos
library(ISLR)
glimpse(Hitters)

##----------------------------
## Forward stepwise regression
##----------------------------
library(leaps)
mejores_modelos_forward <- regsubsets(Salary~., data = Hitters, 
                                      nvmax = ncol(Hitters)-1,
                                      method = "forward")

# Modelos
summary(mejores_modelos_forward)$which

# Métricas calculadas
summary(mejores_modelos_forward) %>% names


# Función para graficar
resumen_forward <- summary(mejores_modelos_forward)

graf_metricas <- function(x){
  data.frame(n_predictores = 1:19,
             metrica = resumen_forward[[x]]) %>% 
    ggplot(aes(x = n_predictores, y = metrica)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = c(0:19)) +
    theme_minimal() +
    labs(x = NULL, y = NULL, title = x)
}

# Gráficos para distintas métricas
library(purrr)
graf_forward <- map(names(resumen_forward)[4:6], graf_metricas)

graf_forward[[1]]
graf_forward[[2]]
graf_forward[[3]]

# Mejor modelo, según BIC
coef(object = mejores_modelos_forward, 
     id = which.min(resumen_forward$bic))

##------
## LASSO
##------
library(glmnet)

# Manejo de los datos
datos <- Hitters %>% 
  filter(!is.na(Salary))

x <- model.matrix(Salary~., data = datos)[,-1]
y <- datos %>% 
  pull(Salary)

# Estimar todos los modelos LASSO (varía lambda)
modelos_lasso <- glmnet(x = x, y = y, alpha = 1)
plot(modelos_lasso, xvar = "lambda", label = TRUE)

# Ver "el mejor lambda" según cross validation
set.seed(1)
cv_error_lasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10)
plot(cv_error_lasso)

c("min" = cv_error_lasso$lambda.min,
  "1se" = cv_error_lasso$lambda.1se)

# Estimar LASSO con "mejor lambda"
modelo_final_lasso <- glmnet(x = x, 
                             y = y, 
                             alpha = 1, 
                             lambda = cv_error_lasso$lambda.1se)
coef(modelo_final_lasso)

##------------------------
## Componentes principales
##------------------------
library(pls)
pca <- x %>% 
  prcomp(scale = TRUE)

summary(pca)

prop_varianza <- cumsum(pca$sdev^2 / sum(pca$sdev^2))

data.frame(n_componentes = 1:19,
           varianza = prop_varianza) %>% 
  ggplot(aes(x = n_componentes, y = varianza)) +
  geom_line() +
  geom_point() +
  theme_minimal() + scale_x_continuous(breaks = 0:19)

##----------------
## Compara métodos
##----------------

## Datos
library(faraway)
names(meatspec)
summary(meatspec$fat)

# Preparar datos
library(rsample)

set.seed(1)
split <- initial_split(data = meatspec, prop = 0.8)
datos_train <- training(split)
datos_test <- testing(split)

dim(datos_train)
dim(datos_test)

# MCO
modelo_mco <- lm(fat ~ ., data = datos_train)

glance(modelo_mco)$adj.r.squared

tidy(modelo_mco) %>% 
  filter(abs(statistic) >= 1.96) %>% 
  pull(term)

(ecm_mco_train <- mean((modelo_mco$fitted.values - datos_train$fat)^2))
(ecm_mco_test <- mean((predict(modelo_mco, newdata = datos_test) - datos_test$fat)^2))

# Forward stepwise

datos_train_cv <- datos_train %>% 
  mutate(g = rep(1:10, length.out = nrow(datos_train)))

# Función para extraer modelos
predict.forward  <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object , id = id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}

# Matriz para almacenar resultados de loop
error_matrix <- matrix(data = NA, nrow = 10, ncol = 100,
                       dimnames = list(NULL, c(1:100)))
for (k in 1:10) {
  train <- datos_train_cv %>% filter(g != k)
  mejores_modelos <- regsubsets(fat ~ ., 
                                data = train, 
                                nvmax = 100,
                                method = "forward")
  
  for (i in 1:100) {
    test <- datos_train_cv %>% filter(g == k)
    predicciones <- predict.forward(object = mejores_modelos,
                                    newdata = test, id = i)
    error_matrix[k,i] <- mean((test$fat - predicciones)^2)
  }
}

mean_cv_error <- apply(X = error_matrix, MARGIN = 2, FUN = mean)

(mejor_modelo <- error_matrix %>% 
    as_tibble() %>% 
    summarise_all(mean) %>% 
    which.min())

modelo_final <- regsubsets(fat ~ ., data = datos_train, nvmax = 100,
                           method = "forward")
coef(object = modelo_final, mejor_modelo)

(ecm_forward_test <- mean((predict.forward(modelo_final, datos_test, id = mejor_modelo) - datos_test$fat)^2))

# LASSO
set.seed(1)

datos_train_x <- model.matrix(fat~., data = datos_train)[,-1]
datos_train_y <- datos_train %>% pull(fat)
datos_test_x <- model.matrix(fat~., data = datos_test)[,-1]
datos_test_y <- datos_test %>% pull(fat)

cv_error_lasso <- cv.glmnet(x = datos_train_x, y = datos_train_y, alpha = 1,
                            nfolds = 10,
                            type.measure = "mse")

modelo_lasso <- glmnet(x = datos_train_x, y = datos_train_y, alpha = 1,
                       lambda = cv_error_lasso$lambda.1se)

predicciones <- predict(object = modelo_lasso, newx = datos_test_x,
                        s = cv_error_lasso$lambda.1se, exact = TRUE)

(ecm_lasso_test <- mean((predicciones - datos_test_y)^2))

# PCA
pca <- datos_train %>% prcomp(scale = TRUE)
summary(pca)$importance[, 1:15]

set.seed(1)
modelo_pcr <- pcr(formula = fat ~ ., data = datos_train, 
                  scale. = TRUE, validation = "CV")
validationplot(modelo_pcr, val.type = "MSE")

modelo_pcr$validation$PRESS %>% which.min()

(ecm_pcr_test <- mean((predict(modelo_pcr, newdata = datos_test, 
                               ncomp = 20) - datos_test$fat)^2))

#################
## COMPARACIÓN ##
#################

data.frame(Error = c(ecm_mco_test, ecm_forward_test, ecm_lasso_test, ecm_pcr_test),
           Modelo = c("MCO", "Forward", "LASSO", "PCR")) %>% 
  ggplot(aes(x = reorder(Modelo, -Error), y = Error)) + 
  geom_col() +
  labs(x = NULL)