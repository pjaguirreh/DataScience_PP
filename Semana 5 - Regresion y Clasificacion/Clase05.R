library(ggplot2)
library(dplyr)
library(broom)
library(AER)
library(tidymodels)

# Estudiantes/Profesor vs Resultados

# crear vectores de datos
REP <- c(15, 17, 19, 20, 22, 23.5, 25)
Resultados <- c(680, 640, 670, 660, 630, 660, 635) 

# juntar ambos vectores en un data frame
datos_colegio <- data.frame(REP, Resultados)

ggplot(datos_colegio, aes(REP, Resultados)) +
  geom_point(size = 3) +
  theme_minimal()

# Una linea que describe esta relación (?) 
ggplot(datos_colegio, aes(REP, Resultados)) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = 713, slope = -3), size = 2, col = "red") +
  theme_minimal()

# Ahora muchas lineas
curvas <- data.frame(int = sample(713:720, 50, replace = TRUE),
                     pend = -3 + rnorm(50, mean = 0, sd = 0.2))

ggplot(datos_colegio, aes(REP, Resultados)) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = int, slope = pend), data = curvas, alpha = 0.5, col = "grey") +
  theme_minimal()

# Datos California (USA)
data("CASchools")
str(CASchools)

# Preparar datos
(datos_reg <- CASchools %>% 
  rename(ingresos = income) %>%
  transmute(district, school,
            Resultados = (read + math)/2,
            REP = students/teachers,
            ingresos,
            grupo_ingresos = as.factor(ifelse(ingresos >= median(.$ingresos), 1, 0))))

# Relación entre variables
datos_reg %>% 
  ggplot(aes(REP, Resultados)) + 
  geom_point() +
  theme_minimal()

# Graficar la curva de regresión
datos_reg %>% 
  ggplot(aes(REP, Resultados)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, size = 1.5) +
  theme_minimal()

# Estimar coeficientes "a mano"
beta1 <- sum((datos_reg$REP - mean(datos_reg$REP)) * (datos_reg$Resultado - mean(datos_reg$Resultado))) / 
  sum((datos_reg$REP - mean(datos_reg$REP))^2)
beta0 <- mean(datos_reg$Resultados) - (beta1*mean(datos_reg$REP))
round(c(beta0, beta1), 4)

SCR <- sum(((beta0 + (beta1*datos_reg$REP)) - mean(datos_reg$Resultados))^2)
SCT <- sum((datos_reg$Resultados - mean(datos_reg$Resultados))^2)
R2 <- SCR/SCT
round(R2, 5)

# Por suerte `R` lo hace más simple
modelo1 <- lm(Resultados ~ REP, data = datos_reg)
summary(modelo1)

# ¿Qué es esto?
str(modelo1)

# Paquete `broom`
tidy(modelo1)
glance(modelo1)
augment(modelo1)

# Por suerte `R` lo hace más simple
modelo2 <- lm(Resultados ~ REP + ingresos, data = datos_reg)
summary(modelo2)

# Comparemos
glance(modelo1)$r.squared
glance(modelo2)$r.squared

# $R^2$ ajustado
glance(modelo1)$adj.r.squared
glance(modelo2)$adj.r.squared

# Modelos con interacciones
(base <- datos_reg %>% 
    ggplot(aes(x = REP, y = Resultados)) +
    geom_point(aes(col = grupo_ingresos)) +
    theme_minimal() +
    theme(legend.position = "none"))


tidy(lm(Resultados ~ REP, data = datos_reg))
base +
  geom_smooth(method = "lm", se = FALSE)

reg_int1 <- lm(Resultados ~ REP + grupo_ingresos, data = datos_reg)
tidy(reg_int1)
base +
  geom_line(data = filter(augment(reg_int1), grupo_ingresos == 1),
            aes(x = REP, y = .fitted), col = "#00BFC4", size = 1) +
  geom_line(data = filter(augment(reg_int1), grupo_ingresos == 0),
            aes(x = REP, y = .fitted), col = "#F8766D", size = 1)

reg_int2 <- lm(Resultados ~ REP*grupo_ingresos, data = datos_reg)
tidy(reg_int2)
base +
  geom_smooth(aes(col = grupo_ingresos),
              method = "lm", se = FALSE)

# Datos de créditos hipotecarios
data(HMDA)
datos_logit <- HMDA %>% 
  select(rechazado = deny, pago_ingreso = pirat) %>% 
  mutate(rechazado = as.numeric(rechazado)-1)
summary(datos_logit)

# ¿Cómo se ve esto?
datos_logit %>% 
  ggplot(aes(x = pago_ingreso, y = rechazado)) +
  geom_point() +
  theme_minimal() 

# Modelo de probabilidad lineal
datos_logit %>% 
  ggplot(aes(x = pago_ingreso, y = rechazado)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_minimal() 

# Modelo logt
modelo_logit <- glm(rechazado ~ pago_ingreso, family = "binomial", data = datos_logit)

# ¿como se ve esto?
datos_logit %>% 
  ggplot(aes(x = pago_ingreso, y = rechazado)) +
  geom_point() +
  geom_smooth(method = "glm", se = FALSE,
              method.args = list(family = "binomial")) +
  theme_minimal()

# Interpretar el resultado
tidy(modelo_logit)

predict(modelo_logit,
        newdata = data.frame("pago_ingreso" = c(0.1, 0.3, 0.5, 0.7, 1, 2)),
        type = "response") %>% round(4)

tidy(modelo_logit)

# Pseudo R^2
glance(modelo_logit)
1 - (glance(modelo_logit)$deviance/glance(modelo_logit)$null.deviance)

# ¿Cómo evaluamos este modelo?
augment(modelo_logit, type.predict = "response")
estimacion_logit <- augment(modelo_logit, type.predict = "response") %>% 
  transmute(rechazado = as.factor(rechazado), 
            .fitted,
            clasificacion = as.factor(ifelse(.fitted >= 0.5, 1, 0)))

(matriz_confusion <- conf_mat(estimacion_logit, rechazado, clasificacion)$table)

VP <- matriz_confusion[2,2]
FP <- matriz_confusion[2,1]
VN <- matriz_confusion[1,1]
FN <- matriz_confusion[1,2]

(tasa_VP <- VP/(VP+FN))
(tasa_FP <- FP/(FP+VN))

# Curca ROC / Area bajo la curva
roc_auc(estimacion_logit, rechazado, .fitted)

roc_curve(estimacion_logit, rechazado, .fitted) %>%
  autoplot()