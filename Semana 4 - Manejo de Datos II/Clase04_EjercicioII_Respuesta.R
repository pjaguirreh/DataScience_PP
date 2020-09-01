######################
## Cargar librerías ##
######################
library(dplyr) # Manejo de datos
library(purrr) # Iteraciones
library(ggplot2) 

##################
## Cargar datos ##
##################
mtcars # Por defecto en R
diamonds # Por defecto en R

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Cree una función llamada "pitagoras" que tome dos valores a y b (correspondientes a los argumentos)
# y calcule la raíz cuadrada de la suma del cuadrado de ambos valores (teorema de pitágoras).
# Pruebe la función creada con los valores a = 3, y b = 4. El resultado debería ser 5

pitagoras <- function(a, b){
  sqrt(a^2 + b^2)
}

pitagoras(3, 4)

# Use un for loop para calcular el promedio de cada columna en "mtcars"

resultado <- vector()
for (i in 1:ncol(mtcars)){
  resultado[i] <- mean(mtcars[,i])
}

# Repita lo hecho en los dos ejercicios anteriores pero usando funciones "map"

map_dbl(mtcars, mean)

# Un último ejemplo de como hacer lo mismo
mtcars %>% 
  summarise(across(everything(), mean))
