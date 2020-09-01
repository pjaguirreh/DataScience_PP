######################
## Cargar librerías ##
######################
library(dplyr) # Manejo de datos
library(purrr) # Iteraciones

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

*** <- function(***, ***){
  ***(*** + ***)
}

pitagoras(3, 4)

# Use un for loop para calcular el promedio de cada columna en "mtcars"

resultado <- vector()
for (i in 1:ncol(mtcars)){
  ***[i] <- ***(mtcars[,i])
}

# Use un for loop para calcular el valor máximo de cada columna en "diamonds"

resultado2 <- ***
for (i in 1:***){
  ***[i] <- ***(***[,i])
}

# Repita lo hecho en los dos ejercicios anteriores pero usando funciones "map"

***_dbl(mtcars, ***)
***_dbl(diamonds, ***)

# Un último ejemplo de como hacer lo mismo
mtcars %>% 
  summarise(across(everything(), mean))

diamonds %>% 
  summarise(across(everything(), max))
