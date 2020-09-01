library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

## ¿Cómo se crea una función?
ElevaryDividir <- function(x, y, z){
  (x^y)/z
}

ElevaryDividir(3, 2, 4)

## Ejemplo 
datosONU_tidy <- read_csv("datos/DatosONU_tidy.csv")

datosONU_tidy %>% 
  filter(country_name == "Chile") %>% 
  ggplot(aes(x = year, y = population_total)) +
  geom_point() +
  geom_line() +
  labs(x = NULL) +
  theme_minimal()

## Crear una función que haga esto {.smaller}
graf_indicador_pais <- function(x, y){
  
  y <- enquo(y)
  
  datosONU_tidy %>% 
    filter(country_name == x) %>% 
    ggplot(aes(x = year, y = !!y)) +
    geom_point() +
    geom_line() +
    labs(x = NULL) +
    theme_minimal() 
  
}

graf_indicador_pais("Chile", population_total)

## Y ahora podemos hacer muchas cosas más {.smaller}
graf_indicador_pais("Chile", population_total)

graf_indicador_pais("Argentina", 
                    gdp_per_capita_constant_2005_us)

## Iteraciones | ¿Para qué?
graf_indicador_pais("Chile", gdp_per_capita_constant_2005_us)
graf_indicador_pais("Argentina", gdp_per_capita_constant_2005_us)
graf_indicador_pais("United States", gdp_per_capita_constant_2005_us)

## `for` loops
for (i in 1:10){
  print(2^i)
}

## `for` loop aplicado a nuestro ejemplo {.smaller}
graficos <- list()
paises <- c("Chile", "Argentina", "United States")
for (i in seq_along(paises)){
  graficos[[i]] <- graf_indicador_pais(paises[i], gdp_per_capita_constant_2005_us)
}

graficos[[1]]
graficos[[2]]

## Otra opción | `map` {.smaller}

paises <- c("Chile", "Argentina", "United States")
graficos <- map(paises, graf_indicador_pais, gdp_per_capita_constant_2005_us)

graficos[[1]]
graficos[[2]]
