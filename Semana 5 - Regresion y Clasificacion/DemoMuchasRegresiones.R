######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(dplyr) # Manejo de datos
library(tidyr) # Transformación de datos
library(purrr) # Programación funcional
library(ggplot2) # Visualizar datos
library(broom) # convertir objetos tipo "lm" a data.frames (tibbles)

## Cargar datos 
datos_onu <- read_csv("datos/DatosONU_tidy.csv") %>% 
  filter(year == 2007) %>% 
  transmute(country_name, income_group, 
            log_gdp = log(gdp_per_capita_constant_2005_us),
            log_co2 = log(co2_emissions_metric_tons_per_capita))

## Explorar datos
summary(datos_onu)

## Haremos una regresión entre log_co2 y log_gdp para cada grupo de ingresos.
## Es decir, para cada grupo de ingresos se ocupa la información de los países que lo componen

# Una forma es filtrando los datos según grupo de ingreso antes de estimar cada regresión
datos_onu %>% 
  filter(income_group == "High Income") %>% # ESTE VALOR SE REEMPLAZA
  lm(log_co2 ~ log_gdp, data = .) %>% 
  tidy() %>% 
  mutate(grupo = "High Income", .before = term) # ESTE VALOR SE REEMPLAZA

# Esta tarea modemos generalizarla a través de una función y luego el uso de "map" (paquete purrr)
modelo_f <- function(x){
  
  datos_onu %>% 
    filter(income_group == x) %>% 
    lm(log_co2 ~ log_gdp, data = .) %>% 
    tidy() %>% 
    mutate(grupo = x, .before = term)
  
}

(grupos <- datos_onu %>% distinct(income_group) %>% pull())
map(grupos, modelo_f)

# O también podemos usar una función extra de "tidyr" (nest) que es muy poderosa para este tipo de acciones
datos_anidados <- datos_onu %>% 
  group_by(income_group) %>% 
  nest() # NUEVA FUNCIÓN

datos_anidados %>% 
  unnest(data) # NUEVA FUNCIÓN (contraria a nest())

# ¿Qué acabamos de crear?
datos_anidados # es un data frame donde una de sus columnas es una lista :S

# Podemos "hacer zoom" en esa columna/lista
datos_anidados$data # hemos comprimido los datos a una fila por grupo de ingresos

# Teniendo esto ahora podemos hacer cálculos en data frames como si fueran filas
(datos_anidados_lm <- datos_anidados %>% 
  mutate(lm = map(data, ~lm(log_co2 ~ log_gdp, data = .x))))

# Hemos generado un objeto "lm" (linear model) por grupo de ingresos
datos_anidados_lm$lm

# Y ahora podemos extrar la información que este objeto "lm" contiene
(datos_anidados_tidy <- datos_anidados_lm %>% 
  mutate(tidy = map(lm, tidy)))

datos_anidados_tidy$tidy

# Y podemos finalmente extraer estos valores para que queden como columnas en el data frame
# y ocuparlos para extraer información valiosa
(datos_resultados_sinanidar <- datos_anidados_tidy %>% 
  unnest(tidy)) 

datos_resultados_sinanidar %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = reorder(income_group, estimate), y = estimate)) +
  geom_col(width = 0.5, fill = "dark blue") + 
  coord_flip() +
  theme_bw() +
  labs(x = NULL, 
       y = "Cambio en log(co2) al aumentar en una unidad log(pib)",
       title = "Relación entre aumentos en el PIB y emisiones de CO2 por grupo de ingresos",
       subtitle = "A medida que los países tienen mayores ingresos el crecimiento es más verde")
