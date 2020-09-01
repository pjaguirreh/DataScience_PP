#1) Cargar paquetes
library(readr) # para cargar datos
library(dplyr) # para manejo de datos
library(ggplot2) # para visualizar

# Cargar datos
datos_mundo <- read_csv("datos/datos_mundo.csv")
datos_mundo = read_csv("datos/datos_mundo.csv")

datos_mundo # tibble df
as.data.frame(datos_mundo) # df

# Explorar datos

summary(datos_mundo)
glimpse(datos_mundo)
str(datos_mundo)

# Filtrar datos

datos_mundo[datos_mundo$anio == 2007,]
subset(datos_mundo, anio == 2007)
filter(datos_mundo, anio == 2007) 

datos_mundo %>% filter(anio == 2007) 

datos_mundo_select <- datos_mundo %>% 
  filter(anio == 2007) 

# Generar grafico
ggplot(datos_mundo_select, aes(x = ExpVida, y = pob)) +
  geom_point()

# EJERCICIO #
# Crear un grafico de gdpPercap (x) vs ExpVida (y) considerando solo datos del 2002
## Donde vea "***" es donde debe escribir algo

# Crear subset con datos 2002
datos_mundo_select <- datos_mundo %>% 
  filter(anio == ***)

# Generar gráfico gdpPercap (x) vs ExpVida (y)
ggplot(datos_mundo_select, aes(x = ***, y = ***)) +
  geom_point()
