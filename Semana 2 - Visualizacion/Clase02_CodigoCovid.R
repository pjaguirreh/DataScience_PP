# Cargar paquetes y datos
library(readr)
library(ggplot2)
(datos_covid <- read_csv("datos/covid_datos_region.csv"))

## Gráfico base 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob))

## Agregar `geom` de lineas | Pero algo se ve mal 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line()

## Cada linea representa una región (i) 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob, group = region)) +
  geom_line()

## Cada linea representa una región (ii) 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob, col = region)) +
  geom_line()

## Separemos cada linea en su propio panel (i) 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region ))

## Separemos cada linea en su propio panel (ii) 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region), scales = "free_y")

## Separemos cada linea en su propio panel (iii) 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region), ncol = 5)

## Fondo blanco pareciera quedar mejor (i) 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) +
  theme_bw()

## Fondo blanco pareciera quedar mejor (ii) 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) +
  theme_minimal()

## La leyenda pareciera no ser de mucha ayuda 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) +
  theme_minimal() +
  theme(legend.position = "none")

## Títulos/Ejes como detalles finales 
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) + theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Evolución casos COVID-19 en Chile por región",
       subtitle = "La región de Tarapacá y Metropolitana lideran la tabla",
       x = NULL, y = "Casos por cada 10.000 habitantes")
