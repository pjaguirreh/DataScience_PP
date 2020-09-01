library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

## ifelse
(x <- 1:10)
ifelse(x >= 4, 1, 0)
ifelse(x == 5, "A", "B")

## Se puede aplicar en `data frames`
datosONU_tidy <- read_csv("datos/DatosONU_tidy.csv") %>%
  select(country_name, year) %>% 
  filter(year >= 2004, country_name %in% c("Chile", "Argentina"))

datosONU_tidy %>% 
  mutate(nueva_col = ifelse(year > 2005, 1, 0))

## ¿Y si queremos poner más de un valor?
datosONU_tidy %>% 
  mutate(nueva_col = ifelse(year > 2005, 1, ifelse(year < 2005, 2, 0)))

## `case_when`
datosONU_tidy %>% 
  mutate(nueva_col = case_when(
    year > 2005 ~ 1,
    year < 2005 ~ 2,
    TRUE ~ 0
  ))

## Pivot_wider 
table2

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count)

## Pivot_longer 
table4a

table4a %>% 
  pivot_longer(2:3, 
               names_to = "year", 
               values_to = "value")

## Enfermedades respiratorias 
glimpse(who)

## Donde comenzamos
enfermedades <- who %>% 
  select(-iso2, -iso3)

## ¿Qué información tenemos disponible? 
names(enfermedades)

## Transformamos la forma de nuestros datos 
(enfermedades2 <- enfermedades %>% 
    pivot_longer(-c(country:year), 
                 names_to = "variables", 
                 values_to = "valores"))

## Eliminaremos parte de la columna "variables" que no nos sirve 
(enfermedades3 <- enfermedades2 %>% 
    mutate(variables = str_remove(variables, "new_"),
           variables = str_remove(variables, "new")))
  
## `separate`
(enfermedades4 <- enfermedades3 %>% 
    separate(variables, 
             into = c("enfermedad", "otro"), 
             sep = "_")) 

## `separate` (ii) 
(enfermedades5 <- enfermedades4 %>% 
    separate(otro, 
             into = c("sexo", "edad"), 
             sep =  1))

## Se podría llegar a lo mismo usando `stringr` 
enfermedades2 %>% 
  transmute(country, year,
            
            enfermedad = case_when(
              str_detect(variables, "rel") ~ 
                str_sub(variables, 4, 6),
              TRUE ~ str_sub(variables, 5,6)),
            
            sexo = case_when(
              str_detect(variables, "m") ~ "m",
              TRUE ~ "f"),
            
            edad = str_extract(variables, "\\d+"),
            valores)

## Modificaremos algunos valores para mayor claridad 
(enfermedades6 <- enfermedades5 %>% 
    mutate(
      edad = case_when(
        edad == "014" ~ "0-14",
        edad == "1524" ~ "15-24",
        edad == "2534" ~ "25-34",
        edad == "3544" ~ "35-44",
        edad == "4554" ~ "45-54",
        edad == "5564" ~ "55-64",
        edad == "65" ~ "65+"
      ),
      sexo = case_when(
        sexo == "m" ~ "hombres",
        sexo == "f" ~ "mujeres"
      )))

## Calculamos el total de enfermedades por sexo y rango de edad 
(resumen_enfermedades <- enfermedades6 %>% 
    filter(year == 2010) %>% 
    group_by(sexo, edad) %>% 
    summarise(total = sum(valores, na.rm = TRUE)))

## `Pivot_wider` para dar la forma final a la tabla
(tabla_final <- resumen_enfermedades %>% 
    pivot_wider(names_from = edad, values_from = total))

## Resumen 
tabla_final <- who %>% 
  # Eliminar columnas que no usaremos
  select(-iso2, -iso3) %>% 
  # Ajustar forma de los datos (de ancho a largo)
  pivot_longer(-c(country:year), names_to = "variables", values_to = "valores", values_drop_na = TRUE) %>% 
  # Extraer información de la columna "variables
  mutate(variables = str_remove(variables, "new_"),
         variables = str_remove(variables, "new")) %>% 
  separate(variables, into = c("enfermedad", "otro"), sep = "_") %>% 
  separate(otro, into = c("sexo", "edad"), sep =  1) %>% 
  # Re-codificar las columnas edad y sexo
  mutate(edad = case_when(
    edad == "014" ~ "0-14",
    edad == "1524" ~ "15-24",
    edad == "2534" ~ "25-34",
    edad == "3544" ~ "35-44",
    edad == "4554" ~ "45-54",
    edad == "5564" ~ "55-64",
    edad == "65" ~ "65+"),
    sexo = case_when(
      sexo == "m" ~ "hombres",
      sexo == "f" ~ "mujeres")) %>% 
  # Generar tabla final para el año 2010
  filter(year == 2010) %>% 
  group_by(sexo, edad) %>% 
  summarise(total = sum(valores, na.rm = TRUE)) %>% 
  pivot_wider(names_from = edad, values_from = total)
