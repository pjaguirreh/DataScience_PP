######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(ggplot2) # Visualizar datos
library(dplyr) # Verbos de manipulación de datos

## Pipe

# Ejemplos

log(sqrt(10))
10 %>% sqrt() %>% log()

summary(iris)
iris %>% summary()

round(3.45, digits = 1)
3.45 %>% round(digits = 1)

## Cargar datos
datosONU_tidy <- read_csv("datos/DatosONU_tidy.csv")

# Examinar datos
names(datosONU_tidy)
glimpse(datosONU_tidy)
summary(datosONU_tidy)

## FILTER

# Solo las observaciones correspondientes a Chile
datosONU_tidy %>% 
  filter(country_name == "Chile")

# Solo las observaciones correspondientes a Chile y para años posteriores al 2000
datosONU_tidy %>% 
  filter(country_name == "Chile", year > 2000)

# Solo las observaciones correspondientes a los años 1995, 2000, y 2005
datosONU_tidy %>% 
  filter(year %in% c(1995, 2000, 2005))

# Solo las observaciones **NO** correspondientes a los años 1995, 2000, y 2005
datosONU_tidy %>% 
  filter(!year %in% c(1995, 2000, 2005))

## SLICE

# La quinta fila
datosONU_tidy %>% 
  slice(5)

# Las primeras 5 filas
datosONU_tidy %>% 
  slice(1:5)

## SELECT

# Seleccionar 5 variables/columnas
datosONU_tidy %>% 
  select(country_name, income_group, region, year, population_total)

# Dejar todas las columnas menos dos
datosONU_tidy %>% 
  select(-region, -income_group)

# Dejar todas las columnas que contengan *per capita*
datosONU_tidy %>% 
  select(contains("per_capita"))

# Dejar todas las columnas que comiencen con *p*
datosONU_tidy %>% 
  select(starts_with("p"))

# Dejar todas las columnas numéricas
datosONU_tidy %>% 
  select(where(is.numeric))

## MUTATE

# Nueva columna calculando el logaritmo de una existente
datosONU_tidy %>%
  mutate(log_co2_emissions = log(co2_emissions_metric_tons_per_capita))

## RENAME

# Nombres muy largos
names(datosONU_tidy)

# Se pueden cambiar cuantos nombres queramos
datosONU_tidy <- datosONU_tidy %>% 
  rename(
    "co2_emissions" = "co2_emissions_metric_tons_per_capita",
    "fertility_rate" = "fertility_rate_total_births_per_woman",
    "forest_area" = "forest_area_percent_of_land_area",
    "gdp_per_capita" = "gdp_per_capita_constant_2005_us",
    "health_expenditure" = "health_expenditure_per_capita_ppp_constant_2005_international",
    "labor_force_participation" = "labor_force_participation_rate_female_percent_of_female_population_ages_15_modeled_ilo_estimate",
    "life_expectancy" = "life_expectancy_at_birth_total_years",
    "malnutrition_prevalence" = "malnutrition_prevalence_weight_for_age_percent_of_children_under_5",
    "urban_population" = "urban_population_percent_of_total",
    "fossil_fuel_consumption" = "fossil_fuel_energy_consumption_percent_of_total",
    "poverty" = "poverty_headcount_ratio_at_2_a_day_ppp_percent_of_population",
    "public_spending_education" = "public_spending_on_education_total_percent_of_government_expenditure"
  )

# Chequear
names(datosONU_tidy)

## ARRANGE

datosONU_tidy

datosONU_tidy %>% 
  arrange(year)

## DISTINCT

# Tantos valores como observaciones hay
datosONU_tidy %>% 
  select(income_group)

# Pero son pocos valores únicos/distintos
datosONU_tidy %>% 
  select(income_group) %>% 
  distinct()

# Se puede hacer para cualquier combinación de columnas/variables
datosONU_tidy %>% 
  select(income_group, region) %>% 
  distinct() %>% 
  arrange(income_group, region)

## SUMMARISE

# Número de observaciones
datosONU_tidy %>% 
  summarise(n_observaciones = n())

# Promedio de la columna *fertility_rate*
datosONU_tidy %>% 
  summarise(promedio_fertility_rate = mean(fertility_rate, na.rm = TRUE))

# Se puede calcular más de un valor
datosONU_tidy %>% 
  summarise(n_observaciones = n(),
            promedio_fertility_rate = mean(fertility_rate, na.rm = TRUE))

## GROUP_BY

# Por si sola no pasa nada
datosONU_tidy %>% 
  group_by(region)

# Pero con `summarise` aparecen las ventajas
datosONU_tidy %>% 
  group_by(region) %>% 
  summarise(n_observaciones = n())

# Se puede agrupar por más de una variable/columna
datosONU_tidy %>% 
  group_by(region, income_group) %>% 
  summarise(n_observaciones = n())
