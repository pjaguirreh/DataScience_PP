region <- read_csv("datos/region.csv")
grupo_ingresos <- read_csv("datos/income_group.csv")
datosONU <- read_csv("datos/DatosONU_select.csv") %>% 
  select(-X1, -`Series Code`) 

datosONU %>% 
  pivot_longer(1:36, names_to = "anio", values_to = "valor") %>% 
  rename(
    pais = `Country Name`,
    indicador = `Series Name`
  ) %>% 
  left_join(region, by = c("pais" = "country_name")) %>% 
  left_join(grupo_ingresos, by = c("pais" = "country_name")) %>% 
  rename(grupo_ingresos = income_group) %>% 
  select(pais, region, grupo_ingresos, everything()) %>% 
  mutate(
    indicador = case_when(
      indicador == "CO2 emissions (metric tons per capita)" ~ "emisiones_co2",
      indicador == "Fertility rate, total (births per woman)" ~ "tasa_fertilidad",
      indicador == "Forest area (% of land area)" ~ "area_bosques",
      indicador == "GDP per capita (constant 2005 US$)" ~ "PIB_percapita",
      indicador == "Health expenditure per capita, PPP (constant 2005 international $)" ~ "gasto_medico_percapita",
      indicador == "Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)" ~ "participacion_laboral_femenina",
      indicador == "Life expectancy at birth, total (years)" ~ "expectativa_vida",
      indicador == "Malnutrition prevalence, weight for age (% of children under 5)" ~ "malnutricion",
      indicador == "Population (Total)" ~ "poblacion",
      indicador == "Urban population (% of total)" ~ "poblacion_urbana",
      indicador == "Fossil fuel energy consumption (% of total)" ~ "consumo_combustible_fosil)",
      indicador == "Poverty headcount ratio at $2 a day (PPP) (% of population)" ~ "pobreza",
      indicador == "Public spending on education, total (% of government expenditure)" ~ "gasto_publico_educacion")) %>%
  mutate(
    grupo_ingresos = case_when(
      grupo_ingresos == "Low Income" ~ "Ingresos Bajos",
      grupo_ingresos %in% c("Lower Middle Income", "Upper Middle Income") ~ "Ingresos Medio-Bajo",
      grupo_ingresos == "High Income" ~ "Ingresos Altos"),
    region = case_when(
      region == "East Asia and Pacific" ~ "Asia Oriente y Pacifico",
      region == "Europe and Central Afica" ~ "Europa y Africa Central",
      region == "Latin America and the Caribbean" ~ "Latinoamerica y el Caribe",
      region == "Middle East and North Africa" ~ "Medio Oriente y Africa del Norte",
      region == "North America" ~ "Norte America",
      region == "South Asia" ~ "Asia del sur",
      region == "Sub-saharan Africa" ~ "Africa subsahariana")) %>% 
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  mutate(anio = str_sub(anio, 1, 4),
      anio = as.numeric(anio)) %>% 
  filter(anio == 2007) %>% 
  group_by(grupo_ingresos, region) %>% 
  summarise(emisiones_co2 = mean(emisiones_co2, na.rm = TRUE)) %>% 
  pivot_wider(names_from = grupo_ingresos, values_from = emisiones_co2)
