##################
## Cargar paquetes
##################
library(rvest) # web scraping
library(dplyr) # manejo de datos
library(tidyr) # transformación de datos
library(stringr) # manejo de texto
library(purrr) # iteraciones y "programación funcional"
library(ggplot2) # visualizaciones

# Primero definimos la ruta de donde queremos obtener la información
# y la pasamos por la función read_html
web <- read_html("https://www.imdb.com/chart/top/?ref_=nv_mv_250")

# Con "selector gadget" identificamos el argumento a incluir en html_nodes
# nombre: .titleColumn a
# año:  .secondaryInfo
# nota/puntaje: strong
# Teniendo los nodos, la siguiente función dependerá del tipo de dato a obtener

# Obtenermos el nombre de todas las peliculas
(nombre_pelicula <- web %>% 
  html_nodes(".titleColumn a") %>% 
  html_text())

# Obtenermos el año de todas las peliculas
(anio_pelicula <- web %>% 
  html_nodes(".secondaryInfo") %>% 
  html_text())

# Obtenermos la nota de todas las peliculas
(nota_pelicula <- web %>% 
  html_nodes("strong") %>% 
  html_text())

# Juntamos los tres vectores en un tibble/data frames
(df_peliculas <- tibble(
  nombre = nombre_pelicula,
  anio = anio_pelicula,
  nota = as.numeric(nota_pelicula)))

# Hacemos algunos ajustes al año
(df_peliculas <- df_peliculas %>% 
  mutate(anio = str_remove(anio, "\\("),
         anio = str_remove(anio, "\\)"),
         anio = as.numeric(anio)))

#####
#####

# Otra forma de hacer lo mismo

# con el nodo "td" obtenemos toda la información junta pero desordenada
(nombre_nota_anio <- web %>% 
  html_nodes("td") %>% 
  html_text())

# vamos a hacer algunos ajustes: eliminar "espacio blanco", transformaremos
# el vector de información en un tibble/data frame y eliminaremos filas
# con información no útil
(nombre_nota_anio <- nombre_nota_anio %>% 
  str_squish() %>% 
  as_tibble() %>% 
  filter(value != "",
         !str_detect(value, "NOT YET RELEASED")) )

# Ahora tenemos filas con el nombre y año y otras filas con la nota/puntaje
# Separaremos esta información

# Primero separaremos las filas con la nota
(nota <- nombre_nota_anio %>% 
  slice(seq(2, 500, by = 2)) %>% 
  transmute(nota = as.numeric(value)))

# Luego el nombre y año
(nombre_anio <- nombre_nota_anio %>% 
  slice(seq(1, 500, by = 2)))

# Ahora haremos algunos arreglos para separa el nobre y el año además de 
# eliminar partes no útiles

# Eliminar los números antes del nombre de la película
# hacemos la separación usando una "expresión regular"
(nombre_anio <- nombre_anio %>% 
  separate(value, into = c(NA, "nombre"), sep = "\\d\\. "))

# Separamos nombre y año además de eliminar paréntesis
(nombre_anio <- nombre_anio %>% 
  separate(nombre, into = c("nombre", "anio"), sep = " \\(") %>% 
  mutate(anio = as.numeric(str_remove(anio, "\\)"))))

# Juntamos todo
nombre_anio %>% 
  bind_cols(nota)

#####
#####

# Objetivo: extraer los ranking para cada género cinematográfico

# En la misma pag del ranking general se puede acceder a todos los géneros que
# IMDB considera y para los que tiene rankings
(generos <- web %>% 
  html_nodes(".subnav_item_main") %>% 
  html_text() %>% 
  str_squish())

# Por ejemplo, podemos obtener el ranking de las películas de acción

# Definimos la url donde está la información
web_accion <- read_html("https://www.imdb.com/search/title/?genres=action&sort=user_rating,desc&title_type=feature&num_votes=25000,&pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=5aab685f-35eb-40f3-95f7-c53f09d542c3&pf_rd_r=4HYDBTMWN2R9EXCBGWH3&pf_rd_s=right-6&pf_rd_t=15506&pf_rd_i=top&ref_=chttp_gnr_1")

# Extraermos los nombres
(nom <- web_accion %>% 
  html_nodes(".lister-item-header a") %>% 
  html_text())

# Y la nota
(not <- web_accion %>% 
  html_nodes(".ratings-imdb-rating strong") %>% 
  html_text())

# Juntamos ambos vectores
tibble(
  nombre = nom,
  nota = as.numeric(not)
)

#####
#####

# Podemos hacer esto mismo para todos los géneros pero sería un poco redundante
# y terminariamos con mucho código. Trataremos de generalizar la obtención de 
# la info a través de una función

# La siguiente función es prácticamente el mismo código que se usó para la
# obtención del ranking de acción pero dividimos la url en dos y dejamos un
# espacio a rellenar. Ese espacio en la url corrseponde al género.
funcion_extraer <- function(x){
  
  web_genero <- read_html(paste0("https://www.imdb.com/search/title/?genres=", x, 
                                 "&sort=user_rating,desc&title_type=feature&num_votes=25000,&pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=5aab685f-35eb-40f3-95f7-c53f09d542c3&pf_rd_r=4HYDBTMWN2R9EXCBGWH3&pf_rd_s=right-6&pf_rd_t=15506&pf_rd_i=top&ref_=chttp_gnr_1"))
  
  nom <- web_genero %>% 
    html_nodes(".lister-item-header a") %>% 
    html_text()
  
  not <- web_genero %>% 
    html_nodes(".ratings-imdb-rating strong") %>% 
    html_text()
  
  tibble(
    genero = rep(x, length(nom)), # sumamos una columna más con el nombre del
                                  # género
    nombre = nom,
    nota = as.numeric(not)
  )
  
}


# Teniendo la función definida, usaremos la función map y el vector de géneros
# obtenidos inicialmente. Esto generará una lista donde cada elemento es un
# ranking de un género
top_50_generos <- map(generos, funcion_extraer)

# Juntamos los elementos de la lista
(top_50_generos <- top_50_generos %>% 
  bind_rows())

# Hacemos algunos gráficos con la información
top_50_generos %>% 
  group_by(genero) %>% 
  summarise(nota = mean(nota, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(genero, nota), y = nota)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = NULL)

top_50_generos %>% 
  group_by(genero) %>% 
  ggplot(aes(x = genero, y = nota)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL)
    
## Hacer lo mismo con un loop y no map

resultado <- data.frame()
for (i in generos){
  
  web_genero <- read_html(paste0("https://www.imdb.com/search/title/?genres=", i, 
                                 "&sort=user_rating,desc&title_type=feature&num_votes=25000,&pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=5aab685f-35eb-40f3-95f7-c53f09d542c3&pf_rd_r=4HYDBTMWN2R9EXCBGWH3&pf_rd_s=right-6&pf_rd_t=15506&pf_rd_i=top&ref_=chttp_gnr_1"))
  
  nom <- web_genero %>% 
    html_nodes(".lister-item-header a") %>% 
    html_text()
  
  not <- web_genero %>% 
    html_nodes(".ratings-imdb-rating strong") %>% 
    html_text()
  
  tab <- tibble(
    genero = rep(i, length(nom)), # sumamos una columna más con el nombre del
    # género
    nombre = nom,
    nota = as.numeric(not))
  
  resultado <- bind_rows(resultado, tab)
  
}

resultado %>% 
  group_by(genero) %>% 
  ggplot(aes(x = genero, y = nota)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL)
