library(tidyverse)

# Preparar datos
summary(USArrests)

df <- USArrests %>% 
  mutate(across(where(is.numeric), scale))

rownames(df) <- rownames(USArrests)

summary(df)

# K-mean con 2 grupos
kmean2 <- kmeans(df, centers = 2)
str(kmean2)
kmean2

library(factoextra)
fviz_cluster(kmean2, data = df)

df_pca <- prcomp(df)
df %>% 
  bind_cols(as_tibble(df_pca$x)) %>% 
  mutate(cluster = as.factor(kmean2$cluster),
         estado = row.names(.)) %>%
  ggplot(aes(PC1, PC2, color = cluster, label = estado)) +
  geom_text()

# Comparar con más grupos
kmean3 <- kmeans(df, centers = 3)
kmean4 <- kmeans(df, centers = 4)
kmean5 <- kmeans(df, centers = 5)

p1 <- fviz_cluster(kmean2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(kmean3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(kmean4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(kmean5, geom = "point",  data = df) + ggtitle("k = 5")

library(patchwork)
(p1|p2)/(p3|p4)

# Elbow
fviz_nbclust(df, kmeans, method = "wss")

# Calcular muchas métricas
library(NbClust)

nbclust_out <- NbClust(
  data = df,
  distance = "euclidean",
  min.nc = 2,
  max.nc = 5, 
  method = "kmeans"
)

nbclust_out$Best.nc %>% 
  as.tibble() %>% 
  slice(1) %>% 
  pivot_longer(1:26) %>% 
  count(value)

km_res <- kmeans(df, centers = 2, nstart = 20)
fviz_cluster(km_res, df, ellipse.type = "norm")