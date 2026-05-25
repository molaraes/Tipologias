# =========================================================
# Clústers
# Fecha: 29 de mayo de 2026
# Profesora: Mónica Lara
# =========================================================

# Previo ------

# Instalamos pacman, el cual es un gestor de paquetes: instala y carga los paquetes a la vez

install.packages("pacman")

# Lo llamamos

library(pacman)

# Lo utilizamos para cargar y llamar los otros paquetes:
p_load(tidyverse, here, factoextra, haven, cluster)

#here() resuelve rutas relativas a la raíz del proyecto (la carpeta donde está el .Rproj o un archivo .here), así no dependemos de setwd()

# Base de datos ------

# Completa
vdem <- read_dta(here("Data", "V-Dem-CY-Full+Others-v16.dta"))

# Filtramos a 2025 y nos quedamos solo con identificador de país + los 21 índices mid-level
base <- vdem %>% 
  filter(year == 2025) %>% 
  select(country_text_id,        # código ISO de 3 letras
         country_name,           # nombre completo (para etiquetar)
         v2x_freexp_altinf,      # Freedom of expression & alt info
         v2x_frassoc_thick,      # Freedom of association (thick)
         v2x_suffr,              # Share of pop. with suffrage
         v2xel_frefair,          # Clean elections
         v2x_elecoff,            # Elected officials
         v2xcl_rol,              # Equality before the law & ind. liberty
         v2x_jucon,              # Judicial constraints on executive
         v2xlg_legcon,           # Legislative constraints on executive
         v2x_cspart,             # Civil society participation
         v2xdd_dd,               # Direct popular vote
         v2xel_locelec,          # Local government
         v2xel_regelec,          # Regional government
         v2xeg_eqprotec,         # Equal protection
         v2xeg_eqaccess,         # Equal access
         v2xeg_eqdr)             # Equal distribution of resources

#Quitamos NAs
base_clean <- base %>% na.omit()

#Ordenamos alfabéticamente
base_clean <- base_clean %>% 
  arrange(country_name)

#Quitamos base de vdem porque es muy pesada
rm(vdem)

# Matriz numérica ------

# Para generar nuestros clústers, primero estandarizamos nuestras variables

base_num <- scale(base_clean[,3:17]) #selecciono columnas 2 a 18
base_num <- as_tibble(base_num) #guardo como base de datos

# k medias ------

# Veamos qué nos dice el criterio del codo
fviz_nbclust(base_num, kmeans, method = "wss") +
  labs(title = "Método del codo")

# Silueta: buscar el k que maximiza el promedio
fviz_nbclust(base_num, kmeans, method = "silhouette") +
  labs(title = "Método de la silueta")

#kmeans(x, centers, nstart)
#x, es la base de datos o matriz
#centers, el número de clusters
#nstart, inicializaciones aleatorias para que se quede con la mejor — reduce la dependencia del punto de partida.

#con 2 clusters
set.seed(100)
kmeansfit2 <- kmeans(base_num, centers=2, nstart=25)
kmeansfit2

#con 3 clusters
set.seed(100)
kmeansfit3 <- kmeans(base_num, centers=3, nstart=25)
kmeansfit3

#con 4 clusters
set.seed(100)
kmeansfit4 <- kmeans(base_num, centers=4, nstart=25)
kmeansfit4

#Razonamiento: de k=1 a k=2, la SS baja de 2500 a 1500 (aprox); de k=2 a k=3 la SS baja de 1400 a 1200. De k=3 a k=4 baja menos. Si nos quedamos con k=2 solo distinguiríamos democracias vs autocracias, lo cual puede ser muy general. k=3 añade la categoría intermedia. Quedemonos con 3, ya luego ustedes replican con 2.

# Veamos de nuevo nuestros resultados de tres
kmeansfit3
#Cluster 2 → Democracias liberales consolidadas
#Todos los índices son positivos y altos. Lo clave: v2xel_frefair (+1.07), v2x_jucon (+0.96), v2xlg_legcon (+0.97), v2xcl_rol (+0.94), v2xeg_eqaccess (+0.94). No solo hay elecciones limpias y libertades civiles, sino que el ejecutivo está efectivamente controlado por el poder judicial y el legislativo, y hay equidad de acceso. Son los países donde la poliarquía se complementa con un Estado de derecho real.

#Cluster 1 → Democracias electorales / regímenes intermedios
#Los centros están cerca de cero en casi todo, levemente positivos. El detalle interesante: v2xeg_eqdr es −0.30, es decir, incluso en países con libertades formales la distribución igualitaria de recursos queda rezagada. Son sistemas que tienen el procedimiento democrático pero con déficits en la dimensión igualitaria. 

#Cluster 3 → Autocracias
#Todos los índices fuertemente negativos. Lo más marcado: v2x_freexp_altinf (−1.22), v2x_frassoc_thick (−1.28), v2x_jucon (−1.18), v2xlg_legcon (−1.15). No solo hay restricción electoral — el ejecutivo opera sin contrapesos y la sociedad civil está suprimida. v2x_suffr es el único índice menos negativo (−0.29), lo cual es consistente con que muchas autocracias modernas mantienen elecciones formales con sufragio universal pero sin competencia real.

#Gráfico
fviz_cluster(kmeansfit3, data = base_num,
             geom         = c("point"),
             labelsize    = 7,
             repel        = TRUE,
             ellipse.type = "norm",
             show.clust.cent = TRUE) +
  labs(title    = "Tipología de regímenes políticos",
       subtitle = "K-means, k = 3",
       caption = "Elaboración propia con datos de Vdem") +
  theme_minimal()

# Clúster jerárquico aglomerativo ------

# Paso 1: matriz de distancias euclidianas entre países
mat_dist <- dist(base_num, method = "euclidean")

# Paso 2: varios métodos de vinculación

# El método controla cómo se mide la distancia entre grupos al fusionar

clus_complete <- hclust(mat_dist, method = "complete")  # distancia máxima
clus_average  <- hclust(mat_dist, method = "average")   # promedio de distancias
clus_single   <- hclust(mat_dist, method = "single")    # distancia mínima
clus_ward     <- hclust(mat_dist, method = "ward.D2")   # minimiza varianza intra


# Paso 3: correlación cofenética

# Mide qué tan bien preserva el dendrograma las distancias originales.
# Cuanto más cerca de 1, mejor representación.

cor(mat_dist, cophenetic(clus_complete))
cor(mat_dist, cophenetic(clus_average))
cor(mat_dist, cophenetic(clus_single))
cor(mat_dist, cophenetic(clus_ward))

# Nos quedamos con average

# Paso: dendrograma con factoextra 
fviz_dend(clus_average, k = 3, cex = 0.5,
          rect = TRUE,
          k_colors = c("red", "steelblue", "darkgreen"),
          main = "Dendrograma — V-Dem mid-level 2025")

# Paso 5: corte del árbol y asignación de países a grupos
grp_h <- cutree(clus_average, k = 3)
table(grp_h)                         # cuántos países en cada cluster


# Paso 6: visualizar en espacio PCA con etiquetas de países
fviz_cluster(list(data = base_num, cluster = grp_h),
             geom         = c("point"),
             labelsize    = 7,
             repel        = TRUE,
             palette      = c("red", "steelblue", "darkgreen"),
             show.clust.cent = TRUE) +
  labs(title = "Jerárquico — V-Dem 2025") +
  theme_minimal()


# Análisis posterior ------

# ── Comparación k-means vs jerárquico ────────────────────────────────────────

# Si la tabla muestra alta concentración en la diagonal, ambos métodos coinciden y la solución es robusta. 

table(kmeans     = kmeansfit3$cluster,
      jerarquico = grp_h)

#¿Qué países están en cada clúster?

# Pegamos los clusters directamente a base_clean
base_clean <- base_clean %>% 
  mutate(
    cluster_kmeans = kmeansfit3$cluster,
    cluster_aglom  = grp_h
  )

# Verificamos que coincida con los países
base_clean %>% 
  select(country_name, cluster_kmeans, cluster_aglom) %>% 
  print(n = 30)

# Veamos los países y los clústers
base_clean %>% filter(cluster_kmeans==1) %>% select(country_name)
base_clean %>% filter(cluster_kmeans==2) %>% select(country_name)
base_clean %>% filter(cluster_kmeans==3) %>% select(country_name)

# Veamos los países y los clústers
base_clean %>% filter(cluster_aglom==1) %>% select(country_name)
base_clean %>% filter(cluster_aglom==2) %>% select(country_name)
base_clean %>% filter(cluster_aglom==3) %>% select(country_name)

#Veamos cuáles países quedaron diferente en cada método
base_clean %>% 
  select(country_name, cluster_kmeans, cluster_aglom) %>% 
  filter(cluster_kmeans != cluster_aglom)
