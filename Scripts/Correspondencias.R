# =========================================================
# Clústers
# Fecha: 29 de mayo de 2026
# Profesora: Mónica Lara
# =========================================================

# Previo ------

rm(list = ls())

# Volvemos a llamar a pacman.

library(pacman)

# Ahora sí, los paquetes de hoy.

p_load(janitor,
       FactoMineR,
       tidyverse,
       factoextra,
       broom,
       gridExtra,
       GGally,
       haven,
       ca)

# Base de datos 
lapop_2023 <- read_dta(here("Data", "MEX_2023_LAPOP_AmericasBarometer_v1.0_w.dta"))

# Bases del ACM ----
# Analiza relaciones entre múltiples variables categóricas:
#       patrones, afinidades, asociaciones
#
# Se basa en frecuencias y distancias chi-cuadrado,
#       no en relaciones lineales entre variables numéricas
#
# Representa las relaciones en un plano multidimensional: 
#       reducción de dimensiones facilita la interpretación 
## Interpretación visual: mapa factorial
#       muestra categorías (individuos, o ambos) para analizar
#       agrupamientos, asociaciones,
#       oposiciones y ejes de diferenciación
#
# Funciona bajo una lógica intuitiva

## Requisitos y supuestos ----

# Utilizar variables categóricas: nominales u ordinales
# Más de 3 variables
# No requiere verificar supuestos como otras técnicas
#      (normalidad, linealidad, homocedasticidad, multicolinealidad...)
# Atención a categorías vacías (generan problemas en los cálculos)
# Considera  muestra "grande" 
#       al menos más de 100 casos
#       regla informal: tener más observaciones que categorías
#       RECOMENDACIÓN: que las variables utilizadas tengan un 
#       mismo número de categorías (no siempre se puede cumplir)

# Utilidad ----
# Analizar perfiles:
#   patrones de consumo
#   perfiles socioeconómicos
#   perfiles demográficos
#      (reproductivos, migratorios, conyugales, laborales, educativos)
#   actitudes
#   participación ciudadana
#   preferencias electorales
#   confianza en instituciones
#   patrones y estilos de vida
#   segmentación
#   tipologías de capacidad institucional
#   identificar de problemáticas sociales y políticas relacionadas entre sí

# Ventajas y desventajas ----
# 
# |       Ventajas          |      Desventajas
# | Muestra asociaciones    | No muestra causalidad
# |_________________________|_____________________________________________
# | Intuitivo y flexible    | Sensibilidad a pocas 
# |                         |frecuencias y al tamaño de muestra
# |_________________________|_____________________________________________
# | Multidimensionalidad    | Requiere interpretación teórica
# |                         | (los ejes no dicen nada por sí solos)
# |_________________________|_____________________________________________

# Flujo de trabajo ----
#   1. Preprar los datos
#      1.1 Codificar en categorías binarias:
#          R lo hace en automático
#   2. Explorar los datos
#      2.1 Categorías con menos de 1%: reagruparlas
#     2.2 Atención a categorías vacías: borrarlas o 
#          tratarlas como una categoría en sí
#   3. Realizar ACM
#      3.1 Tabla de combinación de categorías
#          y cálculo de distancias
#      3.2 Reducción dimensional: 
#          analizar la varianza explicada
#          de los ejes, ¿cómo resumen la información original?
#      3.3 Corregir medidas: opcional pero recomendado
#      3.4 Seleccionar ejes (dimensiones) a utilizar
#      3.5 Elaborar y analizar mapas factoriales
#      3.6 Análisis
#   5. Interpretación y presentación de resultados

# Variables ----

# Sociodemográficas
# ur - 1 urbano, 2 rural
# q1tc - género
# edre -escolaridad

# Políticas
# soct2 - ¿Considera usted que la situación económica del país es mejor, igual o peor que hace doce meses?
# idio2 - ¿Considera usted que su situación económica actual es mejor, igual o peor que la de hace doce meses?
# b2 - ¿Hasta qué punto tiene usted respeto por las instituciones políticas de México?
# m1 - Hablando en general acerca del gobierno actual, ¿diría usted que el trabajo que está realizando el presidente Andrés Manuel López Obrador es...?:
# pn4 - En general, ¿usted diría que está muy satisfecho(a), satisfecho(a), insatisfecho(a) o muy insatisfecho(a) con la forma en que la democracia funciona en México?
# B21a - ¿Hasta qué punto tiene usted confianza en el presidente?
# pol1- ¿Qué tanto interés tiene usted en la política: mucho, algo, poco o nada?
# vb3n - voto en la última elección

# Vamos a seleccionar esas variables y hacer una base nueva.

subset_lapop_2023 <- lapop_2023 %>% 
  select(ur, q1tc_r, edre, soct2, idio2, b2, m1, pn4, b21a, pol1, vb3n) %>% 
  mutate(across(everything(), as_factor)) %>%  # primero a factor
  mutate(across(everything(), as.numeric))      # luego a numérico

# Las vamos a recodificar

# Urbano - Rural
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(ur=case_when(ur==1~"Urbano",
                      ur==2~"Rural"),
         ur=factor(ur, levels=c("Urbano", "Rural")))

# Género
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(q1tc_r = case_when(
    q1tc_r == 1 ~ "Hombre",
    q1tc_r == 2 ~ "Mujer"
  ),
  q1tc_r = factor(q1tc_r, levels = c("Hombre", "Mujer")))

# Escolaridad
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(edre = case_when(
    edre %in% c(1, 2, 3) ~ "Primaria o menos",
    edre %in% c(4, 5) ~ "Secundaria",
    edre %in% c(6, 7) ~ "Universidad"
  ),
  edre = factor(edre, levels = c("Primaria o menos", "Secundaria", "Universidad")))

# Evaluación económica
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(soct2=case_when(soct2==1~"Ec_Mejor",
                         soct2==2~"Ec_Igual",
                         soct2==3~"Ec_Peor"),
         soct2=factor(soct2, levels=c("Ec_Peor", "Ec_Igual", "Ec_Mejor")))

# Evaluación económica personal
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(idio2=case_when(idio2==1~"EcPer_Mejor",
                         idio2==2~"EcPer_Igual",
                         idio2==3~"EcPer_Peor"),
         idio2=factor(idio2, levels=c("EcPer_Peor", "EcPer_Igual", "EcPer_Mejor")))

# Aprobación presidencial
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(m1=case_when(m1 %in% c(1,2)~"Gob_Bueno",
                      m1==3~"Gob_Regular",
                      m1 %in% c(4,5)~"Gob_Malo"),
         m1=factor(m1, levels = c("Gob_Malo", "Gob_Regular", "Gob_Bueno")))

# Respeto por instituciones
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(b2 = case_when(
    b2 %in% c(1, 2) ~ "Poco_respeto",
    b2 %in% c(3, 4, 5) ~ "Medio_respeto",
    b2 %in% c(6, 7) ~ "Mucho_respeto"
  ),
  b2 = factor(b2, levels = c("Poco_respeto", "Medio_respeto", "Mucho_respeto")))

# Satisfacción con la democracia
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(pn4 = case_when(
    pn4 %in% c(1, 2) ~ "Dem_satisfecho",
    pn4 %in% c(3, 4) ~ "Dem_insatisfecho"
  ),
  pn4 = factor(pn4, levels = c("Dem_satisfecho", "Dem_insatisfecho")))

# Interés en la política
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(pol1 = case_when(
    pol1 %in% c(1,2) ~ "Mucho/algo_interés",
    pol1 %in% c(3,4) ~ "Poco/nada_interés"
  ),
  pol1 = factor(pol1, levels = c("Mucho/algo_interés", "Poco/nada_interés")))

# Voto
prop.table(table(subset_lapop_2023$vb3n))

subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(vb3n = case_when(
    vb3n == 3~ "MORENA",
    vb3n == 4 ~ "PAN",
    vb3n == 5 ~ "PRI",
    vb3n %in% c(6,7) ~ "Otro",
    vb3n %in% c(8,9) ~ "No sabe/No responde",
  ),
  vb3n = factor(vb3n, levels = c("MORENA", "PAN", "PRI", "Otro", "No sabe/No responde")))

# Confianza en presidente
subset_lapop_2023 <- subset_lapop_2023 %>% 
  mutate(b21a = case_when(
    b21a %in% c(1, 2) ~ "Poca_conf",
    b21a %in% c(3,4,5) ~ "Media_conf",
    b21a %in% c(6,7) ~ "Mucha_conf"),
    b21a = factor(b21a, levels = c("Poca_conf", "Media_conf", "Mucha_conf")))

# Vamos a omitir datos perdidos
subset_lapop_2023 <- subset_lapop_2023 %>% 
  na.omit()

# Explorar los datos ----

# Tablas de contingencia: 
#        útiles para verificar la consistencia
#        de los datos y posibles relaciones significativas

# Vamos a hacer nuestro primer bucle:

# Hacemos un vector de variables:
variables <- c("ur", "q1tc_r", "edre", "soct2", "idio2", "b2", "m1", "pn4", "b21a", "pol1", "vb3n")

# Tablas de frecuencia: verificar categorías
tablas_frecuencia <- map(variables, ~{  # map() itera sobre cada variable del vector
  subset_lapop_2023 %>%                 # toma base de datos
    tabyl(!!sym(.x)) %>%                # crea tabla de frecuencias para esa variable
    adorn_pct_formatting()              # agrega porcentajes formateados
}) %>%
  set_names(variables)  # nombra cada elemento de la lista con el nombre de la variable
tablas_frecuencia       # muestra todas las tablas

# No hay ninguna categoría que tenga menos del 1%

# Realizar ACM FINAL ----

# Vamos a renombrar las variables
subset_lapop_2023 <- subset_lapop_2023 %>% 
  rename(localidad=ur, 
         género=q1tc_r, 
         escolaridad=edre, 
         ev_econ=soct2, 
         ev_econ_pers=idio2,
         respeto_inst=b2, 
         aprob_pres=m1, 
         satis_dem=pn4, 
         conf_presi=b21a,
         interes=pol1,
         elec_2018=vb3n)

# Análisis
acm <- MCA(subset_lapop_2023, graph = TRUE, ncp = 10) 
#ncp especifica número de dimensiones a guardar

# Normalmente se omite el gráfico (se realiza después).
# Si no se omite: arroja 3 visualizaciones
#
# 1. Mapa de categorías
#
# 2. Mapa de individuos 
#
# 3. Representación de variables: 
#               qué tanto explica cada dimensión cada una variable de las variables que utilizamos. 
#               Si una variable está cerca del origen: puede que no aporte el ACM
#               Mejor aporte = cruce de ejes, lejos del origen
#               Aporte moderado: cercanía a un eje pero lejos del origen

# Utilizar 2 o 3 dimensiones es lo más común
#    se recomienda hacerlo si ya se conocen los datos
#    y la teoría respalda utilizar una representación bi/tridimensional

# Si no se conocen los datos: 
#    hacer el análisis completo para decidir cuántas dimensiones utilizar

## Resultados del análisis ----
# El comando MCA genera un objeto que contiene lo siguiente:
# COMPONENTES PRINCIPALES:
# 1.  $eig            - Eigenvalues: es la varianza explicada por cada dimensión.
#                       Se usa para decidir cuántas dimensiones conservar
#
# VARIABLES Y CATEGORÍAS ($var):
# 2.  $var$coord      - Coordenadas de categorías en el mapa (posición X,Y)
# 3.  $var$cos2       - Calidad de representación de categorías (0-1)
#                       Valores altos = bien representadas en el mapa
# 4.  $var$contrib    - Contribución de categorías a dimensiones (%)
#                       Se usa para identificar las categorías más importantes para cada eje
# 5.  $var$v.test     - Significancia estadística de posición de categorías
# 6.  $var$eta2       - Correlación variables-dimensiones (0-1)
#                       Indica qué variables definen cada dimensión
#
# INDIVIDUOS ($ind):
# 7.  $ind$coord      - Son las coordenadas de individuos en el mapa
# 8.  $ind$cos2       - Indica la calidad de representación de los individuos (0-1)
# 9.  $ind$contrib    - Indica la contribución de los individuos a las dimensiones
#
# OTRA TÉCNICA ($call):
# 10. $call           - Parámetros y resultados intermedios
# 11. $call$marge.col - Pesos/frecuencias de categorías
# 12. $call$marge.li  - Pesos de individuos
#
# PARA INTERPRETAR SE USAN:
# - acm$eig           (varianza explicada)
# - acm$var$coord     (coordenadas para mapas)
# - acm$var$contrib   (importancia de categorías)
# - acm$var$eta2      (qué variables definen cada dimensión)

# Interpretar dimensiones ----

# Obtener resumen del análisis

summary(acm)

# Explorar acm (todas las dimensiones)
# Se puede ver lo siguiente:
# 
#     1. Eigenvalues: cuánta varianza captura cada dimensión
#                     Dim1 Explica el 13.395% de las diferencias
#                     Dim1+Dim2+Dim3 explican el 28.528%
#                     Cuando la varianza acumulada deja de incrementarse podríamos dejar de considerar las siguientes dimensiones. Aunque de forma práctica lo usual es utiliza de 2 a 3 dimensiones nada más 
# 
#     2. Valores individuales
#     Coordenadas de cada individuo en el mapa
#     cos2: calidad de la representación (de 0 a 1),
#     indica en qué dim está mejor representado un valor
#                     
#     3. Valores de las categorías:
#     Cómo contribuyen las categorías a cada dimensión
#    La contribución se lee positivamente (hacia la derecha) o negativamente (hacia la izquierda)
#   
#     4. Correlación entre dimensiones y categorías
#   Cómo se relaciona cada variable con cada dimensión


## Vemos eigenvalues
acm$eig

## Vemos coordenadas de individuos
head(acm$ind$coord)        # Coordenadas
head(acm$ind$contrib)      # Contribuciones
head(acm$ind$cos2)         # Calidad de representación

## Vemos valores de las categorías
acm$var$coord              # Coordenadas de categorías
acm$var$contrib            # Contribuciones de categorías
acm$var$cos2               # Calidad de representación de categorías

## Vemos correlación entre dimensiones y variables
acm$var$eta2               # Eta² (correlación entre variables y dimensiones)

# Mapas ----

## Representación de variables ----
fviz_mca_var(acm, 
             choice = "mca.cor",   # Coordenadas de categorías
             repel = TRUE)

## Elaborar mapa factorial de las categorías ----
fviz_mca_var(acm, repel = TRUE)

### Colorear por contribución de variable -----
fviz_mca_var(acm, 
             col.var = "contrib",
             repel = TRUE,
             title = "Mapa de perfiles de votantes en México",
             subtitle = "Análisis de correspondencias múltiples",
             legend.title = "Contribución de la variable",
             caption = "Elaboración propia con datos de LAPOP 2023") +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5),
    plot.caption = element_text(size = 11),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

# Considerar sólo las categorías que más contribuyen 
fviz_mca_var(acm, 
             select.var = list(contrib = 10),
             col.var = "contrib",
             repel = TRUE)

### Colorear por calidad de representación ----
fviz_mca_var(acm, 
             col.var = "cos2",
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             ggtheme = theme_minimal())

