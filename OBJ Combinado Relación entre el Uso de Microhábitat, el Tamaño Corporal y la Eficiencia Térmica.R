#    -------------    Cargar paquetes --------------------




library(FactoMineR)
library(factoextra)
library(openxlsx)
library(readxl)
library(ggplot2)
library(corrplot)
library(PASWR)
library(tidyverse)
library(Hmisc)
library(skimr)
library(fastDummies)
library(tidyr)
library(flextable)
library(magrittr)
library(indicspecies)
library(dplyr)
library(psych)
library(officer)
library(rvg)
library(ggcorrplot)
library(gridExtra)
library(vegan)
library(cluster)
library(reshape2)
library(ggalt) 
library(grid)
library(car)
library(MASS)


library(seminr)
library(seminrstudio)
library(htmlwidgets)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
# install.packages("SEMinR")
# install.packages("htmlwidgets")
# install.packages("DiagrammeR")
# install.packages("DiagrammeRsvg")
# install.packages("svglite")
#"rsvg"
# use 1000 bootstraps and utilize 2 parallel cores

citation("seminr")

citation("tidyr")

##-------------Cargar datos Bolitoglossa tamaense -------------
PLSDATAB <- read.xlsx("D:/Resultados B.tamaense/mi_archivo4.xlsx")

summary(PLSDATAB)
str(PLSDATAB)





library(seminr)
str(PLSDATAB)
head(PLSDATAB)




#----------  Descripción del modelo de medición   ------------
#  Especificación de modelos de medición con constructos


PLSDATAB_mm = constructs(
  reflective("E", c("E_Blouin_Bt", "E_Hertz_Bt")),
  composite("UM", c("Temp_Sustrato", "Hum_Sustrato", "Dist_Suelo", "Inclin_Sustrato", "Dist_Agua", 
                    "Cober_Dosel", "Altura_Dosel", "Prof_Hojarasca", "Dist_Ejemplares", "Nub", "Precip"), weights = mode_B),
  composite("USV", c("Tipo_Sust","Tipo_Crec"), weights = mode_B),
  composite("UTE", c("Altitud", "Temporada"), weights = mode_B),
  reflective("T", c("LT", "LHC", "LAI", "Peso")))




#  reflective("UTE", c("Altitud", "Temporada")),


#----------       Descripción del modelo estructural          ---------------
#       Especificar el modelo estructural de las relaciones entre constructos.


PLSDATAB_sm = relationships(
  paths(from = "UTE", to = c("T", "UM", "USV", "E")),
  paths(from = "USV", to = c("T", "E")),
  paths(from = "UM", to = c("T", "E","USV")),
  paths(from = "T", to = c("E")))

#  Especificación de modelos de medición con constructos


PLSDATAB_mm = constructs(
  reflective("ET", single_item("E_Blouin_Bt")),
  composite("UM", c("Temp_Sustrato", "Hum_Sustrato", "Inclin_Sustrato", "Prof_Hojarasca", "Tipo_Sust", "Temporada"), weights = mode_B),
  reflective("T", single_item("LT")))



single_item
#  reflective("UTE", c("Altitud", "Temporada")),


#----------       Descripción del modelo estructural          ---------------
#       Especificar el modelo estructural de las relaciones entre constructos.


PLSDATAB_sm = relationships(
  paths(from = "UM", to = c("T", "ET")),
  paths(from = "T", to = c("ET")))

#--------------     Estimación del modelo          ----------------------


mobi_pls2 = estimate_pls(
  data = PLSDATAB,
  measurement_model = PLSDATAB_mm,
  structural_model = PLSDATAB_sm,
  inner_weights = path_weighting
)

summary(mobi_pls2)
plot(mobi_pls2)
plot(PLSDATAB_sm)
plot(PLSDATAB_mm)
HPT <- specify_model(measurement_model = PLSDATAB_mm,
                     structural_model = PLSDATAB_sm,)
plot(HPT)

# Calcular SRMR
srmr_value <- srmr(mobi_pls2)

#
#--------------  Informe de los resultados de la estimación del modelo Reportando el modelo estimado    ----------

model_summary <- summary(mobi_pls2)


meta  #nforma los metadatos sobre la técnica de estimación y la versión
model_summary$iterations #(Sólo PLS) informa el número de iteraciones para converger en un modelo estable
model_summary$paths #informa la matriz de coeficientes de trayectoria,R2, y ajustado R2

#Crear dataframe con los resultados
#Crear dataframe con los resultados
df <- data.frame(model_summary$paths)

library(tibble)

# Convierte los nombres de fila en una columna
df <- rownames_to_column(df, var = "VL")

# Redondea todas las columnas numéricas a 3 dígitos después del punto decimal
df <- df %>% mutate(across(where(is.numeric), ~ round(., 3))) 


# Crea la tabla con flextable
library(flextable)

ft <- flextable(df)
ft
# Mostrar la tabla
ft


# Definir nombre del archivo
nombre_archivo <- "MODELO_ESTRUCTURAL.docx"

# Definir ubicación del archivo
ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"

# Ruta completa donde deseas guardar el archivo
ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)

# Crear un documento Word
doc <- read_docx()

# Añadir la tabla ft_RG5 al documento
doc <- doc %>%
  body_add_flextable(ft)

# Guardar el documento Word en la ubicación especificada
print(doc, target = ruta_archivo)


total_effects #informa los efectos totales del modelo estructural
total_indirect_effects #informa los efectos indirectos totales del modelo estructural
model_summary$loadings #informa las cargas estimadas del modelo de medición
model_summary$weights #informa los pesos estimados del modelo de medición

#Crear dataframe con los resultados
df <- data.frame(model_summary$weights)

library(tibble)

# Convierte los nombres de fila en una columna
df <- rownames_to_column(df, var = "Indicador")

# Redondea todas las columnas numéricas a 3 dígitos después del punto decimal
df <- df %>% mutate(across(where(is.numeric), ~ round(., 3)))

# Crea la tabla con flextable
library(flextable)

ft <- flextable(df)
ft
# Mostrar la tabla
ft


# Definir nombre del archivo
nombre_archivo <- "CargasyPeso_Indicadores.docx"

# Definir ubicación del archivo
ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"

# Ruta completa donde deseas guardar el archivo
ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)

# Crear un documento Word
doc <- read_docx()

# Añadir la tabla ft_RG5 al documento
doc <- doc %>%
  body_add_flextable(ft)

# Guardar el documento Word en la ubicación especificada
print(doc, target = ruta_archivo)

model_summary$validity$vif_items #informa el factor de inflación de la varianza (VIF) para el modelo de medición
model_summary$validity$htmt #informa el HTMT para las construcciones
model_summary$validity$fl_criteria #informa los criterios de Fornell Larcker para los constructos
df <- data.frame(model_summary$validity$fl_criteria)

library(tibble)

# Convierte los nombres de fila en una columna
df <- rownames_to_column(df, var = "VL")

# Redondea todas las columnas numéricas a 3 dígitos después del punto decimal
df <- df %>% mutate(across(where(is.numeric), ~ round(., 3)))


# Crea la tabla con flextable
library(flextable)

ft <- flextable(df)
ft
# Mostrar la tabla
ft


# Definir nombre del archivo
nombre_archivo <- "Validezdiscriminante.docx"

# Definir ubicación del archivo
ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"

# Ruta completa donde deseas guardar el archivo
ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)

# Crear un documento Word
doc <- read_docx()

# Añadir la tabla ft_RG5 al documento
doc <- doc %>%
  body_add_flextable(ft)

# Guardar el documento Word en la ubicación especificada
print(doc, target = ruta_archivo)




model_summary$validity$cross_loadings #(Solo PLS) informa todas las cargas posibles entre construcciones y elementos
model_summary$reliability  #informa confiabilidad compuesta (Rh​odo), alfa de Cronbach, varianza media extraída (AVE) yRh​oA
#Crear dataframe con los resultados
df <- data.frame(model_summary$reliability)

library(tibble)

# Convierte los nombres de fila en una columna
df <- rownames_to_column(df, var = "VL")

# Redondea todas las columnas numéricas a 3 dígitos después del punto decimal
df <- df %>% mutate(across(where(is.numeric), ~ round(., 3)))


# Crea la tabla con flextable
library(flextable)

ft <- flextable(df)
ft
# Mostrar la tabla
ft


# Definir nombre del archivo
nombre_archivo <- "Coeficientes_Modelomedida.docx"

# Definir ubicación del archivo
ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"

# Ruta completa donde deseas guardar el archivo
ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)

# Crear un documento Word
doc <- read_docx()

# Añadir la tabla ft_RG5 al documento
doc <- doc %>%
  body_add_flextable(ft)

# Guardar el documento Word en la ubicación especificada
print(doc, target = ruta_archivo)




model_summary$composite_scores  #informa las puntuaciones constructivas de los compuestos
model_summary$vif_antecedents #Reportar el Factor de Inflación de la Varianza (VIF) para el modelo estructural
model_summary$fSquare #informa los tamaños del efecto (F2) para el modelo estructural
model_summary$descriptives  #Informa las estadísticas descriptivas y las correlaciones tanto para los elementos como para los constructos.
model_summary$fSquare  #informa los tamaños del efecto (F2) para el modelo estructural
it_criteria #informa el AIC y el BIC para los constructos de resultados




# Supongamos que mobi_pls_plot es tu gráfico generado
class(mobi_pls_plot)



#-----------  Arranque de modelos PLS para determinar su significancia    -----------------


boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls,
                                 nboot = 1000,
                                 cores = 2)

summary(boot_mobi_pls)
anyNA(boot_mobi_pls)
any(is.infinite(boot_mobi_pls))
str(boot_mobi_pls)

boot_mobi_pls <- bootstrap_model(seminr_model = mobi_pls2,
                                 nboot = 34,
                                 cores = 2)



summary(boot_mobi_pls)
boot_model_summary <- summary(boot_mobi_pls) 


plot(boot_mobi_pls)
# bootstrap_model() # Devuelve un objeto de clase boot_seminr_modelque contiene los elementos 
#             de estimación del modelo original así como los siguientes elementos de arranque accesibles:


boot_mobi_pls$boot_paths #una matriz de las nbootmatrices de coeficientes de ruta de muestra de bootstrap estimadas
boot_mobi_pls$boot_loadings #una matriz de nbootcargas de elementos de muestra de bootstrap estimadas
boot_mobi_pls$boot_weights #una matriz de nbootpesos de elementos de muestra bootstrap estimados
boot_mobi_pls$boot_HTMT #una matriz de nbootmatrices HTMT del modelo de muestra bootstrap estimado
boot_mobi_pls$boot_total_paths #una matriz de nbootrutas totales estimadas del modelo de muestra bootstrap
boot_mobi_pls$paths_descriptives #una matriz de los coeficientes de la ruta bootstrap y las desviaciones estándar
boot_mobi_pls$loadings_descriptives #una matriz de las cargas de los elementos bootstrap y las desviaciones estándar
boot_mobi_pls$weights_descriptives #una matriz de los pesos de los elementos bootstrap y las desviaciones estándar
boot_mobi_pls$HTMT_descriptives #una matriz del modelo bootstrap HTMT y desviaciones estándar
boot_mobi_pls$total_paths_descriptives #una matriz de las rutas totales y desviaciones estándar del modelo bootstrap


#------------     Informe de resultados de un PLS bootstrap  ----------------

boot_model_summary$nboot #informa el número de arranques realizados
boot_model_summary$bootstrapped_paths #informa una matriz de rutas directas y su desviación estándar, valores t e intervalos de confianza.
boot_model_summary$bootstrapped_weights #informa una matriz de pesos del modelo de medición y su desviación estándar, valores t e intervalos de confianza.
boot_model_summary$bootstrapped_loadings #informa una matriz de cargas del modelo de medición y su desviación estándar, valores t e intervalos de confianza.
boot_model_summary$bootstrapped_HTMT #informa una matriz de valores HTMT y su desviación estándar, valores t e intervalos de confianza.
boot_model_summary$bootstrapped_total_paths #informa una matriz de rutas totales y su desviación estándar, valores t e intervalos de confianza.



meta
#--------- Informe de datos estadísticos descriptivos y construcción de estadísticas descriptivas  -----------
boot_mobi_pls <- summary(mobi_pls2)
boot_mobi_pls$descriptives$statistics$items
boot_mobi_pls$descriptives$correlations$items
boot_mobi_pls$descriptives$statistics$constructs
boot_mobi_pls$descriptives$correlations$constructs






#------------- Trazado de modelos ----------------------

plot(boot_mobi_pls)



summary(boot_mobi_pls)

m <- seminr_theme_create(
  plot.title.fontcolor = "black",
  plot.title.fontsize = 24,
  plot.fontname = "Arial",
  plot.splines = TRUE,
  plot.rounding = 3,
  plot.adj = FALSE,
  plot.specialcharacters = FALSE,
  plot.randomizedweights = TRUE,
  plot.bgcolor = "white",
  mm.node.color = "dimgray",
  mm.node.fill = "gold2",
  mm.node.label.fontsize = 12,
  mm.node.label.fontcolor = "black",
  mm.edge.positive.color = "chartreuse4",
  mm.edge.negative.color = "brown",
  mm.edge.positive.style = "solid",
  mm.edge.negative.style = "dashed",
  mm.edge.label.fontsize = 12,
  mm.edge.label.fontcolor = "black",
  mm.edge.label.show = TRUE,
  mm.edge.width_multiplier = 3,
  mm.edge.width_offset = 0.5,
  mm.edge.minlen = 1,
  mm.edge.use_outer_weights = TRUE,
  mm.edge.boot.show_t_value = FALSE,
  mm.edge.boot.show_p_value = FALSE,
  mm.edge.boot.show_p_stars = TRUE,
  mm.edge.boot.show_ci = FALSE,
  mm.edge.boot.template = "{variable} = {value}{stars}",
  sm.node.color = "black",
  sm.node.fill = "white",
  sm.node.label.fontsize = 12,
  sm.node.label.fontcolor = "black",
  sm.node.endo.template = "<B>{name} </B><BR /><FONT POINT-SIZE='10'>{rstring}</FONT>",
  sm.node.exo.template = "<B>{name} </B>",
  sm.edge.positive.color = "chartreuse4",
  sm.edge.negative.color = "brown",
  sm.edge.positive.style = "solid",
  sm.edge.negative.style = "dashed",
  sm.edge.label.fontsize = 14,
  sm.edge.label.fontcolor = "black",
  sm.edge.label.show = TRUE,
  sm.edge.label.all_betas = TRUE,
  sm.edge.boot.show_t_value = FALSE,
  sm.edge.boot.show_p_value = FALSE,
  sm.edge.boot.show_p_stars = TRUE,
  sm.edge.boot.show_ci = TRUE,
  sm.edge.boot.template = "{variable} = {value}{stars}<BR /><FONT POINT-SIZE='7'>{civalue} {tvalue} {pvalue} </FONT>",
  sm.edge.width_multiplier = 10,
  sm.edge.width_offset = 0.5,
  sm.edge.minlen = 1,
  construct.reflective.shape = "ellipse",
  construct.reflective.arrow = "backward",
  construct.reflective.use_weights = FALSE,
  construct.compositeA.shape = "hexagon",
  construct.compositeA.arrow = "backward",
  construct.compositeA.use_weights = TRUE,
  construct.compositeB.shape = "hexagon",
  construct.compositeB.arrow = "forward",
  construct.compositeB.use_weights = TRUE,
  manifest.reflective.shape = "box",
  manifest.compositeA.shape = "box",
  manifest.compositeB.shape = "box"
)





plot(mobi_pls2, theme = m)
boot_mobi_pls


plot(mobi_pls, theme = m, title = "PLS-SEM Model")
save_plot("myfigure1.pdf")


summary_corp_rep <- summary(mobi_pls)
plot(summary_corp_rep$reliability)


m <-seminr_theme_create(
  plot.title.fontcolor = "black",
  plot.title.fontsize = 24,
  plot.fontname = "Arial",
  plot.splines = TRUE,
  plot.rounding = 3,
  plot.adj = FALSE,
  plot.specialcharacters = FALSE,
  plot.randomizedweights = FALSE,
  plot.bgcolor = "white",
  mm.node.color = "dimgray",
  mm.node.fill = "gold2",
  mm.node.label.fontsize = ,
  mm.node.label.fontcolor = "dimgray",
  mm.edge.positive.color = "chartreuse4",
  mm.edge.negative.color = "brown",
  mm.edge.positive.style = "solid",
  mm.edge.negative.style = "dashed",
  mm.edge.label.fontsize = 7,
  mm.edge.label.fontcolor = "black",
  mm.edge.label.show = TRUE,
  mm.edge.width_multiplier = 3,
  mm.edge.width_offset = 0.5,
  mm.edge.minlen = 1,
  mm.edge.use_outer_weights = TRUE,
  mm.edge.boot.show_t_value = FALSE,
  mm.edge.boot.show_p_value = FALSE,
  mm.edge.boot.show_p_stars = TRUE,
  mm.edge.boot.show_ci = FALSE,
  mm.edge.boot.template = "{variable} = {value}{stars}",
  sm.node.color = "black",
  sm.node.fill = "white",
  sm.node.label.fontsize = 12,
  sm.node.label.fontcolor = "black",
  sm.node.endo.template = "<B>{name} </B><BR /><FONT POINT-SIZE='10'>{rstring}</FONT>",
  sm.node.exo.template = "<B>{name} </B>",
  sm.edge.positive.color = "chartreuse4",
  sm.edge.negative.color = "brown",
  sm.edge.positive.style = "solid",
  sm.edge.negative.style = "dashed",
  sm.edge.label.fontsize = 9,
  sm.edge.label.fontcolor = "black",
  sm.edge.label.show = TRUE,
  sm.edge.label.all_betas = TRUE,
  sm.edge.boot.show_t_value = FALSE,
  sm.edge.boot.show_p_value = FALSE,
  sm.edge.boot.show_p_stars = TRUE,
  sm.edge.boot.show_ci = TRUE,
  sm.edge.boot.template = "{variable} = {value}{stars}<BR /><FONT POINT-SIZE='7'>{civalue} {tvalue} {pvalue} </FONT>",
  sm.edge.width_multiplier = 5,
  sm.edge.width_offset = 0.5,
  sm.edge.minlen = NA,
  construct.reflective.shape = "ellipse",
  construct.reflective.arrow = "backward",
  construct.reflective.use_weights = FALSE,
  construct.compositeA.shape = "hexagon",
  construct.compositeA.arrow = "backward",
  construct.compositeA.use_weights = FALSE,
  construct.compositeB.shape = "hexagon",
  construct.compositeB.arrow = "forward",
  construct.compositeB.use_weights = TRUE,
  manifest.reflective.shape = "box",
  manifest.compositeA.shape = "box",
  manifest.compositeB.shape = "box"
)

plot(boot_mobi_pls, theme = m)



plot(boot_mobi_pls, theme = m, title = "Bootstrapped Model")
save_plot("myfigure2.pdf")



my_plot <- plot(boot_mobi_pls, theme = m)
htmlwidgets::saveWidget(my_plot, "sem_plot.html")
browseURL("sem_plot.html")
# Captura de pantalla y guardado como SVG
webshot("sem_plot.html", file = "myfigure22.pdf", selector = ".plot-container", zoom = 2)



#----------- Guardar graficos -----------

install.packages("")
if(!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github("sem-in-r/seminrstudio")


# Instalar webshot2 si no lo tienes
install.packages("webshot2")
library(webshot2)
seminrstudio
library(seminrstudio)
# Generar el gráfico interactivo
my_plot <- plot(boot_mobi_pls, theme = m)


my_plot <- plot(HPT, theme = m)
htmlwidgets::saveWidget(my_plot, "sem_plot.html", selfcontained = TRUE)
rstudioapi::viewer("sem_plot.html")

HPT <- specify_model(structural_model = PLSDATAB_sm, measurement_model = PLSDATAB_mm)
plot(HPT, theme = m)
my_plot <- plot(x)
htmlwidgets::saveWidget(my_plot, "sem_plot.html")
browseURL("sem_plot.html")
# Captura de pantalla y guardado como SVG
webshot("sem_plot.html", file = "ModeloDescripcion.pdf", selector = ".plot-container", zoom = 2)



# Instala webshot2 si no está instalado
install.packages("webshot2")
library(webshot2)

# Captura de pantalla y guardado como SVG
webshot("sem_plot.html", file = "ModeloDescripcion.pdf", selector = ".plot-container", zoom = 2)
