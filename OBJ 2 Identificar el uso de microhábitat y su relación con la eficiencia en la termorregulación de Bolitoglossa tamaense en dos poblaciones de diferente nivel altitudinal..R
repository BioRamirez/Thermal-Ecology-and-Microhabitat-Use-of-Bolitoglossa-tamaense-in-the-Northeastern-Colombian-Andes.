#----------------------------Remansos (2100) y Asiria (2600)----------------------

#-------1 Validar el FAMD_R-----------------------------


#-------2 Cargar el archivo Excel-----
#-------------Cargar datos Remansos
MicroHabitat_Medidas_R <- read.xlsx("D:/Resultados B.tamaense/MicroHabitat_Medidas_R.xlsx")

summary(MicroHabitat_Medidas_R)
library(dplyr)
str(T_Corporal_R)
str(MicroHabitat_Medidas_R)
#-------3 Agregar las tres columnas a data_existente Agregar los datos de Termorregulación E_num y E_Blou------------
MicroHabitat_Medidas_R <- MicroHabitat_Medidas_R %>%
  mutate(
    E_Blouin_RA = T_Corporal_R$E_Blouin_r,
    E_numeric_RA = T_Corporal_R$E_numeric_R,
  )

str(MicroHabitat_Medidas_R)


#------------Cargar datos Asiria----------------

MicroHabitat_Medidas_A <- read.xlsx("D:/Resultados B.tamaense/MicroHabitat_Medidas_A.xlsx")

summary(MicroHabitat_Medidas_A)
library(dplyr)
str(T_Corporal_A)
str(MicroHabitat_Medidas_A)
summary(MicroHabitat_Medidas_A)
#-------3 Agregar las tres columnas a data_existente Agregar los datos de Termorregulación E_num y E_Blou------------
MicroHabitat_Medidas_A <- MicroHabitat_Medidas_A %>%
  mutate(
    E_Blouin_RA = T_Corporal_A$E_Blouin_A,
    E_numeric_RA = T_Corporal_A$E_numeric_A,
  )

str(MicroHabitat_Medidas_A)

#------------Unir dos dataframe en MicroHabitat_Medidas_AR

# Cargar la librería dplyr
library(dplyr)

# Verificar los nombres de las columnas
names(MicroHabitat_Medidas_A)
names(MicroHabitat_Medidas_R)

# Ajustar los nombres de las columnas para que coincidan exactamente
names(MicroHabitat_Medidas_R) <- names(MicroHabitat_Medidas_A)

# Ahora los nombres de las columnas deben coincidir

# Combinación de los dataframes uno debajo del otro
MicroHabitat_Medidas_RA <- rbind(MicroHabitat_Medidas_A, MicroHabitat_Medidas_R)

# Mostrar la estructura del dataframe combinado
str(MicroHabitat_Medidas_RA)



# Eliminar filas con datos faltantes en la variable "Peso.(gr)"
MicroHabitat_Medidas_RA <- MicroHabitat_Medidas_RA[complete.cases(MicroHabitat_Medidas_RA$`Peso.(gr)`), ]

# Renombrar la columna si es necesario
#names(MicroHabitat_Medidas_RAA)[names(MicroHabitat_Medidas_RAA) == "Peso.(gr)"] <- "Peso_gr"

# Verificar si hay más datos faltantes en la variable Peso_gr
#MicroHabitat_Medidas_RAA <- MicroHabitat_Medidas_RAA[complete.cases(MicroHabitat_Medidas_RAA$Peso_gr), ]

# Seleccionar solo las columnas deseadas
str(MicroHabitat_Medidas_RA)
MicroHabitat_Medidas_RA_2 <- MicroHabitat_Medidas_RA[, c(
  "Vereda",
  "Temporada",
  "Tipo.Sustrato",
  "Tipo.Vegetacion",
  "Tipo.Crecimiento",
  "Precipitacion",
  "Nubosidad",
  "Temperatura.Corporal",
  "Temperatura.Sustrato",
  "Temperatura.1cm.Sus",
  "Humedad.1cm.Sus",
  "Humedad.Sustrato",
  "Temperatura.Ambiente",
  "Humedad.Ambiente",
  "Distancia.Suelo.(m)",
  "Inclinacion.Sustrato",
  "Velocidad.Viento",
  "Distancia.Agua.(m)",
  "Cobertura.Dosel",
  "Altura.Dosel.(m)",
  "Profundidad.Hojarasca.(m)",
  "Distancia.Ejemplares.(m)",
  "E_Blouin_RA",
  "E_numeric_RA",
  "LT"
)]

# Analisis de correlacion
# Verificar el tipo de cada variable
str(MicroHabitat_Medidas_RA_2)
#print(MicroHabitat_Medidas_RA_2$Tipo.Vegetacion)

# Encontrar valores únicos
#unique_values <- unique(MicroHabitat_Medidas_RA_2$Tipo.Vegetacion)
#print(unique_values)

# Asignar nombres de fila únicos
#rownames(MicroHabitat_Medidas_RA_2) <- make.unique(as.character(MicroHabitat_Medidas_RA_2))

# Verificar la asignación de nombres de fila únicos
print(rownames(MicroHabitat_Medidas_RA_2))
#Asigbar nombre secuencial a las filas
# Obtener el número total de filas en el data frame
total_filas <- nrow(MicroHabitat_Medidas_RA_2)

# Generar nombres de fila secuenciales sin salto
nuevos_nombres <- paste( seq_len(total_filas), sep = "")

# Asignar los nuevos nombres de fila al data frame
rownames(MicroHabitat_Medidas_RA_2) <- nuevos_nombres

# Verificar los nombres de fila actualizados
print(rownames(MicroHabitat_Medidas_RA_2))

str(MicroHabitat_Medidas_RA_2)
# Convertir la variable de character a numeric
MicroHabitat_Medidas_RA_2$`Distancia.Ejemplares.(m)` <- as.numeric(MicroHabitat_Medidas_RA_2$`Distancia.Ejemplares.(m)` )

str(MicroHabitat_Medidas_RA_2)
#-------4 Seleccionar solo las variables continuas------------------------
variables_continuas_RA <- MicroHabitat_Medidas_RA_2[, sapply(MicroHabitat_Medidas_RA_2, is.numeric)]


#-------5 Función para normalización robusta seguida de estandarización--------------------
robust_standardize <- function(x) {
  # Normalización robusta (percentiles)
  min_value <- quantile(x, probs = 0.05, na.rm = TRUE)
  max_value <- quantile(x, probs = 0.95, na.rm = TRUE)
  normalized <- (x - min_value) / (max_value - min_value)
  
  # Estandarización (media 0 y desviación estándar 1)
  mean_value <- mean(normalized, na.rm = TRUE)
  sd_value <- sd(normalized, na.rm = TRUE)
  standardized <- (normalized - mean_value) / sd_value
  
  return(standardized)
}

# Ejemplo de aplicación a variables_continuas_R
variables_continuas_RA_norm <- as.data.frame(lapply(variables_continuas_RA, robust_standardize))

# Verificar los primeros registros de las variables normalizadas
head(variables_continuas_RA_norm)


#-------6 Graficar boxplot de variables_continuas_R--------------------------
GRA1<- boxplot(variables_continuas_RA, las = 2, main = "Variables Continuas RA", cex.main = 0.9)

# Cargar la librería ggplot2
library(ggplot2)
library(tidyr)

# Transformar los datos de wide a long format
variables_continuas_long_RA <- variables_continuas_RA %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Crear el boxplot usando ggplot2
GRA1 <- ggplot(variables_continuas_long_RA, aes(x = Variable, y = Valor)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variables Continuas RA") +
  theme(plot.title = element_text(size = 9))

# Mostrar el gráfico
print(GRA1)

#G1
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA1))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA1.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_type(type = "body")) %>%
    base::print(target = output_path)
}

# Graficar boxplot de variables_continuas_R_norm
GRA2<- boxplot(variables_continuas_RA_norm, las = 2, main = "Variables Continuas RA Normalizadas", cex.main = 0.9)
# Transformar los datos de wide a long format
variables_continuas_norm_long_RA <- variables_continuas_RA_norm %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Crear el boxplot usando ggplot2
GRA2 <- ggplot(variables_continuas_norm_long_RA, aes(x = Variable, y = Valor)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variables Continuas RA Normalizadas") +
  theme(plot.title = element_text(size = 9))

# Mostrar el gráfico
print(GRA2)


#GRA2
{
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA2))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA2.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_type(type = "body")) %>%
    base::print(target = output_path)
}

# Restablecer el diseño gráfico a su estado original
library(tidyr)
library(gridExtra)
# Configurar el diseño del gráfico y mostrar ambos gráficos en una fila y dos columnas
grid.arrange(GRA1, GRA2, ncol = 2)

#G1-G2
{
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = grid.arrange(GRA1, GRA2, ncol = 2))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA1_2.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_type(type = "body")) %>%
    base::print(target = output_path)
}
# Calcular la matriz de correlación
matriz_correlacion_RA <- cor(variables_continuas_RA_norm)

#-------7 Crear el gráfico de correlación con corrplot--------------------------


## Instalar el paquete ggcorrplot si no está instalado
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}

# Cargar las librerías necesarias
library(ggcorrplot)

# Crear el gráfico de correlación usando ggcorrplot
GRA3 <- ggcorrplot(matriz_correlacion_R, method = "square", 
                   lab = TRUE, lab_col = "black", lab_size = 3, 
                   colors = c("blue", "white", "red"), 
                   title = "Matriz de Correlación R", 
                   legend.title = "Correlación")

# Mostrar el gráfico
print(GRA3)

#GRA3
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA3))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA3.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


# Verificar las columnas actuales del dataframe
print(colnames(variables_continuas_RA_norm))

# Eliminar la columna "velocidad del viento"
variables_continuas_RA_norm <- subset(variables_continuas_RA_norm, select = -c(Velocidad.Viento))

# Verificar nuevamente las columnas después de la eliminación
print(colnames(variables_continuas_RA_norm))

library(psych)

#-------8 Calcular la prueba KMO--------------------

#kmo_result_R <- KMO(matriz_correlacion_R)
#print(kmo_result_R)

#-------9 Calcular la prueba de Bartlett-----------------------

bartlett_result_RA <- cortest.bartlett(cor(variables_continuas_RA_norm), n = nrow(variables_continuas_RA_norm))
print(bartlett_result_RA)
#Verificar Normalidad de Variables Cuantitativas
# Cargar las librerías necesarias
library(dplyr)
library(tidyr)

#-------10 Crear una función test Normal prueba de Shapiro-Wilk-----------------------
test_normality_RA <- function(data) {
  data %>%
    select_if(is.numeric) %>%
    summarise_all(~ shapiro.test(.)$p.value) %>%
    gather(variable, p_value) %>%
    mutate(normal = ifelse(p_value >= 0.05, "Normal", "No Normal"))
}

# Aplicar la función a tu dataframe
resultados_RA2 <- test_normality_RA(variables_continuas_RA_norm)

# Ver los resultados
print(resultados_RA2)

#-------11 Función para identificar outliers--------------------------
identify_outliers_RA <- function(variables_continuas_RA_norm) {
  outliers <- lapply(variables_continuas_R_norm, function(x) {
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outliers <- x[x < lower_bound | x > upper_bound]
    return(outliers)
  })
  return(outliers)
}

# Ejemplo de uso
# Supongamos que 'df' es tu dataframe
outliers_df_RA <- identify_outliers_RA(variables_continuas_RA_norm)

print(outliers_df_RA)

# Asegurarse de que todas las variables sean numéricas para realizar Vif y revisar colinealidad perfecto
variables_continuas_RA_norm_numeric <- as.data.frame(sapply(variables_continuas_RA_norm, as.numeric))

# Instalar y cargar el paquete 'car' si no está instalado
if (!require(car)) {
  install.packages("car")
  library(car)
}

#-------12 Crear un modelo de regresión múltiple------------------------
lm_model_RA <- lm(data = variables_continuas_RA_norm_numeric)

#-------13 Identificar las variables con colinealidad perfecta-----------------------
aliased_vars_RA <- alias(lm_model_RA)$Complete

# Imprimir las variables aliased
print("Variables aliased:")
print(aliased_vars_RA)

# Eliminar las variables aliased
variables_continuas_RA_norm_numeric <- subset(variables_continuas_RA_norm_numeric, select = -c(E_numeric_RA ))
# Ajustar un nuevo modelo de regresión múltiple
lm_model_clean_RA <- lm(data = variables_continuas_RA_norm_numeric)
variables_continuas_RA_norm_numeric
#-------14 Calcular los VIFs-----------------------
vif_values_RA <- vif(lm_model_clean_RA)

# Mostrar los VIFs
print("Valores VIF:")
print(vif_values_RA)

#-------15 Eliminar las variables con alta colinealidad-----------------------------
variables_continuas_RA_norm_numeric_clean <- subset(variables_continuas_RA_norm_numeric, select = -c(Temperatura.1cm.Sus, Humedad.1cm.Sus, Temperatura.Ambiente, Humedad.Ambiente ))

str(variables_continuas_RA_norm_numeric_clean)

# Ajustar un nuevo modelo de regresión múltiple
lm_model_clean_RA <- lm(data = variables_continuas_RA_norm_numeric_clean)
variables_continuas_RA_norm_numeric_clean
#-------14 Calcular los VIFs-----------------------
vif_values_RA <- vif(lm_model_clean_RA)

# Mostrar los VIFs
print("Valores VIF:")
print(vif_values_RA)


# Calcular la matriz de correlación
matriz_correlacion_RA_Clean <- cor(variables_continuas_RA_norm_numeric_clean)

#-------16 Crear el gráfico de correlación con corrplot_2----------------------
GRA4 <- ggcorrplot(matriz_correlacion_RA_Clean, method = "square", 
                   lab = TRUE, lab_col = "black", lab_size = 3, 
                   colors = c("blue", "white", "red"), 
                   title = "Matriz de Correlación R Clean", 
                   legend.title = "Correlación",
                   tl.cex = 10, tl.srt = 45, 
                   digits = 2)

print(GRA4)

#GRA4
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA4))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA4.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}



#-------17 Agregar la variable Vereda  para crear grafico boxplot por temporada------------------

variables_continuas_RA_norm_numeric_clean$Vereda <- MicroHabitat_Medidas_RA_2$Vereda

str(variables_continuas_RA_norm_numeric_clean)

#-------18 Realizar grafico boxplot por temporada con las variables numéricas-------------------------

library(tidyr)

# Aplicar gather para reorganizar los datos
datos2_RA <- gather(variables_continuas_RA_norm_numeric_clean,
                    key = "Variables",     # Nombre para la nueva columna de variables numéricas
                    value = "Valor_escalado",# Nombre para la nueva columna de valores
                    -Vereda)             # Columna que no queremos agrupar (en este caso 'Vereda')

# Mostrar el resultado
print(datos2_RA)
# Mostrar la estructura del nuevo data frame
str(datos2_RA)
Summary
library(ggplot2)

datos2_RA$Vereda
# Definir los colores deseados para cada temporada
colores_Vereda <- c("Asiria " = "#EF6548",  # Color naranja para Verano
                    "Remansos" = "#4EB3D3" # Color azul para Invierno
                    # Agrega más colores si tienes más categorías de temporada
)

# Graficar el boxplot con nombres de variables en diagonal y colores personalizados para Temporada
GRA5 <- ggplot(datos2_RA, aes(Variables, Valor_escalado)) +
  geom_boxplot(aes(fill = Vereda)) +
  geom_vline(xintercept = seq(0.5, length(unique(datos2_RA$Variables)) + 0.5, 1), linetype = "dashed") +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = colores_Vereda) +  # Especificar los colores personalizados
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

GRA5
# Mostrar el gráfico
print(GRA5)

#GRA5
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA5))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA5.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

#-------19 Revisar la influencia de las variables en cada temporada library(indicspecies)-------------------------------
#Ahora solo necesitamos las abundancias que son de la tercera columna en adelante. Pasamos de un dataframe a una matriz de datos:
#Hacemos la matriz solo con datos numericos
variables_continuas_RA_norm_numeric_clean1 <- subset(variables_continuas_RA_norm_numeric_clean, select = -c(Vereda))
matriz_RA <- variables_continuas_RA_norm_numeric_clean1[,3:ncol(variables_continuas_RA_norm_numeric_clean1)]
matriz_RA <- as.matrix(matriz_RA)
matriz_RA
variables_continuas_RA_norm_numeric_clean
library(indicspecies)
tramo_RA <-variables_continuas_RA_norm_numeric_clean$Vereda#vector con el tramo
tramo_RA
#analizamos las especies:
speciesdiferencias_RA <-multipatt(matriz_RA, tramo_RA, func="r.g", control = how(nperm=9999))
speciesdiferencias_RA
summary(speciesdiferencias_RA)

#Tabla RG5
{
  library(flextable)
  library(magrittr)
  
  # Crear un data frame con los resultados de summary(speciesdiferencias_R)
  resultados_summary_speciesdiferencias_RA <- data.frame(
    Grupo = c(rep("Asiria", 5), rep("Remansos", 2)),
    Variable = c("Profundidad.Hojarasca..m.", "LT", "Inclinacion.Sustrato", 
                 "E_numeric_RA", "Distancia.Ejemplares..m.", "Humedad.Sustrato", 
                 "E_Blouin_RA", "Distancia.Suelo..m.", "Altura.Dosel..m."),
    No_Variables = c(rep(5, 5), rep(2, 2)),
    stat = c(0.491, 0.461, 0.380, 0.352, 0.297, 0.183, 0.148, 0.329, 0.183),
    p.valor = c(0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0093, 0.0352, 0.0001, 0.009)
  )
  
  
  # Crear la tabla con flextable
  ft_RAG5 <- flextable(resultados_summary_speciesdiferencias_RA) 
  
  # Mostrar la tabla
  ft_RAG5
  
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_multinivel_RAG5.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(ft_RAG5)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}

# Verificar la estructura de la matriz
str(matriz_RA)


#-------20 Seleccionar solo las variables categoricas---------------------------------------
variables_categoricas_RA <- MicroHabitat_Medidas_RA_2[, sapply(MicroHabitat_Medidas_RA_2, is.character)]
print(variables_categoricas_RA)

str(variables_categoricas_RA)
summary(variables_categoricas_RA)
# Carga las bibliotecas necesarias
library(tidyr)
library(ggplot2)

#-------21 Crear grafico con frecuencias de las variables categoricas----------------------
# Pivotamos el data frame con gather
variables_categoricas_RA_gathered <- gather(variables_categoricas_RA, key = "variable", value = "categoria", -c(Vereda, Temporada, Tipo.Sustrato, Tipo.Vegetacion, Tipo.Crecimiento, Precipitacion, Nubosidad))

# Load the dplyr package
library(dplyr)

# Pivot the data frame with gather
variables_categoricas_RA_gathered <- gather(variables_categoricas_RA, key = "variable", value = "categoria", Vereda, Temporada, Tipo.Sustrato, Tipo.Vegetacion, Tipo.Crecimiento, Precipitacion, Nubosidad)
head(variables_categoricas_RA_gathered)
str(variables_categoricas_RA_gathered)
# Count the frequency of each category
variables_categoricas_RA_counted <- variables_categoricas_RA_gathered %>% 
  group_by(variable, categoria) %>% 
  tally()

# ------22 Crear grafico  bar chart-----------------------------
GRA6 <- ggplot(variables_categoricas_RA_counted, aes(x = categoria, y = n)) +
  geom_bar(stat = "identity", fill = "gray60") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_bw() +
  labs(x = "Categorías", y = "Frecuencia") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
GRA6

#GRA6
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA6))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA6.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
  
}

# ------23 Crear tablas de contingencia para cada variable-------------------------
tablas_contingencia_RA <- list()
nombres_variables_RA <- colnames(variables_categoricas_RA)

for (i in 1:(length(nombres_variables_RA) - 1)) {
  for (j in (i + 1):length(nombres_variables_RA)) {
    tabla <- table(variables_categoricas_RA[, nombres_variables_RA[i]], 
                   variables_categoricas_RA[, nombres_variables_RA[j]])
    nombres <- paste(nombres_variables_RA[i], nombres_variables_RA[j], sep = "_vs_")
    tablas_contingencia_RA[[nombres]] <- tabla
  }
}

# Crear una tabla de contingencia para cada variable
print(tablas_contingencia_RA[[1]])

# Paso 2: Evaluar colinealidad
if (!require(vcd)) install.packages("vcd")
library(vcd)

v_cramer_resultados_RA <- list()

for (nombre in names(tablas_contingencia_RA)) {
  tabla <- tablas_contingencia_RA[[nombre]]
  v_cramer <- assocstats(tabla)$cramer
  v_cramer_resultados_RA[[nombre]] <- v_cramer
}

#-------24 Ver el coeficiente V de Cramer para cada par de variables---------------------
print(v_cramer_resultados_RA)

#Evitar el uso de la variable Vereda y Tipo.Vegetacion

library(ggplot2)
library(dplyr)
library(tidyr)

# Transformar los datos al formato largo
datos_largos_RA <- variables_categoricas_RA %>%
  pivot_longer(cols = c(Tipo.Sustrato, Tipo.Crecimiento, Precipitacion, Nubosidad),
               names_to = "Variable",
               values_to = "Categoria")
print(datos_largos_RA)
# Contar la frecuencia de cada combinación de categoría y temporada
datos_contados_RA <- datos_largos_RA %>%
  count(Vereda, Variable, Categoria) %>%
  rename(Abundancia = n)  # Renombrar la columna de frecuencia a Abundancia
print(datos_contados_RA)
# Reordenar los niveles de la variable Vereda
datos_contados_RA$Vereda <- factor(datos_contados_RA$Vereda, levels = c("Asiria ", "Remansos"))

#-------25 Crear el gráfico de barras apiladas---------------------------------------

GRA7 <- ggplot(datos_contados_RA, aes(x = Categoria, y = Abundancia, fill = Vereda)) + 
  geom_col(position = "stack", width = 0.7) +  # Ajustar el ancho de las barras
  facet_grid(~ Variable, scales = "free_x", space = "free_x") +  # Agrupar por variable
  labs(title = "Abundancia por categoría y temporada",
       x = "Categoría",
       y = "Abundancia") + 
  theme_minimal() +  # Tema minimalista
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10)) +  # Ajustar ángulo y tamaño de etiquetas en eje x
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +  # Personalizar la leyenda
  scale_fill_manual(values = c("Remansos" = "#4EB3D3", "Asiria " = "#EF6548"))  # Asignar colores a cada nivel de Temporada
GRA7

#GRA7
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA7))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA7.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

#"Lluvia" = "#4EB3D3", "Seco" = "#EF6548"

#-------26 Convertir las categorias en variables dymmy en cada temporada library(indicspecies)-----------------
#install.packages("indicspecies")
library(indicspecies)
str(variables_categoricas_RA)
head(variables_categoricas_RA)
summary(variables_categoricas_RA)
print(variables_categoricas_RA)


install.packages("fastDummies")
library(fastDummies)

variables_categoricas_RA_dummy <- dummy_cols(variables_categoricas_RA, 
                                             select_columns = c("Vereda", "Temporada", "Tipo.Sustrato", 
                                                                "Tipo.Crecimiento", 
                                                                "Precipitacion", "Nubosidad"))

print(variables_categoricas_RA_dummy)
str(variables_categoricas_RA_dummy)

variables_categoricas_RA_dummy <- subset(variables_categoricas_RA_dummy, select = -c(Vereda, Temporada, Tipo.Sustrato, 
                                                                                     Tipo.Vegetacion, Tipo.Crecimiento, 
                                                                                     Precipitacion, Nubosidad))

str(variables_categoricas_RA_dummy)



#-------27 Revisar la influencia de las variables en cada temporada library(indicspecies)------------------------
#Ahora solo necesitamos las abundancias que son de la tercera columna en adelante. Pasamos de un dataframe a una matriz de datos:
#Hacemos la matriz solo con datos numericos
#variables_categoricas_RA_dummy1 <- subset(variables_categoricas_RA_dummy, select = -c(Vereda_Remansos))
matrizc_RA <- variables_categoricas_RA_dummy[,3:ncol(variables_categoricas_RA_dummy)]
matrizc_RA <- as.matrix(matrizc_RA)
matrizc_RA
variables_categoricas_RA_dummy
library(indicspecies)
tramoc_RA <-variables_categoricas_RA$Vereda#vector con el tramo
tramoc_RA
#analizamos las especies:
speciesdiferenciasc_RA <-multipatt(matrizc_RA, tramoc_RA, func="r.g", control = how(nperm=9999))
speciesdiferenciasc_RA
GRA7
summary(speciesdiferenciasc_RA)

#Tabla RAG7
{
  library(flextable)
  library(officer)
  
  # Definir los datos directamente desde el summary(speciesdiferenciasc)
  resultados_summary_speciesdiferenciasc_RA <- data.frame ( 
    Grupo = c("Asiria", "Asiria", "Remansos", "Remansos", "Remansos"),
    Variable = c("Tipo.Sustrato_Tallo", "Precipitacion_NoP", 
                 "Tipo.Crecimiento_Helecho epifito", "Tipo.Sustrato_Hoja", "Precipitacion_SiP"),
    No_Variables = c(2, 2, 3, 3, 3),
    stat = c(0.235, 0.175, 0.354, 0.297, 0.175),
    p.valor = c(0.0051, 0.0496, 0.0001, 0.0007, 0.0496)
    
  )
  
  # Crear la tabla con flextable
  ft_speciesdiferenciasc_RA <- flextable(resultados_summary_speciesdiferenciasc_RA) 
  
  # Mostrar la tabla en la consola
  ft_speciesdiferenciasc_RA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_multinivel_RAG7.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(ft_speciesdiferenciasc_RA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}


#-------28 Unir las variables categóricas con las variables numéricas en un solo dataframe---------------------

str(variables_continuas_RA_norm_numeric_clean)

str(variables_categoricas_RA)

variables_categoricas_RAFAMS <- subset(variables_categoricas_RA, select = -c(Vereda, Tipo.Vegetacion))

str(variables_categoricas_RAFAMS)


MicroHabitat_Medidas_RA_2_FAMD <- cbind(variables_categoricas_RAFAMS, variables_continuas_RA_norm_numeric_clean)
str(MicroHabitat_Medidas_RA_2_FAMD)

#-------29 Convertir las columnas de tipo character a factor----------------
MicroHabitat_Medidas_RA_2_FAMD <- as.data.frame(lapply(MicroHabitat_Medidas_RA_2_FAMD, function(x) {
  if (is.character(x)) {
    return(as.factor(x))
  } else {
    return(x)
  }
}))

# Verificar las clases de las columnas después de la conversión
str(MicroHabitat_Medidas_RA_2_FAMD)

#
#-------30 Realizar un FAMD_R--------------------------------

# Realizar un FAMD
famd_resultado_RA <- FAMD(MicroHabitat_Medidas_RA_2_FAMD, graph = FALSE)
eig.val_RA <- get_eigenvalue(famd_resultado_RA)
print(head(eig.val_RA))
famd_resultado_RA
Dimensiones_Famd_RA <- data.frame(eig.val_RA)
Dimensiones_Famd_RA

{
  library(flextable)
  library(officer)
  
  # Crear un dataframe con los datos proporcionados
  eig.val_Dimensiones_RA <- data.frame(
    Dimension = c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5"),
    eigenvalue = c(4.181901, 2.945740, 2.275408, 2.030722, 1.908660),
    variance.percent = c(12.672427, 8.926484, 6.895175, 6.153703, 5.783817),
    cumulative.variance.percent = c(12.67243, 21.59891, 28.49409, 34.64779, 40.43161)
  )
  
  # Mostrar el dataframe
  print(eig.val_Dimensiones_RA)
  
  # Crear la tabla con flextable
  eig.val_Dimensiones_RA_t <- flextable(eig.val_Dimensiones_RA) 
  
  # Mostrar la tabla en la consola
  eig.val_Dimensiones_RA_t
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_Dimensiones_RAG8.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(eig.val_Dimensiones_RA_t)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}


#-------31 Graficar el scree plot_R----------------------
GRA8 <- fviz_screeplot(famd_resultado_R)
print(GRA8)
GRA8

#GRA8
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA8))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA8.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

# Obtener y graficar las contribuciones de las variables a los dos primeros ejes
var_RA <- get_famd_var(famd_resultado_RA)
GRA9 <- fviz_contrib(famd_resultado_RA, "var", axes = 1)
GRA9

#GRA9
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA9))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA9.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

GRA10 <- fviz_contrib(famd_resultado_RA, "var", axes = 2)
GRA10

#G10
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA10))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA10.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


#-------32 Graficar dimensiones_R-----------------
library(gridExtra)

# Obtener las contribuciones de las variables para las dos primeras dimensiones
contrib_dim1_RA <- get_famd_var(famd_resultado_RA)$contrib[, 1]
contrib_dim2_RA <- get_famd_var(famd_resultado_RA)$contrib[, 2]

# Crear los dos gráficos por separado
plot_dim1_RA <- fviz_contrib(famd_resultado_RA, "var", axes = 1)
plot_dim2_RA <- fviz_contrib(famd_resultado_RA, "var", axes = 2)

# Combinar los gráficos en una sola imagen
grid.arrange(plot_dim1_RA, plot_dim2_RA, nrow = 2)

#GRA11
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code =grid.arrange(plot_dim1_RA, plot_dim2_RA, nrow = 2))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA11.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}



# Análisis cualitativo y cuantitativo de las FAMD
# Obtener la descripción de las dimensiones
dim_desc_RA <- dimdesc(famd_resultado_RA, axes = c(1, 2))

# Imprimir la descripción de las dimensiones
print(dim_desc_RA)
# Variables cualitativas
print("Variables cualitativas - Dim 1")
print(dim_desc_RA$Dim.1$quali)

print("Variables cualitativas - Dim 2")
print(dim_desc_RA$Dim.2$quali)

# Variables cuantitativas
print("Variables cuantitativas - Dim 1")
print(dim_desc_RA$Dim.1$quanti)

print("Variables cuantitativas - Dim 2")
print(dim_desc_RA$Dim.2$quanti)

#-------33 Graficar contribucion_R----------------------------------

str(MicroHabitat_Medidas_RA_2_FAMD)

# Crear el biplot conjunto de variables cualitativas y cuantitativas
GRA12 <- fviz_famd_var(famd_resultado_RA, 
                       col.var = "contrib", 
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                       repel = TRUE)

# Mostrar el biplot
print(GRA12)

#G12
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA12))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA12.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
  
}

GRA13 <- fviz_famd_var(famd_resultado_RA, "quanti.var", col.var = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)


GRA13
# Cargar los paquetes necesarios
library(officer)
library(rvg)
#G13
{
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA13))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA13.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
  
}

# Combinar las visualizaciones en una sola figura
grid.arrange(GRA12, GRA13, ncol = 2)

# Crear el biplot solo para la variable cualitativa seleccionada
selected_quali_vars_RA <- c("HojarascaC", "Hierbas terrestres", "Hemiepifitas leñosas", 
                            "Trepadoras", "Palma arbustiva cespitosas", "Palma arboreas monoestipitadas","Musgo pleurocárpico",
                            "Helecho epifito","Enredaderas","Arbustos", "Arboles", "Hoja", "Hojarasca ", "Tallo", "Musgo", "Tronco")

GRA14 <- fviz_famd_var(famd_resultado_RA, 
                       choice = "quali.var",
                       select.var = list(name = selected_quali_vars_RA),
                       col.var = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                       repel = TRUE)



# Mostrar el biplot
print(GRA14)

#G14
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA14))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA14.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
  
}

# Combinar las visualizaciones en una sola figura
grid.arrange(GRA12, GRA14, ncol = 2)

#G12_14
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = grid.arrange(GRA12, GRA14, ncol = 2))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA12_14.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


#-------34 Graficar las variables cualitativas y cuantitativas-----------------------
fviz_famd_var(famd_resultado_RA, "quali.var", axes = 1:2, geom = "point")
fviz_famd_var(famd_resultado_RA, "quanti.var", axes = 1:2, geom = "point")
GRA15 <- fviz_famd_var(famd_resultado_RA, "quanti.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
GRA15

#GRA15
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA15))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA15.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

GRA16 <- fviz_famd_var(famd_resultado_RA, "quali.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
GRA16

#GRA16
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA16))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA16.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


str(MicroHabitat_Medidas_RA_2)

#-------35 Realizar una matriz de disimilitud de gower----------------------------------
library(vegan)
library(cluster)
str(MicroHabitat_Medidas_RAA_2_FAMD)
variables_categoricas_RA
variables_categoricas_RAANOSIM <- subset(variables_categoricas_RA, select = -c(Vereda))

str(variables_categoricas_RAANOSIM)

variables_continuas_RA_norm_numeric_clean
MicroHabitat_Medidas_RA_2_RAANOSIM <- cbind(variables_categoricas_RAANOSIM, variables_continuas_RA_norm_numeric_clean)
str(MicroHabitat_Medidas_RA_2_RAANOSIM)

#-------36 Convertir las columnas de tipo character a factor------------------
MicroHabitat_Medidas_RA_2_RAANOSIM <- as.data.frame(lapply(MicroHabitat_Medidas_RA_2_RAANOSIM, function(x) {
  if (is.character(x)) {
    return(as.factor(x))
  } else {
    return(x)
  }
}))

# ------37 Calcular la matriz de distancias usando la medida de disimilitud de Gower----------------------
dist_matrix_gower_RA <- daisy(MicroHabitat_Medidas_RA_2_RAANOSIM, metric = "gower")
print(dist_matrix_gower_RA)
#-------38 Crear un mapa de calor de la matriz de distancias-----------------------
GRA17.1 <- heatmap(as.matrix(dist_matrix_gower_RA))
GRA17.1


# Instalar los paquetes necesarios si no están instalados
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")
}

# Cargar las librerías necesarias
library(ggplot2)
library(reshape2)


# Convertir la matriz de distancia a un data frame largo
dist_matrix_long_RA <- melt(as.matrix(dist_matrix_gower_RA))

# Crear el heatmap usando ggplot2
GRA17 <- ggplot(dist_matrix_long_RA, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap de la Matriz de Distancia Gower",
       x = "Variable 1",
       y = "Variable 2",
       fill = "Distancia")

# Mostrar el gráfico
print(GRA17)

#GRA17
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA17))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA17.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}




#-------39 ANOVA PLOT Verificar la homogeneidad de las dispersiones--------------------------
dispersion_gower_RA2 <- betadisper(dist_matrix_gower_RA, MicroHabitat_Medidas_RA_2_RAANOSIM$Vereda)
anova(dispersion_gower_RA2)
GRA18 <- plot(dispersion_gower_RA2)
GRA18

#GRA18


# Cargar las librerías necesarias
library(ggplot2)
library(officer)
library(rvg)
library(vegan)
library(ggalt)  # Para geom_encircle
library(grid)
library(dplyr)

# Realizar la dispersión con betadisper
dispersion_gower_RA2 <- betadisper(dist_matrix_gower_RA, MicroHabitat_Medidas_RA_2_RAANOSIM$Vereda)

# Realizar ANOVA para la dispersión
anova(dispersion_gower_RA2)

# Extraer datos de betadisper para ggplot2
df_dispersion_RA <- as.data.frame(dispersion_gower_RA2$vectors)
df_dispersion_RA$Group <- factor(dispersion_gower_RA2$group)

# Calcular los centroides
centroids_RA <- aggregate(df_dispersion_RA[,1:2], list(Group = df_dispersion_RA$Group), mean)
names(centroids_RA)[2:3] <- c("PCoA1", "PCoA2")

# Crear el gráfico ggplot2 con colores personalizados, formas y elipses
GRA18 <- ggplot(df_dispersion_RA, aes(x = PCoA1, y = PCoA2, color = Group, shape = Group)) +
  geom_point(size = 3) +
  geom_point(data = centroids_RA, aes(x = PCoA1, y = PCoA2, color = Group, shape = Group), size = 5) +
  geom_encircle(aes(x = PCoA1, y = PCoA2, group = Group, color = Group), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Remansos" =  "#4EB3D3", "Asiria " = "#EF6548")) +
  scale_shape_manual(values = c("Remansos" = 17, "Asiria "  = 19)) +
  theme_minimal(base_family = "sans") +  # Utiliza una fuente disponible en Windows
  theme(
    
    panel.background = element_rect(fill = "grey92", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  ) +
  labs(title = "Beta Dispersion Analysis RA",
       x = "PCoA Axis 1",
       y = "PCoA Axis 2")

# Mostrar el gráfico G18
print(GRA18)

{  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2"  # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA18.pptx"  # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint
  doc <- read_pptx()
  
  # Crear un objeto DML a partir del gráfico ggplot2
  dml_g18 <- rvg::dml(ggobj = GRA18)
  
  # Añadir una diapositiva con el gráfico DML
  doc <- doc %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(dml_g18, location = ph_location_fullsize())
  
  # Guardar el documento PowerPoint
  print(doc, target = output_path)
  
  
}

# Load the vegan package
library(vegan)

#-------40 Realizar ANOSIM analysis-----------------------------
anosim_result_RA <- anosim(dist_matrix_gower_RA, MicroHabitat_Medidas_RA_2_RAANOSIM$Vereda, permutations = 999)

# Print the ANOSIM result
print(anosim_result_RA)
GRA19 <- plot(anosim_result_RA)
GRA19

#G19

{# Cargar las librerías necesarias
  library(ggplot2)
  library(officer)
  library(rvg)
  library(vegan)
  
  # Realizar el análisis ANOSIM
  anosim_result_RA <- anosim(dist_matrix_gower_RA, MicroHabitat_Medidas_RA_2_RAANOSIM$Vereda, permutations = 999)
  
  # Imprimir el resultado de ANOSIM
  print(anosim_result_RA)
  
  # Extraer los datos necesarios para ggplot2
  anosim_data_RA <- data.frame(
    Dissimilarity = anosim_result_RA$dis.rank,
    Group = anosim_result_RA$class.vec
  )
  
  
  
  # Crear el gráfico ggplot2 con colores personalizados y cuadrícula blanca
  GRA19 <- ggplot(anosim_data_RA, aes(x = Group, y = Dissimilarity, fill = Group)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#8BCCC4", "#4EB3D3", "#EF6548")) +  # Cambia los colores según tus necesidades
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "grey92", color = NA),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white")
    ) +
    labs(title = "ANOSIM Dissimilarity Ranks R = 0.6158, P = 0.001",
         x = "Group",
         y = "Dissimilarity Rank")
  
  
  GRA19
  
  #"Lluvia" = "#4EB3D3", "Seco" = "#EF6548"
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2"  # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA19.pptx"  # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint
  doc <- read_pptx()
  
  # Crear un objeto DML a partir del gráfico ggplot2
  dml_g19 <- rvg::dml(ggobj = GRA19)
  
  # Añadir una diapositiva con el gráfico DML
  doc <- doc %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(dml_g19, location = ph_location_fullsize())
  
  # Guardar el documento PowerPoint
  print(doc, target = output_path)
  
}


#-------41 Realizar PERMANOVA incluyendo la covariable si está disponible en el dataframe------------------
if("Covariate" %in% colnames(MicroHabitat_Medidas_RA_2_RAANOSIM)) {
  adonis_result_gower_RA <- adonis2(dist_matrix_gower_RA ~ LT + Vereda + Temporada + E_Blouin_RA, data = MicroHabitat_Medidas_RA_2_RAANOSIM)
} else 
  
{
  adonis_result_gower_RA <- adonis2(dist_matrix_gower_RA ~ Temporada, data = MicroHabitat_Medidas_RAA_2_RAANOSIM)
}
summary(adonis_result_gower_RA)
print(adonis_result_gower_RA)
GRA20 <- plot(adonis_result_gower_RA)
GRA20


#Tabla G20
{ 
  adonis_result_RA <- data.frame(
    Terms = c("LT", "Vereda", "Temporada", "E_Blouin_RA", "Residual"),
    Df = c(1, 1, 1, 1, 141),
    SumOfSqs = c(1.1249, 1.8064, 1.9157, 0.4423, 4.6700),
    R2 = c(0.11295, 0.18138, 0.19235, 0.04441, 0.46891),
    F = c(33.965, 54.541, 57.840, 13.354, NA),
    Pr_F = c(0.001, 0.001, 0.001, 0.001, NA),
    Significancia = c("***", "***", "***", "***", "")
  )
  
  print(adonis_result_RA)
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_Permanova_RA <- flextable(adonis_result_RA) 
  
  # Mostrar la tabla en la consola
  Tabla_Permanova_RA
  
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_permanova_RAG20.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_Permanova_RA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}

#-------42 Relaizar un NMDS Escalamiento multidimensional no métrico para representar--------------
#  - gráficamente una serie de puntos de muestreo en función de la similitud o disimilitud

nmds_RA <- metaMDS(dist_matrix_gower_RA, k = 2)  # Run NMDS with 2 dimensions

nmds_RA
G21 <- plot(nmds_RA)

coordenadas_RA <- as.data.frame(scores(nmds_RA))
coordenadas_RA


#-------43 Le añadimos a las coordenadas una columna con los tramos:--------------------
coordenadas_RA$Vereda = MicroHabitat_Medidas_RA_2_RAANOSIM$Vereda
coordenadas_RA$E_Blouin_RA = MicroHabitat_Medidas_RA_2_RAANOSIM$E_Blouin_RA
head(coordenadas_RA)
str(coordenadas_RA)


library(ggplot2)

# Assuming 'coordenadas' is your data frame containing NMDS1, NMDS2, and Temporada
# and 'transformed_data' is your data frame containing E_Blouin_R

# Format E_Blouin_R to 3 decimal places
MicroHabitat_Medidas_RA_2_RAANOSIM$E_Blouin_RA <- formatC(MicroHabitat_Medidas_RA_2_RAANOSIM$E_Blouin_RA, format = "f", digits = 3)

# ------44 Crear el gráfico NMDS con colores personalizados para los puntos---------------------
library(ggplot2)

# Definiendo los colores y formas deseadas
colores_Vereda <- c("Remansos" = "#4EB3D3", "Asiria " = "#EF6548")
formas_Vereda <- c("Remansos" = 17, "Asiria " = 16)  # 17 es triángulo, 16 es punto

GRA22 <- ggplot(coordenadas_RA, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 4, aes(shape = Vereda , colour = Vereda )) +  # Añadiendo puntos con forma y color basados en 'Temporada'
  geom_text(aes(label = MicroHabitat_Medidas_RA_2_RAANOSIM$E_Blouin_RA), hjust = 0.5, vjust = 1.5) +  # Añadiendo etiquetas de texto
  scale_shape_manual(values = formas_Vereda) +  # Definiendo formas manualmente
  scale_colour_manual(values = colores_Vereda) +  # Definiendo colores manualmente
  theme_minimal() + # Aplicando tema minimalista
  theme(
    panel.background = element_rect(fill = "gray92", color = NA),
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.5)
  )
# Mostrar el gráfico
print(GRA22)

#G22
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA22))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA22.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}
# Instalar y cargar paquetes necesarios
#install.packages("ggplot2")
#install.packages("vegan")
#install.packages("cluster")
library(ggplot2)
library(vegan)
library(cluster)

#-------45 Calcular la matriz de distancia y Metodo del codo para clusters-----------------------
dist_matrix_RA.2 <- dist(coordenadas_RA[, c("NMDS1", "NMDS2")])
dist_matrix_RA.2


#--------------METODO DEL CODO-------------


# Calcular la matriz de distancia
dist_matrix <- dist(coordenadas[, c("NMDS1", "NMDS2")])


# Calcular la suma de cuadrados intra-cluster para diferentes valores de k
wss <- numeric(10)  # Vector para almacenar la suma de cuadrados intra-cluster

for (i in 1:10) {
  hc_temp <- hclust(dist_matrix_RA.2, method = "ward.D2")
  clusters_temp <- cutree(hc_temp, k = i)
  wss[i] <- sum((dist_matrix_RA.2^2) * (clusters_temp == unique(clusters_temp)))
}

# Graficar el método del codo
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Número de clusters (k)", ylab = "Suma de cuadrados intra-cluster")


# Agregar líneas para mejorar la interpretación
lines(1:10, wss, type = "b", pch = 19, col = "blue")



#-------46 Realizar clustering jerárquico-------------------------------
hc_RA <- hclust(dist_matrix_RA.2, method = "ward.D2")
hc_RA
clusters_hc_RA <- cutree(hc_RA, k = 5) # Ajusta k según sea necesario
coordenadas_RA$cluster <- as.factor(clusters_hc_RA)
coordenadas_RA
print(clusters_hc_RA)

#-------47 Crear grafico con los clusters integrados------------------------- 
# Get the levels of Temporada and cluster
temporada_levels_RA <- levels(coordenadas_RA$Vereda)
cluster_levels_RA <- levels(coordenadas_RA$cluster)

# Create a vector of colors for Temporada
temporada_colors <- c("#EF6548", "#4EB3D3")

# Create a vector of colors for cluster
cluster_colors <- c("#FC8D59",  "#CB181D", "#1171B5", "#7BCCC4","#9999C6")

# Create a named vector of colors
colors <- c(setNames(temporada_colors, temporada_levels_RA), setNames(cluster_colors, cluster_levels_RA))

# Use the named vector of colors in scale_color_manual()
GRA23 <- ggplot(coordenadas_RA, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(shape = Vereda, color = Vereda), size = 4) + 
  geom_text(aes(label = MicroHabitat_Medidas_RA_2_RAANOSIM$E_Blouin_RA), hjust = 0.5, vjust = 1.5) + 
  stat_ellipse(aes(group = cluster, color = cluster), type = "norm", linetype = 2, size = 0.8) + 
  scale_shape_manual("Vereda", values = c(16, 17), temporada_colors) + 
  scale_color_manual("Grupos", values = colors) + 
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "gray92", color = NA),
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.5)
  )

print(GRA23)

#GRA23
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA23))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA23.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


#-------48 Realizar Modelos Lineales Generalizados (GLMs) ---------------------------
# - para evidencias las relación de las variables mas importantes con la termorregulación.


str(MicroHabitat_Medidas_RA_2_FAMD)
MicroHabitat_Medidas_RA_2_FAMD$E_Blouin_RA
# Cargar las librerías necesarias
library(car)
library(MASS)
# En este caso, ya tienes el dataframe cargado como MicroHabitat_Medidas_RAA_2_FAMD
data_RA <- MicroHabitat_Medidas_RA_2_FAMD

# Inspeccionar los datos
str(data_RA)
summary(data_RA)

# Verificar los valores mínimos de E_Blouin_R
min_value_RA <- min(data_RA$E_Blouin_RA)
print(min_value_RA)

# Si hay valores no positivos, agregar una constante positiva para asegurarlos
if (min_value_RA <= 0) {
  data_RA$E_Blouin_RA <- data_RA$E_Blouin_RA - min_value_RA + 1
}

# Ver la estructura de los datos
str(data_RA)
data_RA$E_Blouin_RA
#-------49 Ajustar el modelo inicial------------------------
glm_model_RA <- glm(E_Blouin_RA ~ Tipo.Sustrato + Tipo.Crecimiento + Precipitacion + Nubosidad + 
                      Temperatura.Corporal + Temperatura.Sustrato + Humedad.Sustrato + 
                      Distancia.Suelo..m. + Inclinacion.Sustrato + Distancia.Agua..m. + 
                      Cobertura.Dosel + Altura.Dosel..m. + Profundidad.Hojarasca..m. + 
                      Distancia.Ejemplares..m. + LT + Temporada, 
                    data = data_RA, family = Gamma(link = "log"))


#-------50 Resumen del modelo inicial---------------
summary(glm_model_RA)

# ------51 Verificación de multicolinealidad--------------
vif(glm_model_RA)

# ------52 Simplificación del modelo usando stepAIC-------------------
simplified_glm_RA <- stepAIC(glm_model_RA, direction = "both")

# Resumen del modelo simplificado
summary(simplified_glm_RA)

#Tabla G24
{ # Crear el dataframe manualmente con los resultados
  results_glm_RA <- data.frame(
    Term = c("(Intercept)", "Temperatura.Corporal", "Humedad.Sustrato", 
             "Distancia.Suelo..m.", "Cobertura.Dosel", "LT"),
    Estimate = c(1.76323, 0.07979, 0.08117, -0.03352, -0.04911, 0.03803),
    Std_Error = c(0.01376, 0.01570, 0.01441, 0.01435, 0.01449, 0.01542),
    t_value = c(128.159, 5.081, 5.632, -2.335, -3.389, 2.467),
    Pr_Gt_t = c("< 2e-16", "1.18e-06", "9.43e-08", "0.020957", "0.000911", "0.014844"),
    Signif = c("***", "***", "***", "*", "***", "*")
  )
  
  # Mostrar el dataframe
  print(results_glm_RA)
  
  # Mostrar el dataframe
  print(results_glm_RA)
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_results_glm_RA <- flextable(results_glm_RA) 
  
  # Mostrar la tabla en la consola
  Tabla_results_glm_RA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_results_glm_RAG24.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_results_glm_RA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}



#-------53 Interpretación del modelo simplificado--------------------
summary(simplified_glm_RA)$coefficients
GRA24 <- plot(simplified_glm_RA, se = TRUE, col = "blue")



#-------54 GAM  Modelos Aditivos Generalizados --------------------- 
library(mgcv)


# Ajuste del modelo GAM
gam_model_RA <- gam(E_Blouin_RA ~ s(Temperatura.Sustrato) + s(Humedad.Sustrato) + s(Distancia.Agua..m.) + s(Distancia.Suelo..m.) + s(Cobertura.Dosel) + 
                      Tipo.Sustrato + Tipo.Crecimiento + Precipitacion + Nubosidad + s(Altura.Dosel..m.) + s(Profundidad.Hojarasca..m.) + s(Distancia.Ejemplares..m.) + s(LT) + Temporada,
                    data = data_RA,
                    family = Gamma(link = "log"))

# Resumen del modelo
summary(gam_model_RA)

#Tabla G25
# Definir los coeficientes del modelo con sus valores y p-values
results_gam_R <- 
  
  #tabla categoricas
  {   
    # Crear el DataFrame para los coeficientes paramétricos
    results_gam_RA_Categoricas <- data.frame(
      Term = c("(Intercept)", "Tipo.SustratoHojarasca", "Tipo.SustratoMusgo", "Tipo.SustratoTallo", 
               "Tipo.SustratoTronco", "Tipo.CrecimientoArbustos", "Tipo.CrecimientoEnredaderas", 
               "Tipo.CrecimientoHelecho arborescente", "Tipo.CrecimientoHelecho epifito", 
               "Tipo.CrecimientoHelecho terrestres", "Tipo.CrecimientoHemiepifitas leñosas", 
               "Tipo.CrecimientoHierbas epifitas", "Tipo.CrecimientoHierbas terrestres", 
               "Tipo.CrecimientoHojarascaC", "Tipo.CrecimientoMusgo pleurocárpico", 
               "Tipo.CrecimientoPalma arboreas monoestipitadas", 
               "Tipo.CrecimientoPalma arbustiva cespitosas", "Tipo.CrecimientoTrepadoras", 
               "PrecipitacionSiP", "NubosidadSi", "TemporadaSeco"),
      Estimate = c(1.678585, 0, 0, 0.018303, 0.139166, -0.080753, 0.029281, 0.022643, -0.026585, 
                   -0.028153, 0.035388, -0.053879, 0.009756, 0.017589, -0.103087, 0.021946, 
                   0.002627, -0.017360, 0.027361, 0.041873, 0.120367),
      Std_Error = c(0.053278, 0, 0, 0.051593, 0.076751, 0.059375, 0.106430, 0.110190, 0.053437, 
                    0.122958, 0.129969, 0.123046, 0.048322, 0.110127, 0.129775, 0.183693, 
                    0.096542, 0.066432, 0.053249, 0.036257, 0.047115),
      t_value = c(31.506, NaN, NaN, 0.355, 1.813, -1.360, 0.275, 0.205, -0.498, -0.229, 0.272, 
                  -0.438, 0.202, 0.160, -0.794, 0.119, 0.027, -0.261, 0.514, 1.155, 2.555),
      Pr_Gt_t = c("<2e-16", NaN, NaN, 0.7235, 0.0726, 0.1767, 0.7838, 0.8376, 0.6199, 0.8193, 
                  0.7859, 0.6624, 0.8404, 0.8734, 0.4287, 0.9051, 0.9783, 0.7944, 0.6084, 
                  0.2507, 0.0120),
      Signif = c("***", "", "", "", ".", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "*")
    )
    
    
    
    
    # Mostrar el dataframe
    print(results_gam_RA_Categoricas)
    
    
    
    library(flextable)
    
    # Crear la tabla con flextable
    Tabla_results_gam_RA_Categoricas <- flextable(results_gam_RA_Categoricas) 
    
    # Mostrar la tabla en la consola
    Tabla_results_gam_RA_Categoricas
    
    # Definir nombre del archivo
    nombre_archivo <- "tabla_results_gam_RA_Categoricas_RG25.docx"
    
    # Definir ubicación del archivo
    ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
    
    # Ruta completa donde deseas guardar el archivo
    ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
    
    # Crear un documento Word
    doc <- read_docx()
    
    # Añadir la tabla ft_RG5 al documento
    doc <- doc %>%
      body_add_flextable(Tabla_results_gam_RA_Categoricas)
    
    # Guardar el documento Word en la ubicación especificada
    print(doc, target = ruta_archivo)
  }

#Tabla continuas suavisadas
{  
  # Crear el DataFrame para los términos suaves
  results_gam_RA_Continuas_suave <- data.frame(
    Term = c("s(Temperatura.Sustrato)", "s(Humedad.Sustrato)", "s(Distancia.Agua..m.)", 
             "s(Distancia.Suelo..m.)", "s(Cobertura.Dosel)", "s(Altura.Dosel..m.)", 
             "s(Profundidad.Hojarasca..m.)", "s(Distancia.Ejemplares..m.)", "s(LT)"),
    edf = c(5.133, 5.182, 1.000, 1.000, 2.392, 1.959, 1.000, 1.000, 1.000),
    Ref_df = c(6.223, 6.273, 1.000, 1.000, 2.970, 2.434, 1.000, 1.000, 1.000),
    F_value = c(12.060, 5.206, 0.238, 3.447, 4.782, 2.209, 2.759, 1.891, 6.563),
    p_value = c("<2e-16", "9.66e-05", "0.62651", "0.06611", "0.00409", "0.12847", "0.09965", 
                "0.17195", "0.01180"),
    Signif = c("***", "***", "", ".", "**", "", ".", "", "*")
  )
  
  
  
  
  # Mostrar el dataframe
  print(results_gam_RA_Continuas_suave)
  
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_results_gam_RA_Continuas_suave <- flextable(results_gam_RA_Continuas_suave) 
  
  # Mostrar la tabla en la consola
  Tabla_results_gam_RA_Continuas_suave
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_results_gam_RA_Continuas_suave_RG25.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_results_gam_RA_Continuas_suave)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}

# ------55 Gráfico de las funciones suavizadas-----------------------------------
GRA25 <- plot(gam_model_RA, se = TRUE, col = "blue")




# ------56 Realizar índices de uso de hábitat y preferencia de Maly--------------------

library(openxlsx)
#MicroHabitat_Medidas_RA <- read.xlsx("D:/Resultados B.tamaense/MicroHabitat_Medidas_R.xlsx")
#MicroHabitat_Medidas_RA_2_1 <- MicroHabitat_Medidas_RA[complete.cases(MicroHabitat_Medidas_RA$`Peso.(gr)`), ]
MicroHabitat_Medidas_RA_3 <- MicroHabitat_Medidas_RA_2[, c(
  "Tipo.Sustrato",
  "Tipo.Vegetacion",
  "Tipo.Crecimiento"
)]


#-------57 tipo de sustrato B--------------
str(MicroHabitat_Medidas_RA_3)
# Eliminar espacios en blanco al principio y al final, y convertir la variable a factor si no lo es
MicroHabitat_Medidas_RA_3$Tipo.Sustrato <- as.factor(trimws(MicroHabitat_Medidas_RA_3$Tipo.Sustrato))

# Contar las frecuencias de cada categoría en Tipo.Sustrato
frecuencias_RA_S <- table(MicroHabitat_Medidas_RA_3$Tipo.Sustrato)

# Imprimir las frecuencias
print(frecuencias_RA_S)

# Convertir las frecuencias en un dataframe
Tipo.Sustrato_RA <- as.data.frame(frecuencias_RA_S)

# Renombrar las columnas para mayor claridad
colnames(Tipo.Sustrato_RA) <- c("Categoria", "Frecuencia")

# Imprimir el dataframe de frecuencias
print(Tipo.Sustrato_RA)

#-------58 Tipo de vegetavion B-------------------------------------------------

# Eliminar espacios en blanco al principio y al final, y convertir la variable a factor si no lo es
MicroHabitat_Medidas_RA_3$Tipo.Vegetacion <- as.factor(trimws(MicroHabitat_Medidas_RA_3$Tipo.Vegetacion))

# Contar las frecuencias de cada categoría en Tipo.Sustrato
frecuencias_RA_V <- table(MicroHabitat_Medidas_RA_3$Tipo.Vegetacion)

# Imprimir las frecuencias
print(frecuencias_RA_V)

# Convertir las frecuencias en un dataframe
Tipo.Vegetacion_RA <- as.data.frame(frecuencias_RA_V)

# Renombrar las columnas para mayor claridad
colnames(Tipo.Vegetacion_RA) <- c("Categoria", "Frecuencia")

# Imprimir el dataframe de frecuencias
print(Tipo.Vegetacion_RA)

#-------59 Tipo de crecimiento B----------------------------------

# Eliminar espacios en blanco al principio y al final, y convertir la variable a factor si no lo es
MicroHabitat_Medidas_RA_3$Tipo.de.Crecimiento <- as.factor(trimws(MicroHabitat_Medidas_RA_3$Tipo.Crecimiento))

# Contar las frecuencias de cada categoría en Tipo.Sustrato
frecuencias_RA_C <- table(MicroHabitat_Medidas_RA_3$Tipo.de.Crecimiento)

# Imprimir las frecuencias
print(frecuencias_RA_C)

# Convertir las frecuencias en un dataframe
Tipo.de.Crecimiento_RA <- as.data.frame(frecuencias_RA_C)

# Renombrar las columnas para mayor claridad
colnames(Tipo.de.Crecimiento_RA) <- c("Categoria", "Frecuencia")

# Imprimir el dataframe de frecuencias
print(Tipo.de.Crecimiento_RA)
#-------60 Cargar datos Azar----------------------------------------------
str(MicroHabitat_DisponibleNoUsado_A)
str(MicroHabitat_DisponibleNoUsado_R)
MicroHabitat_DisponibleNoUsado_R <- read.xlsx("D:/Resultados B.tamaense/MicroHabitat_DisponibleNoUsado_R.xlsx")
MicroHabitat_DisponibleNoUsado_A <- read.xlsx("D:/Resultados B.tamaense/MicroHabitat_DisponibleNoUsado_A.xlsx")
MicroHabitat_DisponibleNoUsado_RA <- rbind(MicroHabitat_DisponibleNoUsado_R, MicroHabitat_DisponibleNoUsado_A)
str(MicroHabitat_DisponibleNoUsado_RA)
MicroHabitat_DisponibleNoUsado_RA
MicroHabitat_DisponibleNoUsado_RA <- MicroHabitat_DisponibleNoUsado_RA[, c(
  "Tipo.Sustrato",
  "Tipo.Vegetacion",
  "Tipo.de.Crecimiento"
)]

#-------61 Tipo de sustrato Az---------------
# Eliminar espacios en blanco al principio y al final, y convertir la variable a factor si no lo es
MicroHabitat_DisponibleNoUsado_RA$Tipo.Sustrato <- as.factor(trimws(MicroHabitat_DisponibleNoUsado_RA$Tipo.Sustrato))

# Contar las frecuencias de cada categoría en Tipo.Sustrato
frecuencias_2_RA_S <- table(MicroHabitat_DisponibleNoUsado_RA$Tipo.Sustrato)

# Imprimir las frecuencias
print(frecuencias_2_RA_S)

# Convertir las frecuencias en un dataframe
Tipo.Sustrato_Azar_RA <- as.data.frame(frecuencias_2_RA_S)

# Renombrar las columnas para mayor claridad
colnames(Tipo.Sustrato_Azar_RA) <- c("Categoria", "Frecuencia")

# Imprimir el dataframe de frecuencias
print(Tipo.Sustrato_Azar_RA)

#-------62 Tipo de Vegetacion Az------------------------------------------------
# Eliminar espacios en blanco al principio y al final, y convertir la variable a factor si no lo es
MicroHabitat_DisponibleNoUsado_RA$Tipo.Vegetacion <- as.factor(trimws(MicroHabitat_DisponibleNoUsado_RA$Tipo.Vegetacion))

# Contar las frecuencias de cada categoría en Tipo.Sustrato
frecuencias_2_RA_V <- table(MicroHabitat_DisponibleNoUsado_RA$Tipo.Vegetacion)

# Imprimir las frecuencias
print(frecuencias_2_RA_V)

# Convertir las frecuencias en un dataframe
Tipo.Vegetacion_Azar_RA <- as.data.frame(frecuencias_2_RA_V)

# Renombrar las columnas para mayor claridad
colnames(Tipo.Vegetacion_Azar_RA) <- c("Categoria", "Frecuencia")

# Imprimir el dataframe de frecuencias
print(Tipo.Vegetacion_Azar_RA)



#-------63 Tipo de crecimiento Az---------------------------------------------

# Eliminar espacios en blanco al principio y al final, y convertir la variable a factor si no lo es
MicroHabitat_DisponibleNoUsado_RA$Tipo.de.Crecimiento<- as.factor(trimws(MicroHabitat_DisponibleNoUsado_RA$Tipo.de.Crecimiento))

# Contar las frecuencias de cada categoría en Tipo.Sustrato
frecuencias_2_RA_C <- table(MicroHabitat_DisponibleNoUsado_RA$Tipo.de.Crecimiento)

# Imprimir las frecuencias
print(frecuencias_2_RA_C)

# Convertir las frecuencias en un dataframe
Tipo.de.crecimiento_Azar_RA <- as.data.frame(frecuencias_2_RA_C)

# Renombrar las columnas para mayor claridad
colnames(Tipo.de.crecimiento_Azar_RA) <- c("Categoria", "Frecuencia")

# Imprimir el dataframe de frecuencias
print(Tipo.de.crecimiento_Azar_RA)


# Filtra el data frame para excluir la categoría "Palma arboreas monoestipitadas"
#Tipo.de.crecimiento_Azar_R <- Tipo.de.crecimiento_Azar_R[Tipo.de.crecimiento_Azar_R$Categoria != "Palma arboreas monoestipitadas", ]

# Muestra las primeras filas del data frame resultante para verificar
print(Tipo.de.crecimiento_Azar_RA)


#-------64 Tabla Tipo de Crecimiento----------



# Combinar los dataframes por 'Categoria'
combined_C_RA <- merge(Tipo.de.Crecimiento_RA, Tipo.de.crecimiento_Azar_RA, by = "Categoria", all = TRUE)

# Renombrar columnas para mayor claridad
names(combined_C_RA) <- c("Categoria", "Frecuencia_RA", "Frecuencia_Azar_RA")

# Rellenar NA con 0 en caso de que alguna categoría no esté presente en uno de los dataframes
combined_C_RA[is.na(combined_C_RA)] <- 0

# Calcular la tasa de uso
combined_C_RA$Tasa_de_Uso_C_RA <- combined_C_RA$Frecuencia_R / combined_C_RA$Frecuencia_Azar_RA

# Reemplazar infinitos y NaN resultantes de divisiones por cero
combined_C_RA$Tasa_de_Uso_C_RA[is.infinite(combined_C_RA$Tasa_de_Uso_C_RA) | is.nan(combined_C_RA$Tasa_de_Uso_C_RA)] <- 0

# Calcular los porcentajes
combined_C_RA$Porcentaje <- combined_C_RA$Tasa_de_Uso_C_RA / sum(combined_C_RA$Tasa_de_Uso_C_RA, na.rm = TRUE)

# Crear la nueva tabla con ambas columnas
nueva_tabla_C_RA <- data.frame(
  Categorias = combined_C_RA$Categoria,
  B.tamaense = combined_C_RA$Frecuencia_RA,
  Habitat = combined_C_RA$Frecuencia_Azar_RA,
  Tasa_de_Uso = combined_C_RA$Tasa_de_Uso_C_RA,
  Porcentaje = combined_C_RA$Porcentaje
)

# Ordenar la tabla por la columna 'Tasa_de_Uso' de mayor a menor
nueva_tabla_C_RA <- nueva_tabla_C_RA[order(-nueva_tabla_C_RA$Tasa_de_Uso), ]

# Imprimir la tabla ordenada
print(nueva_tabla_C_RA)

# Ordenar la tabla por Porcentaje de mayor a menor
nueva_tabla_C_RA <- nueva_tabla_C_RA[order(-nueva_tabla_C_RA$Porcentaje), ]
print(nueva_tabla_C_RA)

# Crear el gráfico de barras
# Define los colores que deseas usar para cada categoría


mis_colores <- c("Helecho epifito" = "#66BD63", 
                 "Hierbas terrestres" = "#1B7837", 
                 "Arboles" = "#7A5C12", 
                 "Hemiepifitas leñosas" = "#838903", 
                 "Trepadoras" = "#019937", 
                 "Arbustos" = "#7FBC41", 
                 "Helecho arborescente" = "#839443", 
                 "Helecho terrestres" =  "#1D6900", 
                 "Palma arbustiva cespitosas" = "#3C9900", 
                 "Enredaderas" = "#4C7022", 
                 "Musgo pleurocárpico" = "#A0D91E", 
                 "Palma arboreas monoestipitadas" = "#617742", 
                 "HojarascaC" = "#A7761D", 
                 "Hierbas epifitas" = "#A8C886")

# Crear el gráfico de barras con la paleta personalizada
GRA26 <- ggplot(nueva_tabla_C_RA, aes(x = reorder(Categorias, -Porcentaje), y = Porcentaje, fill = Categorias)) +
  geom_bar(stat = "identity") +
  labs(x = "Categorías", y = "Porcentaje", title = "Porcentaje por Categoría RA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  scale_fill_manual(values = mis_colores)  # Usar la paleta de colores personalizada

# Imprimir el gráfico G26
print(GRA26)
GA26
#GRA26
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA26))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA26.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

#Tabla G26
{ 
  # Mostrar el dataframe
  print(nueva_tabla_C_RA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_C_RA <- flextable(nueva_tabla_C_RA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_C_RA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_C_RA_RG26.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_C_RA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}


#-------65 Tabla Tipo de Vegetación----------



# Combinar los dataframes por 'Categoria'
combined_V_RA <- merge(Tipo.Vegetacion_RA, Tipo.Vegetacion_Azar_RA, by = "Categoria", all = TRUE)

# Renombrar columnas para mayor claridad
names(combined_V_RA) <- c("Categoria", "Frecuencia_RA", "Frecuencia_Azar_RA")

# Rellenar NA con 0 en caso de que alguna categoría no esté presente en uno de los dataframes
combined_V_RA[is.na(combined_V_RA)] <- 0

# Calcular la tasa de uso
combined_V_RA$Tasa_de_Uso_V_RA <- combined_V_RA$Frecuencia_RA / combined_V_RA$Frecuencia_Azar_RA

# Reemplazar infinitos y NaN resultantes de divisiones por cero
combined_V_RA$Tasa_de_Uso_V_RA[is.infinite(combined_V_RA$Tasa_de_Uso_V_RA) | is.nan(combined_V_RA$Tasa_de_Uso_V_RA)] <- 0

# Calcular los porcentajes
combined_V_RA$Porcentaje <- combined_V_RA$Tasa_de_Uso_V_RA / sum(combined_V_RA$Tasa_de_Uso_V_RA, na.rm = TRUE)

# Crear la nueva tabla con ambas columnas
nueva_tabla_V_RA <- data.frame(
  Categorias = combined_V_RA$Categoria,
  B.tamaense = combined_V_RA$Frecuencia_RA,
  Habitat = combined_V_RA$Frecuencia_Azar_RA,
  Tasa_de_Uso = combined_V_RA$Tasa_de_Uso_V_RA,
  Porcentaje = combined_V_RA$Porcentaje
)

# Ordenar la tabla por la columna 'Tasa_de_Uso' de mayor a menor
nueva_tabla_V_RA <- nueva_tabla_V_RA[order(-nueva_tabla_V_RA$Tasa_de_Uso), ]

# Imprimir la tabla ordenada
print(nueva_tabla_V_RA)

# Ordenar la tabla por Porcentaje de mayor a menor
nueva_tabla_V_RA <- nueva_tabla_V_RA[order(-nueva_tabla_V_RA$Porcentaje), ]

# Crear el gráfico de barras

# Define los colores que deseas usar para cada categoría


mis_colores <- c("Herbacea" = "#66BD63", 
                 "Helecho" = "#1B7837", 
                 "Trepadora" = "#4C7022", 
                 "Cola de caballo" = "#508443", 
                 "Arbol" = "#7A5C12", 
                 "Chusquea" = "#9E9100", 
                 "Anturio" = "#A1D190", 
                 "Arbusto" = "#7FBC41", 
                 "HojarascaV" = "#A7761D", 
                 "Enredadera" = "#4C7022", 
                 "MusgoV" = "#A0D91E", 
                 "Bromelia" ="#B5C80E", 
                 "Palma" = "#617742")

# Crear el gráfico de barras con la paleta personalizada
GRA27 <- ggplot(nueva_tabla_V_RA, aes(x = reorder(Categorias, -Porcentaje), y = Porcentaje, fill = Categorias)) +
  geom_bar(stat = "identity") +
  labs(x = "Categorías", y = "Porcentaje", title = "Porcentaje por Categoría") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  scale_fill_manual(values = mis_colores)  # Usar la paleta de colores personalizada

# Imprimir el gráfico G26
print(GRA27)
GA27
#G27
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA27))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA27.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

#Tabla G27
{ 
  # Mostrar el dataframe
  print(nueva_tabla_V_RA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_V_RA <- flextable(nueva_tabla_V_RA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_V_RA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_V_RA_RAG27.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_V_RA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}



#-------66 Tabla Tipo de Sustrato----------



# Combinar los dataframes por 'Categoria'
combined_S_RA<- merge(Tipo.Sustrato_RA, Tipo.Sustrato_Azar_RA, by = "Categoria", all = TRUE)

# Renombrar columnas para mayor claridad
names(combined_S_RA) <- c("Categoria", "Frecuencia_RA", "Frecuencia_Azar_RA")

# Rellenar NA con 0 en caso de que alguna categoría no esté presente en uno de los dataframes
combined_S_RA[is.na(combined_S_RA)] <- 0

# Calcular la tasa de uso
combined_S_RA$Tasa_de_Uso_S_RA <- combined_S_RA$Frecuencia_RA / combined_S_RA$Frecuencia_Azar_RA

# Reemplazar infinitos y NaN resultantes de divisiones por cero
combined_S_RA$Tasa_de_Uso_S_RA[is.infinite(combined_S_RA$Tasa_de_Uso_S_RA) | is.nan(combined_S_RA$Tasa_de_Uso_S_RA)] <- 0

# Calcular los porcentajes
combined_S_RA$Porcentaje <- combined_S_RA$Tasa_de_Uso_S_RA / sum(combined_S_RA$Tasa_de_Uso_S_RA, na.rm = TRUE)

# Crear la nueva tabla con ambas columnas
nueva_tabla_S_RA <- data.frame(
  Categorias = combined_S_RA$Categoria,
  B.tamaense = combined_S_RA$Frecuencia_RA,
  Habitat = combined_S_RA$Frecuencia_Azar_RA,
  Tasa_de_Uso = combined_S_RA$Tasa_de_Uso_S_RA,
  Porcentaje = combined_S_RA$Porcentaje
)

# Ordenar la tabla por la columna 'Tasa_de_Uso' de mayor a menor
nueva_tabla_S_RA <- nueva_tabla_S_RA[order(-nueva_tabla_S_RA$Tasa_de_Uso), ]

# Imprimir la tabla ordenada
print(nueva_tabla_S_RA)

# Ordenar la tabla por Porcentaje de mayor a menor
nueva_tabla_S_RA <- nueva_tabla_S_RA[order(-nueva_tabla_S_RA$Porcentaje), ]
print(nueva_tabla_S_RA)
nueva_tabla_S_A
nueva_tabla_S_R
# Crear el gráfico de barras


# Define los colores que deseas usar para cada categoría


mis_colores <- c("Hoja" = "#7FBC41", 
                 "Tallo" = "#1B5837", 
                 "Hojarasca" = "#A7761D", 
                 "Tronco" = "#7A5C12",
                 "Musgo" = "#A0D91E") 


# Crear el gráfico de barras con la paleta personalizada
GRA28 <- ggplot(nueva_tabla_S_RA, aes(x = reorder(Categorias, -Porcentaje), y = Porcentaje, fill = Categorias)) +
  geom_bar(stat = "identity") +
  labs(x = "Categorías", y = "Porcentaje", title = "Porcentaje por Categoría") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  scale_fill_manual(values = mis_colores)  # Usar la paleta de colores personalizada

# Imprimir el gráfico G26
print(GRA28)

#G28
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GRA28))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GRA28.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

#Tabla G28
{ 
  # Mostrar el dataframe
  print(nueva_tabla_S_RA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_S_RA <- flextable(nueva_tabla_S_RA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_S_RA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_S_RA_RAG28.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_S_RA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}

