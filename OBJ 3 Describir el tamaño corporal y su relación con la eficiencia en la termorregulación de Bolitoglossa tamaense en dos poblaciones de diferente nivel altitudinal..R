#----------------------------------ZONA--------------------------------------------
## ----Instalar y cargar los paquetes necesarios-----
#install.packages(c("openxlsx", "corrplot", "FactoMineR", "factoextra", "ggplot2"))
#install.packages("FactoMineR")
#install.packages("factoextra")
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

#------------------------------Remansos (2100) y Asiria (2600)---------------------------------


#-------------------------1. A Cargar medidas A-----------------------------------------------
#MicroHabitat_Medidas_A
MicroHabitat_Medidas_A <-read.xlsx("D:/Resultados B.tamaense/MicroHabitat_Medidas_A.xlsx")
summary(MicroHabitat_Medidas_A)

library(dplyr)
str(T_Corporal_A)
str(MicroHabitat_Medidas_A)
#---------- 2. A Agregar las tres columnas a data_existente-------------------
MicroHabitat_Medidas_A <- MicroHabitat_Medidas_A %>%
  mutate(
    E_Blouin_RA = T_Corporal_A$E_Blouin_A,
    E_numeric_RA = T_Corporal_A$E_numeric_A,
  )

str(MicroHabitat_Medidas_A)

#-------------------------1. R Cargar medidas R-----------------------------------------------
#MicroHabitat_Medidas_R
MicroHabitat_Medidas_R <-read.xlsx("D:/Resultados B.tamaense/MicroHabitat_Medidas_R.xlsx")
summary(MicroHabitat_Medidas_R)

library(dplyr)
str(T_Corporal_R)
str(MicroHabitat_Medidas_R)
#---------- 2. R Agregar las tres columnas a data_existente-------------------
MicroHabitat_Medidas_R <- MicroHabitat_Medidas_R %>%
  mutate(
    E_Blouin_RA = T_Corporal_R$E_Blouin_r,
    E_numeric_RA = T_Corporal_R$E_numeric_R,
  )

str(MicroHabitat_Medidas_R)

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
MicroHabitat_Medidas_RA <- MicroHabitat_Medidas_RA[complete.cases(MicroHabitat_Medidas_RA$LCA), ]

# Verificar la asignación de nombres de fila únicos
print(rownames(MicroHabitat_Medidas_RA))
#Asigbar nombre secuencial a las filas
# Obtener el número total de filas en el data frame
total_filas <- nrow(MicroHabitat_Medidas_RA)

# Generar nombres de fila secuenciales sin salto
nuevos_nombres <- paste( seq_len(total_filas), sep = "")

# Asignar los nuevos nombres de fila al data frame
rownames(MicroHabitat_Medidas_RA) <- nuevos_nombres

# Verificar los nombres de fila actualizados
print(rownames(MicroHabitat_Medidas_RA))

str(MicroHabitat_Medidas_RA$Temperatura.Corporal)


# Seleccionar solo las columnas deseadas
MicroHabitat_Tamaño_RA <- MicroHabitat_Medidas_RA[, c(
  "Vereda",
  "Temporada",
  "ID",
  "Peso.(gr)",
  "LCA",
  "AC",
  "LHC",
  "Lto",
  "LAI",
  "LT",
  "LPT",
  "LCO",
  "E_numeric_RA",
  "E_Blouin_RA",
  "Temperatura.Corporal"
)]

str(MicroHabitat_Tamaño_RA)
# Calcular la desviación estándar de la columna E_Blouin_RA
desviacion_estandar <- sd(MicroHabitat_Tamaño_RA$E_Blouin_RA)

# Imprimir el resultado
print(desviacion_estandar)

library(corrplot)
#--------3. A Eliminar la variable TIPO CHARE usando subset ------------------
#Crear un dataframe nuevo sin las variables tipo chare
MicroHabitat_Tamaño_RA_2 <- subset(MicroHabitat_Tamaño_RA, select = -Vereda)
MicroHabitat_Tamaño_RA_2 <- subset(MicroHabitat_Tamaño_RA_2, select = -Temporada)
MicroHabitat_Tamaño_RA_2 <- subset(MicroHabitat_Tamaño_RA_2, select = -ID)
MicroHabitat_Tamaño_RA_2 <- subset(MicroHabitat_Tamaño_RA_2, select = -Temperatura.Corporal)
MicroHabitat_Tamaño_RA_2 <- subset(MicroHabitat_Tamaño_RA_2, select = -E_numeric_RA)
MicroHabitat_Tamaño_RA_2 <- subset(MicroHabitat_Tamaño_RA_2, select = -E_Blouin_RA)
str(MicroHabitat_Tamaño_RA_2)


#-------------------------
library(flextable)
MicroHabitat_Tamaño_RA_22 <- subset(MicroHabitat_Tamaño_RA_2)
MicroHabitat_Tamaño_RA_22$Vereda <- MicroHabitat_Medidas_RA$Vereda

str(MicroHabitat_Tamaño_RA_22)


# Separa el dataframe por la variable Vereda
veredas_separadas <- split(MicroHabitat_Tamaño_RA_22, MicroHabitat_Tamaño_RA_22$Vereda)

# Asigna los resultados a dos dataframes separados
Asiria <- veredas_separadas[[1]]
Asiria <- subset(Asiria, select = -Vereda)
Remansos <- veredas_separadas[[2]]
Remansos <- subset(Remansos, select = -Vereda)
# Utiliza la función sapply() para obtener la media y la desviación estándar de cada variable
media_sd <- sapply(Asiria, function(x) c(media = mean(x), desviacion_estandar = sd(x)))
summary(Asiria$`Peso.(gr)`)
# Convierte el resultado en un dataframe
media_sd <- as.data.frame(media_sd)

# Muestra el resultado
media_sd
media_sd_transpuesta <- t(media_sd)
media_sd_transpuesta <- as.data.frame(media_sd_transpuesta)
library(tibble)

media_sd_transpuesta <- media_sd_transpuesta %>% 
  rownames_to_column("Variable")
# Crear la tabla con flextable

Var <- flextable(media_sd_transpuesta) 

# Mostrar la tabla
Var


# Definir nombre del archivo
nombre_archivo <- "tabla_variables3OBJA.docx"

# Definir ubicación del archivo
ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"

# Ruta completa donde deseas guardar el archivo
ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)

# Crear un documento Word
doc <- read_docx()

# Añadir la tabla ft_RG5 al documento
doc <- doc %>%
  body_add_flextable(Var)

# Guardar el documento Word en la ubicación especificada
print(doc, target = ruta_archivo)



#Revisar la diferencias significativas

# Loop a través de cada variable continua
for (variable in names(MicroHabitat_Tamaño_RA_22[, 1:9])) {
  if (variable != "Velocidad.Viento") {
    # Verificar la normalidad de la variable
    shapiro_test <- shapiro.test(MicroHabitat_Tamaño_RA_22[, variable])
    print(paste("Prueba de Shapiro-Wilk para", variable))
    print(shapiro_test)
    
    # Si la variable es normal, aplicar t-test
    if (shapiro_test$p.value > 0.05) {
      t_test <- t.test(get(variable) ~ Vereda, data = MicroHabitat_Tamaño_RA_22)
      print(paste("T-test para", variable))
      print(t_test)
      p_value <- t_test$p.value
      if (p_value < 0.05) {
        print(paste("Hay diferencias significativas entre las veredas para", variable))
      } else {
        print(paste("No hay diferencias significativas entre las veredas para", variable))
      }
    } 
    # Si la variable no es normal, aplicar Mann-Whitney U test
    else {
      wilcox_test <- wilcox.test(get(variable) ~ Vereda, data = MicroHabitat_Tamaño_RA_22)
      print(paste("Mann-Whitney U test para", variable))
      print(wilcox_test)
      p_value <- wilcox_test$p.value
      if (p_value < 0.05) {
        print(paste("Hay diferencias significativas entre las veredas para", variable))
      } else {
        print(paste("No hay diferencias significativas entre las veredas para", variable))
      }
    }
  }
}















#------------- 4. A Calcular la matriz de correlación---------------
matriz_correlacion_Tamaño_RA <- cor(MicroHabitat_Tamaño_RA_2, use = "complete.obs")
matriz_correlacion_Tamaño_RA
#----------------5. A Crear el gráfico de correlación con corrplot-------------
GTRA1 <- corrplot(matriz_correlacion_Tamaño_RA, method = "color", addCoef.col = "black", 
                  tl.cex = 0.8, number.cex = 0.7, col = colorRampPalette(c("blue", "white", "red"))(200))

## Instalar el paquete ggcorrplot si no está instalado
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}

# Cargar las librerías necesarias
library(ggcorrplot)

# Crear el gráfico de correlación usando ggcorrplot
GTRA1 <- ggcorrplot(matriz_correlacion_Tamaño_RA, method = "square", 
                    lab = TRUE, lab_col = "black", lab_size = 3, 
                    colors = c("blue", "white", "red"), 
                    title = "Matriz de Correlación TR", 
                    legend.title = "Correlación")

# Mostrar el gráfico
print(GTRA1)

#GTR1
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA1))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA1.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

#Eliminar variables con desviacion cero
MicroHabitat_Tamaño_RA_2

#MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2, select = -E_Blouin_A)
#MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -E_numeric_A)
#MicroHabitat_Tamaño_RA_2.1
# Cargar las librerías necesarias
library(dplyr)
library(tidyr)
library(psych)

#----------------6. A Crear una función para realizar la prueba de Shapiro-Wilk---------
test_normality_A <- function(data) {
  data %>%
    select_if(is.numeric) %>%
    summarise_all(~ shapiro.test(.)$p.value) %>%
    gather(variable, p_value) %>%
    mutate(normal = ifelse(p_value >= 0.05, "Normal", "No Normal"))
}


# Aplicar la función a tu dataframe
MicroHabitat_Tamaño_RA_2.1 <- test_normality_A(MicroHabitat_Tamaño_RA_2)
MicroHabitat_Tamaño_RA_2.1
#--------------7. A Crear grsafico Boxplot----------------------------------
print(MicroHabitat_Tamaño_RA_2)
boxplot(MicroHabitat_Tamaño_RA_2)
#-------8. A Función para identificar outliers--------------------------
identify_outliers_Tamaño_A <- function(MicroHabitat_Tamaño_RA_2) {
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

outliers_df_Tamaño_A <- identify_outliers_R(MicroHabitat_Tamaño_RA_2)

print(outliers_df_Tamaño_A)

#-------------9. A Normalizar datos--------------------------------- 
#------- Función para normalización robusta seguida de estandarización--------------------
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




# Ejemplo de aplicación a variables_continuas_A
MicroHabitat_Tamaño_RA_2_norm_robus <- as.data.frame(lapply(MicroHabitat_Tamaño_RA_2, robust_standardize))
MicroHabitat_Tamaño_RA_2_norm_robus
# Verificar los primeros registros de las variables normalizadas
head(MicroHabitat_Tamaño_RA_2_norm_robus)
MicroHabitat_Tamaño_RA_2_norm <- as.data.frame(scale(MicroHabitat_Tamaño_RA_2))
boxplot(MicroHabitat_Tamaño_RA_2_norm)

MicroHabitat_Tamaño_A_2_norm


#-------10. A Graficar boxplot ORIGINAL NORMALIZADO--------------------------
GTRA2<- boxplot(MicroHabitat_Tamaño_RA_2, las = 2, main = "Variables Continuas TR", cex.main = 0.9)

# Cargar la librería ggplot2
library(ggplot2)
library(tidyr)

# Transformar los datos de wide a long format
matriz_correlacion_MicroHabitat_Tamaño_RA_2_long <- MicroHabitat_Tamaño_RA_2 %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Crear el boxplot usando ggplot2
GTRA2 <- ggplot(matriz_correlacion_MicroHabitat_Tamaño_RA_2_long, aes(x = Variable, y = Valor)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variables Continuas TR") +
  theme(plot.title = element_text(size = 9))

# Mostrar el gráfico
print(GTRA2)

#GTA2
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA2))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA2.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_type(type = "body")) %>%
    base::print(target = output_path)
}

# Graficar boxplot de variables_continuas_A_norm
GTRA3 <- boxplot(MicroHabitat_Tamaño_RA_2_norm, las = 2, main = "Variables Continuas TA Normalizadas", cex.main = 0.9)
# Transformar los datos de wide a long format
MicroHabitat_Tamaño_RA_2_norm_long <- MicroHabitat_Tamaño_RA_2_norm %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Crear el boxplot usando ggplot2
GTRA3 <- ggplot(MicroHabitat_Tamaño_RA_2_norm_long, aes(x = Variable, y = Valor)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Variables Continuas TA Normalizadas") +
  theme(plot.title = element_text(size = 9))

# Mostrar el gráfico
print(GTRA3)


#GTA3
{
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA3))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA3.pptx" # Reemplaza con el nombre del archivo que desees
  
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
grid.arrange(GTRA3, GTRA2, ncol = 2)

#GTA3-GTA2
{
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = grid.arrange(GTRA3, GTRA2, ncol = 2))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA3_2.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_type(type = "body")) %>%
    base::print(target = output_path)
}

#MicroHabitat_Tamaño_RA_2_norm
#MicroHabitat_Tamaño_RA_2_norm <- subset(MicroHabitat_Tamaño_RA_2_norm, select = -c(E_Blouin_RA ))
#MicroHabitat_Tamaño_RA_2_norm <- subset(MicroHabitat_Tamaño_RA_2_norm, select = -c(E_numeric_RA ))
#-------11. A Crear un modelo de regresión múltiple------------------------
lm_model_Tamaño_RA <- lm(data = MicroHabitat_Tamaño_RA_2_norm)

#-------12 A Identificar las variables con colinealidad perfecta-----------------------
aliased_vars_Tamaño_RA <- alias(lm_model_Tamaño_RA)$Complete

# Imprimir las variables aliased
print("Variables aliased:")
print(aliased_vars_Tamaño_RA)


# Eliminar las variables aliased
#MicroHabitat_Tamaño_A_2_norm <- subset(MicroHabitat_Tamaño_A_2_norm, select = -c(E_numeric_A ))
#MicroHabitat_Tamaño_A_2_norm <- subset(MicroHabitat_Tamaño_A_2_norm, select = -c(E_Blouin_A ))
# Ajustar un nuevo modelo de regresión múltiple
#lm_model_clean <- lm(data = variables_continuas_R_norm_numeric_clean)

#-------------13. A Calcular la matriz de correlación Variables normalizadas-------------
matriz_correlacion_Tamaño_RA_norm <- cor(MicroHabitat_Tamaño_RA_2_norm, use = "complete.obs")

#-----------14. A Crear el gráfico de correlación con corrplot VARIABLES NORMALES-------------
corrplot(matriz_correlacion_Tamaño_RA_norm, method = "color", addCoef.col = "black", 
         tl.cex = 0.8, number.cex = 0.7, col = colorRampPalette(c("blue", "white", "red"))(200))

## Instalar el paquete ggcorrplot si no está instalado
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}

# Cargar las librerías necesarias
library(ggcorrplot)

# Crear el gráfico de correlación usando ggcorrplot
GTRA4 <- ggcorrplot(matriz_correlacion_Tamaño_RA_norm, method = "square", 
                    lab = TRUE, lab_col = "black", lab_size = 3, 
                    colors = c("blue", "white", "red"), 
                    title = "Matriz de Correlación R", 
                    legend.title = "Correlación")

# Mostrar el gráfico
print(GTRA4)

#GTA4
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA4))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA4.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}




#pca
#install.packages("ggcorrplot")
#install.packages("corrr")
#library("FactoMineR")
#library("corrr")

#library("ggcorrplot")
#------- 15.A Agregar la variable temporada para crear grafico boxplot por temporada------------------


MicroHabitat_Tamaño_RA_2_norm$Vereda <- MicroHabitat_Tamaño_RA$Vereda
MicroHabitat_Tamaño_RA_2_norm <- MicroHabitat_Tamaño_RA_2_norm %>%
  mutate_if(is.character, as.factor)
str(MicroHabitat_Tamaño_RA_2_norm)


#MicroHabitat_Tamaño_RA_2_norm$E_Blouin_RA <- MicroHabitat_Tamaño_RA$E_Blouin_RA
#MicroHabitat_Tamaño_RA_2_norm$E_numeric_RA <- MicroHabitat_Tamaño_RA$E_numeric_RA
str(MicroHabitat_Tamaño_RA_2_norm)
#MicroHabitat_Tamaño_A_2_norm <- subset(MicroHabitat_Tamaño_A_2_norm, select = -cluster)
#------- 16. A Realizar grafico boxplot por temporada con las variables normalizadas-------------------------

library(tidyr)

# Aplicar gather para reorganizar los datos
datos2_RA_Tamaño <- gather(MicroHabitat_Tamaño_RA_2_norm,
                           key = "Variables",     # Nombre para la nueva columna de variables numéricas
                           value = "Valor_escalado",# Nombre para la nueva columna de valores
                           -Vereda)             # Columna que no queremos agrupar (en este caso 'Vereda')

# Mostrar el resultado
print(datos2_RA_Tamaño)
# Mostrar la estructura del nuevo data frame
str(datos2_RA_Tamaño)
Summary
library(ggplot2)


# Definir los colores deseados para cada temporada
colores_temporada <- c("Asiria " = "#EF6548",  # Color naranja para Verano
                       "Remansos" = "#4EB3D3" # Color azul para Invierno
                       # Agrega más colores si tienes más categorías de temporada
)

# Graficar el boxplot con nombres de variables en diagonal y colores personalizados para Temporada
GTRA5 <- ggplot(datos2_RA_Tamaño, aes(Variables, Valor_escalado)) +
  geom_boxplot(aes(fill = Vereda)) +
  geom_vline(xintercept = seq(0.5, length(unique(datos2_RA_Tamaño$Variables)) + 0.5, 1), linetype = "dashed") +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = colores_temporada) +  # Especificar los colores personalizados
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

GTRA5
# Mostrar el gráfico
print(GTRA5)

#GTA5
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA5))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA5.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

#-------17. A Revisar la influencia de las variables en cada temporada library(indicspecies)-------------------------------
#Ahora solo necesitamos las abundancias que son de la tercera columna en adelante. Pasamos de un dataframe a una matriz de datos:
#Hacemos la matriz solo con datos numericos

MicroHabitat_Tamaño_RA_2_norm <- MicroHabitat_Tamaño_RA_2_norm %>%
  mutate_if(is.character, as.factor)
MicroHabitat_Tamaño_RA_2_norm <- subset(MicroHabitat_Tamaño_RA_2_norm, select = -Vereda)
str(MicroHabitat_Tamaño_RA_2_norm)
matriz_RA_Tamaño <- MicroHabitat_Tamaño_RA_2_norm[,0:ncol(MicroHabitat_Tamaño_RA_2_norm)]
matriz_RA_Tamaño <- as.matrix(matriz_RA_Tamaño)
matriz_RA_Tamaño
MicroHabitat_Tamaño_RA_2_norm
library(indicspecies)
tramo_RA_Tamaño <- MicroHabitat_Tamaño_RA$Vereda #vector con el tramo
tramo_RA_Tamaño

# Verificar y convertir matriz_A_Tamaño a una matriz numérica
if (!is.matrix(matriz_RA_Tamaño)) {
  matriz_A_Tamaño <- as.matrix(matriz_RA_Tamaño)
}
if (!is.numeric(matriz_RA_Tamaño)) {
  matriz_RA_Tamaño <- apply(matriz_RA_Tamaño, 2, as.numeric)
}

# Verificar y convertir tramo_A_Tamaño a un factor
if (!is.factor(tramo_RA_Tamaño)) {
  tramo_RA_Tamaño <- as.factor(tramo_RA_Tamaño)
}

# Ejecutar nuevamente multipatt
speciesdiferencias_RA_Tamaño <- multipatt(matriz_RA_Tamaño, tramo_RA_Tamaño, func="r.g", control = how(nperm=9999))

#analizamos las especies:
speciesdiferencias_RA_Tamaño <-multipatt(matriz_RA_Tamaño, tramo_RA_Tamaño, func="r.g", control = how(nperm=9999))
speciesdiferencias_RA_Tamaño
summary(speciesdiferencias_RA_Tamaño)

#Tabla GTR5
{ 
  library(flextable)
  library(magrittr)
  
  # Crear un data frame con los resultados de summary(speciesdiferencias_A)
  resultados_summary_medidadsdiferencias_TRA  <- data.frame(Grupo = c("Asiria","Asiria","Asiria","Asiria","Asiria","Asiria","Asiria","Asiria","Asiria" ),
                                                            Variable = c("AC", "LCA","LT","Peso (gr)" , "LCO", "LHC", "Lto", "LAI", "LPT"),
                                                            No_Variables = c(1,7,7,7,7,7,7,7,1),
                                                            stat = c(0.486, 0.482, 0.462, 0.449, 0.449, 0.424, 0.420, 0.412, 0.298),
                                                            P_Value = c(1e-04, 1e-04,1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 1e-04, 5e-04),
                                                            Signif = c("***", "***", "***", "***", "***", "***", "***", "***", "***")
  )
  
  # Mostrar el dataframe
  print(speciesdiferencias_RA_Tamaño)
  
  
  resultados_summary_medidadsdiferencias_TRA
  
  # Crear la tabla con flextable
  ft_GTRA5<- flextable(resultados_summary_medidadsdiferencias_TRA) 
  
  # Mostrar la tabla
  ft_GTRA5
  
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_multinivel_GTRA54.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(ft_GTRA5)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}



MicroHabitat_Tamaño_RA_2_norm
#-----------------------------18. A Realizar un PCA con la variables normalizadas---------------
#MicroHabitat_Tamaño_RA_2_norm <- subset(MicroHabitat_Tamaño_RA_2_norm, select = -c(E_numeric_A ))
MicroHabitat_Tamaño_RA_2_norm
data.pca_norm_RA <- princomp(MicroHabitat_Tamaño_RA_2_norm)
summary(data.pca_norm_RA)

MicroHabitat_Tamaño_RA_2_norm
library(factoextra)
library(ggplot2)


#Tabla G26
{ 
  # Mostrar el dataframe
  print(data.pca_norm_RA)
  
  # Crear el dataframe a partir de los datos proporcionados
  pca_summary_TRA <- data.frame(
    Component = paste0("Comp.", 1:9),
    Standard_deviation = c(2.8740182, 0.44467787, 0.40104450, 0.37594828, 0.270055756, 0.243489872, 0.176703111, 0.114148360, 0.044631346),
    Proportion_of_Variance = c(0.9241051, 0.02212246, 0.01799399, 0.01581243, 0.008159231, 0.006632911, 0.003493259, 0.001457745, 0.000222855),
    Cumulative_Proportion = c(0.9241051, 0.94622758, 0.96422157, 0.98003400, 0.988193230, 0.994826141, 0.998319400, 0.999777145, 1.000000000)
  )
  
  
  
  
  
  #<- data.frame(
  
  # Component = c("Comp.1", "Comp.2", "Comp.3", "Comp.4", "Comp.5", "Comp.6", "Comp.7", "Comp.8", "Comp.9", "Comp.10"),
  #  Standard_deviation = c(2.8550602, 0.9976455, 0.48873403, 0.38831667, 0.35042198, 0.32903673, 0.274091003, 0.175461370, 0.117977419, 0.0410009257),
  # Proportion_of_Variance = c(0.8242957, 0.1006480, 0.02415448, 0.01524841, 0.01241753, 0.01094816, 0.007596999, 0.003113261, 0.001407506, 0.0001699964),
  #  Cumulative_Proportion = c(0.8242957, 0.9249437, 0.94909813, 0.96434655, 0.97676407, 0.98771224, 0.995309236, 0.998422497, 0.999830004, 1.0000000000)
  #)
  
  # Ver el dataframe
  print(pca_summary_TRA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_Dimensiones_TRA <- flextable(pca_summary_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_Dimensiones_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_Dimensiones_TA_GTRA5.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_Dimensiones_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}




#----------------19. A Graficar las dimensiones del PCA------
GTRA6 <- fviz_eig(data.pca_norm_RA, addlabels = TRUE)
GTRA6
#GTA6
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA6))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA6.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


#----------20. A Visualizar la contribución de las variables en los dos primeros ejes-------

# Obtener las cargas (loadings)
loadings <- data.pca_norm_RA$loadings

# Obtener la varianza explicada por cada componente
variances <- data.pca_norm_RA$sdev^2

# Calcular la contribución de cada variable al primer componente principal
contrib <- loadings[, 1]^2 * (variances[1] / sum(variances))

# Convertir en dataframe para una visualización más clara
contrib_df <- data.frame(
  Variable = rownames(loadings),
  Contribución = contrib
)

# Ordenar por contribución
contrib_df <- contrib_df[order(contrib_df$Contribución, decreasing = TRUE), ]

# Imprimir contribuciones
print(contrib_df)



GTRA9 <-fviz_contrib(data.pca_norm_RA, "var", axes = 1)
GTRA8 <- fviz_contrib(data.pca_norm_RA, "var", axes = 2)


GTRA7 <- biplot_RA_famd_TRA_norm <-fviz_pca_var(data.pca_norm_RA, 
                                                col.var = "contrib", 
                                                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                                                repel = TRUE)
print(GTRA7)

#GTA7
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA9))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA9.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}

# Agregar la columna de IDs de OtraBaseDatos a Basenueva
#MicroHabitat_Tamaño_A_2_norm$ID <- MicroHabitat_Tamaño_A$ID
#MicroHabitat_Tamaño_A_2_norm$Vereda <- MicroHabitat_Tamaño_A$Vereda
#MicroHabitat_Tamaño_A_2_norm$Temporada <- MicroHabitat_Tamaño_A$Temporada

# Agregar la columna de IDs de OtraBaseDatos a Basenueva
#MicroHabitat_Tamaño_RA_2$ID <- MicroHabitat_Tamaño_A$ID

#-----------21. A Obtener las coordenadas de los individuos en los componentes principales----------
coordinates_Tam_RA <- data.pca_norm_RA$scores

coordinates_Tam_RA



# Cargar las bibliotecas necesarias
library(factoextra)
library(ggplot2)
library(cluster)



#---------------22. A Método del codo-----------------------------------------
fviz_nbclust(coordinates_Tam_RA, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")

#---------------23. A Método de la silueta----------------------
fviz_nbclust(coordinates_Tam_RA, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

#--------------24. A Criterio de la brecha--------------------------------
set.seed(123)  # Establecer una semilla para reproducibilidad
gap_stat <- clusGap(coordinates_Tam_RA, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Visualizar la estadística de brecha
fviz_gap_stat(gap_stat) +
  labs(subtitle = "Gap Statistic")

# Cargar las bibliotecas necesarias
library(ggplot2)
library(factoextra)
library(dplyr)

# Agregar la columna de IDs de OtraBaseDatos a Basenueva
#Basenueva$ID <- MicroHabitat_Medidas_A$ID

# Obtener las coordenadas de los individuos en los componentes principales
#coordinates <- data.pca$scores

#------------cluster jerarquico---------------
# Cargar la librería necesaria
library(cluster)

# Realizar clustering jerárquico con el método de linkage completo (complete)
distances <- dist(coordinates_Tam_RA)  # Calcular las distancias entre los individuos
hclust_result_TRA <- hclust(distances, method = "complete")

# Visualizar el dendrograma
plot(hclust_result_TRA, hang = -1, main = "Dendrograma de clustering jerárquico")
hclust_CLUSTER_TRA <- cutree(hclust_result_TRA, k = 4) # Ajusta k según sea necesario
print(hclust_CLUSTER_TRA)
summary(hclust_CLUSTER_TRA)

# Cortar el dendrograma para obtener un número específico de clusters, por ejemplo, 3
rect.hclust(hclust_result_TRA, k = 4, border = 2:4)

# Calcular el índice de silueta
silhouette_info <- silhouette(hclust_CLUSTER_TRA, distances)

# Ver el resumen del índice de silueta
summary(silhouette_info)
#-------------------25. A Especificar el número de clusters--------------------------
k <- 3

# Realizar K-means clustering en las coordenadas de los individuos
set.seed(123)  # Establecer una semilla para reproducibilidad
kmeans_result_RA <- kmeans(coordinates_Tam_RA, centers = k)

# Asignar los clusters a los individuos
MicroHabitat_Tamaño_RA_2_norm$cluster <- hclust_CLUSTER_TRA

library(factoextra)

fviz_cluster(kmeans_result_RA, data = coordinates_Tam_RA, repel = TRUE)

#------------26. A Crear el biplot_A del PCA------------------------
biplot_RA <- fviz_pca_ind(data.pca_norm_RA, geom.ind = "point", habillage = as.factor(MicroHabitat_Tamaño_RA_2_norm$cluster), label = "none")
biplot_RA

#Biplot
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(biplot_RA))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "biplot_RA.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


# Obtener las coordenadas del biplot_A del PCA
biplot_RA_data <- ggplot_build(biplot_RA)$data[[1]]
biplot_RA_data
str(biplot_RA_data)
# Format E_Blouin_A to 3 decimal places
#MicroHabitat_Tamaño_RA_2$E_Blouin_RA <- formatC(MicroHabitat_Tamaño_RA_2$E_Blouin_RA, format = "f", digits = 3)


# Combinar las coordenadas del biplot_A con Basenueva
biplot_RA_data <- biplot_RA_data %>%
  mutate(LT = MicroHabitat_Tamaño_RA_2$LT, E_Blouin_RA = MicroHabitat_Tamaño_RA_2$E_Blouin_RA, cluster = MicroHabitat_Tamaño_RA_2_norm$cluster, Vereda = MicroHabitat_Tamaño_RA$Vereda)

# Contar el número de puntos en cada grupo
counts <- biplot_A_data %>%
  group_by(cluster) %>%
  summarise(count = n())

# Crear una tabla con las coordenadas y el conteo por cluster
label_data <- biplot_A_data %>%
  group_by(cluster) %>%
  summarise(x = mean(x), y = mean(y), count = n())
label_data

library(ggplot2)
library(ggrepel)


# ------------ grafico final Asegurarse de que Vereda y cluster sean factores------------
biplot_RA_data$Vereda <- as.factor(biplot_RA_data$Vereda)
biplot_RA_data$cluster <- as.factor(biplot_RA_data$cluster)

# Definir una paleta de colores para los clusters y una para las veredas
colors_TRA <- c("#EF6548", "#4EB3D3", "#1171B5","#5999C9","#AF087D","#AB281D")  # Colores para los clusters
vereda_shapes <- c(16, 17)  # Valores para las formas de las veredas

#"#FC8D59",  "#CB181D", "#1171B5", "#7BCCC4","#9999C6"

# Redondear LT a un decimal
MicroHabitat_Tamaño_RA$LT <- round(MicroHabitat_Tamaño_RA$LT, 1)
# Format E_Blouin_R to 3 decimal places
MicroHabitat_Tamaño_RA$E_Blouin_RA <- formatC(MicroHabitat_Tamaño_RA$E_Blouin_RA, format = "f", digits = 3)

# Crear el gráfico
GTRA23 <- ggplot(biplot_RA_data, aes(x = x, y = y)) + 
  geom_point(aes(shape = Vereda, color = Vereda), size = 4) + 
  geom_text_repel(
    aes(label = as.character(MicroHabitat_Tamaño_RA$E_Blouin_RA)),
    hjust = 0.5,
    vjust = 1.5,
    box.padding = unit(0.3, "lines"),
    segment.color = "transparent",
    size = 3,
    fontface = "plain"
  ) +
  stat_ellipse(aes(group = cluster, color = cluster), type = "norm", linetype = 2, size = 0.8) + 
  scale_shape_manual("Vereda", values = vereda_shapes) + 
  scale_color_manual("Grupos", values = colors_TRA) + 
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "gray92", color = NA),
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.5),
    text = element_text(size = 8)  # Tamaño general de la fuente
  )

# Mostrar el gráfico
print(GTRA23)


#GTA8
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA23))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA232.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}



#----------------------- 27. A Realizar Anova------------------------------
MicroHabitat_Tamaño_RA_2_norm <- subset(MicroHabitat_Tamaño_RA_2_norm, select = -cluster)
MicroHabitat_Tamaño_RA_2_norm
str(MicroHabitat_Tamaño_RA_2_norm)
MicroHabitat_Tamaño_RA_2_norm$Vereda <- MicroHabitat_Tamaño_RA$Vereda
str(MicroHabitat_Tamaño_RA_2_norm)
# Realizar ANOVA
anova_result_TRA <- aov(LT ~ Vereda, data = MicroHabitat_Tamaño_RA_2_norm)

# Verificar la significancia
summary(anova_result_TRA)


# Prueba de Shapiro-Wilk para normalidad de residuos
shapiro.test(resid(anova_result_TRA))

# Gráfico QQ para normalidad de residuos
qqnorm(resid(anova_result_TRA))
qqline(resid(anova_result_TRA))

# Gráfico de residuos vs. ajustes
plot(anova_result_TRA, 1)

#-----------28. A Ejecutar la prueba de Levene ----------------------------------
library(car)

# Prueba de Levene para homogeneidad de varianzas
leveneTest(LT ~ Vereda, data = MicroHabitat_Tamaño_RA_2_norm)
# Realizar el ANOVA de Welch
welch_result_RA <- oneway.test(LT ~ Vereda, data = MicroHabitat_Tamaño_RA_2_norm, var.equal = FALSE)

# Mostrar los resultados
print(welch_result_RA)



#Tabla ANOVA
{ 
  
  # Creación del dataframe manual
  nueva_tabla_Anova_TRA <- data.frame(
    Analysis = "One-way analysis of means (not assuming equal variances)",
    Data = "LT and Vereda",
    F_value = 36.024,
    num_df = 1.00,
    denom_df = 107.95,
    p_value = "2.654e-08"
  )
  
  
  
  # Mostrar el dataframe
  print(nueva_tabla_Anova_TRA)
  
  library(officer)
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_Anova_TRA <- flextable(nueva_tabla_Anova_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_Anova_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_ttabla_Anova_TRA.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_Anova_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}

#------------29. A Realizar el MANOVA-----------------------
str(MicroHabitat_Tamaño_RA_2_norm)
# Convertir 'Temporada' a factor si no lo es
#MicroHabitat_Tamaño_RA_2_norm$Vereda <- MicroHabitat_Tamaño_A$Vereda
MicroHabitat_Tamaño_RA_2_norm$Vereda <- as.factor(MicroHabitat_Tamaño_RA_2_norm$Vereda)
# Realizar el MANOVA sin 'Temporada' como variable dependiente
resultado_manova_TRA <- manova(cbind(`Peso.(gr)`, LAI, LT) ~ Vereda, data = MicroHabitat_Tamaño_RA_2_norm)

# Resumen del MANOVA
summary(resultado_manova_TRA)
resultado_manova_TRA



#Tabla MANOVA
{ 
  
  # Creación del dataframe manual
  nueva_tabla_Manova_TRA  <- data.frame(
    MANOVA = c("Vereda", "Residuals"),
    Df = c(1, 54),
    Formula = c("cbind(`Peso.(gr)`, LAI, LT) ~ Vereda", NA),  # Incluye la fórmula del MANOVA
    Pillai = c(0.24161, NA),  # El valor de Pillai para 'Residuals' podría ser NA si no está disponible
    approx_F = c(15.0, NA),  # El valor de F para 'Residuals' podría ser NA si no está disponible
    num_Df = c(3, NA),          # El número de grados de libertad para 'Residuals' puede ser NA si no está disponible
    den_Df = c(142 , NA),         # El denominador de grados de libertad para 'Residuals' puede ser NA si no está disponible
    Pr_gt_F = c("1.424e-08" , NA)    # El valor p para 'Residuals' puede ser NA si no está disponible
    
  )
  
  
  
  # Mostrar el dataframe
  print(nueva_tabla_Manova_TRA)
  
  library(officer)
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_Manova_TRA <- flextable(nueva_tabla_Manova_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_Manova_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_Manova_TRA.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_Manova_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}

#install.packages("car")
library(car)


#------------------------disimilitud euclidiana ---------------------------
# Eliminar filas con valores NA
#pasar las variables tipo chare a factor
MicroHabitat_Tamaño_RA_2_norm <- subset(MicroHabitat_Tamaño_RA_2_norm, select = -Vereda)
MicroHabitat_Tamaño_RA_2_norm <- as.data.frame(lapply(MicroHabitat_Tamaño_RA_2_norm, function(x) {
  if (is.character(x)) {
    return(as.factor(x))
  } else {
    return(x)
  }
}))
str(MicroHabitat_Tamaño_RA_2_norm)
MicroHabitat_Tamaño_RA_2_norm_disim <- na.omit(MicroHabitat_Tamaño_RA_2_norm)
MicroHabitat_Tamaño_RA_2_norm_disim
# O imputar valores faltantes (por ejemplo, con la media de la columna)
MicroHabitat_Tamaño_RA_2_norm_disim <- MicroHabitat_Tamaño_RA_2_norm_disim %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Eliminar filas con valores faltantes
MicroHabitat_Tamaño_RA_2_norm_disim <- MicroHabitat_Tamaño_RA_2_norm_disim[complete.cases(MicroHabitat_Tamaño_RA_2_norm_disim), ]
MicroHabitat_Tamaño_RA_2_norm_disim <- subset(MicroHabitat_Tamaño_RA_2_norm_disim, select = -Vereda)
MicroHabitat_Tamaño_RA_2_norm_disim <- subset(MicroHabitat_Tamaño_RA_2_norm_disim, select = -Temporada)
# Calcular la matriz de distancias usando la medida de disimilitud euclidiana
MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean <- dist(MicroHabitat_Tamaño_RA_2_norm_disim)

# Continuar con el análisis...

# Calcular la matriz de distancias usando la medida de disimilitud euclidiana
MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean <- dist(MicroHabitat_Tamaño_RA_2_norm_disim)

# Crear un mapa de calor de la matriz de distancias
heatmap(as.matrix(MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean))

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
dist_matrix_long_TRA <- melt(as.matrix(MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean))

# Crear el heatmap usando ggplot2
GTRA17 <- ggplot(dist_matrix_long_TRA, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Heatmap de la Matriz de Distancia Gower",
       x = "Variable 1",
       y = "Variable 2",
       fill = "Distancia")

# Mostrar el gráfico
print(GTRA17)

#G17
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(GTRA17))
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2" # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA17.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}


# Cargar el paquete vegan
library(vegan)
MicroHabitat_Tamaño_RA_2_norm_disim
# ---------Verificar la homogeneidad de las dispersiones---------------
MicroHabitat_Tamaño_RA_2_norm_disim$Vereda <- MicroHabitat_Tamaño_RA$Vereda
MicroHabitat_Tamaño_RA_2_norm_disim
dispersion_euclidean_MicroHabitat_Tamaño_RA_2_norm <- betadisper(MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean, MicroHabitat_Tamaño_RA_2_norm_disim$Vereda)
anova(dispersion_euclidean_MicroHabitat_Tamaño_RA_2_norm)
plot(dispersion_euclidean_MicroHabitat_Tamaño_RA_2_norm)
dispersion_euclidean_MicroHabitat_Tamaño_RA_2_norm
#G18

{
  # Cargar las librerías necesarias
  library(ggplot2)
  library(officer)
  library(rvg)
  library(vegan)
  library(ggalt)  # Para geom_encircle
  library(grid)
  library(dplyr)
  
  # Realizar la dispersión con betadisper
  dist_matrix_euclidean_TRA <- betadisper(MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean, MicroHabitat_Tamaño_RA_2_norm_disim$Vereda)
  
  # Realizar ANOVA para la dispersión
  anova(dist_matrix_euclidean_TRA)
  
  # Extraer datos de betadisper para ggplot2
  df_dispersion_TRA <- as.data.frame(dist_matrix_euclidean_TRA$vectors)
  df_dispersion_TRA$Group <- factor(dist_matrix_euclidean_TRA$group)
  
  # Calcular los centroides
  centroids_TA <- aggregate(df_dispersion_TA[,1:2], list(Group = df_dispersion_TA$Group), mean)
  names(centroids_TR)[2:3] <- c("PCoA1", "PCoA2")
  centroids_TR
  # Crear el gráfico ggplot2 con colores personalizados, formas y elipses
  GTA18 <- ggplot(df_dispersion_TA, aes(x = PCoA1, y = PCoA2, color = Group, shape = Group)) +
    geom_point(size = 3) +
    geom_point(data = centroids_TA, aes(x = PCoA1, y = PCoA2, color = Group, shape = Group), size = 5) +
    geom_encircle(aes(x = PCoA1, y = PCoA2, group = Group, color = Group), size = 2, alpha = 0.7) +
    scale_color_manual(values = c("1" =  "#4EB3D3", "2" = "#EF6548")) +
    scale_shape_manual(values = c("1" = 17, "2" = 19)) +
    theme_minimal(base_family = "sans") +  # Utiliza una fuente disponible en Windows
    theme(
      
      panel.background = element_rect(fill = "grey92", color = NA),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white")
    ) +
    labs(title = "Beta Dispersion Analysis",
         x = "PCoA Axis 1",
         y = "PCoA Axis 2")
  
  # Mostrar el gráfico G18
  print(GTR18)
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2"  # Reemplaza con la ruta de tu carpeta
  file_name <- "GTR18.pptx"  # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint
  doc <- read_pptx()
  
  # Crear un objeto DML a partir del gráfico ggplot2
  dml_ga18 <- rvg::dml(ggobj = GTR18)
  
  # Añadir una diapositiva con el gráfico DML
  doc <- doc %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(dml_gt18, location = ph_location_fullsize())
  
  # Guardar el documento PowerPoint
  print(doc, target = output_path)
  
  
}


# -----------Realizar ANOSIM (sin covariable)---------------------
anosim_result_euclidean_TRA <- anosim(MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean, MicroHabitat_Tamaño_RA_2_norm_disim$Vereda, , permutations = 999)
summary(anosim_result_euclidean_TRA)
plot(anosim_result_euclidean_TRA)

#G19

{# Cargar las librerías necesarias
  library(ggplot2)
  library(officer)
  library(rvg)
  library(vegan)
  
  
  print(anosim_result_euclidean_TRA)
  
  # Extraer los datos necesarios para ggplot2
  anosim_data_TRA <- data.frame(
    Dissimilarity = anosim_result_euclidean_TRA$dis.rank,
    Group = anosim_result_euclidean_TRA$class.vec
  )
  
  
  
  # Crear el gráfico ggplot2 con colores personalizados y cuadrícula blanca
  GTRA19 <- ggplot(anosim_data_TRA, aes(x = Group, y = Dissimilarity, fill = Group)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#8BCCC4", "#EF6548", "#4EB3D3")) +  # Cambia los colores según tus necesidades
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "grey92", color = NA),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white")
    ) +
    labs(title = "ANOSIM Dissimilarity Ranks TR   R= 0.1564, P = 0.001",
         x = "Group",
         y = "Dissimilarity Rank")
  
  
  GTRA19
  
  #"Lluvia" = "#4EB3D3", "Seco" = "#EF6548"
  
  # Especificar la carpeta y el nombre del archivo
  folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2"  # Reemplaza con la ruta de tu carpeta
  file_name <- "GTRA19.pptx"  # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint
  doc <- read_pptx()
  
  # Crear un objeto DML a partir del gráfico ggplot2
  dml_gta19 <- rvg::dml(ggobj = GTRA19)
  
  # Añadir una diapositiva con el gráfico DML
  doc <- doc %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(dml_gta19, location = ph_location_fullsize())
  
  # Guardar el documento PowerPoint
  print(doc, target = output_path)
  
}

MicroHabitat_Tamaño_RA
MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean
#--------- Realizar PERMANOVA incluyendo la covariable si está disponible en el dataframe----------
if("Covariate" %in% colnames(MicroHabitat_Tamaño_RA_2_norm)) {
  adonis_result_euclidean_TRA <- adonis2(MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean ~ E_Blouin_RA + Vereda + Temporada, data = MicroHabitat_Tamaño_RA_2_norm_disim)
} else {
  adonis_result_euclidean_TRA <- adonis2(MicroHabitat_Tamaño_RA_2_norm_dist_matrix_euclidean ~ Vereda, data = MicroHabitat_Tamaño_RA_2_norm_disim)
}
summary(adonis_result_euclidean_TRA)
print(adonis_result_euclidean_TRA)

MicroHabitat_Tamaño_RA_2_norm_disim$Temporada <- MicroHabitat_Tamaño_RA$Temporada
MicroHabitat_Tamaño_RA_2_norm_disim$Vereda <- MicroHabitat_Tamaño_RA$Vereda
MicroHabitat_Tamaño_RA_2_norm_disim$E_Blouin_RA <- MicroHabitat_Tamaño_RA$E_Blouin_RA
str(MicroHabitat_Tamaño_RA_2_norm_disim)

#Tabla G20
{ adonis_result_euclidean_TRA <- data.frame(
  Term = c( "E_Blouin_RA","Vereda", "Temporada", "Residual"),
  Df = c(1, 1, 1, 142),
  SumOfSqs = c(82.92, 210.10, 1.23, 1010.75),
  R2 = c(0.06354, 0.16099, 0.00094, 0.77452),
  F = c(11.6497, 29.5162, 0.1726, NA),
  Pr_F = c(0.002, 0.001, 0.764, NA),
  Significance = c("**", "***", "", NA),
  stringsAsFactors = FALSE
)
  
  print(adonis_result_euclidean_TRA)
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_Permanova_TRA <- flextable(adonis_result_euclidean_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_Permanova_TRA
  
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_permanova_TEAG20.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_Permanova_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}


#-----------30. A Ejecutar modelo lineal ----------------------------------
#Crear dataframe normalizado para las varaibles de la regresion 
#Crear un dataframe nuevo sin las variables tipo chare
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA, select = -Vereda)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -Temporada)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -ID)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -Temperatura.Corporal)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -label_color)
str(MicroHabitat_Tamaño_RA_2.1)
# Ejemplo de aplicación a variables_continuas_A
MicroHabitat_Tamaño_RA_norm_robus <- as.data.frame(lapply(MicroHabitat_Tamaño_RA_2.1, robust_standardize))
MicroHabitat_Tamaño_RA_norm_robus
MicroHabitat_Tamaño_RA_norm_robus$Temporada <- MicroHabitat_Tamaño_RA$Temporada
str(MicroHabitat_Tamaño_RA_norm_robus)
# Convertir a factor
MicroHabitat_Tamaño_RA_norm_robus$Vereda <- as.factor(MicroHabitat_Tamaño_RA_norm_robus$Vereda)
MicroHabitat_Tamaño_RA_norm_robus$Temporada <- as.factor(MicroHabitat_Tamaño_RA_norm_robus$Temporada)

#Convertir a factor
MicroHabitat_Tamaño_RA$Vereda <- as.factor(MicroHabitat_Tamaño_RA$Vereda)
MicroHabitat_Tamaño_RA$Temporada <- as.factor(MicroHabitat_Tamaño_RA$Temporada)

# Estadísticas descriptivas
library(dplyr)

MicroHabitat_Tamaño_RA %>%
  group_by(Vereda) %>%
  summarise(mean_E_Blouin_RA = mean(E_Blouin_RA), sd_E_Blouin_RA = sd(E_Blouin_RA))

MicroHabitat_Tamaño_RA %>%
  group_by(Temporada) %>%
  summarise(mean_E_Blouin_RA = mean(E_Blouin_RA), sd_E_Blouin_RA = sd(E_Blouin_RA))

# Estadísticas descriptivas
library(dplyr)

MicroHabitat_Tamaño_RA_norm_robus %>%
  group_by(Vereda) %>%
  summarise(mean_E_Blouin_RA = mean(E_Blouin_RA), sd_E_Blouin_RA = sd(E_Blouin_RA))

MicroHabitat_Tamaño_RA_norm_robus %>%
  group_by(Temporada) %>%
  summarise(mean_E_Blouin_RA = mean(E_Blouin_RA), sd_E_Blouin_RA = sd(E_Blouin_RA))


# No hay diferencias entre el modelo lineal simple y generalizado 
# Se debe hacer un modelo lineal con todas las variables identificar las variables mas explicativas
# En este caso el LT y de importancia como la temporada 
# stepwise NOS SIRVE PARA SELECCIONAR VARIABLES CON EL VALOR ACAIKE 
# EN GENERAL NO CAMBIA Y SI HAY RELACION DE TAMAÑO Y EFICIENCIA EN LA TERMORREGULACION 
# RELACION POSITIVA MEJORA E EN UN 0.26968  POR CADA UNA UNIDAD QUE AUMENTA EL TAMAÑO 

# Instalar y cargar el paquete lmtest si no está instalado
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)

# Ajustar el modelo de regresión
str(MicroHabitat_Tamaño_RA)

MicroHabitat_Tamaño_RA_2_norm$Vereda <- MicroHabitat_Tamaño_RA$Vereda
MicroHabitat_Tamaño_RA_2_norm$E_Blouin_RA <- MicroHabitat_Tamaño_RA$E_Blouin_RA
MicroHabitat_Tamaño_RA_2_norm$E_Blouin_RA <- as.numeric(MicroHabitat_Tamaño_RA_2_norm$E_Blouin_RA)

str(MicroHabitat_Tamaño_RA_norm_robus)

modelo_ml_TRA <- lm(E_Blouin_RA ~ Vereda + LT, data = MicroHabitat_Tamaño_RA_norm_robus)
modelo_ml_TRA
summary(modelo_ml_TRA)


modelo_ml_TRA.1 <- lm(E_Blouin_RA ~ Vereda + LT, data = MicroHabitat_Tamaño_RA)
modelo_ml_TRA.1
summary(modelo_ml_TRA.1)
MicroHabitat_Tamaño_RA
# Realizar la prueba de Breusch-Pagan
bptest(modelo_ml_TRA.1)

# Instalar y cargar el paquete necesario
install.packages("sandwich")
library(sandwich)
library(lmtest)
# Calcular errores estándar robustos
robust_se_TRA <- vcovHC(modelo_ml_TRA.1, type = "HC1")
modelo_mlrobust_TRA <- coeftest(modelo_ml_TRA.1, robust_se_TRA)
modelo_mlrobust_TRA
#Tabla modelo lineal simple
{ 
  nueva_tabla_mlrobust_TRA <- data.frame(
    Modelo = c( "", "", "", "E_Blouin_A ~ Vereda + LT"),
    Coeficiente = c("(Intercept)", "VeredaRemansos", "LT",""),
    Estimate = c(-0.1019405, -0.0041187, 0.1787720, ""),
    Std_Error = c(0.0442347, 0.0634871, 0.0636938, ""),
    t_value = c("-2.3045", "-0.0649", "2.8067", ""),
    p_value = c(0.022632, 0.948364, 0.005703, ""),
    Significance = c("*", "", "**", "")
  )
  
  
  
  
  # Mostrar el dataframe
  print(nueva_tabla_mlrobust_TRA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_mlrobust_TRA <- flextable(nueva_tabla_mlrobust_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_mlrobust_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_mlrobust_TRA.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_mlrobust_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}



# Ajustar el modelo lineal No muy funcional 
#modelo_lineal_Tamaño_TRA <- lm(E_Blouin_RA  ~ `Peso..gr.` + LCA + AC + LHC + Vereda +
#       Lto + LAI + LT +  LPT + LCO, data = MicroHabitat_Tamaño_RA_2_norm)
#modelo_lineal_Tamaño_RA
#summary(modelo_lineal_Tamaño_RA)
#----------31. A Calcular VIF para verificar la multicolinealidad lm---------------
library(car)
vif(modelo_ml_TRA.1)


#-------32. A Ajustar el ml------------------------
#Crear dataframe normalizado para las varaibles de la regresion 
#Crear un dataframe nuevo sin las variables tipo chare
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA, select = -Vereda)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -Temporada)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -ID)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -Temperatura.Corporal)
MicroHabitat_Tamaño_RA_2.1 <- subset(MicroHabitat_Tamaño_RA_2.1, select = -label_color)
str(MicroHabitat_Tamaño_RA_2.1)
# Ejemplo de aplicación a variables_continuas_A
MicroHabitat_Tamaño_RA_norm_robus <- as.data.frame(lapply(MicroHabitat_Tamaño_RA_2.1, robust_standardize))
MicroHabitat_Tamaño_RA_norm_robus
MicroHabitat_Tamaño_RA_norm_robus$Vereda <- MicroHabitat_Tamaño_RA$Vereda
str(MicroHabitat_Tamaño_RA_norm_robus)

# Calcular la desviación estándar de la columna E_Blouin_RA
desviacion_estandar <- sd(MicroHabitat_Tamaño_RA$LT)

# Imprimir el resultado
print(desviacion_estandar)


#------------33. A Ajustar un modelo con términos de interacción---------------------------
# simplificar el modelo seleccionando solo las variables significativas

modelo_interacciones_TRA <- lm(E_Blouin_RA  ~ `Peso.(gr)` + LCA + AC + LHC + Vereda +
                                 Lto + LAI + LT + 
                                 LPT + LCO, data = MicroHabitat_Tamaño_RA)
summary(modelo_interacciones_TRA)

modelo_stepwise_interacciones_TRA <- step(modelo_interacciones_TRA)
summary(modelo_stepwise_interacciones_TRA)


modelo_interacciones_TRA1 <- lm(LT  ~ AC + Vereda, data = MicroHabitat_Tamaño_RA)
summary(modelo_interacciones_TRA1)

modelo_stepwise_interacciones_TRA1 <- step(modelo_interacciones_TRA1)
summary(modelo_stepwise_interacciones_TRA1)



modelo_interacciones_TRA2 <- lm(E_Blouin_RA  ~ Vereda + LT , data = MicroHabitat_Tamaño_RA)
summary(modelo_interacciones_TRA2)

vif(modelo_interacciones_TRA2)

modelo_stepwise_interacciones_TRA2 <- step(modelo_interacciones_TRA2)
summary(modelo_stepwise_interacciones_TRA2)

library(lme4)
modelo_mixto <- lmer(E_Blouin_RA ~ 1 + LT + (1 | Vereda), data = MicroHabitat_Tamaño_RA)
summary(modelo_mixto)

#Tabla modelo con términos de interacción
{ 
  
  nueva_tabla_ml_interacciones2_TRA <- data.frame(
    Coefficient = c("(Intercept)", "LT"),
    Estimate = c("-0.775346", "0.009550"),
    Std_Error = c("0.199108", "0.002738"),
    t_value = c("-3.894", "3.487"),
    p_value = c("0.000150", "0.000647"),
    Significance = c("***", "***")
  )
  
  
  
  
  # Mostrar el dataframe
  print(nueva_tabla_ml_interacciones2_TRA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_ml_interacciones2_TRA <- flextable(nueva_tabla_ml_interacciones2_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_ml_interacciones2_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_ml_interacciones2_TRA.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_ml_interacciones2_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}


#-------------35. A Realizar diagnósticos del modelo----------------------------
par(mfrow = c(2, 2))
plot(modelo_stepwise_interacciones_TRA2)
par(mfrow = c(1, 1))
# Instalar y cargar paquetes necesarios
#install.packages("lmtest")
#install.packages("car")
#install.packages("MASS")

library(lmtest)
library(car)
library(MASS)

# Prueba de Breusch-Pagan para homocedasticidad
bptest(modelo_stepwise_interacciones_TRA2)

# Prueba de Shapiro-Wilk para normalidad
shapiro.test(modelo_stepwise_interacciones_TRA2$residuals)

# Prueba de Durbin-Watson para autocorrelación
dwtest(modelo_stepwise_interacciones_TRA2)

#install.packages("caret")
library(caret)

# Configuración de la validación cruzada
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)

# Ajuste del modelo usando validación cruzada
cv_model <- train(E_Blouin_RA ~ LT, data = MicroHabitat_Tamaño_RA, method = "lm", trControl = train_control)

# Resultados de la validación cruzada
print(cv_model)



# Ajuste del modelo de regresión lineal
modelo_lineal <- lm(E_Blouin_RA ~ LT + Vereda, data = MicroHabitat_Tamaño_RA)

# Resumen del modelo
summary(modelo_lineal)

# Gráfico de dispersión con línea de regresión
plot(MicroHabitat_Tamaño_RA$LT, MicroHabitat_Tamaño_RA$E_Blouin_RA, 
     main = "Relación entre LT y E_Blouin_RA", 
     xlab = "Longitud Total (LT)", 
     ylab = "Eficiencia en la Termorregulación (E_Blouin_RA)",
     pch = 19, col = as.factor(MicroHabitat_Tamaño_RA$Vereda))

abline(modelo_lineal, col = "red")

# Añadir leyenda
MicroHabitat_Tamaño_RA$Vereda <- as.factor(MicroHabitat_Tamaño_RA$Vereda)


legend("topleft", legend = levels(MicroHabitat_Tamaño_RA$Vereda), 
       col = 1:length(levels(MicroHabitat_Tamaño_RA$Vereda)), 
       pch = 19)

library(ggplot2)

# Asegurarse de que 'Vereda' sea un factor
MicroHabitat_Tamaño_RA$Vereda <- as.factor(MicroHabitat_Tamaño_RA$Vereda)

# Ajuste del modelo de regresión lineal
modelo_lineal <- lm(E_Blouin_RA ~ LT + Vereda, data = MicroHabitat_Tamaño_RA)

# Resumen del modelo
summary(modelo_lineal)
MicroHabitat_Tamaño_RA
# Crear el gráfico de dispersión con ggplot2
GTRA30 <- ggplot(MicroHabitat_Tamaño_RA, aes(x = LT, y = E_Blouin_RA, color = Vereda, shape = Vereda)) +
  geom_point(size = 3) +  # Tamaño de los puntos
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "red") +  # Línea de regresión
  scale_color_manual(values = c("Remansos" = "#4EB3D3", "Asiria " = "#EF6548")) +  # Colores personalizados
  scale_shape_manual(values = c("Remansos" = 17, "Asiria " = 16)) +  # Formas personalizadas
  labs(title = "Relación entre LT y E_Blouin_RA",
       x = "Longitud Total (LT)",
       y = "Eficiencia en la Termorregulación (E_Blouin_RA)") +
  theme_minimal() +
  theme(legend.position = "topleft")

GTRA30





# Especificar la carpeta y el nombre del archivo
folder_path <- "D:/Resultados B.tamaense/Graficos _OBJ2"  # Reemplaza con la ruta de tu carpeta
file_name <- "GTRA30.pptx"  # Reemplaza con el nombre del archivo que desees

# Crear la ruta completa del archivo
output_path <- file.path(folder_path, file_name)

# Crear un nuevo documento PowerPoint
doc <- read_pptx()

# Crear un objeto DML a partir del gráfico ggplot2
dml_gta19 <- rvg::dml(ggobj = GTRA30)

# Añadir una diapositiva con el gráfico DML
doc <- doc %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with(dml_gta19, location = ph_location_fullsize())

# Guardar el documento PowerPoint
print(doc, target = output_path)


# Validación del modelo
par(mfrow = c(2, 2))  # Configurar ventana de gráficos para mostrar 4 gráficos
plot(modelo_lineal)
par(mfrow = c(1, 1))  # Restaurar configuración de ventana de gráficos



#-------------------método Theil-Sen--------------------------



# Instalar y cargar el paquete mblm
#install.packages("mblm")
library(mblm)

# Tus datos

str(data_tamaño_RA)

# Seleccionar las variables numéricas y la variable dependiente
numeric_vars_TRA <- data_tamaño_RA[, sapply(data_tamaño_RA, is.numeric)]
dependent_var_TRA <- data_tamaño_RA$E_Blouin_RA
dependent_var_TRA <- as.vector(dependent_var_TRA)

# Aplicar el método Theil-Sen a cada variable numérica
models_TRA <- lapply(names(data_tamaño_RA$LT), function(var) {
  formula <- as.formula(paste("dependent_var ~", var))
  mblm(formula, data = data.frame(data_tamaño_RA$E_Blouin_RA, data_tamaño_RA$LT[, var, drop = FALSE]))
})

# Mostrar los resúmenes de cada modelo
lapply(models_TRA, summary)



# Mostrar el resumen del modelo
summary(models_TRA)



#--------------------36. A MGL para Eficiencia en la termorregulcion-------------

class(MicroHabitat_Tamaño_RA$E_Blouin_RA)

MicroHabitat_Tamaño_RA$E_Blouin_RA
# - para evidencias las relación de las variables mas importantes con la termorregulación.


str(MicroHabitat_Tamaño_RA_2_norm)
#MicroHabitat_Tamaño_A_2_norm$Temporada <- MicroHabitat_Tamaño_A$Temporada
#MicroHabitat_Tamaño_A_2_norm$Temperatura.Corporal <- MicroHabitat_Tamaño_A$Temperatura.Corporal
# Cargar las librerías necesarias
library(car)
library(MASS)
# Ajustar los datos para correr el mgl
data_tamaño_RA <- MicroHabitat_Tamaño_RA
MicroHabitat_Tamaño_A
# Inspeccionar los datos
str(data_tamaño_RA)
summary(data_tamaño_RA)

# Verificar los valores mínimos de E_Blouin_A
min_value_tamaño <- min(data_tamaño_RA$E_Blouin_RA)
print(min_value_tamaño)

# Si hay valores no positivos, agregar una constante positiva para asegurarlos
if (min_value <= 0) {
  data_tamaño_RA$E_Blouin_RA <- data_tamaño_RA$E_Blouin_RA - min_value + 2
}


# Ver la estructura de los datos
str(data_tamaño_RA)
data_tamaño_RA$E_Blouin_RA
#-------37. A Ajustar el glm------------------------
glm_model_Tamaño_RA  <- glm(E_numeric_RA ~  LT + Vereda,  
                            data = data_tamaño_RA, family = gaussian(link = "identity"))

glm_model_Tamaño_RA
summary(glm_model_Tamaño_RA)

data_tamaño_RA$Temperatura.Corporal <- (data_tamaño_RA$Temperatura.Corporal)
data_tamaño_RA2 <- subset(data_tamaño_RA, select = -Vereda)
data_tamaño_RA2 <- subset(data_tamaño_RA2, select = -Temporada)
data_tamaño_RA2 <- subset(data_tamaño_RA2, select = -ID)
data_tamaño_RA2 <- subset(data_tamaño_RA2, select = -E_numeric_RA)
data_tamaño_RA2 <- subset(data_tamaño_RA2, select = -Temperatura.Corporal)
data_tamaño_RA2 <- subset(data_tamaño_RA2, select = -label_color)
data_tamaño_RA2 <- subset(data_tamaño_RA2, select = -E_Blouin_RA_log)
glm_model_Tamaño_RA  <- glm(E_Blouin_RA ~  .,  
                            data = data_tamaño_RA2, family = gaussian(link = "identity"))

glm_model_Tamaño_RA
summary(glm_model_Tamaño_RA)

# Ejemplo de modelo Gamma en R
library(MASS)
#install.packages("ResourceSelection")
library(ResourceSelection)
# Ajustar el modelo GLM con distribución Gamma
model_GLM_gamma_TRA <- glm(E_Blouin_RA ~ LT, family = Gamma(link = "identity"), data = data_tamaño_RA2)

# Resumen del modelo
summary(model_GLM_gamma_TRA)

# Pseudo R^2 de McFadden
null_deviance <- model_GLM_gamma_TRA$null.deviance
residual_deviance <- model_GLM_gamma_TRA$deviance
pseudo_r2 <- 1 - (residual_deviance / null_deviance)
cat("Pseudo R^2: ", pseudo_r2, "\n")

# Gráfico de residuos
par(mfrow = c(2, 2))  # Para mostrar múltiples gráficos
plot(model_GLM_gamma_TRA)


# Residuos estandarizados
residuals_std <- rstandard(model_GLM_gamma_TRA)
par(mfrow = c(1, 1))
# Gráfico de residuos estandarizados vs. valores ajustados
plot(fitted(model_GLM_gamma_TRA), residuals_std, 
     main = "Residuos estandarizados vs. valores ajustados",
     xlab = "Valores ajustados", 
     ylab = "Residuos estandarizados")
abline(h = 0, col = "red")

# Histograma de residuos estandarizados
hist(residuals_std, breaks = 20, main = "Histograma de residuos estandarizados")

# Q-Q plot de residuos estandarizados
qqnorm(residuals_std, main = "Q-Q plot de residuos estandarizados")
qqline(residuals_std, col = "red")

# Prueba de bondad de ajuste (test de Chi-cuadrado)
library(ResourceSelection)
hoslem.test(data_tamaño_RA$E_Blouin_RA, fitted(model_GLM_gamma_TRA), g = 10)



#----------38. A Calcular VIF para verificar la multicolinealidad glm---------------
library(car)
vif(glm_model_Tamaño_RA)

#----------39. A Realizar selección de variables mediante stepwise glm--------------
modelo_stepwis_mgl_RA <- step(model_GLM_gamma_TRA)
summary(modelo_stepwis_mgl_RA)



#-------------40. A Realizar diagnósticos del modelo glm----------------------------
par(mfrow = c(2, 2))
plot(modelo_stepwis_mgl_A)

# Instalar y cargar el paquete caret si no está instalado
if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

# Configuración de la validación cruzada
train_control <- trainControl(method = "cv", number = 10)

# Realizar validación cruzada
model_cv_TRA <- train(E_Blouin_RA ~  LT, data = data_tamaño_RA, method = "glm", family = Gamma(link = "identity"), trControl = train_control)
print(model_cv_TRA)
summary(model_cv_TRA)
# Imprimir resultados
print(model_cv_TRA)

# Interpretar los resultados
cat("RMSE: ", model_cv_TRA$results$RMSE, "\n")
cat("R-squared: ", model_cv_TRA$results$Rsquared, "\n")
cat("MAE: ", model_cv_TRA$results$MAE, "\n")


# Dividir el dataframe por la variable Vereda
subdata_Asiria <- subset(data_tamaño_RA, Vereda == "Asiria ")
subdata_Remansos <- subset(data_tamaño_RA, Vereda == "Remansos")


str(data_tamaño_RA)
# Cargar las librerías necesarias
library(caret)
library(ResourceSelection)

# Establecer semilla para reproducibilidad
set.seed(123)

# Definir control de validación cruzada
train_control <- trainControl(method = "cv", number = 10)

# Ajustar el modelo GLM con validación cruzada
model_cv_TRA <- train(Temperatura.Corporal ~ LT, 
                      data = subdata_Remansos,
                      method = "glm", 
                      family = Gamma(link = "identity"), 
                      trControl = train_control)

# Imprimir resultados
print(model_cv_TRA)

# Resumen del modelo ajustado
summary(model_cv_TRA)

# Pseudo R-squared de McFadden
null_deviance <- model_cv_TRA$finalModel$null.deviance
residual_deviance <- model_cv_TRA$finalModel$deviance
pseudo_r2 <- 1 - (residual_deviance / null_deviance)
cat("Pseudo R^2: ", pseudo_r2, "\n")

# Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(model_cv_TRA$finalModel)

# Residuos estandarizados
residuals_std <- rstandard(model_cv_TRA$finalModel)

# Gráfico de residuos estandarizados vs. valores ajustados
plot(fitted(model_cv_TRA$finalModel), residuals_std, 
     main = "Residuos estandarizados vs. valores ajustados",
     xlab = "Valores ajustados", 
     ylab = "Residuos estandarizados")
abline(h = 0, col = "red")

# Histograma de residuos estandarizados
hist(residuals_std, breaks = 20, main = "Histograma de residuos estandarizados")

# Q-Q plot de residuos estandarizados
qqnorm(residuals_std, main = "Q-Q plot de residuos estandarizados")
qqline(residuals_std, col = "red")

# Gráfico de Cook's Distance para identificar puntos influyentes
plot(cooks.distance(model_cv_TRA$finalModel), 
     main = "Cook's Distance", 
     xlab = "Observación", 
     ylab = "Cook's Distance")
abline(h = 4/(nrow(data_tamaño_RA)-length(model_cv_TRA$finalModel$coefficients)-1), col = "red")

# Prueba de bondad de ajuste (test de Chi-cuadrado)
hoslem.test(data_tamaño_RA$E_Blouin_RA, fitted(model_cv_TRA$finalModel), g = 10)




#Tabla mgl inicial  
{ 
  
  nueva_tabla_GLM_gamma_TRA <- data.frame(
    
    Variable = c("(Intercept)", "LT","Pseudo R-squared"),
    Estimate = c("52.99146", "0.00989", "0.04444"),
    Std_Error = c("0.20342", "0.00287",NA),
    t_value = c("14.706", "3.446",NA),
    p_value = c("< 2e-16", "0.000745", NA),
    Significance = c("***", "***", NA))
  
  # Mostrar el dataframe
  print(nueva_tabla_GLM_gamma_TRA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_GLM_gamma_TRA <- flextable(nueva_tabla_GLM_gamma_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_GLM_gamma_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_GLM_gamma_TRAZ.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_GLM_gamma_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
  
}

#Tabla mgl remansos inicial  
{ 
  
  nueva_tabla_GLM_gamma_TRA <- data.frame(
    
    Variable = c("(Intercept)", "LT","Pseudo R-squared"),
    Estimate = c("2.6118", "0.01587", "0.0597"),
    Std_Error = c("0.33504", "0.005292",NA),
    t_value = c("7.795", "3.000",NA),
    p_value = c("< 1.21e-11", "0.00351", NA),
    Significance = c("***", "**", NA))
  
  # Mostrar el dataframe
  print(nueva_tabla_GLM_gamma_TRA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_GLM_gamma_TRA <- flextable(nueva_tabla_GLM_gamma_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_GLM_gamma_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_GLM_gamma_TR222.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_GLM_gamma_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
  
}




# Cargar el paquete si no está instalado
# install.packages("ggplot2")
library(ggplot2)

# Ajustar modelo de regresión local (loess)
modelo_loess <- loess(E_Blouin_RA ~ LT, data = MicroHabitat_Tamaño_RA)

# Generar predicciones para plot
predicciones_loess <- predict(modelo_loess)

# Graficar resultados
ggplot(MicroHabitat_Tamaño_RA, aes(x = LT, y = E_Blouin_RA)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Regresión Local (Loess)", x = "LT", y = "E_Blouin_RA")



# Instalar y cargar el paquete kknn si no está instalado
# install.packages("kknn")
library(kknn)

# Dividir los datos en conjunto de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
indices <- sample(1:nrow(MicroHabitat_Tamaño_RA), nrow(MicroHabitat_Tamaño_RA) * 0.8)  # Ejemplo de división 80-20
train_data <- MicroHabitat_Tamaño_RA[indices, ]
test_data <- MicroHabitat_Tamaño_RA[-indices, ]

# Ajustar modelo de regresión k-NN correctamente
modelo_knn <- kknn(E_Blouin_RA ~ LT, train = train_data, test = test_data, kernel = "gaussian", k = 10)

# Predicciones del modelo k-NN en datos de prueba
predicciones_knn <- fitted(modelo_knn)
summary(modelo_knn)
predicciones_knn
# Graficar resultados (ejemplo de gráfico de puntos)
par(mfrow = c(1, 1)) 
plot(test_data$LT, test_data$E_Blouin_RA, main = "Regresión k-NN (Test)", xlab = "LT", ylab = "E_Blouin_RA")
points(test_data$LT, predicciones_knn, col = "red")

# Calcular métricas de evaluación del rendimiento (ejemplo: RMSE)
rmse <- sqrt(mean((predicciones_knn - test_data$E_Blouin_RA)^2))
print(paste("RMSE:", rmse))



install.packages("mblm")
install.packages("robustbase")
library(mblm)
library(robustbase)

MicroHabitat_Tamaño_RA$E_Blouin_RA <- as.numeric(MicroHabitat_Tamaño_RA$E_Blouin_RA )
MicroHabitat_Tamaño_RA <- as.data.frame(lapply(MicroHabitat_Tamaño_RA, function(x) {
  if (is.character(x)) {
    return(as.factor(x))
  } else {
    return(x)
  }
}))

# Ajustar el modelo Theil-Sen
modelo_theilsen <- mblm(E_Blouin_RA ~ LT, data = data_tamaño_RA)

# Resumen del modelo
summary(modelo_theilsen)


str(data_tamaño_RA)


# Ajustar el modelo de regresión robusta con variables categóricas
modelo_robusto.2 <- lmrob(E_Blouin_RA ~ LT * Vereda, data = data_tamaño_RA)

# Resumen del modelo
summary(modelo_robusto.2)















library(caret)
library(ResourceSelection)

# Establecer semilla para reproducibilidad
set.seed(123)

# Definir control de validación cruzada
train_control <- trainControl(method = "cv", number = 10)

# Dividir el dataframe por la variable Vereda
subdataframes <- split(data_tamaño_RA, data_tamaño_RA$Vereda)

# Función para ajustar el modelo GLM y calcular el pseudo R²
ajustar_modelo_glm <- function(data) {
  # Ajustar el modelo GLM con validación cruzada
  modelo_cv <- train(E_Blouin_RA ~ LT, 
                     data = data, 
                     method = "glm", 
                     family = Gamma(link = "identity"), 
                     trControl = train_control)
  
  # Imprimir resultados del modelo
  print(modelo_cv)
  
  # Resumen del modelo ajustado
  summary(modelo_cv)
  
  # Calcular el pseudo R-squared de McFadden
  null_deviance <- modelo_cv$finalModel$null.deviance
  residual_deviance <- modelo_cv$finalModel$deviance
  pseudo_r2 <- 1 - (residual_deviance / null_deviance)
  
  cat("Pseudo R^2: ", pseudo_r2, "\n")
  
  return(pseudo_r2)
}

# Aplicar el análisis a cada subgrupo
pseudo_r2_Asiria <- ajustar_modelo_glm(subdataframes$Asiria)
pseudo_r2_Remansos <- ajustar_modelo_glm(subdataframes$Remansos)

cat("Pseudo R^2 para Asiria: ", pseudo_r2_Asiria, "\n")
cat("Pseudo R^2 para Remansos: ", pseudo_r2_Remansos, "\n")





#-------------------Relacion de la LT y las variables del habitat------------------
#-------49 RA Ajustar el modelo inicial------------------------

data_completo$LT <- data_RA$LT
data_completo <- subset(data_completo, select = -LT)


glm_model_RA <- glm(LT ~ ., 
                    data = data_completo, family = Gamma(link = "log"))


#-------50 RA Resumen del modelo inicial---------------
summary(glm_model_RA)

# ------51 RA Verificación de multicolinealidad--------------
vif(glm_model_RA)

# ------52 RA Simplificación del modelo usando stepAIC-------------------
simplified_glm_RA <- stepAIC(glm_model_RA, direction = "both")

# Resumen del modelo simplificado
summary(simplified_glm_RA)

# Supongamos que tu modelo GLM se llama simplified_glm_RA
# Primero necesitamos la devianza del modelo ajustado y del modelo nulo
null_deviance <- 11.7149
residual_deviance <- 7.5078

# Calcular el Pseudo R-squared de McFadden
pseudo_r_squared <- 1 - (residual_deviance / null_deviance)

# Imprimir el resultado
print(pseudo_r_squared)



# Crear el dataframe con los coeficientes del modelo y la significancia
simplified_glm_RA_summary <- data.frame(
  Term = c("(Intercept)", "Temperatura.Sustrato", "Cobertura.Dosel", "E_Blouin_RA", 
           "Tipo.Sustrato_Tallo", "Tipo.Crecimiento_Arboles", 
           "Tipo.Crecimiento_Helecho arborescente", "Tipo.Crecimiento_Hierbas epifitas", 
           "Nubosidad_No", "Tipo.Crecimiento_Helecho terrestres","Pseudo R-squared"),
  Estimate = c(2.741738, -0.029343, 0.014356, 0.097562, 0.175007, 0.097520, -0.336990, 
               -0.269639, 0.131638, 0.223738, 0.3592239),
  Std_Error = c(0.947529, 0.008590, 0.009683, 0.029570, 0.050631, 0.053226, 0.131032, 
                0.159249, 0.037454, 0.158009, NA),
  t_value = c(2.894, -3.416, 1.483, 3.299, 3.456, 1.832, -2.572, -1.693, 3.515, 1.416, NA),
  Pr_gt_t = c(0.004438, 0.000838, 0.140488, 0.001237, 0.000731, 0.069109, 0.011190, 
              0.092709, 0.000599, 0.159068, NA),
  Significance = c("**", "***", "", "**", "***", ".", "*", ".", "***", "", NA)
)

# Ver el dataframe
print(simplified_glm_RA_summary)

#Tabla mgl remansos inicial  
{ 
  
  nueva_tabla_GLM_gamma_TRA <- data.frame(
    Term = c("(Intercept)", "Temperatura.Sustrato", "Cobertura.Dosel", "E_Blouin_RA", 
             "Tipo.Sustrato_Tallo", "Tipo.Crecimiento_Arboles", 
             "Tipo.Crecimiento_Helecho arborescente", "Tipo.Crecimiento_Hierbas epifitas", 
             "Nubosidad_No", "Tipo.Crecimiento_Helecho terrestres","Pseudo R-squared"),
    Estimate = c(2.741738, -0.029343, 0.014356, 0.097562, 0.175007, 0.097520, -0.336990, 
                 -0.269639, 0.131638, 0.223738, 0.3592239),
    Std_Error = c(0.947529, 0.008590, 0.009683, 0.029570, 0.050631, 0.053226, 0.131032, 
                  0.159249, 0.037454, 0.158009, NA),
    t_value = c(2.894, -3.416, 1.483, 3.299, 3.456, 1.832, -2.572, -1.693, 3.515, 1.416, NA),
    Pr_gt_t = c(0.004438, 0.000838, 0.140488, 0.001237, 0.000731, 0.069109, 0.011190, 
                0.092709, 0.000599, 0.159068, NA),
    Significance = c("**", "***", "", "**", "***", ".", "*", ".", "***", "", NA)
  )
  # Mostrar el dataframe
  print(nueva_tabla_GLM_gamma_TRA)
  
  
  library(flextable)
  
  # Crear la tabla con flextable
  Tabla_nueva_tabla_GLM_gamma_TRA <- flextable(nueva_tabla_GLM_gamma_TRA) 
  
  # Mostrar la tabla en la consola
  Tabla_nueva_tabla_GLM_gamma_TRA
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_nueva_tabla_GLM_gamma_TRA222.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Graficos _OBJ2/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(Tabla_nueva_tabla_GLM_gamma_TRA)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
  
}