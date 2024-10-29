#-------------------------------------------OBJ1 Estrategia de termorregulacion (Hertz, et al., ...) ----------------------------------------

#Cargar paquetes de trabajo
install.packages("openxlsx")
install.packages("readxl")
install.packages("writexl")
install.packages("ggplot2")
# Ejemplo de uso
#p <- ggplot(data, aes(x = variable1, y = variable2)) + geom_point()
#ggplotly(p)
install.packages("plotly")
install.packages("DT")
# Ejemplo de uso
datatable(data)


library(openxlsx)
library(openxlsx)
library(readxl)
library(writexl)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)



#-------------------------------------------1.1 Remansos (2100 mnsm)-------------------------------------------------------

#-------------------------------------------- Validacion del Modelo Nulo (Agar) R------------------------------

#Regresion lineal bivariable (Sp/Agar)
# Leer datos
Modelo_R <- read.xlsx("D:/Resultados B.tamaense/Termorregulacion/Validacion_Modelo_R.xlsx")

# Definir variables
x_R <- Modelo_R$SpR
y_R <- Modelo_R$AgarR

# Ajustar modelo de regresión
modelo_R <- lm(y_R ~ x_R, data = Modelo_R)
summary(modelo_R)
# Obtener R^2
resumen_modelo_R <- summary(modelo_R)
R2_R <- resumen_modelo_R$r.squared
n_datos_R <- nrow(Modelo_R)
print(n_datos_R)
print(R2_R)

texto_anotacion_R <- paste("R² =", round(R2_R, 2), "\nn =", n_datos_R, sep="")

#  Crear gráfico de regresión para Modelo_B "#2EC3E5"
gráfico_regresion_R <- ggplot(Modelo_R, aes(x = x_R, y = y_R)) +
  geom_point(color = "dodgerblue", alpha = 0.8) + # Puntos de datos
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + # Línea de regresión
  annotate("text", x = 16, y = 28, label = paste("R² =", round(R2_R, 4)), color = "black", fontface = "bold", size = 7) +
  annotate("text", x = 15.5, y = 26, label = paste("n =", round(n_datos_R, 4)), color = "black", fontface = "bold", size = 7) +
  labs(title = "Remansos (2100 msnm)",
       x = "Sp_R",
       y = "Agar-Agar") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray95"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"))

# Imprimir gráficos combinados
print(gráfico_regresion_R)




#-----------------------------------------------Indices de termorregulacion R---------------------------------
# Llama a los datos de Temperaturas operativas (Datalogger Remansos)
T_Operativa_R <- read.xlsx("D:/Resultados B.tamaense/Termorregulacion/Temperaturas_operativas_Remansos.xlsx")
# Encuentra los datos faltantes por columnas
missing_data <- colSums(is.na(T_Operativa_R))
print(missing_data)


# Instala y carga las librerías necesarias
library(dplyr)
library(tidyr)
# Renombra las columnas para asegurar que sean únicas
names(T_Operativa_R) <- make.unique(names(T_Operativa_R))

# Verifica que los nombres sean únicos
print(names(T_Operativa_R))


# Usa separate para dividir la columna datetime en dos columnas nuevas: date y time
T_Operativa_R <- T_Operativa_R %>% 
  separate(Tiempo.Ambiental.01 , into = c("date", "time"), sep = " ")


library(hms)

# Convierte la columna 'time' a formato de hora
T_Operativa_R <- T_Operativa_R %>%
  mutate(time = hms::as_hms(time))

# Filtra las filas donde time está en el rango de 17:02:51 a 02:17:51
df_filtered <- T_Operativa_R %>%
  filter(
    (time >= hms::as_hms("18:00:00") & time <= hms::as_hms("23:59:59")) |
      (time >= hms::as_hms("00:00:00") & time <= hms::as_hms("01:30:00"))
  )

# Muestra el resultado
print(df_filtered)

missing_data <- colSums(is.na(df_filtered))
print(missing_data)


# Convierte las columnas especificadas a character
df_filtered <- df_filtered %>%
  mutate(across(c("Temperatura°C.Sobre.hoja.menor.altura.de.dosel",
                  "Temperatura°C.Sobre.hoja.mayor.altura.de.dosel",
                  "Temperatura°C.Sobre.hojarasca",
                  "Temperatura°C.Bajo.hojarasca",
                  "Temperatura°C.Sobre.madera",
                  "Temperatura°C.bajo.madera",
                  "Temperatura°C.Sobre.musgo",
                  "Temperatura°C.Bajo.Musgo"), as.character))

# Convierte las columnas específicas en una sola columna llamada temperatura
df_filtered <- df_filtered %>%
  pivot_longer(
    cols = c("Temperatura°C.Sobre.hoja.menor.altura.de.dosel",
             "Temperatura°C.Sobre.hoja.mayor.altura.de.dosel",
             "Temperatura°C.Sobre.hojarasca",
             "Temperatura°C.Bajo.hojarasca",
             "Temperatura°C.Sobre.madera",
             "Temperatura°C.bajo.madera",
             "Temperatura°C.Sobre.musgo",
             "Temperatura°C.Bajo.Musgo"),
    names_to = "tipo",
    values_to = "Temperatura_Operativa_R_C"
  )

#limpiar los datos faltantes

df_filtered$Temperatura_Operativa_R_C <- as.character(df_filtered$Temperatura_Operativa_R_C)

cleaned_data <- na.omit(df_filtered)

cleaned_data$Temperatura_Operativa_R_C  <- as.numeric(cleaned_data$Temperatura_Operativa_R_C)


# Elimina las columnas No..Ambiental.02 y No..Ambiental.01
#df_filtered <- df_filtered %>%
#  select(-c("No..Ambiental.02", "No..Ambiental.01"))

# Llamar los datos de las Temperaturas Corporales Remansos
library(readxl)

T_Corporal_R <- read.xlsx("D:/Resultados B.tamaense/Termorregulacion/Temperatura_corporal_Remansos.xlsx")

n_datos_Corp_R <- nrow(T_Corporal_R)
print(n_datos_Corp_R)

T_Preferencia_R <- read.xlsx(
  "D:/Resultados B.tamaense/Termorregulacion/Temperaturas_preferencia_Remansos.xlsx")


# Calcular los percentiles 0.25 y 0.75 y redondearlos a un decimal
Percentiles_R <- quantile(T_Preferencia_R$T_Pref_Remansos, probs = c(0.25, 0.75))
Percentiles_R <- round(Percentiles_R, 1)

TPR <- describe(T_Preferencia_R$T_Pref_Remansos)
TCR <- describe(T_Corporal_R$T_Corporal_Remansos)
TOR <- describe(cleaned_data$Temperatura_Operativa_R_C)
describe(Modelo_R$AgarR)
# Crear variables con los percentiles
Percentil_25_R <- Percentiles_R[1]
Percentil_75_R <- Percentiles_R[2]




# Crear el rango de valores entre Percentil_25_R y Percentil_75_R
PTR50_R <- seq(Percentil_25_R, Percentil_75_R, by = 0.1)

PTR50_R<-data.frame(PTR50_R)


# Imprimimos los resultados
print(Percentiles_R)

#Convertir Temperarura operativa remansos completo a numerico
df_filtered$Temperatura_Operativa_R_C <- as.numeric(df_filtered$Temperatura_Operativa_R_C)

#---------------------de Indice de Calidad termica del habitat R----------------------

# Definimos las condiciones
condicion1 <- df_filtered$Temperatura_Operativa_R_C >= Percentil_25_R & df_filtered$Temperatura_Operativa_R_C <= Percentil_75_R
condicion2 <- df_filtered$Temperatura_Operativa_R_C < Percentil_25_R
condicion3 <- df_filtered$Temperatura_Operativa_R_C > Percentil_75_R

# Inicializamos la columna 'valor' con NA en el dataframe filtrado
df_filtered$Calidad_termica_habita <- NA

# Aplicamos las condiciones y asignamos los valores en el dataframe filtrado
df_filtered$Calidad_termica_habita <- ifelse(condicion1, 0,
                                             ifelse(condicion2, Percentil_25_R - df_filtered$Temperatura_Operativa_R_C,
                                                    ifelse(condicion3, df_filtered$Temperatura_Operativa_R_C - Percentil_75_R, NA)))


# Cambia el nombre de la columna temperatura a nueva_temperatura
#df_filtered <- df_filtered %>%
#rename(Calidad_termica_habita = valor)

# missing_data <- colSums(is.na(df_filtered2))
# print(missing_data)

print(class(df_filtered$Calidad_termica_habita))

df_filtered$Calidad_termica_habita <- as.character(df_filtered$Calidad_termica_habita)

cleaned_data <- na.omit(df_filtered)


# Convertir la columna 'Calidad_termica_habita' a tipo numérico
df_filtered$Calidad_termica_habita <- as.numeric(as.character(df_filtered$Calidad_termica_habita))

# Calcular el promedio
promedio_de_R <- mean(df_filtered$Calidad_termica_habita, na.rm = TRUE)  # Utiliza na.rm = TRUE para ignorar los valores faltantes

promedio_Calidad_termica_habita<-print(promedio_de_R)

describe(df_filtered$Calidad_termica_habita)

#------------------db Indice de Precision en la termorrecgulacion R----------------------

# Definimos las condiciones
condicion1 <- T_Corporal_R$`T_Corporal_Remansos` >= Percentil_25_R & T_Corporal_R$`T_Corporal_Remansos` <= Percentil_75_R
condicion2 <- T_Corporal_R$`T_Corporal_Remansos` < Percentil_25_R
condicion3 <- T_Corporal_R$`T_Corporal_Remansos` > Percentil_75_R

# Inicializamos la columna 'valor' con NA en el dataframe filtrado
T_Corporal_R$Precision_termoregulacion <- NA

# Aplicamos las condiciones y asignamos los valores en el dataframe filtrado
T_Corporal_R$Precision_termoregulacion <- ifelse(condicion1, 0,
                                                 ifelse(condicion2, Percentil_25_R - T_Corporal_R$`T_Corporal_Remansos`,
                                                        ifelse(condicion3, T_Corporal_R$`T_Corporal_Remansos` - Percentil_75_R, NA)))
# Calcular el promedio
promedio_db <- mean(T_Corporal_R$Precision_termoregulacion, na.rm = TRUE)  # Utiliza na.rm = TRUE para ignorar los valores faltantes

promedio_Precision_termorregulacion <-print(promedio_db)

describe(T_Corporal_R$Precision_termoregulacion)
describe(df_filtered$Calidad_termica_habita)
#-----------------------------------------E Eficiencia en la termorregulacion R-----------------------
E<-1-(T_Corporal_R$Precision_termoregulacion/promedio_de_R)
print(E)
# Convertir la lista 'E' en un vector numérico
E_numeric_R <- unlist(E)
# Agregar la lista como una nueva columna en el dataframe
T_Corporal_R$E_numeric_R <- E_numeric_R

# Mostrar el dataframe actualizado
str(T_Corporal_R)

# Calcular el promedio de los valores en 'E_numeric'
promedio_E_R <- mean(E_numeric_R)
print(promedio_E_R)

#-------------------------------------E_Blouin ficiencia en la termorregulacion R------------------------

E_Blouin_r<- promedio_de_R-T_Corporal_R$Precision_termoregulacion
E_Blouin_r <- unlist(E_Blouin_r)
E_Blouin_prom_R <- mean(E_Blouin_r)
print(E_Blouin_prom_R)
# Agregar la lista como una nueva columna en el dataframe
T_Corporal_R$E_Blouin_r <- E_Blouin_r

# Mostrar el dataframe actualizado
str(T_Corporal_R)

#--------------------------------------ExCW96 Aprovechamiento termico R------------------


# Contar las temperaturas corporales dentro del rango
temperaturas_dentro_del_rango <- sum(T_Corporal_R$T_Corporal_Remansos>= Percentil_25_R & T_Corporal_R$T_Corporal_Remansos <= Percentil_75_R)

# Calcular el porcentaje de temperaturas dentro del rango
ExCW96_R <- (temperaturas_dentro_del_rango / length(T_Corporal_R$T_Corporal_Remansos)) * 100
ExCW96_R <- unlist(ExCW96_R)
print(ExCW96_R)
# Imprimir el resultado
print(paste("ExCW96_R:", ExCW96_R, "%"))


# Calcular el porcentaje de temperaturas dentro del rango (ExCW96_R) como ejemplo
temperaturas_dentro_del_rango <- sum(T_Corporal_R$T_Corporal_Remansos >= Percentil_25_R & 
                                       T_Corporal_R$T_Corporal_Remansos <= Percentil_75_R)
ExCW96_R <- (temperaturas_dentro_del_rango / length(T_Corporal_R$T_Corporal_Remansos)) * 100
ExCW96_R <- unlist(ExCW96_R)

# Crear una columna con el valor de ExCW96 para cada observación
T_Corporal_R$ExCW96 <- ifelse(T_Corporal_R$T_Corporal_Remansos >= Percentil_25_R & 
                                T_Corporal_R$T_Corporal_Remansos <= Percentil_75_R, 
                              ExCW96_R, 0)

# Imprimir el dataframe actualizado
print(T_Corporal_R)
str(T_Corporal_R)

promedio_ExCW96_R <- mean(T_Corporal_R$ExCW96)

# Imprimir el resultado
print(paste("ExCW96_R:", ExCW96_R, "%"))











#--------------------------------------ExBW00 Desaprovechamiento termico R------------------------


# Contar las temperaturas corporales por encima o por debajo del rango
temperaturas_fuera_del_rango <- sum(T_Corporal_R$T_Corporal_Remansos < Percentil_25_R | T_Corporal_R$T_Corporal_Remansos > Percentil_75_R)

# Calcular el porcentaje de temperaturas por encima o por debajo del rango
ExBW00_R <- (temperaturas_fuera_del_rango / length(T_Corporal_R$T_Corporal_Remansos)) * 100

print(ExBW00_R)
# Imprimir el resultado
print(paste("ExBW00_R:", ExBW00_R, "%"))



#---------------------------1.2 Asiria (2600 msnm)---------------------------------------------------------------------

#-------------------------------------------- Validacion del Modelo Nulo (Agar) A------------------------------

#Regresion lineal bivariable (Sp/Agar)
# Leer datos
Modelo_A <- read.xlsx("D:/Resultados B.tamaense/Termorregulacion/Validacion_Modelo_A.xlsx")

# Definir variables
x_A <- Modelo_A$Sp_A
y_A <- Modelo_A$Agar_A

# Ajustar modelo de regresión
modelo <- lm(y_A ~ x_A, data = Modelo_A)
summary(modelo)
# Obtener R^2
resumen_modelo <- summary(modelo)
R2_A <- resumen_modelo$r.squared
n_datos_A <- nrow(Modelo_A)
print(R2_A)
print(n_datos_A)

# Crear gráfico de regresión para Modelo_A
gráfico_regresion_A <- ggplot(Modelo_A, aes(x = x_A, y = y_A)) +
  geom_point(color = "dodgerblue", alpha = 0.8) + # Puntos de datos
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + # Línea de regresión
  annotate("text", x = 15, y = 24, label = paste("R² =", round(R2_A, 4)), color = "black", fontface = "bold", size = 8) +
  annotate("text", x = 14.5, y = 22.5, label = paste("n =", round(n_datos_A, 4)), color = "black", fontface = "bold", size = 8) +
  labs(title = "Asiria (2600 msnm)",
       x = "Sp_A",
       y = "Agar_Agar") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray95"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold"))

# Imprimir gráficos combinados
print(gráfico_regresion_A)


#-----------------------------------------------Indices de termorregulacion A---------------------------------


#Llama a los datos de Temperaturas operativas (Datalogger Remansos)
T_Operativa_A <- read.xlsx("D:/Resultados B.tamaense/Termorregulacion/Temperaturas_operativas_Asiria.xlsx")

# Encuentra los datos faltantes por columnas
missing_data <- colSums(is.na(T_Operativa_A))
print(missing_data)


# Instala y carga las librerías necesarias
library(dplyr)
library(tidyr)
# Renombra las columnas para asegurar que sean únicas
names(T_Operativa_A) <- make.unique(names(T_Operativa_A))

# Verifica que los nombres sean únicos
print(names(T_Operativa_A))


# Usa separate para dividir la columna datetime en dos columnas nuevas: date y time
T_Operativa_A <- T_Operativa_A %>% 
  separate(Tiempo.Ambiental.01 , into = c("date", "time"), sep = " ")


library(hms)

# Convierte la columna 'time' a formato de hora
T_Operativa_A <- T_Operativa_A %>%
  mutate(time = hms::as_hms(time))

# Filtra las filas donde time está en el rango de 17:02:51 a 02:17:51
df_filtered_A_SI <- T_Operativa_A %>%
  filter(
    (time >= hms::as_hms("18:00:00") & time <= hms::as_hms("23:59:59")) |
      (time >= hms::as_hms("00:00:00") & time <= hms::as_hms("01:30:00"))
  )

# Muestra el resultado
print(df_filtered_A_SI)

missing_data <- colSums(is.na(df_filtered_A_SI))
print(missing_data)


# Convierte las columnas especificadas a character
df_filtered_A_SI <- df_filtered_A_SI %>%
  mutate(across(c("Temperatura°C.Sobre.hoja.menor.altura.de.dosel",
                  "Temperatura°C.Sobre.hoja.mayor.altura.de.dosel",
                  "Temperatura°C.Sobre.hojarasca",
                  "Temperatura°C.Bajo.hojarasca",
                  "Temperatura°C.Sobre.madera",
                  "Temperatura°C.bajo.madera",
                  "Temperatura°C.Sobre.musgo",
                  "Temperatura°C.Bajo.Musgo"), as.character))

# Convierte las columnas específicas en una sola columna llamada temperatura
df_filtered_A_SI <- df_filtered_A_SI %>%
  pivot_longer(
    cols = c("Temperatura°C.Sobre.hoja.menor.altura.de.dosel",
             "Temperatura°C.Sobre.hoja.mayor.altura.de.dosel",
             "Temperatura°C.Sobre.hojarasca",
             "Temperatura°C.Bajo.hojarasca",
             "Temperatura°C.Sobre.madera",
             "Temperatura°C.bajo.madera",
             "Temperatura°C.Sobre.musgo"),
    names_to = "tipo",
    values_to = "Temperatura_Operativa_A_C_SI"
  )

#limpiar los datos faltantes

df_filtered_A_SI$Temperatura_Operativa_A_C_SI <- as.character(df_filtered_A_SI$Temperatura_Operativa_A_C_SI)

df_filtered_A_SI <- df_filtered_A_SI[complete.cases(df_filtered_A_SI$Temperatura_Operativa_A_C_SI), ]


cleaned_data$Temperatura_Operativa_A_C_SI  <- as.numeric(cleaned_data$Temperatura_Operativa_A_C_SI)


#Convertir Temperarura operativa remansos completo a numerico
df_filtered_A_SI$Temperatura_Operativa_A_C_SI <- as.numeric(df_filtered_A_SI$Temperatura_Operativa_A_C_SI)


# Revisar el tmaño de las variables temperaturas operativas para las dos poblaciones
summary(df_filtered$Temperatura_Operativa_R_C)
summary(df_filtered_A_SI$Temperatura_Operativa_A_C_SI)
# Instalar y cargar psych si aún no lo tienes
#install.packages("psych")
library(psych)
describe(df_filtered$Temperatura_Operativa_R_C)
describe(df_filtered_A_SI$Temperatura_Operativa_A_C_SI)


# Suponiendo que df_filtered y df_filtered_A_SI ya están definidos
# Crear secuencias para los índices
indices_R <- seq(1, length(df_filtered$Temperatura_Operativa_R_C))
indices_A <- seq(1, length(df_filtered_A_SI$Temperatura_Operativa_A_C_SI))

# Interpolación lineal de Temperatura_Operativa_A_C
Interpolated_A_C <- approx(x = indices_A, y = df_filtered_A_SI$Temperatura_Operativa_A_C_SI, 
                           xout = seq(1, length(df_filtered_A_SI$Temperatura_Operativa_A_C_SI), 
                                      length.out = length(df_filtered$Temperatura_Operativa_R_C)))$y

# Crear un nuevo data frame con las variables interpoladas
df_filtered_A <- data.frame(
  Temperatura_Operativa_R_C = df_filtered$Temperatura_Operativa_R_C,
  Temperatura_Operativa_A_C = Interpolated_A_C
)

# Verificar estadísticas descriptivas
library(psych)
describe(df_filtered_A$Temperatura_Operativa_R_C)
describe(df_filtered_A$Temperatura_Operativa_A_C)
#Datos anteriores
describe(df_filtered_A_SI$Temperatura_Operativa_A_C_SI)


# Llamar los datos de las Temperaturas Corporales Remansos

T_Corporal_A <- read.xlsx(
  "D:/Resultados B.tamaense/Termorregulacion/Temperatura_corporal_Asiria.xlsx")

n_datos_Corp_A <- nrow(T_Corporal_A)
print(n_datos_Corp_A)

T_Preferencia_A <- read.xlsx(
  "D:/Resultados B.tamaense/Termorregulacion/Temperaturas_preferencia_Asiria.xlsx")

# Calcular los percentiles 0.25 y 0.75 y redondearlos a un decimal
Percentiles_A <- quantile(T_Preferencia_A$T_Pref_Asiria, probs = c(0.25, 0.75))
Percentiles_A <- round(Percentiles_A, 1)

TPA <- describe(T_Preferencia_A$T_Pref_Asiria)
TCA <- describe(T_Corporal_A$T_Corporal_Asiria)
TOA <- describe(cleaned_data$Temperatura_Operativa_A_C)


# Creamos variables con los percentiles
Percentil_25_A <- Percentiles_A[1]
Percentil_75_A <- Percentiles_A[2]

# Imprimimos los resultados
print(Percentiles_A)


#Convertir Temperarura operativa remansos completo a numerico
df_filtered_A$Temperatura_Operativa_A_C <- as.numeric(df_filtered_A$Temperatura_Operativa_A_C)

#-------------------------- de Indice de Calidad termica del habitat A------------------------------

# Definimos las condiciones
condicion1 <- df_filtered_A$Temperatura_Operativa_A_C >= Percentil_25_A & df_filtered_A$Temperatura_Operativa_A_C <= Percentil_75_A
condicion2 <- df_filtered_A$Temperatura_Operativa_A_C < Percentil_25_A
condicion3 <- df_filtered_A$Temperatura_Operativa_A_C > Percentil_75_A

# Inicializamos la columna 'valor' con NA en el dataframe filtrado
df_filtered_A$Calidad_termica_habita_A <- NA

# Aplicamos las condiciones y asignamos los valores en el dataframe filtrado
df_filtered_A$Calidad_termica_habita_A <- ifelse(condicion1, 0,
                                                 ifelse(condicion2, Percentil_25_A - df_filtered_A$Temperatura_Operativa_A_C,
                                                        ifelse(condicion3, df_filtered_A$Temperatura_Operativa_A_C - Percentil_75_A, NA)))


# Cambia el nombre de la columna temperatura a nueva_temperatura
#df_filtered_A <- df_filtered_A %>%
# rename(Calidad_termica_habita_A = valor)

# missing_data <- colSums(is.na(df_filtered2))
# print(missing_data)

print(class(df_filtered_A$Calidad_termica_habita_A))

df_filtered_A$Calidad_termica_habita_A <- as.character(df_filtered_A$Calidad_termica_habita_A)

cleaned_data <- na.omit(df_filtered_A)


# Convertir la columna 'Calidad_termica_habita' a tipo numérico
df_filtered_A$Calidad_termica_habita_A <- as.numeric(as.character(df_filtered_A$Calidad_termica_habita_A))

# Calcular el promedio
promedio_de_A <- mean(df_filtered_A$Calidad_termica_habita_A, na.rm = TRUE)  # Utiliza na.rm = TRUE para ignorar los valores faltantes

promedio_Calidad_termica_habita_A<-print(promedio_de_A)

describe(df_filtered_A$Calidad_termica_habita_A)

#------------------- db Indice de Precision en la termorrecgulacion A------------------------------------

# Definimos las condiciones
condicion1 <- T_Corporal_A$`T_Corporal_Asiria` >= Percentil_25_A & T_Corporal_A$`T_Corporal_Asiria` <= Percentil_75_A
condicion2 <- T_Corporal_A$`T_Corporal_Asiria` < Percentil_25_A
condicion3 <- T_Corporal_A$`T_Corporal_Asiria` > Percentil_75_A

# Inicializamos la columna 'valor' con NA en el dataframe filtrado
T_Corporal_A$Precision_termoregulacion_A <- NA

# Aplicamos las condiciones y asignamos los valores en el dataframe filtrado
T_Corporal_A$Precision_termoregulacion_A <- ifelse(condicion1, 0,
                                                   ifelse(condicion2, Percentil_25_A - T_Corporal_A$`T_Corporal_Asiria`,
                                                          ifelse(condicion3, T_Corporal_A$`T_Corporal_Asiria` - Percentil_75_A, NA)))
# Calcular el promedio
promedio_db_A <- mean(T_Corporal_A$Precision_termoregulacion_A, na.rm = TRUE)  # Utiliza na.rm = TRUE para ignorar los valores faltantes

promedio_Precision_termorregulacion_A <-print(promedio_db_A)

describe(T_Corporal_A$Precision_termoregulacion_A)
describe(df_filtered_A$Calidad_termica_habita_A)
#-----------------------------------------E Eficiencia en la termorregulacion A------------
E<-1-(T_Corporal_A$Precision_termoregulacion_A/promedio_de_A)
print(E)

# Convertir la lista 'E' en un vector numérico
E_numeric_A <- unlist(E)
# Agregar la lista como una nueva columna en el dataframe
T_Corporal_A$E_numeric_A <- E_numeric_A

# Mostrar el dataframe actualizado
str(T_Corporal_A)
# Calcular el promedio de los valores en 'E_numeric'
promedio_E_A <- mean(E_numeric_A)
print(promedio_E_A)
#-------------------------------------E_Blouin ficiencia en la termorregulacion A------------------------
E_Blouin<- promedio_de_A-T_Corporal_A$Precision_termoregulacion_A
E_Blouin_A <- unlist(E_Blouin)
E_Blouin_prom_A <- mean(E_Blouin_A)
print(E_Blouin_prom_A)
# Agregar la lista como una nueva columna en el dataframe
T_Corporal_A$E_Blouin_A <- E_Blouin_A
str(T_Corporal_A)
#--------------------------------------ExCW96 Aprovechamiento termico A------------------


# Contar las temperaturas corporales dentro del rango
temperaturas_dentro_del_rango <- sum(T_Corporal_A$T_Corporal_Asiria>= Percentil_25_A & T_Corporal_A$T_Corporal_Asiria <= Percentil_75_A)

# Calcular el porcentaje de temperaturas dentro del rango
ExCW96_A <- (temperaturas_dentro_del_rango / length(T_Corporal_A$T_Corporal_Asiria)) * 100

print(ExCW96_A)
# Imprimir el resultado
print(paste("ExCW96_A:", ExCW96_A, "%"))



#--------------------------------------ExBW00 Desaprovechamiento termico A------------------------


# Contar las temperaturas corporales por encima o por debajo del rango
temperaturas_fuera_del_rango <- sum(T_Corporal_A$T_Corporal_Asiria < Percentil_25_A | T_Corporal_A$T_Corporal_Asiria > Percentil_75_A)

# Calcular el porcentaje de temperaturas por encima o por debajo del rango
ExBW00_A <- (temperaturas_fuera_del_rango / length(T_Corporal_A$T_Corporal_Asiria)) * 100

print(ExBW00_A)
# Imprimir el resultado
print(paste("ExBW00_A:", ExBW00_A, "%"))

#---------------------------1.3 Resultados------------------------------------

#----------------------Diferencias significativas-----------------
TPR <- describe(T_Preferencia_R$T_Pref_Remansos)
TCR <- describe(T_Corporal_R$T_Corporal_Remansos)
TOR <- describe(cleaned_data$Temperatura_Operativa_R_C)
describe(Modelo_R$AgarR)
describe(T_Corporal_R$Precision_termoregulacion)
describe(df_filtered$Calidad_termica_habita)
E_Blouin_r
E_numeric_R
describe(E_Blouin_r)
describe(E_numeric_R)
sd(E_Blouin_r)
sd(E_numeric_R)
mean(E_numeric_R)
mean(E_numeric_R)
str(E_numeric_R)
str(E_numeric_A)
x_R <- Modelo_R$SpR
y_R <- Modelo_R$AgarR



TPA <- describe(T_Preferencia_A$T_Pref_Asiria)
TCA <- describe(T_Corporal_A$T_Corporal_Asiria)
TOA <- describe(cleaned_data$Temperatura_Operativa_A_C)
describe(T_Corporal_A$Precision_termoregulacion_A)
describe(df_filtered_A$Calidad_termica_habita_A)
describe(Modelo_A$AgarA)
describe(E_numeric_A)
describe(E_Blouin_A)
E_numeric_A
E_Blouin_A
sd(E_Blouin_A)
sd(E_numeric_A)
mean(E_Blouin_A + E_Blouin_r)
mean(E_numeric_A)
str(T_Corporal_A$Precision_termoregulacion_A)
str(T_Corporal_R$Precision_termoregulacion)
x_A <- Modelo_A$Sp_A
y_A <- Modelo_A$Agar_A



#----------------Temperatura corporal----------
# Datos de ejemplo
T_Corporal_Remansos <- T_Corporal_R$T_Corporal_Remansos
T_Corporal_Asiria <- T_Corporal_A$T_Corporal_Asiria

# Crear un data frame para cada variable
df_remansos <- data.frame(T_Corporal = T_Corporal_Remansos, Fuente = "Remansos")
df_asiria <- data.frame(T_Corporal = T_Corporal_Asiria, Fuente = "Asiria")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(df_combinado)

# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(T_Corporal_Remansos)
shapiro_asiria <- shapiro.test(T_Corporal_Asiria)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(T_Corporal_Remansos, T_Corporal_Asiria)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(T_Corporal_Remansos, T_Corporal_Asiria)
  print(wilcox_test_result)
}

# Histogramas
hist(T_Corporal_Remansos, main="Histograma de T_Corporal_Remansos", xlab="T_Corporal_Remansos", col="lightblue")
hist(T_Corporal_Asiria, main="Histograma de T_Corporal_Asiria", xlab="T_Corporal_Asiria", col="lightgreen")

# QQ-plots
qqnorm(T_Corporal_Remansos)
qqline(T_Corporal_Remansos, col="red")
qqnorm(T_Corporal_Asiria)
qqline(T_Corporal_Asiria, col="red")


boxplot(T_Corporal_Remansos, T_Corporal_Asiria, names=c("Remansos", "Asiria"), main="Comparación de T_Corporal", ylab="T_Corporal")


library(car)
leveneTest(T_Corporal ~ Fuente, data = df_combinado)

#------------------Temperatura de preferencia-----------

# Datos de ejemplo
T_Preferencia_Remansos <- T_Preferencia_R$T_Pref_Remansos
T_Preferencia_Asiria <- T_Preferencia_A$T_Pref_Asiria

# Crear un data frame para cada variable
df_remansos <- data.frame(T_Preferencia = T_Preferencia_Remansos, Fuente = "Remansos")
df_asiria <- data.frame(T_Preferencia = T_Preferencia_Asiria, Fuente = "Asiria")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(df_combinado)
str(df_combinado)
# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(T_Preferencia_Remansos)
shapiro_asiria <- shapiro.test(T_Preferencia_Asiria)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(T_Preferencia_Remansos, T_Preferencia_Asiria)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(T_Preferencia_Remansos, T_Preferencia_Asiria)
  print(wilcox_test_result)
}

# Histogramas
hist(T_Preferencia_Remansos, main="Histograma de T_Preferencia_Remansos", xlab="T_Preferencia_Remansos", col="lightblue")
hist(T_Preferencia_Asiria, main="Histograma de T_Preferencia_Asiria", xlab="T_Preferencia_Asiria", col="lightgreen")

# Boxplot
boxplot(T_Preferencia_Remansos, T_Preferencia_Asiria, names=c("Remansos", "Asiria"), main="Comparación de T_Preferencia", ylab="T_Preferencia")

library(car)
leveneTest(T_Preferencia ~ Fuente, data = df_combinado)

#------------------------Temperatura Operativa----------------

# Crear un data frame para cada variable
df_remansos <- data.frame(Temperatura_Operativa = cleaned_data$Temperatura_Operativa_R_C, Fuente = "Remansos")
df_asiria <- data.frame(Temperatura_Operativa = cleaned_data$Temperatura_Operativa_A_C, Fuente = "Asiria")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(head(df_combinado))  # Mostrar las primeras filas para verificar

# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(cleaned_data$Temperatura_Operativa_R_C)
shapiro_asiria <- shapiro.test(cleaned_data$Temperatura_Operativa_A_C)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(cleaned_data$Temperatura_Operativa_R_C, cleaned_data$Temperatura_Operativa_A_C)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(cleaned_data$Temperatura_Operativa_R_C, cleaned_data$Temperatura_Operativa_A_C)
  print(wilcox_test_result)
}

# Histogramas
hist(cleaned_data$Temperatura_Operativa_R_C, main="Histograma de Temperatura Operativa Remansos", xlab="Temperatura Operativa", col="lightblue")
hist(cleaned_data$Temperatura_Operativa_A_C, main="Histograma de Temperatura Operativa Asiria", xlab="Temperatura Operativa", col="lightgreen")

# Boxplot
boxplot(cleaned_data$Temperatura_Operativa_R_C, cleaned_data$Temperatura_Operativa_A_C, names=c("Remansos", "Asiria"), main="Comparación de Temperatura Operativa", ylab="Temperatura Operativa")

library(car)
leveneTest(Temperatura_Operativa ~ Fuente, data = df_combinado)

#-------------------Precision en la termorregulacion-------------

# Crear un data frame para cada variable
df_remansos <- data.frame(Precision_termoregulacion = T_Corporal_R$Precision_termoregulacion, Fuente = "Remansos")
df_asiria <- data.frame(Precision_termoregulacion = T_Corporal_A$Precision_termoregulacion_A, Fuente = "Asiria")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(head(df_combinado))  # Mostrar las primeras filas para verificar


# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(T_Corporal_R$Precision_termoregulacion)
shapiro_asiria <- shapiro.test(T_Corporal_A$Precision_termoregulacion_A)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(T_Corporal_R$Precision_termoregulacion, T_Corporal_A$Precision_termoregulacion_A)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(T_Corporal_R$Precision_termoregulacion, T_Corporal_A$Precision_termoregulacion_A)
  print(wilcox_test_result)
}

# Histogramas
hist(T_Corporal_R$Precision_termoregulacion, main="Histograma de Precision Termoregulacion Remansos", xlab="Precision Termoregulacion", col="lightblue")
hist(T_Corporal_A$Precision_termoregulacion_A, main="Histograma de Precision Termoregulacion Asiria", xlab="Precision Termoregulacion", col="lightgreen")

# Boxplot
boxplot(T_Corporal_R$Precision_termoregulacion, T_Corporal_A$Precision_termoregulacion_A, names=c("Remansos", "Asiria"), main="Comparación de Precision Termoregulacion", ylab="Precision Termoregulacion")


library(car)
leveneTest(Precision_termoregulacion ~ Fuente, data = df_combinado)

#------------------Calidad termica del habitat-----------------

# Crear un data frame para cada variable
df_remansos <- data.frame(Calidad_termica_habita = df_filtered$Calidad_termica_habita, Fuente = "Remansos")
df_asiria <- data.frame(Calidad_termica_habita = df_filtered_A$Calidad_termica_habita_A, Fuente = "Asiria")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(head(df_combinado))  # Mostrar las primeras filas para verificar


# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(df_filtered$Calidad_termica_habita)
shapiro_asiria <- shapiro.test(df_filtered_A$Calidad_termica_habita_A)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(df_filtered$Calidad_termica_habita, df_filtered_A$Calidad_termica_habita_A)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(df_filtered$Calidad_termica_habita, df_filtered_A$Calidad_termica_habita_A)
  print(wilcox_test_result)
}


# Histogramas
hist(df_filtered$Calidad_termica_habita, main="Histograma de Calidad Térmica Habitáculo Remansos", xlab="Calidad Térmica Habitáculo", col="lightblue")
hist(df_filtered_A$Calidad_termica_habita_A, main="Histograma de Calidad Térmica Habitáculo Asiria", xlab="Calidad Térmica Habitáculo", col="lightgreen")

# Boxplot
boxplot(df_filtered$Calidad_termica_habita, df_filtered_A$Calidad_termica_habita_A, names=c("Remansos", "Asiria"), main="Comparación de Calidad Térmica Habitáculo", ylab="Calidad Térmica Habitáculo")


library(car)
leveneTest(Calidad_termica_habita ~ Fuente, data = df_combinado)

#----------------------E Blouin------------------

# Crear un data frame para cada variable
df_remansos <- data.frame(E_Blouin = E_Blouin_r, Fuente = "Remansos")
df_asiria <- data.frame(E_Blouin = E_Blouin_A, Fuente = "Asiria")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(head(df_combinado))  # Mostrar las primeras filas para verificar


# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(E_Blouin_r)
shapiro_asiria <- shapiro.test(E_Blouin_A)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(E_Blouin_r, E_Blouin_A)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(E_Blouin_r, E_Blouin_A)
  print(wilcox_test_result)
}


# Histogramas
hist(E_Blouin_r, main="Histograma de E Blouin Remansos", xlab="E Blouin", col="lightblue")
hist(E_Blouin_A, main="Histograma de E Blouin Asiria", xlab="E Blouin", col="lightgreen")

# Boxplot
boxplot(E_Blouin_r, E_Blouin_A, names=c("Remansos", "Asiria"), main="Comparación de E Blouin", ylab="E Blouin")


library(car)
leveneTest(E_Blouin ~ Fuente, data = df_combinado)

#------------------------------ E Numeric-----------------

# Crear un data frame para cada variable
df_remansos <- data.frame(E_numeric = E_numeric_R, Fuente = "Remansos")
df_asiria <- data.frame(E_numeric = E_numeric_A, Fuente = "Asiria")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(head(df_combinado))  # Mostrar las primeras filas para verificar


# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(E_numeric_R)
shapiro_asiria <- shapiro.test(E_numeric_A)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(E_numeric_R, E_numeric_A)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(E_numeric_R, E_numeric_A)
  print(wilcox_test_result)
}


# Histogramas
hist(E_numeric_R, main="Histograma de E_numeric Remansos", xlab="E_numeric", col="lightblue")
hist(E_numeric_A, main="Histograma de E_numeric Asiria", xlab="E_numeric", col="lightgreen")

# Boxplot
boxplot(E_numeric_R, E_numeric_A, names=c("Remansos", "Asiria"), main="Comparación de E_numeric", ylab="E_numeric")


library(car)
leveneTest(E_numeric ~ Fuente, data = df_combinado)

#--------------------------------Validacion del modelo Remansos-------------------


# Crear un data frame para cada variable
df_A <- data.frame(Variable = x_R, Fuente = "Sp")
df_R <- data.frame(Variable = y_R, Fuente = "Agar")

# Unir los data frames
df_combinado <- rbind(df_A, df_R)

# Ver el data frame combinado
print(head(df_combinado))  # Mostrar las primeras filas para verificar


# Verificar normalidad usando Shapiro-Wilk
shapiro_A <- shapiro.test(x_R)
shapiro_R <- shapiro.test(y_R)

print(shapiro_A)
print(shapiro_R)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_A$p.value > 0.05 && shapiro_R$p.value > 0.05) {
  t_test_result <- t.test(x_R, y_R)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(x_R, y_R)
  print(wilcox_test_result)
}


# Histogramas
hist(x_R, main="Histograma de X_R (Sp)", xlab="x_R", col="lightblue")
hist(y_R, main="Histograma de y_R (Agar)", xlab="y_R", col="lightgreen")

# Boxplot
boxplot(x_R, y_R, names=c("Asiria", "Remansos"), main="Comparación de Variables", ylab="Valor de la Variable")


library(car)
leveneTest(Variable ~ Fuente, data = df_combinado)

#--------------------------------Validacion del modelo Asiria-------------------


# Crear un data frame para cada variable
df_remansos <- data.frame(Valor = y_A, Fuente = "Agar")
df_asiria <- data.frame(Valor = x_A, Fuente = "Sp")

# Unir los data frames
df_combinado <- rbind(df_remansos, df_asiria)

# Ver el data frame combinado
print(head(df_combinado))  # Mostrar las primeras filas para verificar


# Verificar normalidad usando Shapiro-Wilk
shapiro_remansos <- shapiro.test(y_A)
shapiro_asiria <- shapiro.test(x_A)

print(shapiro_remansos)
print(shapiro_asiria)

# Prueba de diferencia significativa
# Si ambos son normales, usar t-test
if (shapiro_remansos$p.value > 0.05 && shapiro_asiria$p.value > 0.05) {
  t_test_result <- t.test(y_A, x_A)
  print(t_test_result)
} else {
  # Si no son normales, usar Mann-Whitney U test
  wilcox_test_result <- wilcox.test(y_A, x_A)
  print(wilcox_test_result)
}


# Histogramas
hist(x_R, main="Histograma de y_A (Agar)", xlab="y_A", col="lightblue")
hist(x_A, main="Histograma de x_A (Sp)", xlab="x_A", col="lightgreen")

# Boxplot
boxplot(y_A, x_A, names=c("Remansos", "Asiria"), main="Comparación de Valores", ylab="Valor")


library(car)
leveneTest(Valor ~ Fuente, data = df_combinado)

#-----------------Datos generales Precision_termoregulacion---------------------

# Crear data frames individuales
df_A <- data.frame(Precision_termoregulacion = T_Corporal_A$Precision_termoregulacion_A)
df_R <- data.frame(Precision_termoregulacion = T_Corporal_R$Precision_termoregulacion)

# Unirlos en un solo data frame
df_combined <- rbind(df_A, df_R)

# Calcular la media y la desviación estándar
mean_precision <- mean(df_combined$Precision_termoregulacion)
sd_precision <- sd(df_combined$Precision_termoregulacion)

# Imprimir los resultados
cat("Media:", mean_precision, "\n")
cat("Desviación estándar:", sd_precision, "\n")

#-----------------Datos generales Calidad termica del habitat---------------------

# Crear data frames individuales
df1 <- data.frame(Calidad_termica_habita = df_filtered$Calidad_termica_habita)
df2 <- data.frame(Calidad_termica_habita = df_filtered_A$Calidad_termica_habita_A)

# Unirlos en un solo data frame
df_combined <- data.frame(
  Calidad_termica_habita = c(df1$Calidad_termica_habita, df2$Calidad_termica_habita)
)

# Calcular la media y la desviación estándar
mean_calidad <- mean(df_combined$Calidad_termica_habita)
sd_calidad <- sd(df_combined$Calidad_termica_habita)

# Imprimir los resultados
cat("Media:", mean_calidad, "\n")
cat("Desviación estándar:", sd_calidad, "\n")

# Crear data frames individuales (si no están creados)
df1 <- data.frame(Calidad_termica_habita = df_filtered$Calidad_termica_habita)
df2 <- data.frame(Calidad_termica_habita = df_filtered_A$Calidad_termica_habita_A)

# Unir los vectores en un solo data frame
df_combined <- data.frame(
  Calidad_termica_habita = c(df1$Calidad_termica_habita, df2$Calidad_termica_habita)
)

# Calcular la media y la desviación estándar
mean_calidad <- mean(df_combined$Calidad_termica_habita, na.rm = TRUE)  # Añadir na.rm = TRUE para manejar NAs
sd_calidad <- sd(df_combined$Calidad_termica_habita, na.rm = TRUE)      # Añadir na.rm = TRUE para manejar NAs

# Imprimir los resultados
cat("Media:", mean_calidad, "\n")
cat("Desviación estándar:", sd_calidad, "\n")



#-----------------Datos generales Eficioencia de hertz---------------------
# Crear data frames individuales
df_R <- data.frame(E_numeric = E_numeric_R)
df_A <- data.frame(E_numeric = E_numeric_A)

# Unirlos en un solo data frame
df_combined <- rbind(df_R, df_A)


# Calcular la media y la desviación estándar
mean_E_numeric <- mean(df_combined$E_numeric)
sd_E_numeric <- sd(df_combined$E_numeric)

# Imprimir los resultados
cat("Media:", mean_E_numeric, "\n")
cat("Desviación estándar:", sd_E_numeric, "\n")


#-----------------Datos generales Eficiencia de Blouin--------------------
# Crear data frames individuales
df_A <- data.frame(E_Blouin = E_Blouin_A)
df_R <- data.frame(E_Blouin = E_Blouin_r)

# Unirlos en un solo data frame
df_combined <- rbind(df_A, df_R)

# Calcular la media y la desviación estándar
mean_E_Blouin <- mean(df_combined$E_Blouin)
sd_E_Blouin <- sd(df_combined$E_Blouin)

# Imprimir los resultados
cat("Media:", mean_E_Blouin, "\n")
cat("Desviación estándar:", sd_E_Blouin, "\n")



#------------------------------Grafico ggplot Termorregulacion A/R---------------------------------------------------------------------
#install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
# "#EF6548", "#4EB3D3", "#66C2A5" Verde 

# Histograma 1: Temperatura Corporal en Remansos
p4 <- ggplot(T_Corporal_R, aes(x = T_Corporal_Remansos)) +
  geom_histogram(aes(fill = "T_Corporal"), color = "black", bins = 30) +
  geom_vline(aes(xintercept = Percentil_25_R, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = Percentil_75_R, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  annotate("text", x = 7, y = 45, label = paste("E=", round(promedio_E_R, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 7, y = 41, label = paste("de=", round(promedio_Calidad_termica_habita, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 7, y = 37, label = paste("db=", round(promedio_Precision_termorregulacion, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 7, y = 33, label = paste("Tpref=", round(Percentil_25_R, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 7, y = 29, label = paste("Tpref=", round(Percentil_75_R, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 7, y = 25, label = paste("E-Blou", round(E_Blouin_prom_R, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 7, y = 21, label = paste("Ex96=", round(ExCW96_R, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 7, y = 17, label = paste("Ex00", round(ExBW00_R, 4)), color = "black", fontface = "bold") +
  labs(x = "Remansos (2100 m)", y = "Frecuencia")+
  ylim(0, 50) +
  xlim(4, 22) +
  scale_fill_manual(values = c("T_Corporal" =  "#4EB3D3"), name = "") +
  scale_color_manual(values = c("T_Preferencia" = "red"), name = "") +
  theme(
    legend.position = c(0.20, 0.25),
    legend.background = element_rect(colour = NA, fill = NA),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.key.size = unit(0.6, "lines"),  # Tamaño de los cuadros de leyenda
    legend.text = element_text(size = 11),  # Tamaño del texto de leyenda
    legend.margin = margin(r = 10, l = 10),  # Añade un margen de 10 puntos a la derecha y izquierda
    legend.spacing = unit(4, "pt")) +  # Añade un espacio de 10 puntos entre las leyendas
  guides(fill = guide_legend(order = , title.theme = element_blank()),
         color = guide_legend(order = 2, title.theme = element_blank()))

#df_interpolated$Temperatura_Operativa_R_C
# Histograma 2: Temperatura Operativa en Remansos
p2 <- ggplot(df_filtered, aes(x = Temperatura_Operativa_R_C)) +
  geom_histogram(aes(fill = "T_Operativas"), color = "black", bins = 30) +
  geom_vline(aes(xintercept = Percentil_25_R, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = Percentil_75_R, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  #annotate("text", x = 6, y = 900, label = paste("de=", round(promedio_Calidad_termica_habita, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 6, y = 820, label = paste("db=", round(promedio_Precision_termorregulacion, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 6, y = 740, label = paste("E=", round(promedio_E_R, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 6, y = 660, label = paste("Tpref=", round(Percentil_25_R, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 6, y = 580, label = paste("Tpref=", round(Percentil_75_R, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 6, y = 660, label = paste("E-Blu", round(E_Blouin_prom_R, 4)), color = "black", fontface = "bold") + 
  #annotate("text", x = 6, y = 580, label = paste("ExCW96=", round(ExCW96_R, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 6, y = 500, label = paste("ExBW00", round(ExBW00_R, 4)), color = "black", fontface = "bold") +
  labs(x = "Remansos (2100 m)", y = "Frecuencia") +
  ylim(0, 1000) +
  xlim(4, 22) +
  scale_fill_manual(values = c("T_Operativas" = "#7FBC41"), name = "") +
  scale_color_manual(values = c("T_Preferencia" = "red"), name = "") +
  theme(
    legend.position = c(0.20, 0.25),
    legend.background = element_rect(colour = NA, fill = NA),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.key.size = unit(0.6, "lines"),  # Tamaño de los cuadros de leyenda
    legend.text = element_text(size = 11),  # Tamaño del texto de leyenda
    legend.margin = margin(r = 10, l = 10),  # Añade un margen de 10 puntos a la derecha y izquierda
    legend.spacing = unit(4, "pt")) +  # Añade un espacio de 10 puntos entre las leyendas
  guides(fill = guide_legend(order = , title.theme = element_blank()),
         color = guide_legend(order = 2, title.theme = element_blank()))

# Histograma 3: Temperatura Corporal en Asiria
p3 <- ggplot(T_Corporal_A, aes(x = T_Corporal_Asiria)) +
  geom_histogram(aes(fill = "T_Corporal"), color = "black", bins = 30) +
  geom_vline(aes(xintercept = Percentil_25_A, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = Percentil_75_A, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  #annotate("text", x = 19, y = 45, label = paste("de=", round(promedio_Calidad_termica_habita_A, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 19, y = 41, label = paste("db=", round(promedio_Precision_termorregulacion, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 19, y = 37, label = paste("E=", round(promedio_E_A, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 19, y = 33, label = paste("Tpref=", round(Percentil_25_A, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 19, y = 29, label = paste("Tpref=", round(Percentil_75_A, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 19, y = 33, label = paste("E-Blu=", round(E_Blouin_prom_A, 4)), fontface = "bold") +
  #annotate("text", x = 19, y = 29, label = paste("ExCW96=", round(ExCW96_R, 4)), color = "black", fontface = "bold") +
  #annotate("text", x = 19, y = 25, label = paste("ExBW00", round(ExBW00_R, 4)), color = "black", fontface = "bold") +
  labs(x = "Asiria (2600 m)", y = "Frecuencia") +
  ylim(0, 50) +
  xlim(4, 22) +
  scale_fill_manual(values = c("T_Corporal" = "#EF6548"), name = "") +
  scale_color_manual(values = c("T_Preferencia" = "red"), name = "") +
  theme(
    legend.position = c(0.8, 0.85),
    legend.background = element_rect(colour = NA, fill = NA),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.key.size = unit(0.6, "lines"),  # Tamaño de los cuadros de leyenda
    legend.text = element_text(size = 11),  # Tamaño del texto de leyenda
    legend.margin = margin(r = 10, l = 10),  # Añade un margen de 10 puntos a la derecha y izquierda
    legend.spacing = unit(4, "pt")) +  # Añade un espacio de 10 puntos entre las leyendas
  guides(fill = guide_legend(order = , title.theme = element_blank()),
         color = guide_legend(order = 2, title.theme = element_blank()))

# Histograma 4: Temperatura Operativa en Asiria

p1 <- ggplot(df_filtered_A, aes(x = Temperatura_Operativa_A_C)) +
  geom_histogram(aes(fill = "T_Operativas"), color = "black", bins = 30) +
  geom_vline(aes(xintercept = Percentil_25_A, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = Percentil_75_A, color = "T_Preferencia"), linetype = "dashed", size = 1) +
  annotate("text", x = 19, y = 900, label = paste("E=", round(promedio_E_A, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 19, y = 820, label = paste("de=", round(promedio_Calidad_termica_habita_A, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 19, y = 740, label = paste("db=", round(promedio_Precision_termorregulacion_A, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 19, y = 660, label = paste("Tpref=", round(Percentil_25_A, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 19, y = 580, label = paste("Tpref=", round(Percentil_75_A, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 19, y = 500, label = paste("E-Blou=", round(E_Blouin_prom_A, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 19, y = 420, label = paste("Ex96=", round(ExCW96_A, 4)), color = "black", fontface = "bold") +
  annotate("text", x = 19, y = 340, label = paste("Ex00=", round(ExBW00_A, 4)), color = "black", fontface = "bold") +
  labs(x = "Asiria (2600 m)", y = "Frecuencia") +
  ylim(0, 1000) +
  xlim(4, 22) +
  scale_fill_manual(values = c("T_Operativas" = "#7FBC41"), name = "") +
  scale_color_manual(values = c("T_Preferencia" = "red"), name = "") +
  theme(
    legend.position = c(0.8, 0.25),
    legend.background = element_rect(colour = NA, fill = NA),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.key.size = unit(0.6, "lines"),  # Tamaño de los cuadros de leyenda
    legend.text = element_text(size = 11),  # Tamaño del texto de leyenda
    legend.margin = margin(r = 10, l = 10),  # Añade un margen de 10 puntos a la derecha y izquierda
    legend.spacing = unit(4, "pt")) +  # Añade un espacio de 10 puntos entre las leyendas
  guides(fill = guide_legend(order = , title.theme = element_blank()),
         color = guide_legend(order = 2, title.theme = element_blank()))



library(gridExtra)
library(ggplot2)
library(grid)


# Convertir los gráficos en objetos grob
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)

# Encontrar las dimensiones máximas de ancho y alto
max_widths <- do.call(unit.pmax, lapply(list(g1, g2, g3, g4), function(x) x$widths))
max_heights <- do.call(unit.pmax, lapply(list(g1, g2, g3, g4), function(x) x$heights))

# Ajustar las dimensiones de los grobs
g1$widths <- max_widths
g2$widths <- max_widths
g3$widths <- max_widths
g4$widths <- max_widths

g1$heights <- max_heights
g2$heights <- max_heights
g3$heights <- max_heights
g4$heights <- max_heights

# Organizar los gráficos en una cuadrícula
Te <- grid.arrange(g3, g4, g1, g2,  
                   ncol = 2, nrow = 2, 
                   widths = c(1, 1), 
                   heights = c(1, 1), 
                   padding = unit(0.1, "line"))


#----------------- Grafico Regresiones lineales A/R (Validacion)---------------------
remove.packages("gridExtra")
install.packages("gridExtra")

library(gridExtra)
library(ggplot2)
library(ggplot2)
#"#EF6548", "#4EB3D3"
# Crear gráfico de dispersión con línea de regresión lineal para Modelo_A
gráfico_correlacion_A <- ggplot(Modelo_A, aes(x = x_A, y = y_A)) +
  geom_point(color = "#EF6548", alpha = 0.8) + # Puntos de datos
  geom_smooth(method = "lm", se = FALSE, color = "red3", linetype = "dashed") + # Línea de regresión
  annotate("text", x = 16, y = 23, label = paste("R² =", round(R2_A, 3)), color = "black", fontface = "bold", size = 8) +
  annotate("text", x = 15.8, y = 22, label = paste("n =", round(n_datos_A, 4)), color = "black", fontface = "bold", size = 8) +
  labs(title = "Asiria (2600 msnm)",
       x = "Sp_A",
       y = "Agar_A") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray93"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold")) +
  scale_x_continuous(limits = c(14, 25)) + # Establecer límites x
  scale_y_continuous(limits = c(14, 25)) # Establecer límites y

# Crear gráfico de dispersión con línea de regresión lineal para Modelo_B
gráfico_correlacion_R <- ggplot(Modelo_R, aes(x = x_R, y = y_R)) +
  geom_point(color = "#4EB3D3", alpha = 0.8) + # Puntos de datos
  geom_smooth(method = "lm", se = FALSE, color = "red3", linetype = "dashed") + # Línea de regresión
  annotate("text", x = 16, y = 23, label = paste("R² =", round(R2_R, 3)), color = "black", fontface = "bold", size = 8) +
  annotate("text", x = 15.8, y = 22, label = paste("n =", round(n_datos_R, 4)), color = "black", fontface = "bold", size = 8) +
  labs(title = "Remansos (2100 msnm)",
       x = "Sp_R",
       y = "Agar_R") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "gray93"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12, face = "bold")) +
  scale_x_continuous(limits = c(14, 25)) + # Establecer límites x
  scale_y_continuous(limits = c(14, 25)) # Establecer límites y

# Combinar gráficos utilizando grid.arrange()
gráficos_combinados <- grid.arrange(gráfico_correlacion_A, gráfico_correlacion_R, ncol = 2)
print(gráficos_combinados)
gráficos_combinados

install.packages("ggpubr")
library(ggpubr)


# Combinar gráficos en un solo objeto usando ggpubr
gráficos_combinados <- ggarrange(gráfico_correlacion_A, gráfico_correlacion_R, 
                                 ncol = 2, nrow = 1)

# Mostrar el objeto combinado
print(gráficos_combinados)

#---------------------------- Tabla A/R-------------------------------------------------------

#install.packages("ggplot2")
#install.packages("flextable")
#install.packages("ftExtra")

library(ggplot2)
library(flextable)
library(ftExtra)





# Cargar indices 
Datos_tabla <- data.frame(
  Variable = c("Temperatura Corporal (Tc)", "Temperatura Operativa (To)", "Temperatura de preferencia (Tpref)",   "Modelo nulo R²",  "Calidad térmica del hábitat (de)", 
               "Exactitud en la termorregulación (db)", "Temperatura de preferencia 25%", "Temperatura de preferencia 75%", "Eficiencia en la termorregulación (E)",
               "Eficiencia en la termorregulación (E-Blou)", "Aprovechamiento térmico (Ex96)", "Desaprovechamiento térmico (Ex00)", "Temperaturas corporales en campo (n)"),
  Asiria = c(TCA$mean, TOA$mean, TPA$mean, R2_A,  promedio_de_A, promedio_db_A, Percentil_25_A, Percentil_75_A, promedio_E_A, E_Blouin_prom_A, ExCW96_A, ExBW00_A, n_datos_Corp_A),
  Remansos = c(TCR$mean, TOR$mean, TPR$mean, R2_R, promedio_de_R, promedio_db, Percentil_25_R, Percentil_75_R,  promedio_E_R, E_Blouin_prom_R, ExCW96_R, ExBW00_R, n_datos_Corp_R)
)
library(flextable)

ft <- flextable(Datos_tabla)

ft <- flextable(Datos_tabla) %>%
  colformat_double(digits = 4) %>%
  set_header_labels(Variable = "Variable",
                    Asiria = "Asiria",
                    Remansos = "Remansos") %>%
  theme_vanilla()

library(dplyr)

ft <- ft %>%
  set_col_order(c("Indices de termorregulacion", "Asiria", "Remansos")) %>%
  set_row_order(c("Modelo nulo R²", "Eficiencia en la termorregulación (E)", "Calidad térmica del hábitat (de)", "Exactitud en la termorregulación (db)", "Temperatura de preferencia 25%", "Temperatura de preferencia 75%", "Eficiencia en la termorregulación estacional (E-Blou)", "Aprovechamiento térmico (Ex96)", "Desaprovechamiento térmico (Ex00)", "Individuos (n)"))
print(ft)

#Tabla GTR5
{ 
  library(flextable)
  library(magrittr)
  
  
  # Mostrar la tabla
  ft
  
  
  # Definir nombre del archivo
  nombre_archivo <- "tabla_TermorregulacionN.docx"
  
  # Definir ubicación del archivo
  ubicacion_archivo <- "D:/Resultados B.tamaense/Termorregulacion/Plots/"
  
  # Ruta completa donde deseas guardar el archivo
  ruta_archivo <- paste0(ubicacion_archivo, nombre_archivo)
  
  # Crear un documento Word
  doc <- read_docx()
  
  # Añadir la tabla ft_RG5 al documento
  doc <- doc %>%
    body_add_flextable(ft)
  
  # Guardar el documento Word en la ubicación especificada
  print(doc, target = ruta_archivo)
}

#---------------Grafico powerpointbien ----------------------------
#GTR1
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  library(magrittr)  # Asegúrate de cargar el paquete magrittr
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(gráficos_combinados))
  p_dmlft <- rvg::dml(code = grid::grid.draw(gráficos_combinados))
  # Especificar la carpeta y el nombre del archivo
  folder_path <- 'D:/Resultados B.tamaense/Termorregulacion/Plots' # Reemplaza con la ruta de tu carpeta
  file_name <- "regresion2.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}





#---------------Grafico powerpointbien ----------------------------
#GTR1
{
  # Cargar los paquetes necesarios
  library(officer)
  library(rvg)
  library(magrittr)  # Asegúrate de cargar el paquete magrittr
  # Suponiendo que G1 ya existe, lo convertimos a formato DML
  p_dmlft <- rvg::dml(code = print(Te))
  p_dmlft <- rvg::dml(code = grid::grid.draw(Te))
  # Especificar la carpeta y el nombre del archivo
  folder_path <- 'D:/Resultados B.tamaense/Termorregulacion/Plots' # Reemplaza con la ruta de tu carpeta
  file_name <- "Te2.pptx" # Reemplaza con el nombre del archivo que desees
  
  # Crear la ruta completa del archivo
  output_path <- file.path(folder_path, file_name)
  
  # Crear un nuevo documento PowerPoint, añadir una diapositiva, insertar el gráfico y guardar el archivo
  officer::read_pptx() %>%
    officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
    officer::ph_with(p_dmlft, location = officer::ph_location_fullsize()) %>%
    base::print(target = output_path)
}







#----------------Tablas y graficos antiguos-------

# Cargar librerías necesarias
library(flextable)
library(dplyr)

# Crear los datos de la tabla
Datos_tabla <- data.frame(
  Variable = c("Modelo nulo R²", "Eficiencia en la termorregulación (E)", "Calidad térmica del hábitat (de)", 
               "Exactitud en la termorregulación (db)", "Temperatura de preferencia 25%", "Temperatura de preferencia 75%", 
               "Eficiencia en la termorregulación (E-Blou)", "Aprovechamiento térmico (Ex96)", "Desaprovechamiento térmico (Ex00)", "Individuos (n)"),
  Asiria = c(R2_A, promedio_E_A, promedio_de_A, promedio_db_A, Percentil_25_A, Percentil_75_A, E_Blouin_prom_A, ExCW96_A, ExBW00_A, 63),
  Remansos = c(R2_R, promedio_E_R, promedio_de_R, promedio_db_R, Percentil_25_R, Percentil_75_R, E_Blouin_prom_R, ExCW96_R, ExBW00_R, 84)
)

# Transponer la tabla
Datos_tabla_transpuesta <- as.data.frame(t(Datos_tabla))

# Renombrar las columnas y filas de la tabla transpuesta
colnames(Datos_tabla_transpuesta) <- Datos_tabla$Variable
Datos_tabla_transpuesta <- Datos_tabla_transpuesta[-1,]  # Eliminar la fila de nombres de las variables
Datos_tabla_transpuesta <- cbind(Indice = rownames(Datos_tabla_transpuesta), Datos_tabla_transpuesta)

# Convertir la tabla transpuesta a flextable
ft <- flextable(Datos_tabla_transpuesta) %>%
  colformat_double(digits = 4) %>%
  set_header_labels(
    Indice = "Índice",
    `Modelo nulo R²` = "R²",
    `Eficiencia en la termorregulación (E)` = "(E)",
    `Calidad térmica del hábitat (de)` = "(de)",
    `Exactitud en la termorregulación (db)` = "(db)",
    `Temperatura de preferencia 25%` = "Q 25%",
    `Temperatura de preferencia 75%` = "Q 75%",
    `Eficiencia en la termorregulación (E-Blou)` = "(E-Blou)",
    `Aprovechamiento térmico (Ex96)` = "(Ex96)",
    `Desaprovechamiento térmico (Ex00)` = "(Ex00)",
    `Individuos (n)` = "(n)"
  ) %>%
  theme_vanilla()


# Exportar la tabla a un archivo Word
library(officer)
doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "Tabla4.docx")



# Imprimir la flextable
print(ft)

file.choose()
setwd('D:/Resultados B.tamaense/Termorregulacion/Plots')
#D:/Resultados B.tamaense/Termorregulacion/Plots

pdf(file="ft.pdf")
ft
dev.off()

# Mostrar el gráfico
print(gráficos_combinados)

