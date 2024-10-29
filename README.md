# Thermal-Ecology-and-Microhabitat-Use-of-Bolitoglossa-tamaense-in-the-Northeastern-Colombian-Andes.
El objeto de este estudio es comprender los efectos de las variaciones del hábitat, en la ecología térmica y el uso de microhábitat empleando dos poblaciones de B. tamaense ubicadas en los extremos altitudinales de su distribución en Colombia. 

1.1	Análisis de Datos 

1.1.1	Objetivo 1: Estrategia de Termorregulación 
Para distinguir entre especies termorreguladoras y termo conformistas, se emplearon los índices de termorregulación propuestos por Hertz et al. (1993). Asimismo, se empleó el análisis de la asimetría y la curtosis de las temperaturas seleccionadas en el gradiente térmico (Dewitt y Friedman, 1979).

Precisión en la Termorregulación
Siguiendo las metodologías antes mencionadas se calculó la exactitud en la termorregulación (db) como la desviación media absoluta de la temperatura corporal (Tc) respecto al 50% central de las temperaturas preferidas (Tsel). Este índice mide cuánto se desvía la Tc respecto a su óptimo; donde valores próximos a cero indicaron una alta precisión en la termorregulación y viceversa.

Calidad Térmica del Hábitat 
Asimismo, se calculó el índice de calidad térmica del hábitat (de) como la desviación media absoluta de las temperaturas operativas (To) respecto al 50% central de las temperaturas preferidas (Tsel). Este índice mide cuánto se desvían las To respecto al óptimo preferido; valores próximos a cero indican alta calidad térmica del hábitat y valores cercanos a 1 indican que las temperaturas micro ambiéntales están fuera de su óptimo de preferencia o selección (Hertz, 1993; Navas et al., 2021; Ortega, 2015).

Eficiencia en la Termorregulación
Finalmente, se calculó la eficiencia en la termorregulación (E) como E = 1-(db/de). Donde valores de E cercanos a 0 indican termoconformismo ineficiente y valores cercanos a 1 indican termorregulación activamente eficiente. En consecuencia, fue utilizado el índice de Blouin-Demers y Weatherhead (2001) para medir la desviación de la termo conformidad perfecta mediante la fórmula de – db (Blouin-Demers y Nadeau, 2005), denominado (E-Blouin).

Aprovechamiento Térmico del Hábitat
Para distinguir el porcentaje de hábitat térmico que usa la especie fue calculado el índice de aprovechamiento térmico de Christian y Weavers (1996) (Ex96), considerando como el porcentaje de Tc dentro de la Tsel. Además, se aplicaron las modificaciones de Brown y Weatherhead (2000), denominada (Ex00) para determinar la proporción de datos de Tc que caían por encima o por debajo de Tsel.

Distribución, Asimetría y Curtosis de las temperaturas de selección 
Para determinar el comportamiento termorregulador dentro del gradiente térmico, se analizaron los parámetros de la distribución de la temperatura seleccionada (Tsel). Evaluando la curtosis de los datos de Tsel mediante una prueba de bondad de ajuste de Jarque-Bera., empleando el paquete matrixTests (Koncevičius, 2021). Una distribución de Tsel con una curtosis menor a 3 se considera platicúrtica, sugiriendo que las salamandras utilizan indistintamente un rango amplio dentro de las temperaturas seleccionadas en el gradiente térmico. Por el contrario, una curtosis mayor a 3 indica una distribución leptocúrtica, donde los individuos buscan preferentemente en un rango más corto dentro de temperaturas seleccionadas en el gradiente térmico. Asimismo, se utilizó una prueba t para distinguir si las temperaturas de selección se distanciaban significativamente de cero., así como densidad de Kernel para distinguir las temperaturas seleccionadas con mayor frecuencia en cada una de las poblaciones (Giacometti y Tattersall, 2024). Todo esto mediante el paquete stats (R Core Team, 2024).  

Todos los análisis fueron realizados usando el software estadístico R (R Core Team, 2024). Utilizando paquetes como openxlsx, readxl, writexl para el manejo de archivos Excel (Walker, 2023; Wickham et al., 2023; Ooms, 2023); dplyr, tidyr para la manipulación de los datos (Wickham et al., 2023); hms para el manejo de tiempos y fechas (Müller & Wickham, 2023).

Asimismo, se usó el paquete psych para los análisis psicométricos (Revelle, 2023); ggplot2 y plotly para la visualización de datos (Wickham, 2023; Sievert, 2020); gridExtra y grid para la disposición de gráficos en cuadrículas (Auguie, 2017; R Core Team, 2024), así como flextable, ftExtra, officer para la creación, manejo de tablas y documentos de Microsoft Office (Xie et al., 2023; Gohel, 2023; Yu, 2023).

1.1.2	Objetivo 2: Uso y Selección de Microhábitat
Uso de Microhábitat
Para analizar el uso del Microhábitat se realizó una descripción de las variables, seguida de pruebas de normalidad y homogeneidad de varianza utilizando los tests de Shapiro-Wilk. Así como, la t de Student y Mann-Whitney U test respectivamente. Posteriormente se utilizo un Análisis de patrones multinivel para las variables cuantitativas y cualitativas indicspecies y fastDummies (Caceres y Jansen, 2023; Schaefer, 2023)
 Luego, se realizó un análisis factorial de datos mixtos (FAMD) para reducir la dimensión e interpretar la estructura de los datos. Este proceso se llevó a cabo utilizando los paquetes Psych (Revelle, 2018), FactoMineR (Lê et al., 2008), Car (Fox y Weisberg, 2019), Corrplot (Wei y Simko, 2017) y Readxl (Wickham et al., 2019). 

Los análisis fueron aplicados individualmente para cada zona de estudio como para todos los datos en general. Posteriormente, se calculó la matriz de disimilitud de Gower, utilizando el paquete clúster (Maechler et al., 2019). El paquete vegan (Oksanen et al., 2019) fue utilizado para realizar un análisis de la varianza (ANOVA), el Análisis de Similaridad por Rango (ANOSIM) y un Análisis de Variación Permutacional Multivariado (PERMANOVA). Y el escalado multidimensional no métrico (NMDS) con agrupación jerárquica.


Uso y Selección de Microhábitat 
El uso y selección de microhábitat se analizó mediante los métodos propuestos por Manly, utilizando un diseño de disponibilidad de recurso tipo dos, según Manly et al. (2002), y el índice propuesto por Manly et al. (2007). Bajo este índice, Wi indica la tasa de uso de recursos, es decir, el número de veces que la especie de estudio utilizó un recurso específico en proporción a la disponibilidad de dicho recurso. Para obtener Wi, es necesario identificar Oi (el uso del recurso específico) y Pi (la disponibilidad del recurso no utilizado o disponible para la especie), este recurso correspondiente a las variables de uso y selección de vegetación.

Asimismo, se calculó el Alpha de Manly (α) como la proporción de la tasa de uso total representada por el recurso. En consecuencia, la sumatoria hace referencia a todas las tasas de uso de los recursos evaluados. Valores Wi mayores a 1 indican una selección positiva del hábitat específico, mientras que valores Wi menores a 1 indican una selección negativa del mismo (Manly et al., 2007; Morillas et al., 2023). 

Todo esto fue realizado mediante el software estadístico de base R (R Core Team, 2024). Utilizando paquetes como openxlsx, readxl, writexl para el manejo de archivos Excel (Walker, 2023; Wickham et al., 2023; Ooms, 2023); dplyr, tidyr para la manipulación de los datos.


Relación de la Eficiencia Térmica y el Microhábitat 
Finalmente, se utilizaron Modelos lineales generalizados para establecer la relación entre variables con ayuda del paquete MASS (Venables, 2022) y el paquete car (Fox y Weisberg, 2019). Asimismo, para evaluar la multicolinealidad entre las variables independientes mediante un análisis del Factor de Inflación de la Varianza (VIF).  Y en última instancia, se aplicó un Análisis de redundancia basado en distancias entre las variables, con el uso de paquetes como cluster (Maechler et al., 2019), y vegan (Oksanen et al., 2019).

1.1.3	Objetivo 3: Tamaño Corporal 
Variaciones en el Tamaño Corporal
Para describir el tamaño corporal, se utilizó estadística descriptiva para cada una de las medidas., seguida de pruebas de normalidad y homogeneidad de varianza utilizando los tests de Shapiro-Wilk. Así como, la t de Student y Mann-Whitney U test respectivamente. Posteriormente se utilizo un Análisis de patrones multinivel para las variables de tamaño corporal, ussando los paquetes indicspecies y fastDummies (Caceres y Jansen, 2023; Schaefer, 2023)

Asimismo, se redujo la dimensionalidad de las variables y se buscó las variables de mayor explicación mediante matrices de correlación y análisis de componentes principales (PCA) y análisis de conglomerados (clúster), utilizando los paquetes Psych (Revelle, 2018), FactoMineR (Lê et al., 2008), factoextra (Kassambara y Mundt, 2020), Car (Fox y Weisberg, 2019), Corrplot (Wei y Simko, 2017) y Readxl (Wickham et al., 2019). 

Los análisis se aplicaron tanto para cada zona de estudio por separado como para todos los datos en general. Posteriormente, para determinar las diferencias entre zonas de estudio y la temporada climática en la que se muestreo, se calcularon la matriz de disimilitud euclidiana utilizando el paquete clúster (Maechler et al., 2019) y vegan (Oksanen et al., 2019). Asimismo, fue realizado un análisis de la varianza (ANOVA), el Análisis de Similaridad por Rango (ANOSIM) y un Análisis de Variación Permutacional Multivariado (PERMANOVA).

Relación del Tamaño Corporal y el Hábitat
Para establecer la relación entre las variables de mayor contribución en el tamaño corporal y la matriz de disimilitud del hábitat, se utilizaron Modelos lineales generalizados con ayuda del paquete MASS (Venables, 2022) y el paquete car (Fox y Weisberg, 2019). Asimismo, para evaluar la multicolinealidad entre las variables independientes mediante un análisis del Factor de Inflación de la Varianza (VIF).   

Relación de la Eficiencia Térmica y el Hábitat
Para establecer la relación entre las variables de mayor contribución en el tamaño corporal y la eficiencia en la termorregulación, se utilizaron Modelos lineales generalizados con ayuda del paquete MASS (Venables, 2022) y el paquete car (Fox y Weisberg, 2019). En último, para evaluar la multicolinealidad entre las variables independientes mediante un análisis del Factor de Inflación de la Varianza (VIF).   

1.1.4	Relación entre Objetivos Mediante Modelo de Ecuaciones Estructurales con Mínimos Cuadrados Parciales (PLS-SEM) 
Para establecer la relación lineal entre los fenómenos que componen la ecología térmica y el uso de microhábitat de Bolitoglossa tamaense se realizó un modelo de ecuaciones estructurales con mínimos cuadrados parciales (PLS-SEM) (Lohmöller, 1989; Wold, 1985). Con ayuda del paquete SEMinR y seminrstudio (Calero, 2024; Ray y Calero, 2024; R Core Team, 2024).

Utilizando paquetes como openxlsx, readxl, writexl para el manejo de archivos Excel (Walker, 2023; Wickham et al., 2023; Ooms, 2023); dplyr, tidyr para la manipulación de los datos y Readxl (Wickham et al., 2019). flextable, ftExtra, officer para la creación y manejo de tablas y documentos de Microsoft Office (Xie et al., 2023; Gohel, 2023; Yu, 2023).

Los PLS-SEM cómo se observar en la Figura 5, se forman a partir de un modelo de medida que se establece en múltiples variables observadas o indicadores medidos en campo (Ramirez et al., 2014). Los cuales representan fenómenos complejos de medir directamente en la práctica., a los que se les denomina constructos o variables latentes (VL) (Martínez y Fierro, 2018). Asimismo, los PLS-SEM se componen de un modelo estructural el cual establece las diferentes hipótesis de relación entre los distintos constructos (Hair et al., 2017). 

 En consecuencia, se realizó un modelo de medición con enfoque formativo donde las variables observadas dan forma al constructo, y reflectivo donde las variables observadas son un reflejo del mismo (Becker et al., 2018; Hair et al., 2017). Para esto se establecieron tres constructos formativos, denominados como: Ubicación temporal espacial (UTE), uso y selección de vegetación (USV), uso de microhábitat (UM). Y dos constructos reflectivos denominados como: tamaño corporal (T) y Eficiencia en la termorregulación (E). 

Para la creación del modelo estructural se plantearon 10 hipótesis de relación positiva producto de los planteamientos y resultados obtenidos en esta misma investigación. Con los cuales se analizó la ecología térmica y uso de microhábitat de Bolitoglossa tamense mediante la relación de UTE con UM, USV, T y E. Asimismo, como UM se relaciona con USV, T y E., Como USV se relaciona con T y E. y en ultimo como T se relaciona con E.

Figura 5
Modelo de medida y estructural PLS-SEM para la ecología térmica y uso de microhábitat de Bolitoglossa tamaense
 
Nota. Los rectángulos amarillos muestran las variables de importancia observadas en campo, los hexágonos muestran los constructos formativos y los círculos los constructos reflectivos, mientras que las líneas verdes muestran las hipótesis de relación positiva del modelo.

Una vez realizado el modelo, es crucial evaluar su calidad para garantizar que cumple con los estándares estadísticos aceptables, lo cual, en el contexto de PLS-SEM, requiere al menos 100 observaciones (Reinartz, Haenlein, & Henseler, 2009). Es fundamental evaluar por separado los modelos de medida formativos y reflectivos, así como realizar la respectiva evaluación del modelo estructural (Hair et al., 2017; Martínez & Fierro, 2018).

Para evaluar los modelos de medida formativos se verifica la validez convergente mediante la fiabilidad predictiva de los indicadores (Barclay et al., 1995)., la colinealidad entre variables observadas mediante el factor de inflación de la varianza (VIF) donde valores superiores a 10 indican multicolinealidada (Myers, 1990)., y la significancia y relevancia de los pesos (w) para los indicadores (Hair et al., 2017; Martínez y Fierro, 2018). 

Para evaluar los modelos de medida reflectivos se tuvo en cuenta la consistencia interna de los indicadores mediante el Alfa de Cronbach (alpha), la confiabilidad compuesta (rhoC), la fiabilidad del constructo o rho de Dillon-Goldstein (RhoA), la validez convergente entre la carga de los indicadores y la varianza media extraída (AVE), donde valores superiores a 0.70 son los ideales (NUNNALLY, 1978). Asimismo, la AVE indica la varianza media extraída que refleja la variable latente y por si sola los valores adecuados superan o igualan a 0.50 (CHIN, 1998). Fornell y Larcker (1981). Por último, para asegurar que cada constructo sea distinto, se evaluó la validez discriminante mediante el criterio de Heterotrait-Monotrait Ratio (HTMT) y el Fornell Larcker (FL), donde valores 0.85 son los ideales.

 Finalmente, para evaluar el modelo estructural es necesario verificar el coeficiente de determinación (R2) de los constructos donde 1 indica una explicación perfecta de la varianza, la relevancia o capacidad predictiva (Q2) donde valores positivos indican un buen ajuste de los mismos., el tamaño negativo o positivo y la significancia p valor mediante un bootstrapping y los coeficientes path para las hipótesis establecidas y el tamaño de efectos (f2 y q2) para los constructos que componen el modelo, donde 0.02 indica un efecto predictivo pequeño, 0.15 un efecto mediano y 0.35 un gran efecto predictivo (Hair et al., 2017).
Referencias 
Auguie, B. (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics. R package version 2.3. https://CRAN.R-project.org/package=gridExtra
Barclay, D., Higgins, C., & Thompson, R. (1995). The partial least squares (PLS) approach to causal modelling: Personal computer adoption and use as an illustration. Technology Studies, 2(2), 285-309.
Becker, J.-M., Ringle, C. M., & Sarstedt, M. (2018). Estimating moderating effects in PLS-SEM and PLSc-SEM: Interaction term generation*data treatment. Journal of Applied Structural Equation Modeling, 2(2), 1–21. https://doi.org/10.47263/JASEM.2(2)01
Blouin-Demers, G. and Nadeau, P. (2005), The cost–benefit model of thermoregulation does not predict lizard thermoregulatory behavior. Ecology, 86: 560-566. https://doi.org/10.1890/04-1403
Blouin-Demers, G., y Weatherhead, P. J. (2001). Thermal ecology of black rat snakes (Elaphe obsoleta) in a thermally challenging environment. Ecology, 82(11), 3025-3043.  
Brown, G. P., y Weatherhead, P. J. (2000). Thermal ecology and sexual size dimorphism in northern water snakes, Nerodia sipedon. Ecological monographs, 70(2), 311-330.  
Brown, G. P., y Weatherhead, P. J. (2000). Thermal ecology and sexual size dimorphism in northern water snakes, Nerodia sipedon. Ecological monographs, 70(2), 311-330.  
Caceres, M., & Jansen, F. (2023). indicspecies: Relationship Between Species and Groups of Sites. R package version 1.8.0. Retrieved from https://cran.r-project.org/web/packages/indicspecies/index.html
Calero Valdez, A. (2024). seminrstudio: seminrstudio provides an interactive interface to the seminr package (Version 0.0.1) [R package]. GitHub. https://github.com/sem-in-r/seminrstudio
Chin, W. W. (1998). The partial least squares approach for structural equation modeling. En G. A. Marcoulides (Ed.), Modern methods for business research (pp. 295-336). Lawrence Erlbaum Associates.
Christian, K. A., y Weavers, B. W. (1996). Thermoregulation of monitor lizards in Australia: an evaluation of methods in thermal biology. Ecological monographs, 139-157.
Fornell, C., & Larcker, D. F. (1981). Evaluating Structural Equation Models with Unobservable Variables and Measurement Error. Journal of Marketing Research, 18(1), 39–50. https://doi.org/10.2307/3151312
Fox, J., & Weisberg, S. (2019). Car: Companion to Applied Regression (Version 3.0-4) [Software]. https://cran.r-project.org/web/packages/car/index.html  
Giacometti, D. y Tattersall, GJ (2024). Variación estacional de la termorregulación conductual en una salamandra fosorial ( Ambystoma maculatum ). Ciencia Abierta de la Royal Society, 11 (240537). https://doi.org/10.1098/rsos.240537
Gohel, D. (2023). flextable: Functions for Tabular Reporting. R package version 0.6.6. https://CRAN.R-project.org/package=flextable  
Hair, J., Hult, T., Ringle, C., Sarstedt, M., Castillo, J., Cepeda, G., & Roldan, J. (2019). Manual de Partial Least Squares Structural Equation Modeling (PLS-SEM) (2nd ed.). Sage Publishing. https//doi.org/10.39926/oss.37
Hertz, P. E., Huey, R. B., & Stevenson, R. D. (1993). Evaluating temperature regulation by field-active ectotherms: the fallacy of the inappropriate question. The American naturalist, 142(5), 796–818. https://doi.org/10.1086/285573
Kassambara, A., & Mundt, F. (2020). factoextra: Extract and Visualize the Results of Multivariate Data Analyses (Version 1.0.7) [Software]. https://CRAN.R-project.org/package=factoextra  
Koncevičius, K. ( 2021 ) . matrixTests : Pruebas de hipótesis estadísticas rápidas en filas y columnas de matrices . Paquete R. https://CRAN.R-project.org/package=matrixTests
Lê, S., Josse, J., & Husson, F. (2008). FactoMineR: An R Package for Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18. https://doi.org/10.18637/jss.v025.i01  
Lohmöller, J.-B. (1989). Modelado de rutas de variables latentes con mínimos cuadrados parciales. Heidelberg: Física.
Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K. (2019). cluster: Cluster Analysis Basics and Extensions (R package version 2.1.0) [Software]. Retrieved from https://cran.r-project.org/web/packages/cluster/index.html  
Manly, B. F. L., McDonald, L., Thomas, D. L., McDonald, T. L., Erickson, W. P. (2007). Resource selection by animals: Statistical design and analysis for field studies. Londres: Chapman & Hall Press.
Manly, B.F.J., McDonald, L.L., Thomas, D.L., McDonald, T.L., &Erickson, E.P. (2002). Resource selection by animals: statistical design and analysis of field studies (2a Ed.). The Netherlands: Springer Netherlands
Martínez Ávila, Minerva, & Fierro Moreno, Eréndira. (2018). Aplicación de la técnica PLS-SEM en la gestión del conocimiento: un enfoque técnico práctico. RIDE. Revista Iberoamericana para la Investigación y el Desarrollo Educativo, 8(16), 130-164. https://doi.org/10.23913/ride.v8i16.336
Morillas, E., Torrico-Paz, S., Romero, L., Zurita, L., Antezana, N., Carvajal, G., Miranda-Calle, A. B., & Pacheco, L. F. (2023). Ámbito de hogar y uso de microhábitat de Liolaemus forsteri (Iguania: Liolaemidae) en una región altoandina de La Paz, Bolivia. Acta zoológica lilloana, 67(1), 121-135. DOI: https://doi.org/10.30550/j.azl/2023.67.1/2023-03-06 
Müller, K., & Wickham, H. (2023). hms: Pretty Time of Day. R package version 1.1.1. https://CRAN.R-project.org/package=hms  
Myers, R. M. (1990). Classical and modern regression with applications (2nd ed.). Duxbury Press.
Navas, C. A., Gouveia, S. F., Solano-Iguarán, J. J., Vidal, M. A., & Bacigalupe, L. D. (2021). Amphibian responses in experimental thermal gradients: Concepts and limits for inference. Comparative Biochemistry and Physiology Part B: Biochemistry and Molecular Biology, 254, 110576. https://doi.org/10.1016/j.cbpb.2021.110576
Nunnally, J. (1978). Psychometric theory (2.ª ed.). McGraw-Hill.
Oksanen, J., Blanchet, F. G., Friendly, M., Kindt, R., Legendre, P., McGlinn, D., Minchin, P. R., et al. (2019). Vegan: Community Ecology Package (Version 2.5-6) [Software]. https://cran.r-project.org/web/packages/vegan/index.html  
Ooms, J. (2023). writexl: Export Data Frames to 'xlsx' Format. R package version 1.4.0. https://CRAN.R-project.org/package=writexl  
Ortega Diago, Z. (2015). Biología térmica de lagartijas de alta montaña del género Iberolacerta. [Tesis de grado para Doctor en Ciencias, Universidad de Salamanca, España]. https://doi.org/10.14201/GREDOS.128479 
R Core Team. (2024). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/ 
Ramírez, P. E., Mariano, A. M., & Salazar, E. A. (2014). Propuesta Metodológica para aplicar modelos de ecuaciones estructurales con PLS: El caso del uso de las bases de datos científicas en estudiantes universitarios. Revista ADMPG, 7(2). Recuperado de https://revistas.uepg.br/index.php/admpg/article/view/14062
Ray, S., Danks, N., & Calero Valdez, A. (2024). seminr: Building and Estimating Structural Equation Models (Versión 2.3.3) [Paquete R]. CRAN. https://CRAN.R-project.org/package=seminr
Reinartz, W., Haenlein, M., & Henseler, J. (2009). An empirical comparison of the efficacy of covariance-based and variance-based SEM. International Journal of Research in Marketing, 26(4), 332-344. https://doi.org/10.1016/j.ijresmar.2009.08.001
Revelle, W. (2023). psych: Procedures for Psychological, Psychometric, and Personality Research. R package version 2.3.6. https://CRAN.R-project.org/package=psych  
Schaefer, J. (2023). fastDummies: Fast Creation of Dummy (Binary) Columns and Rows from Categorical Variables. R package version 1.7.3. Retrieved from https://cran.r-project.org/web/packages/fastDummies/index.html 
Sievert, C. (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC. https://plotly-r.com 
Venables, W. N., & Ripley, B. D. (2002). Modern Applied Statistics with S (4ª ed.). Springer, New York. ISBN 0-387-95457-0.
Walker, A. (2023). openxlsx: Read, Write and Edit XLSX Files. R package version 4.2.5. https://CRAN.R-project.org/package=openxlsx  
Wei, T., & Simko, V. (2017). Corrplot: Visualization of a Correlation Matrix (Version 0.84) [Software]. https://cran.r-project.org/web/packages/corrplot/index.html  
Wickham, H. (2023). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. https://ggplot2.tidyverse.org  
Wickham, H., Bryan, J., & Lin, X. (2019). Readxl: Read Excel Files (Version 1.3.1) [Software]. https://cran.r-project.org/web/packages/readxl/index.html  
Wickham, H., François, R., Henry, L., & Müller, K. (2023). dplyr: A Grammar of Data Manipulation. R package version 1.1.2. https://CRAN.R-project.org/package=dplyr  
Wold, H. (1985). En S. Kotz y NL Johnson (Eds.), Enciclopedia de ciencias estadísticas. Mínimos cuadrados parciales (Vol. 6, págs. 581-591). New York, NY: Wiley
Xie, Y., Cheng, J., & Tan, X. (2023). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.27. https://CRAN.R-project.org/package=DT  
Yu, G. (2023). ftExtra: Extra Functions for 'flextable'. R package version 0.0.5. https://CRAN.R-project.org/package=ftExtra  
