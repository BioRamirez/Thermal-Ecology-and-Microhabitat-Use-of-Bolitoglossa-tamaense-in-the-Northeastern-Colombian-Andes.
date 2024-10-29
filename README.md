# Thermal-Ecology-and-Microhabitat-Use-of-Bolitoglossa-tamaense-in-the-Northeastern-Colombian-Andes.
The objective of this study is to understand the effects of habitat variations on thermal ecology and microhabitat use using two populations of B. tamaense located at the altitudinal extremes of its distribution in Colombia.. 

1.1	Data Analysis 

1.1.1	Objective 1: Thermoregulation Strategy 
To distinguish between thermoregulatory and thermoconformist species, the thermoregulation indices proposed by Hertz et al. (1993). Likewise, the analysis of the skewness and kurtosis of the selected temperatures in the thermal gradient was used (Dewitt and Friedman, 1979).

Precision in Thermoregulation
Following the aforementioned methodologies, thermoregulation accuracy (db) was calculated as the mean absolute deviation of body temperature (Tc) with respect to the central 50% of preferred temperatures (Tsel). This index measures how much the Tc deviates from its optimum; where values ​​close to zero indicated high precision in thermoregulation and vice versa..

Habitat Thermal Quality 
Likewise, the habitat thermal quality index (de) was calculated as the mean absolute deviation of operating temperatures (To) with respect to the central 50% of preferred temperatures (Tsel). This index measures how much the To deviate from the preferred optimum; Values ​​close to zero indicate high thermal quality of the habitat and values ​​close to 1 indicate that microenvironmental temperatures are outside their optimal preference or selection (Hertz, 1993; Navas et al., 2021; Ortega, 2015).

Efficiency in Thermoregulation


Thermal Use of the Habitat
To distinguish the percentage of thermal habitat used by the species, the thermal use index of Christian and Weavers (1996) (Ex96) was calculated, considering it as the percentage of Tc within the Tsel. Additionally, the modifications of Brown and Weatherhead (2000), called (Ex00), were applied to determine the proportion of Tc data that fell above or below Tsel.

Distribution, Asymmetry and Kurtosis of selection temperatures 
To determine the thermoregulatory behavior within the thermal gradient, the parameters of the selected temperature distribution (Tsel) were analyzed. Evaluating the kurtosis of the Tsel data using a Jarque-Bera goodness-of-fit test, using the matrixTests package (Koncevičius, 2021). A Tsel distribution with a kurtosis less than 3 is considered platykurtic, suggesting that salamanders indiscriminately use a wide range within selected temperatures. in the thermal gradient. In contrast, a kurtosis greater than 3 indicates a leptokurtic distribution, where individuals preferentially search a shorter range within selected temperatures on the thermal gradient. Likewise, at test was used to distinguish whether the selection temperatures were significantly distanced from zero, as well as Kernel density to distinguish the most frequently selected temperatures in each of the populations (Giacometti and Tattersall, 2024). All this through the stats package (R Core Team, 2024).  

All analyzes were performed using R statistical software (R Core Team, 2024). Using packages such as openxlsx, readxl, writexl to manage Excel files (Walker, 2023; Wickham et al., 2023; Ooms, 2023); dplyr, tidyr for data manipulation (Wickham et al., 2023); hms for managing times and dates (Müller &amp; Wickham, 2023).

Likewise, the psych package was used for psychometric analyzes (Revelle, 2023); ggplot2 and plotly for data visualization (Wickham, 2023; Sievert, 2020); gridExtra and grid for the arrangement of graphics in grids (Auguie, 2017; R Core Team, 2024), as well as flextable, ftExtra, officer for the creation and management of tables and Microsoft Office documents (Xie et al., 2023; Gohel , 2023, 2023).

1.1.2	Objective 2: Microhabitat Use and Selection
Use of Microhabitat
To analyze the use of the Microhabitat, a description of the variables was carried out, followed by tests of normality and homogeneity of variance using the Shapiro-Wilk tests. As well as, the Student's t and Mann-Whitney U test respectively. Subsequently, a multilevel pattern analysis was used for the quantitative and qualitative variables indicspecies and fastDummies (Caceres and Jansen, 2023; Schaefer, 2023)
 Then, factor analysis of mixed data (FAMD) was performed to reduce the dimension and interpret the structure of the data. This process was carried out using the packages Psych (Revelle, 2018), FactoMineR (Lê et al., 2008), Car (Fox and Weisberg, 2019), Corrplot (Wei and Simko, 2017) and Readxl (Wickham et al.., 2019). 

The analyzes were applied individually for each study area as for all the data in general. Subsequently, the Gower dissimilarity matrix was calculated using the cluster package (Maechler et al., 2019). The vegan package (Oksanen et al., 2019) was used to perform an analysis of variance (ANOVA), Analysis of Rank Similarity (ANOSIM) and a Permutational Multivariate Analysis of Variance (PERMANOVA). And non-metric multidimensional scaling (NMDS) with hierarchical clusteringarchitecture.


Microhabitat Use and Selection 
Microhabitat use and selection was analyzed using the methods proposed by Manly, using a type two resource availability design, according to Manly et al. (2002), and the index proposed by Manly et al. (2007). Under this index, Wi indicates the rate of resource use, that is, the number of times the study species used a specific resource in proportion to the availability of said resource. To obtain Wi, it is necessary to identify Oi (the use of the specific resource) and Pi (the availabilityity of the resource not used or available for the species), this resource corresponding to the variables of use and selection of vegetation.

Likewise, Manly's Alpha (α) was calculated as the proportion of the total use rate represented by the resource. Consequently, the sum refers to all the rates of use of the evaluated resources. Wi values ​​greater than 1 indicate positive selection of the specific habitat, while Wi values ​​less than 1 indicate negative selection of the same (Manly et al., 2007; Morillas et al., 2023). 

All this was done using the R core statistical software (R Core Team, 2024). Using packages such as openxlsx, readxl, writexl to manage Excel files (Walker, 2023; Wickham et al., 2023; Ooms, 2023); dplyr, tidyr for data manipulation.


Relationship between Thermal Efficiency and Microhabitat 
Endly, Generalized Linear Models were used to establish the relationship between variables with the help of the MASS package (Venables, 2022) and the car package (Fox and Weisberg, 2019). Likewise, to evaluate multicollinearity between the independent variables through a Variance Inflation Factor (VIF) analysis. And ultimately, a Redundancy Analysis was applied based on distances between the variables, with the use of packages such as cluster (Maechler et al., 2019), and vegan et al., 2019).

1.1.3	Goal 3: Body Size 
Variations in Body Size
To describe body size, descriptive statistics were used for each of the measurements, followed by tests of normality and homogeneity of variance using the Shapiro-Wilk tests. As well as, the Student's t and Mann-Whitney U test respectively. Subsequently, a multilevel pattern analysis was used for the body size variables, using the indicspecies and fastDummies packages (Caceres and Jansen, 2023; Schaefer, 2023)

Likewise, the dimensionality of the variables was reduced and the variables with the greatest explanation were searched through correlation matrices and principal component analysis (PCA) and conglomerate analysis (cluster), using the Psych (Revelle, 2018), FactoMineR (Lê) packages. et al., 2008), factoextra (Kassambara and Mundt, 2020), Car (Fox and Weisberg, 2019), Corrplot (Wei and Simko, 2017) and Readxl (Wickham et al., 2019). 

The analyzes were applied both for each study area separately and for all the data in general. Subsequently, to determine the differences between study areas and the climatic season in which sampling was carried out, the Euclidean dissimilarity matrix was calculated using the cluster (Maechler et al., 2019) and vegan (Oksanen et al., 2019) packages. Likewise, an analysis of variance (ANOVA), the Analysis of Rank Similarity (ANOSIM) and a Permutational Analysis of Variation M were carried out.ultivariate (PERMANOVA).

Relationship of Body Size and Habitat
To establish the relationship between the variables with the greatest contribution to body size and the habitat dissimilarity matrix, Generalized Linear Models were used with the help of the MASS package (Venables, 2022) and the car package (Fox and Weisberg, 2019). Likewise, to evaluate multicollinearity between the independent variables through a Variance Inflation Factor (VIF) analysis).   

Relationship between Thermal Efficiency and Habitat
To establish the relationship between the variables with the greatest contribution to body size and thermoregulation efficiency, generalized linear models were used with the help of the MASS package (Venables, 2022) and the car package (Fox and Weisberg, 2019). Lastly, to evaluate multicollinearity between the independent variables using a Variance Inflation Factor (VIF) analysis).   

1.1.4	Relationship between Objectives Using Partial Least Squares Structural Equations Model (PLS-SEM) 
To establish the linear relationship between the phenomena that make up the thermal ecology and the use of microhabitat of Bolitoglossa tamaense, a structural equation model with partial least squares (PLS-SEM) was carried out (Lohmöller, 1989; Wold, 1985). With the help of the SEMinR and seminrstudio package (Calero, 2024; Ray and Calero, 2024; R Core Team, 2024).

ORsing packages such as openxlsx, readxl, writexl to manage Excel files (Walker, 2023; Wickham et al., 2023; Ooms, 2023); dplyr, tidyr for data manipulation and Readxl (Wickham et al., 2019). flextable, ftExtra, officer for creating and managing Microsoft Office tables and documents (Xie et al., 2023; Gohel, 2023; Yu, 2023).

The PLS-SEM, as seen in Figure 5, are formed from a measurement model that is established in multiple observed variables or indicators measured in the field (Ramirez et al., 2014). Which represents complex phenomena to measure directly in practice, which are called constructs or latent variables (VL) (Martínez and Fierro, 2018). Likewise, the PLS-SEM are made up of a structural model which establishes the different hypotheses of the relationship between the different constructs. (Hair et al.., 2017). 

 Consequently, a measurement model was carried out with a formative approach where the observed variables shape the construct, and a reflective one where the observed variables are a reflection of it (Becker et al., 2018; Hair et al., 2017). For this, three formative constructs were established, called: Spatial temporal location (UTE), use and selection of vegetation (USV), use of microhabitat (UM). And two reflective constructs called: body size (T) and Efficiency in thermoregulation.ion (E). 

To create the structural model, 10 positive relationship hypotheses were proposed as a result of the approaches and results obtained in this same research. With which the thermal ecology and microhabitat use of Bolitoglossa tamense was analyzed through the relationship of UTE with UM, USV, T and E. Likewise, how UM is related to USV, T and E., How USV is related to T and E. and ultimately how T relates to AND.

Figure 5
PLS-SEM measurement and structural model for the thermal ecology and microhabitat use of Bolitoglossa tamaense
 
Note. The yellow rectangles show the important variables observed in the field, the hexagons show the formative constructs and the circles the reflective constructs, while the green lines show the positive relationship hypotheses of the model..

Once the model is made, it is crucial to evaluate its quality to ensure that it meets acceptable statistical standards, which, in the context of PLS-SEM, requires at least 100 observations (Reinartz, Haenlein, &amp; Henseler, 2009). It is essential to separately evaluate the formative and reflective measurement models, as well as carry out the respective evaluation of the structural model (Hair et al., 2017; Martínez &amp; Iron, 2018).

To evaluate the formative measurement models, the convergent validity is verified through the predictive reliability of the indicators (Barclay et al., 1995), the collinearity between observed variables through the variance inflation factor (VIF) where values ​​greater than 10 indicate multicollinearity (Myers, 1990)., and the significance and relevance of the weights (w) for the indicators (Hair et al., 2017; Martínez and Iron, 2018). 

To evaluate the reflective measurement models, the internal consistency of the indicators was taken into account using Cronbach's Alpha (alpha), composite reliability (rhoC), construct reliability or Dillon-Goldstein rho (RhoA), convergent validity between the loading of the indicators and the average variance extracted (AVE), where values ​​greater than 0.70 are ideal (NUNNALLY, 1978). Likewise, the AVE indicates the average variance extracted that reflects the latent variable and the values ​​alone. adequate values ​​exceed or equal 0.50 (CHIN, 1998). Fornell and Larcker (1981). Finally, to ensure that each construct is different, discriminant validity was evaluated using the Heterotrait-Monotrait Ratio (HTMT) and Fornell Larcker (FL) criteria, where values ​​​​0.85 are ideal..

 Endly, to evaluate the structural model it is necessary to verify the coefficient of determination (R2) of the constructs where 1 indicates a perfect explanation of the variance, relevance or predictive capacity (Q2) where positive values ​​indicate a good fit of the same. the negative or positive size and the significance p value through bootstrapping and the path coefficients for the established hypotheses and the effect size (f2 and q2) for the constructs that make up the model, wherAND 0.02 indicates a small predictive effect, 0.15 a medium effect and 0.35 a large predictive effect (Hair et al., 2017).
References 
Augie, B. (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics. R package version 2.3. https://CRAN.R-project.org/package=gridExtra

Barclay, D., Higgins, C.., &amp; Thompson, R. (1995). The partial least squares (PLS) approach to causal modeling: Personal computer adoption and use as an illustration. Technology Studies, 2(2), 285-309.

Becker, J.-M., Ringle, C.M.., &amp; Sarstedt, M. (2018). Estimating moderating effects in PLS-SEM and PLSc-SEM: Interaction term generation*data treatment. Journal of Applied Structural Equation Modeling, 2(2), 1–21. https://doi.org/10.47263/JASEM.2(2)01

Blouin-Demers, G. and Nadeau, P. (2005), The cost–benefit model of thermoregulation does not predict lizard thermoregulatory behavior. Ecology, 86: 560-566. https://doi.org/10.1890/04-1403

Blouin-Demers, G., &amp; Weatherhead, P.J. (2001). Thermal ecology of black rat snakes (Elaphe obsoleta) in a thermally challenging environment. Ecology, 82(11), 3025-3043.  

Brown, G.P.., &amp; Weatherhead, P.J. (2000). Thermal ecology and sexual size dimorphism in northern water snakes, Nerodia sipedon. Ecological monographs, 70(2), 311-330.  

Brown, G.P..., &amp; Weatherhead, P.J. (2000). Thermal ecology and sexual size dimorphism in northern water snakes, Nerodia sipedon. Ecological monographs, 70(2), 311-330.  

Caceres, M., &amp; Jansen, F. (2023). indicspecies: Relationship Between Species and Groups of Sites. R package version 1.8.0. Retrieved from https://cran.r-project.org/web/packages/indicspecies/index.html

Calero Valdez, A. (2024). seminrstudio: seminrstudio provides an interactive interface to the seminr package (Version 0.0.1) [R package]. GitHub. https://github.com/sem-in-r/seminrstudio

Chin, W. W. (1998). The partial least squares approach for structural equation modeling. In GA Marcoulides (Ed.), Modern methods for business research (pp. 295-336). Lawrence Erlbaum Associates.

Christian, K.A.., &amp; Weavers, B.W. (1996). Thermoregulation of monitor lizards in Australia: an evaluation of methods in thermal biology. Ecological monographs, 139-157.

Fornell, C..., &amp; Larcker, D. F. (1981). Evaluating Structural Equation Models with Unobservable Variables and Measurement Error. Journal of Marketing Research, 18(1), 39–50. https://doi.org/10.2307/3151312

Fox, J.., &amp; Weisberg, S. (2019). Car: Companion to Applied Regression (Version 3.0-4) [Software]. https://cran.r-project.org/web/packages/car/index.html  

Giacometti, D. and Tattersall, G.J. (2024). Seasonal variation of behavioral thermoregulation in a fossorial salamander (Ambystoma maculatum). Royal Society Open Science, 11 (240537). https://doi.org/10.1098/rsos.240537

Gohel, D. (2023). flextable: Functions for Tabular Reporting. R package version 0.6.6. https://CRAN.R-project.org/package=flextable  

Hair, J., Hult, T., Ringle, C., Sarstedt, M., Castillo, J., Cepeda, G.., &amp; Roldan, J. (2019). Manual of Partial Least Squares Structural Equation Modeling (PLS-SEM) (2nd ed.). Sage Publishing. https://doi.org/10.39926/oss.37

Hertz, P.E., Huey, R.B.., &amp; Stevenson, R. D. (1993). Evaluating temperature regulation by field-active ectotherms: the fallacy of the inappropriate question. The American naturalist, 142(5), 796–818. https://doi.org/10.1086/285573

Kassambara, A.., &amp; Mundt, F. (2020). factoextra: Extract and Visualize the Results of Multivariate Data Analyzes (Version 1.0.7) [Software]. https://CRAN.R-project.org/package=factoextra  

Koncevičius, K. (2021). matrixTests: Fast statistical hypothesis tests on rows and columns of matrices. R Package. https://CRAN.R-project.org/package=matrixTests

Lê, S., Josse, J., &amp; Husson, F. (2008). FactoMineR: An R Package for Multivariate Analysis. Journal of Statistical Software, 25(1), 1-18. https://doi.org/10.18637/jss.v025.i01  

Lohmöller, J.-B. (1989). Path modeling of latent variables with partial least squares. Heidelberg: Physics.

Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M.., &amp; Hornik, K. (2019). cluster: Cluster Analysis Basics and Extensions (R package version 2.1.0) [Software]. Retrieved from https://cran.r-project.org/web/packages/cluster/index.html  

Manly, B.F.L., McDonald, L., Thomas, D.L., McDonald, T.L., Erickson, W.P. (2007). Resource selection by animals: Statistical design and analysis for field studies. London: Chapman &amp; Hall Press.

Manly, BFJ, McDonald, LL, Thomas, DL, McDonald, TL, &amp;Erickson, E. P. (2002). Resource selection by animals: statistical design and analysis of field studies (2nd Ed.). The Netherlands: Springer Netherlands

Martínez Ávila, Minerva, &amp; Fierro Moreno, Eréndira. (2018). Application of the PLS-SEM technique in knowledge management: a practical technical approach. RIDE. Ibero-American Journal for Educational Research and Development, 8(16), 130-164. https://doi.org/10.23913/ride.v8i16.336

Morillas, E., Torrico-Paz, S., Romero, L., Zurita, L., Antezana, N., Carvajal, G., Miranda-Calle, AB, &amp; Pacheco, L.F. (2023). Home range and microhabitat use of Liolaemus forsteri (Iguania: Liolaemidae) in a high Andean region of La Paz, Bolivia. Acta zoologica lilloana, 67(1), 121-135. DOI: https://doi.org/10.30550/j.azl/2023.67.1/2023-03-06 

Müller, K.., &amp; Wickham, H. (2023). hms: Pretty Time of Day. R package version 1.1.1. https://CRAN.R-project.org/package=hms  

Myers, R. M. (1990). Classical and modern regression with applications (2nd ed.). Duxbury Press.

Navas, CA, Gouveia, SF, Solano-Iguarán, JJ, Vidal, MA, &amp; Bacigalupe, L.D. (2021). Amphibian responses in experimental thermal gradients: Concepts and limits for inference. Comparative Biochemistry and Physiology Part B: Biochemistry and Molecular Biology, 254, 110576. https://doi.org/10.1016/j.cbpb.2021.110576

Nunnally, J. (1978). Psychometric theory (2nd ed.). McGraw-Hill.

Oksanen, J., Blanchet, F.G., Friendly, M., Kindt, R., Legendre, P., McGlinn, D., Minchin, P.R., et al. (2019). Vegan: Community Ecology Package (Version 2.5-6) [Software]. https://cran.r-project.org/web/packages/vegan/index.html  

Ooms, J. (2023). writexl: Export Data Frames to 'xlsx' Format. R package version 1.4.0. https://CRAN.R-project.org/package=writexl  

Ortega Diago, Z. (2015). Thermal biology of high mountain lizards of the genus Iberolacerta. [Degree thesis for Doctor of Sciences, University of Salamanca, Spain]. https://doi.org/10.14201/GREDOS.128479 

R CoreTeam. (2024). A: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/ 

Ramírez, PE, Mariano, AM, &amp; Salazar, EA (2014). Methodological Proposal to apply structural equation models with PLS: The case of the use of scientific databases in university students. ADMPG Magazine, 7(2). Recovered from https://revistas.uepg.br/index.php/admpg/article/view/14062

Ray, S., Danks, N.., &amp; Calero Valdez, A. (2024). seminr: Building and Estimating Structural Equation Models (Version 2.3.3) [R package]. CRAN. https://CRAN.R-project.org/package=seminr

Reinartz, W., Haenlein, M.., &amp; Henseler, J. (2009). An empirical comparison of the efficacy of covariance-based and variance-based SEM. International Journal of Research in Marketing, 26(4), 332-344. https://doi.org/10.1016/j.ijresmar.2009.08.001

Revelle, W. (2023). psych: Procedures for Psychological, Psychometric, and Personality Research. R package version 2.3.6. https://CRAN.R-project.org/package=psych  

Schaefer, J. (2023). fastDummies: Fast Creation of Dummy (Binary) Columns and Rows from Categorical Variables. R package version 1.7.3. Retrieved from https://cran.r-project.org/web/packages/fastDummies/index.html 

Sievert, C. (2020). Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC. https://plotly-r.com 

Venables, W.N.., &amp; Ripley, B. D. (2002). Modern Applied Statistics with S (4th ed.). Springer, New York. ISBN 0-387-95457-0.

Walker, A. (2023). openxlsx: Read, Write and Edit XLSX Files. R package version 4.2.5. https://CRAN.R-project.org/package=openxlsx  

Wei, T..., &amp; Simko, V. (2017). Corrplot: Visualization of a Correlation Matrix (Version 0.84) [Software]. https://cran.r-project.org/web/packages/corrplot/index.html  

Wickham, H. (2023). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. https://ggplot2.tidyverse.org  

Wickham, H., Bryan, J..., &amp; Lin, X. (2019). Readxl: Read Excel Files (Version 1.3.1) [Software]. https://cran.r-project.org/web/packages/readxl/index.html  

Wickham, H., François, R., Henry, L..., &amp; Müller, K. (2023). dplyr: A Grammar of Data Manipulation. R package version 1.1.2. https://CRAN.R-project.org/package=dplyr  

Wold, H. (1985). In S. Kotz and NL Johnson (Eds.), Encyclopedia of statistical sciences. Partial least squares (Vol. 6, pp. 581-591). New York, NY: Wiley

Xie, Y., Cheng, J..., &amp; Tan, X. (2023). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.27. https://CRAN.R-project.org/package=DT  

Yu, G. (2023). ftExtra: Extra Functions for 'flextable'. R package version 0.0.5. https://CRAN.R-project.org/package=ftExtra  
