library(pacman)
p_load(haven, dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych, readxl)

setwd('D:/Usuarios/Orta/Documentos/Actuaria/8° semestre/Computo cientifico/2do parcial/Practica 3')
################################## Data_pca ####################################
data_pca <- read.csv2('data_pca.csv') #.csv2 para leer un archivo delimitado por ";"
View(data_pca)

# Normalizar datos
data11 <- scale(data_pca)
View(data1)

# PCA
det(cor(data11))
# Al ser cercano a cero el valor, implica entonces que están altamente correlacionadas

# Se omiten los criterios KMO

pca1 <- princomp(data11)
pca1$loadings
summary(pca1)

# Se observa que los principales componentes que aportan más varianza son el Comp1-6
# Revisar varianza y eigenvalores
fviz_eig(pca1, choice = "variance")
# Los seis primeros componentes son los que efectivamente más varianza aportan
fviz_eig(pca1, choice = "eigenvalue")
# Existen seis componentes cuyo eigenvalor es mayor a la unidad, por lo que resulta
# adecuado tomar todos estos

# Análisis gráfico
fviz_pca_ind(pca1,
             col.ind = "cos2",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)
# Se muestran los individuos coloreados por su coseno cuadrado, es decir, se muestra la
# calidad de la representación en el espacio de los componentes principales.

fviz_pca_var(pca1,
             col.var = "contrib",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)
# Se muestran las variables coloreadas por la contribución a cada componente
# Las variables x15, x14, x2, x8, x10, x5 son las que más alta contribución reportan

fviz_pca_biplot(pca1,
                col.var = "red",
                col.ind = "black")
# La representación simultánea de individuos y variables muestra poca relación para 
# la mayoría de los datos

pca12 <- psych::principal(data11, nfactors = 6, residuals = FALSE, rotate = "varimax",
                         scores = TRUE, oblique.scores = FALSE, method = "regression",
                         use = "pairwise", cor = "cor", weight = NULL)
pca12
pca12$weights[,1]
pca12$weights[,2]
pca12$weights[,3]
pca12$weights[,4]
pca12$weights[,5]
pca12$weights[,6]
pca12$scores
# El modelo explica el 71% dela varianza total, con cargas bien diferenciadas entre factores
# La complejidad media de los ítems es baja (1.4), indicando asignaciones claras a los componentes
# Con un rsmr de 0.08, se puede decir que se tiene un ajuste aceptable y una prueba
# chi-cuadrada significativa, esto entonces respalda la adecuación de extraer 6 componentes

################################ PoblacionUSA ##################################
PoblacionUSA <- read_excel("PoblacionUSA.xlsm")
View(PoblacionUSA)
# Separar datos en data2020 y data2021 observando que las columnas pares e impares corresponden
# a cierto año respectivamente
data_2020 <- PoblacionUSA[,c(2,3,5,7,9,11,13,15,17,19)]
data_2021 <- PoblacionUSA[, c(4,6,8,10,12,14,16,18,20)]

# Normalizar ambos datasets y corroborar mediante "View" que se haya efectuado correctamente
data_20 <- scale(data_2020)
data_21 <- scale(data_2021)
View(data_20)
View(data_21)

# PCA 
det(cor(data_20))
det(cor(data_21))
# Ambos determinantes son, esencialmente 0 (-8.80948e-41 y -4.747386e-25 respectivamente), 
# por lo que están altamente correlacionadas incluso más que las del dataset previo

# Calculando KMO
psych::KMO(data_20)
psych::KMO(data_21)
# Las variables tienen un msa de exactamente 0.5 por lo que es pertinente realizar el pca

# Cálculo del PCA para ambos años
pca20 <- princomp(data_20)
pca20$loadings
summary(pca20)

pca21 <- princomp(data_21)
pca21$loadings
summary(pca21)

# Revisar varianza y eigenvalores gráficamente
fviz_eig(pca20, choice = "variance")
# Retomando el "summary", dos componentes acumulan más del 85% de la varianza
fviz_eig(pca20, choice = "eigenvalue")
# Efectivamente, sólo dos componentes tienen un eigenvalor mayor a 1, por lo que la 
# decisión se respalda

fviz_eig(pca21, choice = "variance")
# De "summary", igualmente, dos componentes apuntan a acumular casi el 90% de la varianza
fviz_eig(pca21, choice = "eigenvalue")
# Se confirma que son dos los componentes que poseen un eigenvalor mayor a 1, igual que para
# el año 2020

# Análisis gráfico
fviz_pca_ind(pca20,
             col.ind = "cos2",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)
# Para el año 2020, se trata de una minoría en la que se encuentra 26, 23, 48 y 42 que no
# son representadas, sin embargo, se trara de una minoría
fviz_pca_ind(pca21,
             col.ind = "cos2",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)
#Para el año 2021, son aún menos elementos que no son representados, entre los que se encuentra
# 41, 42 y 21, explicando entonces que las variables están bien representadas por los
# componentes principales

fviz_pca_var(pca20,
             col.var = "contrib",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)
# En lo anterior se observa que a excepción de Migration, Residual y Federal/Civilian Movement
# las demás variables se encuentran en una dimensión, mientras que de los 3 mencionados, los 2 últimos
# corresponden a otra dimensión
fviz_pca_var(pca21,
             col.var = "contrib",
             gradient.cols = c("red", "yellow", "green"),
             repel = FALSE)
# Se aprecia que exceptuando a Net Domestic Migration y Residual que pertenecen a otra dimensión,
# todas las demás variables pertenecen a la dimensión que explica la gran mayoría de la información
# tomando en cuenta que Movement from Abroad está exluído de este par de componentes

fviz_pca_biplot(pca20,
                col.var = "red",
                col.ind = "black")
# Se tienen elementos mayormente en el cuadrante 1 y 4, habiendo casos particulares como
# el 6 o 3, cuyas puntuaciones altas van orientadas a Domestic Migration, o 11 con alta 
# afinidad por Residual
fviz_pca_biplot(pca21,
                col.var = "red",
                col.ind = "black")
# Para el caso del año 2021, se encuentra al menos en cada cuadrante una dirección en la que
# contribuye al menos una variable, sin embargo la mayoría de sujetos se ubica cercana al origen
# habiendo casos como 48, nuevamente 3 y 6, así como 11, 16 y 43 que tienen alta afinidad con
# Movement from Abroad

pca20 <- psych::principal(data_20, nfactors = 2, residuals = FALSE, rotate = "varimax",
                         scores = TRUE, oblique.scores = FALSE, method = "regression",
                         use = "pairwise", cor = "cor", weight = NULL)
pca20
pca20$weights[,1]
pca20$weights[,2]
# El primer componente explica el 74% de la varianza y el segun el 14%, sumando un total de 88%
# La mayoría de las variables cargan fuertemente en el rpimer componente, indicando que casi
# toda su varianza está explicada. La complejidad media de los items es baja (1.1) lo que 
# sugiere asignaciones simples y claras a los factores. El ajuste del modelo, en cambio, es 
# excelente, con un rsmr de 0.06 y un valor de chi-cuadrada no significativo (p > 0.94)
# se respalda así que dos componentes para el 2020 sean suficientes

pca21 <- psych::principal(data_21, nfactors = 2, residuals = FALSE, rotate = "varimax",
                          scores = TRUE, oblique.scores = FALSE, method = "regression",
                          use = "pairwise", cor = "cor", weight = NULL)
pca21
pca21$weights[,1]
pca21$weights[,2]
# El primer componente explica el 70% de la varianza total y el segundo componente el 20%
# logrando una varianza acumulada del 90%. Esto sugiere que el modelo con dos componentes
# representa adecuadamente la estructura de los datos.

# La mayoría de las variables, especialmente las relacionadas con la población total, nacimientos,
# muertes y migración internacional, cargan fuertemente en el primero componente, esto sugiere una
# asociación con las dinámicas poblacionales generales.
# Por otro lado el segundo componente captura principalmente la variación asociada con la 
# migración doméstica neta y el residual poblacional. 

# La complejidad media de los ítems es baja (1.1), indicando que la mayoría de la variables se 
# asocian principalmente a un solo componente, determinante en facilitar la interpretación.
# Finalmente, el ajuste general del modelo es excelente, puesto que tiene el rmsr más bajo de los
# tres datasets (0.05) y el test de chi-cuadrada resulta no significativo (p > 0.99), elemento que
# respalda la adecuación del modelo de dos componentes para describir los datos sin evidencia
# de mal ajuste.