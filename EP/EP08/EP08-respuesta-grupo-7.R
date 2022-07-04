############# PRÁCTICO 8 #############
#Alumnos: Matías Bozo - Aylin Rodriguez - Ignacio Villarroel.
library (ggplot2)
library (ggpubr)
library (ez)

# Un equipo de investigadores del área de interacción humano-información está estudiando si el área 
# temática y el nivel de dificultad del problema de información influyen en el tiempo (en segundos) 
# que toma un usuario en formular una consulta de búsqueda para resolver dicho problema. Para ello, 
# han reclutado a un grupo de participantes voluntarios, asignados aleatoriamente a distintos grupos. 
# Cada participante debe resolver tres problemas de información con diferentes niveles de dificultad: 
# baja, media y alta. A su vez, cada grupo debe resolver problemas relacionados a una temática diferente. 

#### Actividad ####
# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que tardan 
# los usuarios en formular una consulta para un problema de dificultad fácil en las áreas de biología, 
# leyes y psicología.


# Dado que las muestras son independientes, y se busca comparar simultáneamente 
# 3 medias muestrales, se piensa utilizar ANOVA para muestras independientes. 

# Formulación de hipótesis:
# H0: El tiempo que tardan los usuarios en formular una consulta es la misma de todas las áreas.
# HA: El tiempo que tardan los usuarios en formular una consulta es diferente en al menos un área de todas las áreas.

# Verifiación de condiciones para usar ANOVA para muestras 
# independientes:

# 1.- La escala con que se mide la variable independiente (tiempo de consulta),
# tiene las propiedades de una escala de intervalos iguales, ya que si para una instancia i 
# una consulta se demora x [seg] y otra tiene y [seg], la diferencia debería ser la misma
# que se presenta para una instancia j en que una consulta se demora a [seg] y b [seg].

# 2.- Dado que las muestras provienen de un equipo de investigadores y éstas representan
# menos del 10% de la población, por lo que se puede decir que las muestras
# son obtenidas de manera aleatoria e independiente desde la población de origen.

# 3.- Dado que en el gráfico Q-Q se observan algunos valores que podrían ser atípicos,
# es mejor proceder con cautela y usar un nivel de significación alfa = 0,01.

# 4.- Al calcular la homogeneidad de las varianzas, la razón entre la máxima y la mínima
# varianza muestral de cada raza resulta ser superior a 1.5, por lo que, 
# al igual que en el caso anterior, debemos ser cautelosos y usar un alfa = 0.01.

library(MASS)
library (dplyr)
library(ggpubr)
library(ez)

# Se cargan los datos.
datos1 <- read.csv2(file.choose(), stringsAsFactors = FALSE)

datos1[["area"]] <- factor(datos1[["area"]])
datos1[["id"]] <- factor(1:nrow(datos1))

biologia <- datos1 %>% filter(area=="Biolog�a")
leyes <- datos1 %>% filter(area=="Leyes")
psicologia <- datos1 %>% filter(area=="Psicolog�a")

muestrabiologia <- biologia[["tiempo"]]
muestraleyes <- leyes[["tiempo"]]
muestrapsicologia <- psicologia[["tiempo"]]
muestras <- rbind(biologia, leyes, psicologia)

# Comprobación de normalidad
g <- ggqqplot(muestras, x = "tiempo", y ="area", color="area")
g <- g + facet_wrap(~ area)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Homogeneidad de las varianzas
varbiologia <- sd(muestrabiologia)^2
print(varbiologia)
varleyes <- sd(muestraleyes)^2
print(varleyes)
varpsicologia <- sd(muestrapsicologia)^2
print(varpsicologia)

varianzas <- c(varbiologia, varleyes, varpsicologia)
homogeneidad <- max(varianzas) / min(varianzas)
cat("Homogeneidad de las varianzas", homogeneidad)
# Al ser la homogeneidad de las varianzas 1.236733, la condición de homocedasticidad se verifica para el ejemplo

cat("\n Procedimiento ANOVA usando ezANOVA\n")


prueba <- ezANOVA(data = muestras, dv = tiempo, wid = id, between = area, return_aov = TRUE, type = 2)
print(prueba)

# Gráfico del tamaño del efecto
g2 <- ezPlot(data = muestras, dv = tiempo, wid = id, between = area, y_lab = "Tiempo promedio de consultas [seg]",
             x = area)
print(g2)
# 
# Dado que se obtiene un p-value mucho menor a nuestro alfa = 0.01 (6.331011e-05 < 0.01), se rechaza la hipótesis nula
# en favor de la hipótesis alternativa con un 99% de confianza. Es decir, el tiempo que tardan los usuarios en formular 
# una consulta es diferente en al menos un área de todas las áreas. Es por lo 
# anterior, que se realiza un análisis POST-HOC con correcciones de Bonferroni y Holm.


alfa <- 0.01

# Procedimiento post-hoc de Bonferroni
cat("Procedimiento post-hoc de Bonferroni\n")
bonferroni <- pairwise.t.test(muestras[["tiempo"]], muestras[["area"]], p.adj = "bonferroni",
                              pool.sd = TRUE, paired = FALSE, conf.level = 1-alfa)

print(bonferroni)

# Procedimiento post-hoc de Holm
cat("Procedimiento post-hoc de Holm\n")
holm <- pairwise.t.test(muestras[["tiempo"]], muestras[["area"]], p.adj = "holm",
                        pool.sd = TRUE, paired = FALSE, conf.level = 1-alfa)
print(holm)


####### CONCLUSIONES #######
# Los valores p obtenidos para los métodos de Bonferroni y Holm don levemente distintos.
# Sin embargo, en ambos casos podemos que el área de Psicología presenta una diferencia
# significativa con las otras áreas, al comparar el valor p ajustado 
# con el nivel de significación (0.01). Ahora, si observamos el gráfico del
# tamaño del efecto obtenido para el procedimiento ANOVA, podemos concluir con
# un 99% de confianza que los problemas de información del área de Psicología
# requieren menor tiempo en formular una consulta que los problemas 
# del área de biología y de leyes.
