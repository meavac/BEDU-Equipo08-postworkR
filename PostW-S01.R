library(dplyr) # Cargar paquete

#Establecer directorio de trabajo
setwd("C:/Users/avila/Documents/Files-R/PWS01")
dir()

#Cargar archivo en vector
liga<-read.csv("LigaPrimeraDiv-2019-20.csv")
str(liga)
pw 

#Seleccionar columnas de interés
goles<-select(liga, FTHG:FTAG)

# A) Elaborar tablas de frecuencias relativas,

# 1) Obtener tabla de frecuencia absoluta
f.abs <- table(goles)

# 2) Obtener tabla de frecuencia relativa
(f.rel<- round(prop.table(f.abs, margin=NULL),3))#Global

# 3) Agregar columna de totales marginales sobre frecuencia relativa
(tmarg.rel<- addmargins(f.rel))
?addmargins

# Estimar las siguientes probabilidades:
#3) Probabilidad Marginal

x<-c(1:6) # Establecer rango de valores para x
y<-c(1:5) # Establecer rango de valores para y

(x.goal<-sample(x, 1, F)) # Generar un valor dentro del rango
(y.goal<-sample(y, 1, F))

#La probabilidad (marginal) de que el equipo 
#que juega en casa anote x goles (x = 0, 1, 2, ...)
tmarg.rel[x.goal, "Sum"]
paste("La probabilidad marginal que un equipo en casa anote", 
      x.goal, "goles es", round(tmarg.rel[x.goal, 7]*100,2), "%")

#La probabilidad (marginal) de que el equipo 
#que juega como visitante anote y goles (y = 0, 1, 2, ...)
tmarg.rel["Sum",y.goal]
paste("La probabilidad marginal que un equipo visitante anote", 
      y.goal, "goles es", round(tmarg.rel[8,y.goal]*100,2),"%")

#La probabilidad (conjunta) de que el equipo 
#que juega en casa anote x goles (x = 0, 1, 2, ...)
#y el equipo que juega como visitante anote y goles 
#(x = 0, 1, 2, ..., y = 0, 1, 2, ...)

#4) Probabilidad conjunta
tmarg.rel[x.goal,y.goal] #Recuperar la coordenada producida

paste("La probabilidad conjunta que un equipo en casa anote", 
      x.goal, "goles y el visitante meta", y.goal, 
      "es", round(tmarg.rel[x.goal,y.goal]*100,2), "%")
