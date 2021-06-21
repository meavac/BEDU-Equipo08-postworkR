library(dplyr) # Cargar paquete

#Establecer directorio de trabajo
setwd("C:/Users/avila/Documents/Files-R/PWS02")
dir()

#Cargar archivo en vector
liga2019<-read.csv("LigaPrimeraDiv-2019-20.csv")
liga2018<-read.csv("LigaPrimeraDiv-2018-19.csv")
liga2017<-read.csv("LigaPrimeraDiv-2017-18.csv")

# 1) Revisa la estructura delos data frames 
# usando las funciones: str, head, View y summary


# Cambiar el tipo de dato de DATE, TIME (no necesario)
str(liga2019)
head(liga2019)
summary(liga2019)
View(liga2019)

str(liga2018)
head(liga2018)
summary(liga2018)
View(liga2018)

str(liga2017)
head(liga2017)
summary(liga2017)
View(liga2017)

# 2) Con la función select del paquete dplyr 
# selecciona únicamente las columnas:
# Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; 
# para cada uno de los data frames. (Hint: también puedes usar lapply).
liga2019<-liga2019[,-3] # Eliminar columna TIME
teams2019<- select (liga2019,Date:FTR)
teams2018<- select (liga2018,Date:FTR)
teams2017<- select (liga2017,Date:FTR)

# 3) Asegúrate de que los elementos 
    #de las columnas correspondientes de los nuevos data frames 
    #sean del mismo tipo (
#Hint 1: usa as.Date y mutate para arreglar las fechas). 
#Con ayuda de la función rbind, 
  # forma un único data frame que contenga
  # las seis columnas mencionadas en el punto 3 (
#Hint 2: la función do.call podría ser utilizada).

firstmerge<-rbind(teams2017,teams2018)
teams1719<-rbind(firstmerge, teams2019)

teams1719<-mutate(teams1719, Date=as.Date(Date,"%d/%m/%Y"))
teams1719

