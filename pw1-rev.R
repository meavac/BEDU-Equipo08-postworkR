# Postwork 1

# Instrucciones:

# 1. Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, 
# los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php.
# 2. Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números 
# de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG).
# 3. Consulta cómo funciona la función table en R al ejecutar en la consola ?table.

# Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
# La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
# La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)

# Notas para los datos de soccer: https://www.football-data.co.uk/notes.txt

# SOLUCIÓN:

# Importamos dplyr para usar select()
library(dplyr)

# Obtenemos el working directory, leemos los datos y los agregamos a la variable
data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

# Seleccionamos las columnas necesarias y creamos una tabla para cada ejercicio
data.FTHG.table <- table(select(data, FTHG), dnn = "Prob. marginal equipo casa anote x goles")
data.FTAG.table <- table(select(data, FTAG), dnn = "Prob. marginal equipo afuera anote y goles")
data.goals.table <- table(select(data, FTHG, FTAG), dnn = c("Goles de casa", "Goles de afuera"))

# Dividimos los resultados de las tablas entre la cantiad de renglones, para obtener la probabilidad marginal y compuesta de cada valor
n <- nrow(data) # Obtenemos la cantidad de renglones (en este caso 1 renglón = 1 juego)

(data.FTHG.table / n) # Prob marginal: x goles de casa en decimal
(data.FTAG.table / n) # Prob marginal: y goles de afuera en decimal
data.goals.table <- (data.goals.table / n) # Prob compuesta de que los dos equipos anoten z goles en decimal

# Creamos vectores aleatorios que representan la cantidad de goles para el equipo de casa y los visitantes
data.goals.table <- addmargins(data.goals.table)
data.goals.table
vec1 <- c(0:6)
vec2 <- c(0:5)
x <- sample(vec1,1,F)
y <- sample(vec2,1,F)
paste("La probabilidad de que el equipo que juega en casa anote", x, "gol(es) es de",
      round(data.goals.table[x+1,7]*100,4),"%")
paste("La probabilidad de que el equipo que juega como visitante anote", y, "gol(es) es de",
      round(data.goals.table[8,y+1]*100,4),"%")
paste("La probabilidad de que el equipo que juega en casa anote", x, "gol(es) y el equipo que juega como visitante anote" , y, "gol(es) es de",
      round(data.goals.table[x+1,y+1]*100,4),"%")
