# Importamos dplyr para usar select()
library(dplyr)

# Obtenemos el working directory, leemos los datos y los agregamos a la variable
data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

# Seleccionamos las columnas necesarias y creamos una tabla para cada ejercicio
data.FTHG.table <- table(select(data, FTHG), dnn = "Prob. marginal equipo casa anote x goles")

data.FTAG.table <- table(select(data, FTAG), dnn = "Prob. marginal equipo afuera anote x goles")

data.goals.table <- table(select(data, FTHG, FTAG), dnn = c("Goles de casa", "Goles de afuera"))

# Agregamos nombres a las dimensiones

# Dividimos los resultados de las tablas entre la cantiad de renglones, para obtener la probabilidad marginal y compuesta de cada valor
n <- nrow(data) # Obtenemos la cantidad de renglones (en este caso 1 renglon = 1 juego)

(data.FTHG.table / n) # Prob marginal x goles de casa en decimal

(data.FTAG.table / n) # Prob marginal x goles de afuera en decimal

(data.goals.table / n) # Prob compuesta que los dos equipos anoten x goles en decimal
