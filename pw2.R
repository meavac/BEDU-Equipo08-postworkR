# Postwork 2

# Instrucciones:

# 1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, 
# los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
# 2. Revisa la estructura de de los data frames al usar las funciones: str, head, View y summary
# 3. Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; 
# esto para cada uno de los data frames. (Hint: también puedes usar lapply).
# 4. Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo 
# (Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data frame que contenga las 
# seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).

# Notas para los datos de soccer: https://www.football-data.co.uk/notes.txt

# SOLUCIÓN:

# Cargamos dplyr y leemos los datos csv
library(dplyr)
data.1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
data.1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
data.1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

# Revisamos la estructura de los datos
str(data.1718)
summary(data.1819)
view(data.1920)

head(data.1718)
head(data.1819)
head(data.1920)

# Guardamos los datos en una lista para usar lapply y do.call
data.list <- list(data.1718, data.1819, data.1920)

# Usamos lapply para seleccionar las columnas necesarias de cada documento
data.list <- lapply(data.list, select, Date, HomeTeam:FTR)

# Usamos rbind para juntar los tres data frames en uno
data.games <- do.call(rbind, data.list)

# Corregimos la fecha del dataframe con mutate y as.Date
data.games.date <- mutate(data.games, Date = as.Date(Date, "%d/%m/%y"))

# Revisamos la estructura de nuesto nuevo data frame
head(data.games.date)
tail(data.games.date)
str(data.games.date)
