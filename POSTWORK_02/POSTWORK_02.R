# Sesión 2. Programación y manipulación de datos en R.

# Postwork 2. Sesión del 17/06/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. Importa los datos de fútbol de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de
# la liga española a R; los datos los podrás encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php.
# 2. Revisa la estructura de los datos obtenidos con las funciones: 'str()', 'head()', 'View()' y 'summary()'.
# 3. Con la función 'select()' del paquete 'dplyr', selecciona únicamente las columnas "Date", "HomeTeam",
# "AwayTeam", "FTHG", "FTAG" y "FTR" (esto para cada uno de los "data frames").
# 4. Asegúrate de que los elementos de las columnas en los nuevos "data frames" sean del mismo tipo.
# 5. Con ayuda de la función 'rbind()', forma un único "data frame" que contenga las seis columnas mencionadas
# en el punto número tres.


# SOLUCIÓN:

library(dplyr)

data.1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv") # "Data set" 1: 2017/2018.
data.1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv") # "Data set" 2: 2018/2019.
data.1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv") # "Data set" 3: 2019/2020.

# Se revisa la estructura de los datos y la naturaleza de las variables:
str(data.1718)
summary(data.1819)
View(data.1920)

head(data.1718) # Encabezado 1 (se muestran las primeras seis filas del "data frame" correspondiente).
head(data.1819) # Encabezado 2 (se muestran las primeras seis filas del "data frame" correspondiente).
head(data.1920) # Encabezado 3 (se muestran las primeras seis filas del "data frame" correspondiente).

# Se guardan los datos en una lista para usar 'lapply()' y 'do.call()':
data.list <- list(data.1718, data.1819, data.1920)

# Se utiliza la función 'lapply()' para seleccionar únicamente las columnas de interés en cada documento:
data.list <- lapply(data.list, select, Date, HomeTeam:FTR)

# Se usa la función 'rbind()' para juntar todo en un solo "data frame":
data.games <- do.call(rbind, data.list)

# Se cambia la tipología de la columna de fechas con las funciones 'mutate()' y 'as.Date()':
data.games.date <- mutate(data.games, Date = as.Date(Date, "%d/%m/%y")) # Nótese que en este punto hay un
# detalle con las fechas, sin embargo, no se pondrá atención a él sino hasta el quinto "Postwork".
# Por el momento se cumple el requerimiento y, para fines demostrativos, las funciones 'mutate()' y
# 'as.Date()' quedan bien aplicadas.

# Se revisa la estructura del nuevo "data frame", así como la naturaleza de sus variables:
head(data.games.date)
tail(data.games.date)
str(data.games.date)
