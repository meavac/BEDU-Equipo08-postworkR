# Sesión 5. Regresión lineal y clasificación.

# Postwork 5. Sesión del 29/06/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. A partir del conjunto de datos de la liga española de las temporadas 2017/2018, 2018/2019 y 2019/2020,
# crea el data frame "SmallData", que contenga las columnas "date", "home.team", "home.score", "away.team" y
# "away.score"; esto lo podrás hacer con ayuda de la función 'select()' del paquete 'dplyr'. Luego, establece
# un directorio de trabajo y, con ayuda de la función 'write.csv()', guarda el "data frame" como un archivo CSV
# con el nombre "Soccer.csv" (puedes colocar como argumento 'row.names = FALSE').
# 2. Con la función 'create.fbRanks.dataframes()' del paquete 'fbRanks', importa el archivo "Soccer.csv" a R y
# al mismo tiempo asígnalo a una variable llamada "listasoccer". Se creará una lista con los elementos "scores"
# y "teams", que son "data frames" listos para ser usados con la función 'rank.teams()'; asigna estos "data
# frames" a variables llamadas "anotaciones" y "equipos".
# 3. Con ayuda de la función 'unique()', crea el vector de fechas "fecha". Asegúrate de que las fechas
# correspondan con las fechas en las que se jugaron los partidos. Crea además una variable llamada "n", que
# contenga el número de fechas diferentes totales. Posteriormente, con la función 'rank.teams()' y usando como
# argumentos los "data frames" 'anotaciones' y 'equipos', crea un ranking de equipos únicamente desde la fecha
# inicial hasta la penúltima fecha en la que se jugaron los partidos; estas fechas las deberás especificar en
# los argumentos "max.date" y "min.date" de la función 'rank.teams()'. Guarda los resultados con el nombre
# "ranking".
# 4. Finalmente, estima las probabilidades de los eventos: el equipo de casa gana, el equipo visitante gana y
# el resultado es un empate, para todos los partidos que se jugaron en la última fecha.
# Esto lo podrás hacer con ayuda de la función 'predict()' y usando como argumentos "ranking" y "fecha[n]", la
# cual deberás especificar en los argumentos "date" de la función 'predict()'.


# SOLUCIÓN:

# install.packages("fbRanks")
library(dplyr); library(fbRanks) # Se cargan los paquetes que se van a utilizar.

data.1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv") # "Data set" 1: 2017/2018.
data.1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv") # "Data set" 2: 2018/2019.
data.1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv") # "Data set" 3: 2019/2020.

# Cambio de tipologías (de 'character' a 'Date'):
data.1718 <- mutate(data.1718, Date = as.Date(data.1718$Date, format = "%d/%m/%y")) 
data.1819 <- mutate(data.1819, Date = as.Date(data.1819$Date, format = "%d/%m/%Y"))
data.1920 <- mutate(data.1920, Date = as.Date(data.1920$Date, format = "%d/%m/%Y"))

data.list <- list(data.1718, data.1819, data.1920) # Se crea una lista con tres objetos.
data.list <- lapply(data.list, select, Date, HomeTeam:FTAG) # Se filtran las columnas no deseadas.

SmallData <- do.call(rbind, data.list) # Se combinan los objetos de la lista creada en un solo "data frame".
SmallData <- subset(SmallData, select = c(1, 2, 4, 3, 5)) # Se reordenan las columnas del "data frame".
names(SmallData) <- c("date", "home.team", "home.score", "away.team", "away.score") # Se renombran las columnas.

# Antes de ejecutar la línea de código siguiente, copie a su portapales la ruta del directorio que desea fijar.
directory <- scan("clipboard", what = "string", sep = ";")

setwd(directory) # Se establece un directorio de trabajo.
write.csv(SmallData, file = "soccer.csv", row.names = FALSE) # Se guarda el "data frame" creado en el directorio.

listasoccer <- create.fbRanks.dataframes("soccer.csv")
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

fecha <- (unique(anotaciones$date)) # Vector de fechas no repetidas.
n <- length(fecha) # Longitud del vector de fechas no repetidas.

identical(fecha, sort(fecha, decreasing = FALSE)) # Se comprueba que las fechas de los encuentros
# estén ordenadas, de la más antigua a la más reciente. El resultado de esta línea de comando es "TRUE".

ranking <- rank.teams(anotaciones, equipos, max.date = fecha[n-1], min.date = fecha[1]) # Ranking.
prediction <- predict(ranking, max.date = fecha[n], min.date = fecha[n]) # Predicción para la última jornada.