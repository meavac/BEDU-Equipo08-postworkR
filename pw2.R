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