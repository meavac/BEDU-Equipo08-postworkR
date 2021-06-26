#Postwork 3

# Instrucciones:

# 1.- Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
   #La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
   #La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
   #La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)

# 2.- Realiza lo siguiente:
   #Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa.
   #Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo visitante.
   #Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo visitante en un partido.

#Notas para los datos de soccer: https://www.football-data.co.uk/notes.txt

#SOLUCIÓN:

# Cargamos dplyr y leemos los datos csv
packs <- c("dplyr", "ggplot2", "plotly")
lapply(packs, library, character.only = TRUE)

data.1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
data.1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
data.1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

# Guardamos los datos en una lista para usar lapply y do.call
data.list <- list(data.1718, data.1819, data.1920)

# Usamos lapply para seleccionar las columnas necesarias de cada documento
data.list <- lapply(data.list, select, FTHG, FTAG)

# Usamos rbind para juntar los tres data frames en uno
data.games <- do.call(rbind, data.list)

# Postwork 3
# Creamos dataframes con los datos de cada gráfica de probabilidad
games.FTHG <- select(data.games, FTHG)
games.FTAG <- select(data.games, FTAG)
games.goals <- select(data.games, FTHG, FTAG)

# Creamos tablas con las probabilidades marginales y conjuntas dividiendo las ocurrencias entre la cantidad de juegos
n <- nrow(games.goals)

FTHG.mprob <- round(table(games.FTHG) / n, 4)
FTAG.mprob <- round(table(games.FTAG) / n, 4)
games.cprob <- round(table(games.goals) / n, 4)

# Convertimos las tablas de probabilidades a data frames, ya que ggplot para hacer tablas solo recibe data frames
FTAG.mprob.df <- as.data.frame(FTAG.mprob)
FTHG.mprob.df <- as.data.frame(FTHG.mprob)
games.cprob.df <- as.data.frame(games.cprob)

# Creamos las gráficas
data.games.heatmap <- ggplot(games.cprob.df, aes(x=FTHG, y=FTAG, fill=Freq)) + 
  geom_tile() +
  xlab("Equipo de casa anota x goles") +
  ylab("Equipo de afuera anota y goles") +
  scale_fill_continuous(name = "Prob. Conjunta")

data.FTHG.bar <- ggplot(FTHG.mprob.df, aes(x=games.FTHG, y=Freq)) + 
  geom_bar(stat="identity") +
  xlab("Equipo de casa anota x goles") +
  ylab("Probabilidad marginal")

data.FTAG.bar <- ggplot(FTAG.mprob.df, aes(x=games.FTAG, y=Freq)) + 
  geom_bar(stat="identity") +
  xlab("Equipo de afuera anota x goles") +
  ylab("Probabilidad marginal")

# Usamos plotly junto con ggplot2 para meter tooltips a las gráficas

ggplotly(data.games.heatmap)
ggplotly(data.FTHG.bar)
ggplotly(data.FTAG.bar)
