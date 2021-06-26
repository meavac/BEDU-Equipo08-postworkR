#Postwork 4

# Instrucciones:

# 1.- Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), y el equipo visitante
# anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por el producto de las probabilidades 
# marginales correspondientes.

# 2.- Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla del punto anterior.
# Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona en cuáles casos le parece razonable
# suponer que los cocientes de la tabla en el punto 1, son iguales a 1 (en tal caso tendríamos independencia de las variables aleatorias X y Y).

# Notas para los datos de soccer: https://www.football-data.co.uk/notes.txt

#SOLUCIÓN:

# Cargamos dplyr y leemos los datos csv
packs <- c("dplyr", "boot")
lapply(packs, library, character.only = TRUE)

data.1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
data.1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
data.1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

# Guardamos los datos en una lista para usar lapply y do.call
data.list <- list(data.1718, data.1819, data.1920)
data.list <- lapply(data.list, select, FTHG, FTAG)

# Usamos rbind para juntar los tres data frames en uno
data.games <- do.call(rbind, data.list)

# Creamos dataframes con los datos de cada gráfica de probabilidad
games.FTHG <- select(data.games, FTHG)
games.FTAG <- select(data.games, FTAG)
games.goals <- select(data.games, FTHG, FTAG)

# Creamos tablas con las probabilidades marginales y conjuntas dividiendo las ocurrencias entre la cantidad de juegos
n <- nrow(games.goals)

FTHG.mprob <- round(table(games.FTHG) / n, 4)
FTAG.mprob <- round(table(games.FTAG) / n, 4)
games.cprob <- round(table(games.goals) / n, 4)

# Postwork 4

# Definimos unas variables que usaremos en el loop
quot.table <- games.cprob
games.rows <- as.numeric(dim(games.cprob)[1])
games.cols <- as.numeric(dim(games.cprob)[2])

# Hacemos una tabla vacía para guardar los cocientes luego
for (y in 1:games.rows) {
  for (x in 1:games.cols){
    quot.table[y, as.character(x-1)] <- 0
  }
}

# Para cada valor de la tabla de prob conjunta, obtenemos el cociente de dividirlos entre el producto de las dos probabilidades marginales implicadas
# Luego los añadimos a la tabla que hicimos antes
for (y in 1:games.rows) {
  for (x in 1:games.cols){
    cur.games.cprob <- games.cprob[y, as.character(x-1)]
    cur.FTAG.mprob <- FTAG.mprob[as.character(x-1)]
    cur.FTHG.mprob <- FTHG.mprob[as.character(y-1)]
    quotient <- cur.games.cprob / (cur.FTAG.mprob*cur.FTHG.mprob)
    quot.table[y, as.character(x-1)] <- quotient
  }
}

quot.table

#######  Significado de los Coeficientes  ###############################################################################################################
##                                                                                                                                                     ## 
## Los coeficientes serán tomarán valor de 1 cuando la probabilidad conjunta sea igual al producto de las probabilidades marginales correspondientes.  ##
## Esto coincide con la definición de independencia de variables aleatorias, la cual indica que la probabilidad conjunta es igual al producto de las   ##
## marginales siempre que y únicamente cuando las variables aleatorias son independientes.                                                             ##
## Cuando se obtiene un 1 en la tabla se tiene un par de eventos independientes.                                                                       ##
## Y si las variables aleatorias fueran independientes todos los valores deberían dar resultado de 1                                                   ##
## (salvo la posible división entre cero de eventos no probables, pero estos se excluyen de la tabla).                                                 ##
## Así que sería razonable esperar una tabla de 1 si los goles de visitantes no se relacionaran con los goles de casa.                                 ##
## En general, podemos interpretar los coeficientes lejanos de 1 como eventos fuertemente relacionados.                                                ##
##                                                                                                                                                     ## 
#########################################################################################################################################################




# Hacemos bootstrap a los valores de la tabla

fc <- function(d, i){
  frame <- select(as.data.frame(d), Freq)
  return(frame[i, ])
}

bootobj <- boot(quot.table, fc, R=10)
bootobj
head(bootobj$t)

# Con este remuestreo podemos aplicar pruebas de hipotesis que determinen la distribución de los coeficientes


