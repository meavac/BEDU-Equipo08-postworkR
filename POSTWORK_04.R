# Sesión 4. Algunas distribuciones, teorema central del límite y contraste de hipótesis.

# Postwork 4. Sesión del 24/06/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. Ya se han estimado las probabilidades conjuntas de que el equipo de casa anote 'X' = 'x' goles
# ('x' = 0, 1, ..., 8) y el equipo visitante anote 'Y' = 'y' goles ('y' = 0, 1, ..., 6) en un partido.
# Genera entonces una tabla de cocientes, los cuales se obtengan de dividir las probabilidades conjuntas
# entre el producto de las probabilidades marginales correspondientes.
# 2. Mediante un procedimiento "Boostrap", obtén más cocientes similares a los obtenidos en la tabla del
# punto anterior; esto para tener una idea de la distribución de la cual provienen.
# 3. Menciona en qué casos te parece razonable suponer que los cocientes de la tabla, en el punto número uno,
# son iguales a "1" (en tal caso se tendría independencia de las variables aleatorias 'X' y 'Y').


# SOLUCIÓN:

packs <- c("dplyr", "boot")
lapply(packs, library, character.only = TRUE) # Se cargan a R las librerías que se van a utilizar.

data.1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv") # "Data set" 1: 2017/2018.
data.1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv") # "Data set" 2: 2018/2019.
data.1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv") # "Data set" 3: 2019/2020.

# Se guardan los datos en una lista para usar 'lapply()' y 'do.call()':
data.list <- list(data.1718, data.1819, data.1920)

# Se utiliza la función 'lapply()' para seleccionar únicamente las columnas de interés en cada documento:
data.list <- lapply(data.list, select, FTHG, FTAG)

# Se usa la función 'rbind()' para juntar todo en un solo "data frame":
data.games <- do.call(rbind, data.list)

# Del último "data frame", se extraen las columnas de interés para generar tablas de probabilidades:
games.FTHG <- select(data.games, FTHG)
games.FTAG <- select(data.games, FTAG)
games.goals <- select(data.games, FTHG, FTAG)

# Se crean tablas de probabilidades, marginales y conjunta, dividiendo las ocurrencias entre el número de
# juegos que hubo:
n <- nrow(games.goals) # Número total de partidos jugados.

FTHG.mprob <- round(table(games.FTHG)/n, 4) # Probabilidad marginal (equipo local).
FTAG.mprob <- round(table(games.FTAG)/n, 4) # Probabilidad marginal (equipo visitante).
games.cprob <- round(table(games.goals)/n, 4) # Probabilidad conjunta.

# Se definen algunas variables que se utilizarán más adelante dentro de un "ciclo for":
quot.table <- games.cprob
games.rows <- as.numeric(dim(games.cprob)[1])
games.cols <- as.numeric(dim(games.cprob)[2])

# Se genera una tabla vacía para después almacenar dentro de ella los cocientes que en seguida se calcularán:
for (y in 1:games.rows) {
  for (x in 1:games.cols) quot.table[y, as.character(x-1)] <- 0
}

for (y in 1:games.rows) {
  for (x in 1:games.cols) {
    cur.games.cprob <- games.cprob[y, as.character(x-1)]
    cur.FTAG.mprob <- FTAG.mprob[as.character(x-1)]
    cur.FTHG.mprob <- FTHG.mprob[as.character(y-1)]
    quotient <- cur.games.cprob/(cur.FTAG.mprob*cur.FTHG.mprob)
    quot.table[y, as.character(x-1)] <- quotient
  }
}

# Comentarios generales con respecto al tema de los cocientes:
# 1. Los cocientes tomarán valor unitario ("1") cuando la probabilidad conjunta sea igual al producto
# de las probabilidades marginales correspondientes.
# 2. La definición de independencia entre variables aleatorias indica que la probabilidad conjunta es igual al
# producto de las probabilidades marginales, únicamente cuando las variables aleatorias son totalmente
# independientes.
# 3. Nótese que cuando se obtiene un "1" en la tabla de cocientes, se tiene un par de eventos independientes.
# 4. Si las variables aleatorias fueran independientes, todos los valores deberían ser igual a "1" (salvo
# la posible división entre cero de eventos no probables).
# 5. Sería razonable esperar una tabla llena de "1's" si los goles que el equipo visitante anotara no se
# relacionaran con los goles del equipo local.
# 6. En general y aterrizando los comentarios, se puede asumir que los cocientes lejanos de "1" son eventos
# fuertemente relacionados, mientras que los cercanos a "1" están débilmente relacionados, es decir, tienden
# claramente a ser independientes y, por tanto, a distribuirse normalmente.

# Se le aplica "bootstrap" a la tabla obtenida:
set.seed(2020) # Se fija una "semilla" para que exista repetibilidad en la ejecución del código.
nboot <- 1000 # Se consideran 1000 réplicas (o muestras).

fc <- function(d, i) mean(d[i])
(bootobj <- boot(quot.table, fc, nboot))

hist(bootobj$t, freq = FALSE, main = "Histograma\n Distribución de cocientes\n Procedimiento 'Bootstrap'",
     xlab = "", ylab = "")

# Obsérvese el histograma obtenido y nótese que el resultado permitiría generar pruebas de hipótesis que
# ayudasen a determinar si es posible aproximar características de la población.

# De entrada y a primera vista, parece coherente sugerir que los cocientes se distribuyen de manera normal, con
# una media "µ" = 0.8723662. Esto significa que para todo valor cercano a "µ", la independencia entre variables
# es verdadera, pero no así en el caso donde los cocientes están muy alejados de él.