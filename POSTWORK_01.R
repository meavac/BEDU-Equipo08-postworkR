# Sesión 1. Introducción a R y Software.

# Postwork 1. Sesión del 15/06/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. Importa los datos de fútbol, de la temporada 2019/2020, de la primera división de la liga española a R;
# los datos los podrás encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php.
# 2. Del "data frame" que resulta de importar los datos a R, extrae las columnas que contienen los números 
# de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron
# como visitante (FTAG).
# 3. Consulta cómo funciona la función 'table()' en R al ejecutar en la consola '?table'.
# 4. Elabora tablas de frecuencias relativas para estimar las siguientes probabilidades;
# 4.1. La probabilidad marginal de que el equipo que juega en casa anote 'x' goles ('x' = 0, 1, ..., n).
# 4.2. La probabilidad marginal de que el equipo que juega como visitante anote 'y' goles ('y' = 0, 1, ..., n).
# 4.3. La probabilidad conjunta de que el equipo que juega en casa anote 'x' goles y el equipo que juega como
# visitante anote 'y' goles.


# SOLUCIÓN:

library(dplyr) # Se importa a R la líbrería 'dplyr', pues más adelante se utiliza la función 'select()'.

data <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv") # "Data set" a utilizar: 2019/2020.

# Del "data set" previamente cargado, se seleccionan las columnas de interés y se generan las tablas
# de frecuencias relativas:
data.FTHG.table <- table(select(data, FTHG), dnn = "Goles 'x' del equipo local")
data.FTAG.table <- table(select(data, FTAG), dnn = "Goles 'y' del equipo visitante")
data.goals.table <- table(select(data, FTHG, FTAG), dnn = c("Goles local", "Goles visitante"))

# Se dividen los resultados anteriores entre el número de renglones (eventos), para obtener las probabilidades
# marginales y conjunta:
n <- nrow(data) # Cantidad de renglones (nótese que un renglón es igual a un juego).

(data.FTHG.table/n) # Probabilidad marginal de que el equipo local anote 'x' goles, en decimal.
(data.FTAG.table/n) # Probabilidad marginal de que el equipo visitante anote 'y' goles, en decimal.
data.goals.table <- (data.goals.table/n) # Probabilidad conjunta de que los dos equipos anoten 'x' y 'y'
# goles, respectivamente.

# Se crean vectores aleatorios, los cuales se forman con los números de goles posibles que podrían anotar
# ambos equipos, respectivamente:
data.goals.table <- addmargins(data.goals.table)
data.goals.table

vec1 <- c(0:6)
vec2 <- c(0:5)

# Nótese que no se fija ninguna semilla, pues se pretende que tras cada ejecución la combinación de los eventos
# sea distinta.
x <- sample(vec1, 1, F)
y <- sample(vec2, 1, F)

paste("La probabilidad de que el equipo que juega en casa anote '", x, "' gol(es) es de ",
      round(data.goals.table[x+1,7]*100,4), " %", sep = "")
paste("La probabilidad de que el equipo que juega como visitante anote '", y, "' gol(es) es de ",
      round(data.goals.table[8,y+1]*100,4), " %", sep = "")
paste("La probabilidad de que el equipo que juega en casa anote '", x,
      "' gol(es) y el equipo que juega como visitante anote '" , y, "' gol(es) es de ",
      round(data.goals.table[x+1,y+1]*100,4), " %", sep = "")
