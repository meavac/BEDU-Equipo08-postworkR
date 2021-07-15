# Sesión 3. Análisis exploratorio de datos (AED o EDA) con R.

# Postwork 3. Sesión del 22/06/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. Con el último "data frame" generado en el "Postwork" de la sesión 2, elabora tablas de frecuencias
# relativas para estimar las siguientes probabilidades;
# 1.1. La probabilidad marginal de que el equipo que juega en casa anote 'x' goles ('x' = 0, 1, ..., n).
# 1.2. La probabilidad marginal de que el equipo que juega como visitante anote 'y' goles ('y' = 0, 1, ..., n).
# 1.3. La probabilidad conjunta de que el equipo que juega en casa anote 'x' goles y el equipo que juega como
# visitante anote 'y' goles.
# 2. Realiza lo siguiente;
# 2.1. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el
# equipo de casa.
# 2.2. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el
# equipo visitante.
# 2.3. Un "HeatMap" para las probabilidades conjuntas estimadas de los números de goles que anotan los equipos
# local y visitante en un partido.


# SOLUCIÓN:

packs <- c("dplyr", "ggplot2", "plotly")
lapply(packs, library, character.only = TRUE) # Se cargan a R las librerías que se van a utilizar.

data.1718 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv") # "Data set 1": 2017/2018.
data.1819 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv") # "Data set 2": 2018/2019.
data.1920 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv") # "Data set 3": 2019/2020.

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

# Se modifica la tipología de las tablas para proceder a graficar los datos:
FTAG.mprob.df <- as.data.frame(FTAG.mprob)
FTHG.mprob.df <- as.data.frame(FTHG.mprob)
games.cprob.df <- as.data.frame(games.cprob)

# Se modifican los títulos de las columnas de los nuevos "data frames" para hacer las gráficas más amigables:
names(FTAG.mprob.df) <- c("Goles.visitante", "P.marginal")
names(FTHG.mprob.df) <- c("Goles.local", "P.marginal")
names(games.cprob.df) <- c("Goles.local", "Goles.visitante", "P.conjunta")

# Gráficas:
data.games.heatmap <- ggplot(games.cprob.df,
                             aes(x = Goles.local, y = Goles.visitante, fill = P.conjunta)) +
         geom_tile() + xlab("Goles del equipo local") + ylab("Goles del equipo visitante") +
                labs(title = "Probabilidad conjunta:\n anotaciones de ambos equipos") +
                                          theme(plot.title = element_text(size = 12)) +
                                          scale_fill_gradient(name = "Probabilidad\nconjunta",
                                                              low = "#f13c77", high = "#f5e6ad") +
                                          theme(plot.margin = margin(20, 20, 20, 15),
                                                title = element_text(face = 'bold', color = "#71092A"))

data.FTHG.bar <- ggplot(FTHG.mprob.df, aes(x = Goles.local, y = P.marginal)) +
            geom_bar(stat = "identity", color = "#F02D3A", fill = "#273043") +
       theme_dark() + xlab("Goles anotados") + ylab("Probabilidad marginal") +
       labs(title = "Probabilidad marginal:\n anotaciones del equipo local") +
       theme(plot.margin = margin(25, 20, 30, 25),
             title = element_text(size = 9, face = 'bold', color = "#273043"))

data.FTAG.bar <- ggplot(FTAG.mprob.df, aes(x = Goles.visitante, y = P.marginal)) +
                geom_bar(stat = "identity", color = "#99E1D9", fill = "#32292F") +
                          xlab("Goles anotados") + ylab("Probabilidad marginal") +
       labs(title = "Probabilidad marginal:\n anotaciones del equipo visitante") +
       theme(plot.margin = margin(25, 25, 20, 20),
             title = element_text(size = 9, face = 'bold', color = "#32292F"))

# Se utilizaron las librerías 'plotly' y 'ggplot2' para insertar "tooltips" en las gráficas:
ggplotly(data.games.heatmap)
ggplotly(data.FTHG.bar)
ggplotly(data.FTAG.bar)
