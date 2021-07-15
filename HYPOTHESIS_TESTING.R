# PRUEBA DE HIPÓTESIS

# A continuación, se contrastará una hipótesis para determinar si en un encuentro de fútbol, de la liga
# española, coviene apostar por algún equipo en particular considerando únicamente su localidad, es decir
# si es de casa o visitante.

# install.packages("qcc")
library(dplyr)
library(qcc)

set.seed(100)
data.2017 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")
data.2018 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
data.2019 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")

lista <- list(data.2017, data.2018, data.2019)

# Resultados finales de cada partido ("Full time scores"):
graficar_FTR <- lapply(lista, select, FTR)
graficar_FTR <- do.call(rbind, graficar_FTR); head(graficar_FTR)
graficar_FTR <- table(graficar_FTR)

# Se aplica un Diagrama de Pareto:
pareto.chart(graficar_FTR, ylab = "Frecuencia", ylab2 = "Porcentaje acumulativo",
             cumperc = seq(0, 100, by = 25),
             main = "Resultados finales de cada partido")

# Resultados al medio tiempo de cada partido ("Half time scores"):
graficar_HTR <- lapply(lista, select, HTR)
graficar_HTR <- do.call(rbind, graficar_HTR); head(graficar_HTR)
graficar_HTR <- table(graficar_HTR)

# Se aplica otro Diagrama de Pareto:
pareto.chart(graficar_HTR, ylab = "Frecuencia", ylab2 = "Porcentaje acumulativo",
             cumperc = seq(0, 100, by = 25),
             main = "Resultados al medio tiempo de cada partido")

# Con base en ambos diagramas, es posible suponer que los equipos locales tienen mayor probabilidad de ganar.
# Por lo anterior, se puede pensar que apostar por el equipo local siempre dejaría más ganancias que pérdidas,
# es decir, el razonamiento podría ser válido para implementarse como una estrategia de apuesta.
# No obstante, para comprobar si lo anterior es cierto, se deberá realizar una prueba de hipótesis.

# En la siguiente prueba se demostrará si existe diferencia significativa entre apostar por un equipo u
# otro según sea su localidad (de casa o visitante). Nótese que la hipótesis será bastante simple, pues se
# descartará el escenario donde ocurren los empates. Aun así, el desarrollo que se sigue a continuación
# resultará ser significativo para fines demostrativos.

apuestas <- lapply(lista, select, Date, HomeTeam, AwayTeam, B365H:B365A)
apuestas <- do.call(rbind, apuestas)
names(apuestas) <- c("Fecha", "Local", "Visitante", "Cuota local", "Cuota empate", "Cuota visitante")
head(apuestas)


# EJERCICIO (SEGMENTO DE CÓDIGO BREVE PARA ENTENDER MEJOR EL MANEJO DE LAS APUESTAS)

# Obsérvese que si una persona apostara una cantidad cualquiera a un equipo 'X', la casa de apuesta
# le pagaría la cantidad de la cuota multiplicada por la cantidad apostada.

# Ejemplos:
j <- length(apuestas$`Cuota local`)
x <- sample(1:2, j, replace = T) # Se generan apuestas aleatorias.
ej <- lapply(lista, select, HomeTeam, AwayTeam, B365H:B365A)
ej <- do.call(rbind, ej)
names(ej) <- c("Equipo local", "Equipo visitante", "Cuota local",
               "Cuota empate", "Cuota visitante")

ej[, "Apuesta (unidad)"] <- x # Apuestas aleatorias.
ej[, "Ganancia local"] <- ej[, 6]*ej[, 3] # Ganancia si el equipo local ganara.
ej[, "Ganancia por empate"] <- ej[, 6]*ej[, 4] # Ganancia si hubiese empate.
ej[, "Ganancia visitante"] <- ej[, 6]*ej[, 5] # Ganancia si el equipo visitante ganara.
head(ej) # Se muestran las ganancias que se obtendrían si se apostara por el equipo local, por el empate o 
# por el equipo visitante.


# DESARROLLO DE LA HIPÓTESIS

apuestas[, "probabilidad exito local"] <- 1/apuestas[, 4]
apuestas[, "probabilidad exito empate"] <- 1/apuestas[, 5]
apuestas[, "probabilidad exito visitante"] <- 1/apuestas[, 6]
head(apuestas)

m1 <- apuestas$`probabilidad exito local`
m2 <- apuestas$`probabilidad exito visitante`

# Se toma una muestra aleatoria de 56 elementos (n1 = 56) para el primer grupo y una muestra de 63 elementos
# (n2 = 63) para el segundo grupo:
muestra1 <- sample(m1, 56, replace = FALSE)
muestra2 <- sample(m2, 63, replace = FALSE)
n1 <- length(muestra1)
n2 <- length(muestra2)

# Planteamiento de las hipótesis nula y alternativa;
# H0: m1-m2 = 0
# Ha: m1-m2 != 0 (contraste de dos colas)

# Estadístico de prueba:
(z0 <- (mean(muestra1)-mean(muestra2)-0)/sqrt(var(muestra1)/n1+var(muestra2)/n2))

# Se busca la region de rechazo (de dos colas) con un nivel de significancia "alpha" = 0.05; para ello se
# debe encontrar el valor z_{0.025} que satisfaga P(Z > z_{0.025}) = 0.025.

(z.025 <- qnorm(p = 0.025, lower.tail = FALSE))
(z0 < -z.025) | (z0 > z.025)
(pvalue <- 2*pnorm(z0, lower.tail = FALSE)) 

x <- seq(-6, 6, 0.02)
y <- dnorm(x)
plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad normal estándar")
axis(side = 1, at = -z.025, font = 2, padj = 2, lwd = 2)
axis(side = 1, at = z.025, font = 2, padj = 2, lwd = 2)

# El valor del estadístico de prueba se ubicó en la zona de rechazo, consiguientemente se acepta la hipótesis
# alternativa. Entonces, se dice que no hay pruebas estadísticamente significativas para afirmar que las
# medias sean iguales y, por tanto, se concluye que la probabilidad de ganar apostando por el equipo local no
# es igual a la probabilidad de ganar apostando por el equipo visitante.

# Finalmente, se realiza una prueba 't.test()' para comprobar los resultados obtenidos anteriormente:
t.test(x = muestra1, y = muestra2, alternative = "two.sided", mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)