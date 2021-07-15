# Sesión 6. Series de tiempo.

# Postwork 6. Sesión del 01/07/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. Importa el conjunto de datos 'match.data.csv' a R y realiza los siguientes pasos.
# 2. Agrega una nueva columna, llamada "suma.goles", que contenga la suma de goles por partido.
# 3. Obtén el promedio, por mes, de la suma de goles.
# 4. Crea la serie de tiempo del promedio, por mes, de la suma de goles hasta diciembre de 2019.
# 5. Grafica la serie de tiempo.


# SOLUCIÓN:

library(dplyr)
match.data <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv")

match.data <- mutate(match.data, suma.goles = match.data[, 3] + match.data[, 5],
                     date = as.Date(date, format = "%Y-%m-%d"))

days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 1) # Se utilizará para generar los promedios.
monthly.avg <- seq(1, 120) # Se utilizará para almacenar los promedios.

for (i in 1:10) { # Número de años de los que se tiene registro.
  for (j in 1:12) { # Número de meses que tiene un año.
    match.month <- filter(match.data, date >= as.Date(paste("201", i-1, "-", j, "-01", sep = "")) &
                          date <= as.Date(paste("201", i-1, "-", j, "-", days[j], sep = "")))
    monthly.avg[days[13]] <- round(mean(match.month$suma.goles), 4)
    days[13] <- days[13] + 1
  }
}

monthly.avg # Nótese que, dentro del conjunto de datos obtenido, existe un promedio perdido (3.8000), el cual
# corresponde al mes de junio del año 2013.
# Nótese también que el registro de datos inicia en agosto de 2010 y que en los meses de junio y julio nunca
# hay partidos en la liga española de fútbol.
# Entonces es posible suponer, del "data set" original, que en mayo de 2013 hubo una temporada desfasada, la
# cual se jugó hasta el primero de junio del mismo año. Por lo anterior, sería incorrecto considerar el promedio
# obtenido para junio de 2013 como un promedio adicional dentro de la serie de tiempo.
# Por lo tanto, se decidió promediar los goles de la temporada perdida (junio de 2013) con los goles de las
# temporadas jugadas en el mes previo (mayo de 2013).

mes.doble <- filter(match.data, date >= as.Date("2013-05-01") & date <= as.Date("2013-07-01")) # Filtro.
monthly.avg[41] <- mean(mes.doble$suma.goles) # Promedio de goles anotados en mayo y junio de 2013.
monthly.avg[42] <- NaN # Se elimina el dato perdido del vector de promedios obtenido.

(monthly.avg <- na.omit(monthly.avg)) # Nuevo conjunto de datos, a partir del cual se crea la serie de tiempo.

ts.avg <- ts(monthly.avg, start = c(2010, 1), end = c(2019, 5), freq = 10) # Serie de tiempo.
ts.plot(ts.avg, main = "Serie de tiempo", sub = "Liga española de fútbol",
        xlab = "Registro en años",
        ylab = "Promedios mensuales de las sumas de goles")
abline(h = mean(monthly.avg), lty = 2, col = "blue", lwd = 2)
legend(x = 2010, y = 2.15, legend = "mean", col = "blue", lty = 2, lwd = 1)