# Sesión 7. RStudio Cloud & GitHub, conexiones con BDs y lectura de datos externos.

# Postwork 7. Sesión del 06/07/2021.


# Instrucciones (obtenidas directamente del repositorio de BEDU en la sesión correspondiente):

# 1. Utilizando el manejador de bases de datos "MongoDB Compass", realiza las siguientes acciones.
# 2. Aloja el fichero 'match.data.csv' en una base de datos llamada 'match_games', nombrando al
# "collection" como 'match'.
# 3. Una vez realizado el paso anterior, realiza un 'count()' para conocer el número de registros que se
# tiene en la base de datos previamente creada.
# 4. Utilizando la sintaxis de "MongoDB", realiza una consulta para conocer el número de goles que metió el
# equipo "Real Madrid" el 20 de diciembre de 2015. ¿Contra que equipo jugó? ¿Perdió o goleó?
# 5. Por último, no olvides cerrar la conexión con la BDD.


# SOLUCIÓN:

# install.packages("mongolite")
library(mongolite)

# Se almacena en una variable la dirección del servidor que suministra la base de datos (servidor de MongoDB):
MongoDB.ServerURL <- "mongodb+srv://mangomongoDB:AzCxEv1937-5@cluster0.zojvs.mongodb.net/test?authSource=admin&replicaSet=atlas-wl1x00-shard-0&readPreference=primary&appname=MongoDB%20Compass&ssl=true"

data.MongoDB <- mongo(collection = "match",
                      db = "match_games",
                      url = MongoDB.ServerURL) # Se establece la conexión con la base de datos.

Registros <- data.MongoDB$count('{}') # Se obtiene el número de registros con los que cuenta la base de datos.

# Se realiza una consulta utilizando la sintaxis nativa de MongoDB:
(Query <- data.MongoDB$find(query = '{"date": "2015-12-20", "$or": [{"home.team": "Real Madrid"}, {"away.team": "Real Madrid"}]}', fields = '{"_id": 0, "date": 0}'))
class(Query) # Se observa la tipología de la estructura obtenida.
str(Query) # Se observa la naturleza de sus variables.

# Se despliega el resultado de la consulta utilizando 'paste()', una función nativa de R:
paste("El 20 de diciembre de 2015, el equipo ", Query$home$team, " goleó al equipo ",
      Query$away$team, ". El resultado fue: {", Query$home$score, " - ", Query$away$score, "}.", sep = "")

data.MongoDB$disconnect(gc = TRUE) # Siempre es una buena práctica desconectarse del servidor.
