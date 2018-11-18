# Se dispone de un registro que contiene cientos de emails con informacion de
# cada uno de ellos. El objetivo de estudio es intentar crear un modelo que permita
# filtrar que emails son “spam” y cuáles no, en funcion de determinadas características.
# Ejemplo extraido del libro OpenIntro Statistics.
library(openintro)
data(email)
str(email)

# En este caso se van a emplear unicamente como posibles predictores variables categoricas.
# Esto se debe a que los outliers complican bastante la creación de estos modelos y en el
# data set que se emplea como ejemplo las variables cuantitativas son muy asimetricas.
# En particular, las variables que se van a estudiar como posibles predictores son:

## spam: si el email es spam (1) si no lo es (0)
## to_multiple: si hay mas de una persona en la lista de distribucion.
## format: si esta en formato HTLM
## cc: si hay otras direcciones en copia.
## attach: si hay archivos adjuntos
## dollar: si el email contiene la palabra dollar o el simbolo $.
## inherit: si contiene la palabra inherit
## winner: si el email contiene la palabra winner.
## password: si el email contiene la palabra password.
## re_subj: si la palabra “Re:” esta escrita en el asunto del email.
## exclaim_subj: si se incluye algun signo de exclamación en el email.
# En primer lugar se genera el modelo completo introduciendo todas las variables como predictores.
modelo_completo <- glm( spam ~ to_multiple + format + cc + attach + dollar +
                        winner + inherit + password + re_subj + exclaim_subj,
                        data = email, family = binomial())
summary(modelo_completo)

# Se mejora el modelo mediante el proceso basado en p-values. El resultado es el siguiente modelo.
modelo_final <- glm( spam ~ to_multiple + format + attach + dollar + winner +
                    inherit + password + re_subj, data = email,
                    family = binomial())
summary(modelo_final)

# Con este modelo podemos saber la probabilidad, dadas unas determinadas caracteristicas,
# de que el email sea spam (valor 1 de la variable).

# Para evaluar el modelo, se puede comparar el valor real (si realmente es spam) con
# el predicho por el modelo.
library(vcd)
predicciones <- ifelse(test = modelo_final$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo_final$model$spam, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

# El modelo generado no es capaz de diferenciar bien entre spam y no spam, solo 12 de los
# 367 correos spam se han identificado correctamente. Si fuese capaz, la probabilidad
# calculada por el modelo para aquellos emails del dataset que sí son spam deberían
# estar por encima del 0.5.
