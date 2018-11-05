# En el siguiente ejemplo se emplea el metodo bootstrap para estudiar la
# variabilidad en la estimacion de los coeficientes de regresion β0 y β1
# (interseccion y pendiente) de un modelo de regresión lineal. El modelo en
# cuestion emplea la variable horsepower para predecir el consumo de un
# vehiculo mpg, los datos empleados son del data set Auto.

# El proceso de bootstraping consiste en generar de forma iterativa diferentes
# modelos lineales, empleando en cada caso una bootstrap-sample creada mediante
# resampling del mismo tamaño que la muestra inicial. Para cada modelo ajustado
# se registran los valores de los coeficientes β0 y β1 y finalmente se estudia su distribucion.

# Se define la función que devuelve el estadístico de interés, los coeficientes
# de regresión
fun_coeficientes <- function(data, index){
    return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

# Se implementa un bucle que genere los modelos de forma iterativa y almacene
# los coeficientes. El data frame Auto tiene 392 observaciones
beta_0 <- rep(NA,9999)
beta_1 <- rep(NA,9999)
for(i in 1:9999) {
    coeficientes <- fun_coeficientes(data = Auto,
                                     index = sample(1:392, 392, replace = TRUE))
    beta_0[i] <- coeficientes[1]
    beta_1[i] <- coeficientes[2]
}
# Se muestra la distribución de los coeficientes
p5 <- ggplot(data = data.frame(beta_0 = beta_0), aes(beta_0)) +
      geom_histogram(colour = "firebrick3") +
      theme_bw()
p6 <- ggplot(data = data.frame(beta_1 = beta_1), aes(beta_1)) +
      geom_histogram(colour = "firebrick3") +
      theme_bw()
grid.arrange(p5,p6, ncol = 2,
             top = "Bootstrap distribution de los coeficientes")

# El valor del estadistico estimado mediante bootstrapping es la media de la
# bootstrap-distribution y la desviación estandar del estadistico la desviacion
# estandar de la distribucion:

# mean(beta_0) =39.9652841
# sd(beta_0) = 0.8568472
# mean(beta_1) = -0.1582257
# sd(beta_1) = 0.007442

# Para este caso, si se comparan los valores estimados mediante bootstrapping
# con los devueltos por la función lm() (obtenidos por t-test), se observa
# una diferencia muy pequeña.

summary(lm(mpg~horsepower,data = Auto))$coef


# El mismo resultado se puede obtener empleando la funcion boot().
boot(data = Auto, statistic = fun_coeficientes, R = 9999)
