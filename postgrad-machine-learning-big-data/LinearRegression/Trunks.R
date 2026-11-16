# La siguiente tabla recoge informacion sobre el diametro (en pulgadas), 
# la altura (en pies) y el volumen (en pies cubicos) del tronco de 
# distintos cerezos en una determinada region

#########################
# Crear un data frame de nombre Cerezos que almacene los datos de los arboles


diam <- c(8.3, 8.6, 8.8, 10.5, 10.5, 10.8, 11.0, 11.0)

alt <- c(70, 65, 63, 72, 81, 83, 66, 75)

vol <- c(10.3, 10.3, 10.2, 16.4, 18.8, 19.7, 15.6, 16.3)

Cerezos <- data.frame(diam, alt, vol)

Cerezos

#########################
# Dibujar el diagrama de dispersion de las variables altura y volumen 
# y determinar si puede existir una cierta relacion lineal entre ambas

plot(Cerezos$vol, Cerezos$alt)

# En el grafico se aprecia un cierto patron lineal entre las variables, 
# aunque esto debe confirmarse mediante metodos analiticos.

#########################
# ¿Cual es la recta de regresion lineal simple que considera a la altura 
# como variable dependiente y al volumen como variable independiente? 
# Interpreta los parametros de esa recta.

reg_lin <- lm(formula = alt ~ vol, data = Cerezos)
reg_lin

# La recta de regresiÃ³n lineal simple es
# alt = 48.336 + 1.601 * vol

# Los dos parametros de la recta se interpretan del siguiente modo: 
# 48.336 es la altura esperada para un cerezo que tiene un volumen 
# de 0 pies cubicos. Por otra parte, por cada pie cubico de incremento 
# en el volumen del tronco del cerezo, se espera un aumento en su altura de 1.601 pies.

#########################
# ¿Son significativos estos parametros? 
# ¿Que puede decirse del ajuste del modelo a los datos?

summary(reg_lin)

# Considerando un nivel de significacion del 5%, ambos parametros 
# son significativamente distintos de 0, ya que los p-valores 
# asociados a los contrastes t de Student de los dos parÃ¡metros son inferiores a 0.05.

# El ajuste del modelo a los datos es aceptable, ya que el valor de RÂ² es de 0.7206.

#########################
# ¿Cual es la correlacion lineal de Pearson entre ambas variables? 
# ¿Es significativa?
  
cor(Cerezos$vol, Cerezos$alt)# El coeficiente de correlacion lineal de Pearson entre las 
# dos variables es de 0.8488805.

cor.test(Cerezos$vol, Cerezos$alt)

# Con un p-valor de 0.00768 < 0.05, rechazamos la hipotesis de que el 
# coeficiente de correlacion lineal de Pearson entre ambas 
# variables pueda considerarse 0.

#########################
# ¿Cual es la recta de regresion lineal si se considera 
# tambien como variable independiente el diametro?

reg_lin2 <- lm(formula = alt ~ vol + diam)
reg_lin2

# alt = 85.340 + 3.231 * vol - 6.135 * diam
  
