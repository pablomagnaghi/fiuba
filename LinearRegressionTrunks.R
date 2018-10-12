# La siguiente tabla recoge información sobre el diámetro (en pulgadas), 
# la altura (en pies) y el volumen (en pies cúbicos) del tronco de 
# distintos cerezos en una determinada región

#########################
# Crear un data frame de nombre Cerezos que almacene los datos de los árboles


diam <- c(8.3, 8.6, 8.8, 10.5, 10.5, 10.8, 11.0, 11.0)

alt <- c(70, 65, 63, 72, 81, 83, 66, 75)

vol <- c(10.3, 10.3, 10.2, 16.4, 18.8, 19.7, 15.6, 16.3)

Cerezos <- data.frame(diam, alt, vol)

Cerezos

#########################
# Dibujar el diagrama de dispersión de las variables altura y volumen 
# y determinar si puede existir una cierta relación lineal entre ambas

plot(Cerezos$vol, Cerezos$alt)

# En el gráfico se aprecia un cierto patrón lineal entre las variables, 
# aunque esto debe confirmarse mediante métodos analíticos.

#########################
# ¿Cuál es la recta de regresión lineal simple que considera a la altura 
# como variable dependiente y al volumen como variable independiente? 
# Interpreta los parámetros de esa recta.

reg_lin <- lm(formula = alt ~ vol, data = Cerezos)
reg_lin

# La recta de regresión lineal simple es
# alt = 48.336 + 1.601 * vol

# Los dos parámetros de la recta se interpretan del siguiente modo: 
# 48.336 es la altura esperada para un cerezo que tiene un volumen 
# de 0 pies cúbicos. Por otra parte, por cada pie cúbico de incremento 
# en el volumen del tronco del cerezo, se espera un aumento en su altura de 1.601 pies.

#########################
# ¿Son significativos estos parámetros? 
# ¿Qué puede decirse del ajuste del modelo a los datos?

summary(reg_lin)

# Considerando un nivel de significación del 5%, ambos parámetros 
# son significativamente distintos de 0, ya que los p-valores 
# asociados a los contrastes t de Student de los dos parámetros son inferiores a 0.05.

# El ajuste del modelo a los datos es aceptable, ya que el valor de R² es de 0.7206.

#########################
# ¿Cuál es la correlación lineal de Pearson entre ambas variables? 
# ¿Es significativa?
  
cor(Cerezos$vol, Cerezos$alt)# El coeficiente de correlación lineal de Pearson entre las 
# dos variables es de 0.8488805.

cor.test(Cerezos$vol, Cerezos$alt)

# Con un p-valor de 0.00768 < 0.05, rechazamos la hipótesis de que el 
# coeficiente de correlación lineal de Pearson entre ambas 
# variables pueda considerarse 0.

#########################
# ¿Cuál es la recta de regresión lineal si se considera 
# también como variable independiente el diámetro?

reg_lin2 <- lm(formula = alt ~ vol + diam)
reg_lin2

# alt = 85.340 + 3.231 * vol - 6.135 * diam
  
