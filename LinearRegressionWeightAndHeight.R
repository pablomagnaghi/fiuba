# Linear regression

# En primer lugar, vamos a almacenar 
# los datos de las dos variables en dos vectores
peso <- c(74, 92, 63, 72, 58, 78, 85, 
          85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 
            169, 190, 186, 176, 170, 176, 179)

plot(altura, peso)

reg_lin <- lm(peso ~ altura)
reg_lin

# Coefficients:
# (Intercept)    altura  
# -80.222        0.877
# Peso = -80.222 + 0.877 * altura
# -80.222  es el valor del peso para una persona de altura 0, 
# lo cual no tiene sentido. De hecho, en multitud de ocasiones 
# la interpretación del parámetro no es relevante y todo el 
# interés recae sobre la interpretación del resto de parámetros

summary(reg_lin)

# Residuals:
# Min     1Q Median     3Q    Max 
# -6.872 -4.084 -1.196  3.038 10.005 

# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -80.2220    29.8888  -2.684 0.022934 *  
# altura        0.8770     0.1692   5.184 0.000411 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 5.572 on 10 degrees of freedom
# Multiple R-squared:  0.7288,	Adjusted R-squared:  0.7017 
# F-statistic: 26.87 on 1 and 10 DF,  p-value: 0.0004106

# encontramos información sobre los residuos (en el apartado 
# Residuals), que se definen como la diferencia entre el 
# verdadero valor de la variable dependiente y el valor que 
# pronostica el modelo de regresión. Cuanto más pequeños 
# sean estos residuos mejor será el ajuste del modelo a los 
# datos y más acertadas serán las predicciones que se realicen 
# a partir de dicho modelo.


# En la tabla Coefficients encontramos los valores de los 
# parámetros que aparecían en la salida por defecto junto 
# a su error estándar. Cada parámetro aparece acompañado del 
# valor de un estadístico t de Student y un p-valor que sirven 
# para contrastar la significación del parámetro en cuestión, 
# es decir, para resolver los siguientes contrastes de hipótesis:
# H0 ≡ B0 = 0 vs H1 ≡ B0 ≠ 0
# H0 ≡ B1 = 0 vs H1 = B1 ≠ 0

# Lo que se pretende mediante estos contrastes es determinar si 
# los efectos de la constante y de la variable independiente 
# son realmente importantes para de explicar la variable 
# dependiente o si, por el contario, pueden considerarse nulos.

# En nuestro ejemplo, los p-valores que nos ayudan a resolver 
# estos contrastes son 0.0229 y 0.0004, ambos menores que 0.05. 
# Así, considerando un nivel del significación del 5%, 
# rechazamos la hipótesis nula en ambos contrastes, de manera 
# que podemos suponer ambos parámetros significativamente 
# distintos de 0.

# Por último, en la parte final de la salida, encontramos el 
# valor de R² (Multiple R-squared) y de R² ajustado 
# (Adjusted R-squared), que son indicadores de la bondad del 
# ajuste de nuestro modelo a los datos. R² oscila entre 0 y 1, 
# de manera que, valores de R² próximos a 1 indican un buen 
# ajuste del modelo lineal a los datos. Por otro lado, R² 
# ajustado es similar a R², pero penaliza la introducción en 
# el modelo de variables independientes poco relevantes a la 
# hora de explicar la variable dependiente. Por tanto, R² 
# ajustado <=  R². En nuestro ejemplo, R² = 0.7288 y 
# R² ajustado = 0.7017, por lo que podemos concluir que el 
# modelo lineal se ajusta de forma aceptable a nuestros datos. 
# La última línea de la salida incluye un estadístico 
# F de Snedecor y el p-valor correspondiente que se utilizan 
# para resolver el siguiente contraste:
# H0 ≡ Bi ∀ i
# H1 ≡ Bi ≠ 0 para al menos un i
# que se conoce habitualmente como contraste ómnibus. Mediante 
# este contraste se comprueba si, de forma global, el modelo 
# lineal es apropiado para modelizar los datos. En nuestro 
# ejemplo, el p-valor asociado a este contraste es inferior 
# a 0.05 por lo que, al 5% de significación podemos rechazar 
# la hipótesis nula y afirmar que, efectivamente, el modelo 
# lineal es adecuado para nuestro conjunto de datos.

# R nos permite dibujar la recta de regresión lineal sobre el 
# diagrama de dispersión mediante la orden abline. De este 
# modo podemos visualizar la distancia existente entre los 
# valores observados y los valores que el modelo pronostica 
# (esto es, los residuos).
  
plot(altura, peso)
abline(reg_lin)