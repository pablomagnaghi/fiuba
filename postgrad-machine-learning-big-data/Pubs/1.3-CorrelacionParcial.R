# Se quiere estudiar la relacion entre las variables precio y peso de
# los automoviles. Se sospecha que esta relacion podría estar influenciada
# por la variable potencia del motor, ya que a mayor peso del vehiculo se
# requiere mayor potencia y, a su vez, motores mas potentes son mas caros.
require(MASS)
require(ggplot2)
data("Cars93")
# e emplea el log del precio porque mejora la linealidad
ggplot(data = Cars93, aes(x = Weight, y = log(Price))) +
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# El grafico permite intuir que existe una relacion lineal entre
# el peso de un coche y el logaritmo de su precio.
# install.packages('ppcor')
require(ppcor)
cor.test(x = Cars93$Weight, y = log(Cars93$Price), method = "pearson")

pcor.test(x = Cars93$Weight, y = log(Cars93$Price), z = Cars93$Horsepower,
          method = "pearson")

# La correlacion entre el peso y el logaritmo del precio es alta (r=0.764) y
# significativa (p-value < 2.2e-16). Sin embargo, cuando se estudia su
# relacion bloqueando la variable potencia de motor, a pesar de que la relacion
# sigue siendo significativa (p-value = 6.288649e-05) pasa a ser baja (r=0.4047).


# Conclusion
# La relacion lineal existente entre el peso y el logaritmo del precio esta 
# influenciada por el efecto de la variable potencia de motor. Si se controla
# el efecto de la potencia, la relacion lineal existente es baja (r=0.4047).
