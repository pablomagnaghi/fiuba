# Se dispone de un data set con informacion sobre diferentes coches.
# Se quiere estudiar si existe una correlación entre el peso de un
# vehículo (Weight) y la potencia de su motor (Horsepower).

require(MASS)
# install.packages('ggplot2')
require(ggplot2)
data("Cars93")

# En primer lugar se representan las dos variables mediante un
# diagrama de dispersion (+) para intuir si existe relacion
# lineal o monotonica.

ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) +
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# 1. Analisis de normalidad
# Representación gráfica
par(mfrow = c(1, 2))
hist(Cars93$Weight, breaks = 10, main = "", xlab = "Weight", border = "darkred")
hist(Cars93$Horsepower, breaks = 10, main = "", xlab = "Horsepower",
  border = "blue")

qqnorm(Cars93$Weight, main = "Weight", col = "darkred")
qqline(Cars93$Weight)
qqnorm(Cars93$Horsepower, main = "Horsepower", col = "blue")
qqline(Cars93$Horsepower)

par(mfrow = c(1,1))

#Test de hipotesis para el analisis de normalidad
shapiro.test(Cars93$Weight)

shapiro.test(Cars93$Horsepower)


# El analisis grafico y el contraste de normalidad muestran
# que para la variable Horsepower no se puede asumir normalidad
# y que la variable Weight esta en el limite. Siendo estrictos,
# este hecho excluye la posibilidad de utilizar el coeficiente de Pearson
# Sin embargo, dado que la distribucion no se aleja mucho de la normalidad
# y de que el coeficiente de Pearson tiene cierta robustez, a fines
# practicos si que se podria utilizar siempre y cuando se tenga en cuenta
# este hecho en los resultados. Otra posibilidad es tratar de transformar
# las variables para mejorar su distribucion.

# Representacion grafica
par(mfrow = c(1, 2))
hist(log10(Cars93$Horsepower), breaks = 10, main = "", xlab = "Log10(Horsepower)",
  border = "blue")
qqnorm(log10(Cars93$Horsepower), main = "", col = "blue")
qqline(log10(Cars93$Horsepower))

par(mfrow = c(1, 1))
shapiro.test(log10(Cars93$Horsepower))

# La trasformacion logaritmica de la variable Horsepower consigue una
# distribucion de tipo normal.

# 2. Homocedasticidad
# La homocedasticidad implica que la varianza se mantenga constante. Puede analizarse
# de forma grafica representando las observaciones en un diagrama de dispersion y
# viendo si mantiene una homogeneidad en su dispersion a lo largo del eje X. Una
# forma conica es un claro indicativo de falta de homocedasticidad.
# Tal como muestra el diagrama de dispersion generado al inicio del ejercicio,
# sí hay un patron conico. Esto debe de tenerse en cuenta si se utiliza
# Pearson puesto que viola una de sus condiciones.
ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) +
  geom_point(colour = "red4") +
  geom_segment(aes(x = 1690, y = 70, xend = 3100, yend = 300),linetype="dashed") +
  geom_segment(aes(x = 1690, y = 45, xend = 4100, yend = 100),linetype="dashed") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# 3. Calculo de correlacion
# Debido a la falta de homocedasticidad, los resultados generados por
# Pearson no son precisos, desde el punto de vista teorico Spearman o
# Kendall son mas adecuados. Sin embargo, en la bibliografía emplean
# Pearson, asi que se van a calcular tanto Pearson como Spearman.

cor(x = Cars93$Weight, y = log10(Cars93$Horsepower), method = "pearson")
# La funcion cor() tambien acepta matrices o data frames y calcula todas las
# correlaciones dos a dos

# Muestran una correlacion alta (>0.8). Sin embargo para poder considerar
# que existe realmente correlacion entre las dos variables es necesario
# calcular su significancia, de lo contrario podría deberse al azar

# 4. Significancia de la correlacion
# Por muy alto que sea un coeficiente de correlacion, si no es significativa
# se ha de considerar inexistente.
cor.test(x = Cars93$Weight, y = log10(Cars93$Horsepower),
  alternative = "two.sided", conf.level = 0.95, method = "pearson")

# el coeficiente de correlacion es significativo p value -> 0

# 5. Coeficiente de determinacion R2 (tamaño del efecto)
R2_pearson <- cor(x = Cars93$Weight, y = log10(Cars93$Horsepower),
                  method = "pearson") ^ 2
R2_pearson

# 6. Conclusion

# Existe una correlacion significativa entre el peso del vehículo y la
# potencia de su motor (r=0.8, p-value < 2.2e-16), con un tamaño de
# efecto medio-alto (R2=0.66).
