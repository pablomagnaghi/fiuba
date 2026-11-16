# La funcion cv.glm() del paquete boot puede emplearse para realizar K-fold
# Cross-Validation de cualquier modelo creado mediante la función glm(),
# especificando el número de grupos en el argumento k.

# Se genera el modelo lineal
modelo <- glm(mpg ~ horsepower, data = Auto)
# Se emplea la función cv.glm() para la validación, empleando en este caso k=10
set.seed(1)
cv_error <- cv.glm(data = Auto, glmfit = modelo, K = 10)
cv_error$delta

# La estimacion de error del modelo lineal mediante K-fold Cross-Validation k=10
# es de 24.1, un valor muy proximo al estimado mediante LOOCV, pero empleando
# mucho menos tiempo de computacion. Cuando se especifica el numero de grupos K
# a emplear en la validación, la funcion cv.glm() devuelve dos resultados, uno
# con corrección de continuidad y otro sin ella.

# Si se estudia la influencia de la flexibilidad del modelo, los resultados
# muestran la misma tendencia que LOOCV. Se obtiene una mejora importante
# empleando una función cuadratica o cubica en comparacion al modelo lineal.
# Para polinomios de grado > 3 la mejora es mínima.
cv_MSE_k10 <- rep(NA,10)

for (i in 1:10) {
  modelo <- glm(mpg ~ poly(horsepower, i), data = Auto)
  set.seed(17)
  cv_MSE_k10[i] <- cv.glm(data = Auto, glmfit = modelo, K = 10)$delta[1]
}
ggplot(data = data.frame(polinomio = 1:10, cv_MSE = cv_MSE_k10),
       aes(x = polinomio, y = cv_MSE)) +
geom_point(colour = c("firebrick3")) +
geom_path() +
scale_x_continuous(breaks = c(0:10)) +
theme_bw() +
labs(title  =  'Test Error ~ Grado del polinomio') +
theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

