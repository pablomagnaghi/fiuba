# Outlier u observacion atipica: Observaciones que no se ajustan bien al
# modelo. El valor real de la variable respuesta se aleja mucho del valor
# predicho, por lo que su residuo es excesivamente grande. En una representacion
# bidimensional se corresponde con desviaciones en el eje Y.

set.seed(123)
datos <- data.frame(x = 1:10, y = 1:10 + rnorm(10))
modelo_1 <- lm(y ~ x, data = datos)

observacion <- c(5.5, 12)
modelo_2 <- lm(y ~ x, data = rbind(datos, observacion))
plot(y ~ x, data = rbind(datos, observacion), pch = 19)
points(5.5, 12, pch = 4, cex = 3, col = "red")
legend(x = "topleft", legend = c("con outlier", "sin outlier"), lty = c(2, 1))
abline(modelo_1)
abline(modelo_2, lty = 2)


# Observacion con alto leverage: Observacion con un valor extremo para alguno
# de los predictores. En una representacion bidimensional se corresponde con
# desviaciones en el eje X. Son potencialmente puntos influyentes.
observacion <- c(15, 15.1)
modelo_2 <- lm(y ~ x, data = rbind(datos, observacion))
plot(y ~ x, data = rbind(datos, observacion), pch = 19)
points(15, 15.1, pch = 4, cex = 3, col = "red")
legend(x = "topleft", legend = c("con obs. leverage", "sin obs. leverage"),
       lty = c(2, 1))
abline(modelo_1)
abline(modelo_2, lty = 2)

# Observacion influyente: Observacion que influye sustancialmente en el
# modelo, su exclusion afecta al ajuste. No todos los outliers u
# observaciones con alto leverage tienen por qué ser influyentes
observacion <- c(15, 5.1)
modelo_2 <- lm(y ~ x, data = rbind(datos, observacion))
plot(y ~ x, data = rbind(datos, observacion), pch = 19)
points(15, 5.1, pch = 4, cex = 3, col = "red")
legend(x = "topleft", legend = c("con obs. influyente","sin obs. influyente"),
       lty = c(2, 1), cex = 0.9)
abline(modelo_1)
abline(modelo_2, lty = 2)

