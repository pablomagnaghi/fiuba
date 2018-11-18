# La siguiente imagen muestra el perfil de dos observaciones definidas por 10 variables
# (espacio con 10 dimensiones).

library(ggplot2)
observacion_a <- c(4, 4.5, 4, 7.5, 7, 6, 5, 5.5, 5, 6)
observacion_b <- c(4, 4.5, 4, 7.5, 7, 6, 5, 5.5, 5, 6) + 4
datos <- data.frame(observacion = rep(c("a", "b"), each = 10),
                    valor = c(observacion_a, observacion_b),
                    predictor = 1:10)
ggplot(data = datos, aes(x = as.factor(predictor), y = valor,
                         colour = observacion)) +
  geom_path(aes(group = observacion)) +
  geom_point() +
  geom_line(aes(group = predictor), colour = "firebrick", linetype = "dashed") +
  labs(x = "predictor") +
  theme_bw() +
  theme(legend.position = "none")

# La distancia euclídea entre las dos observaciones equivale a la raíz cuadrada de la suma
# de las longitudes de los segmentos rojos que unen cada par de puntos. Tiene en cuenta por
# lo tanto el desplazamiento individual de cada una de las variables. 
