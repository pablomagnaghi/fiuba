# La siguiente imagen muestra una comparación entre la distancia euclídea (segmento azul) y la
# distancia de manhattan (segmento rojo y verde) en un espacio bidimensional. Existen múltiples
# caminos para unir dos puntos con el mismo valor de distancia de manhattan, ya que su valor es
# igual al desplazamiento total en cada una de las dimensiones.

datos <- data.frame(observacion = c("a", "b"), x = c(2,7), y = c(2,7))
manhattan <- data.frame(
              x = rep(2:6, each = 2),
              y = rep(2:6, each = 2) + rep(c(0,1), 5),
              xend = rep(2:6, each = 2) + rep(c(0,1), 5),
              yend = rep(3:7, each = 2))

manhattan_2 <- data.frame(
                x = c(2, 5, 5, 7),
                y = c(2, 2, 4, 4),
                xend = c(5, 5, 7, 7),
                yend = c(2, 4, 4, 7))

ggplot(data = datos, aes(x = x, y = y)) +
geom_segment(aes(x = 2, y = 2, xend = 7, yend = 7), color = "blue", size = 1.2) +
geom_segment(data = manhattan, aes(x = x, y = y, xend = xend, yend = yend),
             color = "red", size = 1.2) +
geom_segment(data = manhattan_2, aes(x = x, y = y, xend = xend, yend = yend),
             color = "green3", size = 1.2) +
geom_point(size = 3) +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = 2),
      panel.background = element_rect(fill = "gray",
                                      colour = "white",
                                      size = 0.5, linetype = "solid"))

