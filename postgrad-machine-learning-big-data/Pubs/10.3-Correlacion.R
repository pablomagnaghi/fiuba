# En la siguiente imagen se muestra el perfil de 3 observaciones. Acorde a la distancia
# euclídea, las observaciones b y c son las más similares, mientras que acorde a la
# correlación de Pearson, las observaciones más similares son a y b.

library(ggplot2)
observacion_a <- c(4, 4.5, 4, 7.5, 7, 6, 5, 5.5, 5, 6)
observacion_b <- c(4, 4.5, 4, 7.5, 7, 6, 5, 5.5, 5, 6) + 4
observacion_c <- c(5, 5.5, 4.8, 5.4, 4.7, 5.6, 5.3, 5.5, 5.2, 4.8)

datos <- data.frame(observacion = rep(c("a", "b", "c"), each = 10),
                    valor = c(observacion_a, observacion_b, observacion_c),
                    predictor = 1:10)

ggplot(data = datos, aes(x = as.factor(predictor), y = valor,
                         colour = observacion)) +
  geom_path(aes(group = observacion)) +
  geom_point() +
  labs(x = "predictor") +
  theme_bw() +
  theme(legend.position = "bottom")

dist(x = rbind(observacion_a, observacion_b, observacion_c), method = "euclidean")

1 - cor(x = cbind(observacion_a, observacion_b, observacion_c), method = "pearson")

# Este ejemplo pone de manifiesto que no existe una única medida de distancia que sea mejor
# que las demás, sino que, dependiendo del contexto, una será más adecuada que otra.

