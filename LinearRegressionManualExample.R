# Cuadrados minimos

y <- c(1, 2, 3, 5)
x <- c(0.8, 1, 1.2, 1.3)

plot(x, y)

sum_y <- sum(y)
sum_y
sum_x <- sum(x)
sum_x

sum_y_2 <- sum(y^2)
sum_y_2
sum_x_2 <- sum(x^2)
sum_x_2
sum_x_y <- sum(x*y)
sum_x_y

pred <- function (x) sum(x)/length(x)

pred_y <- pred(y)
pred_y
pred_x <- pred(x)
pred_x

mean_x <- mean(x)
mean_x

# varianza poblacion
variance <- function (x)   sum((x-mean(x))^2)/(length(x)-1)
var_y <- variance(y)
var_x <- variance(x)
var_x_y <- ((sum_x_y/4) - (pred_x * pred_y))
var_x_y
var_x

b <- var_x_y / var_x
a <- pred_y - b * pred_x

# Y = -5.0847 + 7.2881 * X

reg_lin <- lm(y ~ x)
reg_lin
summary(reg_lin)

# varianza muestral n -1 en vez de n
var_muestral <- var(x)

varianzaMuestral = var(x)
varianzaMuestral
desvTipicaMuestral = sd(x)

n <- length(x)
varianzaPoblacional=((n-1)/n)*varianzaMuestral
varianzaPoblacional
desvTipicaPoblacional=sqrt(varianzaPoblacional)
desvTipicaPoblacional
