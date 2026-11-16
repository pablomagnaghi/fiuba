# Del mismo modo que se realiza best subset selection, con la función regsubsets()
# se puede realizar stepwise selection en ambos sentidos (forward o backward)
# indicandolo en el argumento  method.

# Backward
require(leaps)
mejores_modelos_backward <- regsubsets(Salary~., data = datos, nvmax = 19,
                                       method = "backward")
# Se identifica el valor maximo de R ajustado
which.max(summary(mejores_modelos_backward)$adjr2)

coef(object = mejores_modelos_backward, 11)

# Forward

mejores_modelos_forward <- regsubsets(Salary~., data = datos, nvmax = 19,
                                      method = "forward")
# Se identifica el valor maximo de R ajustado
which.max(summary(mejores_modelos_forward)$adjr2)

coef(object = mejores_modelos_forward, 11)

# Ambos metodos (backward y forward) identifican como mejor modelo el formado
# por los mismos 11 predictores.
