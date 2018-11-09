# El proceso para realizar un ajuste mediante lasso y la identificacion del mejor
# valor de λ es equivalente al seguido en el caso de ridge regression pero
# indicando en la función glmnet() que  alpha=1.

library(glmnet)
# x e y son la matriz modelo y el vector respuesta creados anteriormente con
# los datos de Hitters
modelos_lasso <- glmnet(x = x, y = y, alpha = 1)
plot(modelos_lasso, xvar = "lambda", label = TRUE)

set.seed(1)
cv_error_lasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10)
plot(cv_error_lasso)

cv_error_lasso$lambda.min

cv_error_lasso$lambda.1se

# Se reajusta el modelo con todas las observaciones empleando el valor de
# lambda óptimo
modelo_final_lasso <- glmnet(x = x, y = y, alpha = 1, lambda = cv_error_lasso$lambda.1se)
coef(modelo_final_lasso)

# Tanto el metodo ridge regression como el de lasso consiguen, empleando sus
# respectivos valores óptimos de λ, reducir el MSE (test error) a unos niveles
# muy parecidos. La ventaja del modelo final obtenido por lasso es que es
# mucho mas simple ya que contiene únicamente 5 predictores.
par(mfrow = c(1,2))
plot(cv_error_ridge,ylab = "Mean Square Error ridge regression" )
abline(h = 120000)
plot(cv_error_lasso,ylab = "Mean Square Error lasso")
abline(h = 120000)

par(mfrow = c(1,1))
