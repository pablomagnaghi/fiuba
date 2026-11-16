# De nuevo, se pretende crear un modelo que permita predecir el salario de los
# jugadores empleando las variables disponibles en el dataset Hitters.

# Para realizar ridge regression se va a emplear la función glmnet() del paquete glmnet.
# Esta funcion se caracteriza por no recibir como argumento una funcion ~ sino una
# matriz modelo x que contiene el valor de los predictores para cada observacion y
# un vector y que contiene la variable respuesta. La función model.matrix() permite
# crear de forma rápida una matriz modelo a partir de un data.frame, identificando
# los predictores y generando las variables dummy necesarias en caso de que los
# predictores sean cualitativos.

# El objeto devuelto por la función glmnet() contiene toda la información relevante
# del o de los modelos ajustados. Además, el paquete incorpora funciones que
# permiten extraer dicha información de forma eficiente: plot(), print(), coef() y predict().
datos <- na.omit(Hitters)
x <- model.matrix(Salary~., data = datos)[, -1]
head(x)

y <- datos$Salary

# El resultado de un ajuste por ridge regression depende del tunning parameter λ que
# determina el grado de penalizacion. Mediante el argumento lambda se puede especificar
# un valor concreto obteniendo un unico modelo. Si no se tiene conocimiento previo
# de que valor de λ es el adecuado, se puede abarcar el rango 1010 a 10−2, que va
# desde un modelo muy estricto que no contiene ningún predictor, hasta uno sin
# penalizacion equivalente al ajuste por minimos cuadrados.

# La funcion glmnet() estandariza por defecto las variables antes de realizar el ajuste del modelo.
library(glmnet)
# Para obtener un ajuste mediante ridge regression se indica argumento alpha=0.
modelos_ridge <- glmnet(x = x, y = y, alpha = 0)

# glmnet() almacena en una matriz el valor de los coeficientes de regresion de los
# predictores para cada valor de λ. Esto permite acceder mediante la función coef()
# a los coeficientes resultantes para un determinado valor de λ (que haya sido
# incluido en el rango cuando se han generado los modelos) y también para graficar
# la evolucion de los coeficientes a medida que se incrementa λ.

plot(modelos_ridge, xvar = "lambda", label = TRUE)

# Como es de esperar, los coeficientes se van haciendo mas pequeños a medida que
# se incrementa el valor de λ.

# Con el fin de identificar el valor de λ que da lugar al mejor modelo, se puede
# recurrir a Cross-Validation. La función cv.glmnet() calcula el cv-test-error,
# utilizando por defecto k=10.
set.seed(1)
# x e y son la matriz modelo y el vector respuesta creados anteriormente con
# los datos de Hitters
cv_error_ridge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10,
                            type.measure = "mse")
plot(cv_error_ridge)

# El grafico muestra el cv-test-error (Mean Square Error) para cada valor de λ
# junto con la barra de error correspondiente. Entre la informacion almacenada
# en el objeto devuelto por la funcion  cv.glmnet() se encuentra el valor de λ
# con el que se consigue el menor cv-test error y el valor de λ con el que se
# consigue el modelo mas sencillo que se aleja menos de 1 desviacion estandar
# del minimo cv-test-error posible.

# Valor lambda con el que se consigue el mínimo test-error
cv_error_ridge$lambda.min

# Valor lambda optimo: mayor valor de lambda con el que el test-error no se
# aleja mas de 1 sd del minimo test-error posible.
cv_error_ridge$lambda.1se

# Acorde al principio de parsimonia y la norma de one standard error rule,
# el mejor modelo es el que se obtiene con λ = 3535.367. lambda.1se
# siempre es mayor que lambda.min.

# Se muestra el valor de los coeficientes para el valor de lambda optimo
modelo_final_ridge <- glmnet(x = x, y = y, alpha = 0, lambda = 3535.367)
coef(modelo_final_ridge)

