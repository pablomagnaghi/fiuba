# El set de datos khan contiene información sobre 83 muestras pertenecientes a 4 tipos
# distintos de tumores. Para cada una de las muestras se dispone del perfil de expresión
# de 2308 genes. Se pretende crear un clasificador multiclase basado en SVMs que permita
# predecir el tipo de tumor en función de la expresión de los genes. El set de datos
# está dividido en training set (xtrain, ytrain) y test set (xtest, ytest), 63 observaciones
# destinadas a entrenamiento y 20 a la evaluación del modelo.

library(ISLR)
data("Khan")
names(Khan)

dim(Khan$xtrain)

dim(Khan$xtest)

length(Khan$ytrain)

length(Khan$ytest)

# En este tipo de escenario, en el que el número de predictores es varios órdenes de magnitud
# mayor que el de observaciones, los modelos son proclives a sufrir overfitting. Esto sugiere
# que, de entre los diferentes tipos de kernels, sea adecuado emplear el de menor flexibilidad,
# el kernel lineal. El único hiperparámetro de un SVM con kernel lineal es el valor de penalizacion C.

library(e1071)

# Como la variable respuesta está separa de los predictores, se unen en un
# único dataframe. La variable respuesta tiene que ser de tipo factor.
datos_train <- data.frame(y = as.factor(Khan$ytrain), Khan$xtrain)

svm_cv <- tune("svm", y ~ ., data = datos_train, kernel = "linear", ranges = list(cost = c(1e-04,
    5e-04, 0.001, 0.01, 0.1, 1)))

ggplot(data = svm_cv$performances, aes(x = cost, y = error)) + geom_line() +
    geom_point() + labs(title = "Error de clasificación vs hiperparámetro C") +
    theme_bw()

svm_cv$best.parameters

modelo_svm <- svm_cv$best.model

# Aciertos del modelo con los datos de entrenamiento
paste("Error de entrenamiento:", 100 * mean(datos_train$y != modelo_svm$fitted),
    "%")

table(prediccion = modelo_svm$fitted, clase_real = datos_train$y)

# El modelo obtenido tiene un training error del 0%, es capaz de clasificar correctamente
# todas las observaciones empleadas para crearlo. Un training error muy bajo puede ser
# un indicativo de overfitting, lo que haría que el modelo no fuese capaz de predecir
# correctamente nuevas observaciones. Para evaluar si es este el caso, se emplea el
# modelo para predecir las 20 observaciones del test set.

datos_test <- data.frame(y = as.factor(Khan$ytest), Khan$xtest)
predicciones <- predict(object = modelo_svm, newdata = datos_test)

paste("Error de test:", 100 * mean(datos_test$y != predicciones), "%")


table(prediccion = predicciones, clase_real = datos_test$y)

# De las 20 observaciones de test, el modelo SVM con kernel = "linear" y cost = 10 predice
# correctamente 18 y falla en 2, su test error es solo del 10%. 

