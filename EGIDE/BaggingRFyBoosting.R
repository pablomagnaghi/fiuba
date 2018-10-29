# Bagging and Random Forests
install.packages("randomForest")
library(randomForest)
library(MASS)

str(Boston)
?Boston

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# recordemos que bagging es un caso especial de RF en el que m = p.
# Por consiguiente la función randomForest() se puede usar tanto para generar modelos 
# de bagging como de RF


bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

# El argumento mtry = 13 indica que los 13 predictores deben considerarse
# para cada partición del árbol, en otras palabras, o sea: bagging 
# ¿Funciona bien el modelo de bagging en el set de validación?


yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, Boston[-train,"medv"], xlab="Predicciones", ylab="Observados")
abline(0,1)
mean((yhat.bag - Boston[-train,"medv"])^2)

# Podriamos cambiar
# el número de árboles generados por randomForest () usando el argumento ntree:


#  Voy a probar con menos cantidad de árboles 
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=40)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])

mean((yhat.bag-Boston[-train,"medv"])^2)

# Ahora probemos con Random Forest. El único argumento que modificaremos es la cant de predictores
# a utilizar para entrenar cada árbol:  mtry. Por defecto, randomForest ()
# usa  p/3 variables al construir un RF de árboles de regresión, y
# raizcuadrada(p) al construir un RF de árboles de clasificación. Nosotros usaremos
# mtry = 6.

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)

# Usando la función importance(), podemos ver la importancia de cada predictor

yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-Boston[-train,"medv"])^2)
importance(rf.boston)


# Se utilizan dos medidas de performance por variable predictora. La primera , %IncMSE
# es el promedio de la disminución de la exactitud (accuracy) en las predicciones 
# cuando la  variable en cuestión se excluye del modelo. La segunda mide 
# la disminución total en la impureza del nodo que resulta de las particiones realizadas 
# sobre esa variable, promediada en todos los árboles.
# En el caso de árboles de regresión, la impureza del nodo se mide por el SCR, y 
# para árboles de clasificación por la desviación. 

# ordenada por %IncMSE
importance(rf.boston)[rev(order(importance(rf.boston)[,1])),]

# ordenada por IncNodePurity
importance(rf.boston)[rev(order(importance(rf.boston)[,2])),]

# Lo grafico
varImpPlot(rf.boston)


##############################################################################
# Boosting
##############################################################################

# GBM = GRADIENT BOOSTED MACHINES
install.packages("gbm")
library(gbm)


set.seed(1)

# EL argumento "dsitribution" es Gaussain, porque la función de pérdida es la suma de los cuadrados del error
# por ser una estimación de una variable cuantitativa (regresión)
# interaction.depth limita la profunidad (niveles de Particiones/ramificaciones) 
# de cada árbol.

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,
                 interaction.depth=4)

# la función summary nos dá el ranking de variables y las grafica
summary(boost.boston)

str(boost.boston)

# predicciones en el set de validación
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-Boston[-train,"medv"])^2)

# El MSE de prueba obtenido es bueno; similar a la MSE de validación para RF
# y mejor  al de bagging. Si queremos, podemos correr el alg de boosting
# con un valor diferente del parámetro de regularización lambda . El valor por defecto
# es 0.001. Probemos con lambda = 0.2.

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)


mean((yhat.boost-Boston[-train,"medv"])^2)
# mean((yhat.boost-boston.test)^2)


