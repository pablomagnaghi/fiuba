##############################################################################
# Boosting en Boston
##############################################################################

# GBM = GRADIENT BOOSTED MACHINES
# install.packages("gbm")
library(gbm)
library(MASS)

str(Boston)

set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)




# EL argumento "distribution" es Gaussain, porque la funcion de perdida es la suma de los cuadrados del error
# por ser una estimacion de una variable cuantitativa (regresion)
# interaction.depth limita la profunidad (niveles de Particiones/ramificaciones)
# de cada arbol.

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,
                 interaction.depth=4)

# la funcion summary nos da el ranking de variables y las grafica
summary(boost.boston)

str(boost.boston)

# predicciones en el set de validacion
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-Boston[-train,"medv"])^2)

# El MSE de prueba obtenido es bueno; similar a la MSE de validacion para RF
# y mejor  al de bagging. Si queremos, podemos correr el alg de boosting
# con un valor diferente del parametro de regularizacion lambda . El valor por defecto
# es 0.001. Probemos con lambda = 0.2.

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)

mean((yhat.boost-boston.test)^2)


# Vamos a probar con diferente cantida de arboles
cant.arb =seq(100, 10000, by=100 )

boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=10000,interaction.depth=4,shrinkage=0.01,verbose=F)


# la funcion predict de gbm permite probar con distinta cantidad de
# arboles, por lo que no hay que armar el loop for

matriz.pred = predict(boost.boston,newdata = Boston[-train,],n.trees = cant.arb)


cme = apply((matriz.pred-Boston[-train,"medv"])^2,2,FUN=mean)

matplot(cant.arb,cme,  pch=19, col="red")



##############################################################################
# Boosting en churn
##############################################################################

# GBM = GRADIENT BOOSTED MACHINES
# install.packages("gbm")
library(gbm)

ds = read.csv("/Users/pmagnaghi/Desktop/Projects/r-egide/EGIDE/Churn_Modelling.csv")

# quito campos que no uso factorizo la variabnle objetivo
ds = ds[-c(1:3)]
str(ds)


set.seed(1)
train = sample(nrow(ds), 0.8*nrow(ds))


boost=gbm(Exited~.,data=ds[train,],distribution="adaboost",n.trees=20000)
