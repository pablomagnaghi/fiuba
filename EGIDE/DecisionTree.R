#  ARBOLES DE DECISION
##########################
library(ISLR)
library(tree)

attach(Carseats)

str(Carseats)

hist(Sales)

# transformo la variable en factor. Si es mayor a 8 digo que son altas las ventas, 
# si no digo que son bajas

High <- ifelse(Sales>8, "Alta", "Baja")
Carseats2 <- data.frame(Carseats, High)

str(Carseats2)
table(High)

# primer árbol con todo el training

ad.Carseats <- tree(High ~ . -Sales, Carseats2)

summary(ad.Carseats)
plot(ad.Carseats)
text(ad.Carseats)


ad.Carseats

# ahora con un set de validación
set.seed(1221)
train <- sample(nrow(Carseats2),0.7*nrow(Carseats2))


# no hay que indicarle el dataset de training, sólo el subset de indices "train" para entrenar

ad.Carseats <- tree(High ~ . -Sales, Carseats2, subset=train)

summary(ad.Carseats)
ad.Carseats
plot(ad.Carseats)
text(ad.Carseats)

# set de validación
Car_test <- Carseats2[-train,]


#  Predicciones
ad.predict <- predict(ad.Carseats,Car_test, type="class")

ad.predict.raw <- predict(ad.Carseats,Car_test, type="vector")

mc = table(ad.prune.predict,Car_test$High)
sum(diag(mc))/nrow(Car_test)


# vamos a hacer prunning con CV en el set de entremamiento
cv.carseats <- cv.tree(ad.Carseats, FUN=prune.misclass)

# características
cv.carseats

# ploteo
plot(cv.carseats) 

# veo que el mejor tamaño del árbol se da en 10-13. 
# Entonces voy a elegir el valor 13

# corro el árbol con este tamaño ideal=11 en toda el set de entrenamiento completo

prune.carseats <- prune.misclass(ad.Carseats, best = 13)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# y ahora predigo con este árbol podado

ad.prune.predict <- predict(prune.carseats,Car_test, type="class")

mc = table(ad.prune.predict,Car_test$High)

sum(diag(mc))/sum(mc)


#######################
# Churn Modelling #
#######################

# Impo
ds <- read.csv("C:/Users/pablomagnaghi/Desktop/Proyectos/r-egide/EGIDE/Churn_Modelling.csv")
str(ds)
               
ds = ds[-c(1:3)]

# factorizo
ds$Exited = factor(ds$Exited, levels = c(0,1))
               
#  validacion
train <- sample(nrow(ds), 0.8*nrow(ds))

ds.tree <- tree(Exited ~ .,ds, subset=train)

summary(ds.tree)
plot(ds.tree)
text(ds.tree, pretty = 0)
ds.tree


# set de prueba
ds_test <- ds[-train,]


ad.predict <- predict(ds.tree,ds_test, type="class")
ad.predict.raw <- predict(ds.tree,ds_test, type="vector")

