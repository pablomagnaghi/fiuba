#  NAIVE BAYES

install.packages("e1071")
library(e1071)
setwd("C:/Users/pablomagnaghi/Desktop/Proyectos/r-egide/EGIDE")

ds <- read.csv2("adult.csv")
str(ds)



ds <- na.omit(ds)

train <- sample(nrow(ds), 0.8*nrow(ds))

# armamos los sets entr/prueba

ds_train <- ds[train,]
ds_test <- ds[-train,]


# Entrenamos el modelo

mod_bayes <- naiveBayes(ing~ ., data=ds_train)

str(mod_bayes)


# Para evaluar el clasificador NB, debemos probar sus predicciones en los datos de prueba

test_pred <- predict(mod_bayes, ds_test[-ncol(ds_test)], type="raw")
