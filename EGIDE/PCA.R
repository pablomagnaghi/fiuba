##########################################
# PCA
##########################################

# importo el dataset
ds = read.csv("/Users/pmagnaghi/Desktop/Projects/r-egide/EGIDE/Wine.csv")

# train y test

set.seed(123)
train =sample(nrow(ds),0.8*nrow(ds))
training_set <- ds[train,]
test_set <- ds[-train,]

# estandarizo de las varaibles
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])


# PCA
library(e1071)

# pasa de tener 13 variables a tener 2 que  representen la mayor cantidad de la variable total
pca = prcomp(x = training_set[-14], rank=2)

#  veo los coeficientes de cada PC
pca

# ahora el train expresado en PC
train_set_pca = predict(pca, training_set)

# le agrego la variable respuesta "segmento" al nuevo train set
train_set_pca = cbind(train_set_pca,Customer_Segment=training_set$Customer_Segment)
colnames(train_set_pca)

# el test expresado en PC y le agrego la variable respuesta "segmento" al nuevo test set
test_set_pca = cbind(predict(pca, test_set),Customer_Segment=test_set$Customer_Segment)
colnames(test_set_pca)

# AHORA VOY A APLICAR un algoritmo de clasificaci�n con los datasets con PC
#  -----> SVM

mod_svm = svm(formula = Customer_Segment ~ .,
                 data = train_set_pca,
                 type = 'C-classification',
                 kernel = 'linear')

# las predicciones
y_pred = predict(mod_svm, newdata = test_set_pca[,-3])

# matriz de confusion
cm = table(real=test_set_pca[,3], pred=y_pred)



# visualizacion de SVM
# grilla PARA GRAFICAR

# aplico PCA a todo el dataset
# estandarizo
ds_scale=ds
ds_scale[-14] = scale(ds[-14])

ds_pca = cbind(predict(pca, ds_scale),Customer_Segment=ds_scale$Customer_Segment)
colnames(test_set_pca)

# hacemos una grilla de tama�o n
set = data.frame(ds_pca)
n=100
gridx1 = seq(min(set$PC1),max(set$PC1),(max(set$PC1)-min(set$PC1))/n)
gridx2 = seq(min(set$PC2),max(set$PC2),(max(set$PC2)-min(set$PC2))/n)

grid=expand.grid(PC1=gridx1,PC2=gridx2)



pred.grilla = predict(mod_svm,grid)
plot(grid, col=c("red","blue","green")[factor(pred.grilla)],pch=20,cex=0.2)

# los datos reales del test
points(test_set_pca[,1],test_set_pca[,2],
       col=c("red","blue","green")[as.factor(test_set_pca[,3])],pch=19)

