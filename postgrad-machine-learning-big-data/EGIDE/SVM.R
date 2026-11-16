#############################################
# mAquina de vectores de soporte (SVM)
#############################################

# dataset genes genes
library(e1071)

ds=read.csv("/Users/pmagnaghi/Desktop/Projects/r-egide/EGIDE/genes.csv")

# como factor
ds$y=as.factor(ds$y)

str(ds)

plot(ds$X1,ds$X2,col=as.numeric(ds$y)+1, pch=19)

svmfit=svm(ds$y~., data=ds, scale= FALSE, kernel="radial", cost=5)

# caracteristicvas basicvas del objeto svm
print(svmfit)

plot(svmfit, ds)

svmfit$index
summary(svmfit)


# hacemos una grilla de tamaño n
n=80
X1 = ds$X1
X2 = ds$X2
gridx1 = seq(min(X1),max(X1),(max(X1)-min(X1))/n)
gridx2 = seq(min(X2),max(X2),(max(X2)-min(X2))/n)

?expand.grid

grid=expand.grid("X1"=gridx1,"X2"=gridx2)


# predicciones en la grilla

pred.grilla = predict(svmfit,grid)

plot(grid, col=c("red","blue")[factor(pred.grilla)],pch=20,cex=0.2)
points(X1,X2,col=c("red","blue")[as.factor(ds$y)],pch=19)



func=predict(svmfit,grid, decision.values = TRUE)
func.dec = attributes(func)$decision


contour(gridx1, gridx2, matrix(func.dec, length(gridx1),length(gridx2)), level=0, add=TRUE)

contour(gridx1, gridx2, matrix(func.dec, length(gridx1),length(gridx2)), level=0.5,
        add=TRUE, col="blue")

contour(gridx1, gridx2, matrix(func.dec, length(gridx1),length(gridx2)), level=-0.5,
        add=TRUE, col="red")

contour(gridx1, gridx2, matrix(func.dec, length(gridx1),length(gridx2)), level=1,
        add=TRUE, col="red")

contour(gridx1, gridx2, matrix(func.dec, length(gridx1),length(gridx2)), level=-1,
        add=TRUE, col="red")

# AHORA en modo probability

svmfit=svm(ds$y~., data=ds, scale= FALSE, kernel="radial", cost=5, probability=TRUE)

func=predict(svmfit,grid, decision.values = TRUE, probability = TRUE)
func.prob = as.vector(attributes(func)$probabilities[,2])
str(func.prob)

# predicciones en la grilla

pred.grilla = predict(svmfit,grid)
y=ds$y

plot(grid, col=c("red","blue")[pred.grilla],pch=20,cex=0.2)
points(X1,X2,col=c("red","blue")[as.factor(y)],pch=19)

# el contorno
contour(gridx1, gridx2, matrix(func.prob, length(gridx1),length(gridx2)), level=0.5,
        add=TRUE, col="red")

contour(gridx1, gridx2, matrix(func.prob, length(gridx1),length(gridx2)), level=0.2,
        add=TRUE, col="red")


########################
# en churn
########################

library(e1071)
ds = read.csv("/Users/pmagnaghi/Desktop/Projects/r-egide/EGIDE/Churn_Modelling.csv")

# quito campos que no uso y factorizo la variable objetivo
ds = ds[-c(1:3)]
ds$Exited = factor(ds$Exited, levels = c(0, 1))
str(ds)


set.seed(1)
train = sample(nrow(ds), 0.8*nrow(ds))

svm.churn=svm(Exited~., data=ds[train,], scale= FALSE, kernel="radial",probability=TRUE,
              cost=100, gamma=1)

# da muy mal la cant de vectores (igual a l training)

svm.churn=svm(Exited~. ,
              data=ds[train,], scale= TRUE, kernel="linear",probability=TRUE,
              cost=100, gamma=1e-5)
svm.churn

svm.churn=svm(Exited~. ,
              data=ds[train,], scale= TRUE, kernel="sigmoid",probability=TRUE,
              cost=10, gamma=2)
svm.churn


pred.churn=predict(svm.churn,ds[-train,-11], probability = TRUE)
pred.prob = as.vector(attributes(pred.churn)$probabilities[,2])


pred.churn.prob = as.vector(attributes(pred.churn)$probabilities[,2])

# anda mal !!!!!!!!!!!!!!!!!!!

########################################
#  con glass
#######################################

library(e1071)
ds = read.csv('/Users/pmagnaghi/Desktop/Projects/r-egide/EGIDE/glass.csv')

ds$Type <- as.factor(ds$Type)

# split data into a train and test set

set.seed(1)
train = sample(nrow(ds), 0.7*nrow(ds))
str(ds)

svm.model <- svm(Type ~ ., ds[train,], cost = 10, gamma = 1,
                 kernel="radial",probability=TRUE)

svm.pred <- predict(svm.model, ds[-train,-10], decision.values=TRUE, probability=TRUE)

svm.pred.prob <- attr(svm.pred, "probabilities")


colnames(svm.pred.prob)

svm.pred.class=factor(6, levels = c(1,2,3,5,6,7))



for (i in 1:nrow(svm.pred.prob)){
  col.max=which(svm.pred.prob[i,]==max(svm.pred.prob[i,]))
  svm.pred.class[i] = factor(colnames(svm.pred.prob)[col.max])
}

table(pred = svm.pred.class, true = ds[-train,10])

table(ds[-train,10])

