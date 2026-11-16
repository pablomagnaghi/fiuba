#############################################################################################################
# K-Nearest Neighbors
#############################################################################################################

library(class)
library(ISLR)

ds = read.csv('C:/Users/pablomagnaghi/Desktop/Proyectos/r-egide/EGIDE/Churn_Modelling.csv')

# saco los campos innecesarios: RowNumber, CustomerId, Surname
ds <- ds[-c(1:3)]

ds$Exited <- as.factor(ds$Exited)

str(ds)

set.seed(123)

set.seed(5656)
train <- sample(10000, 8000)

# convierto a numeric

ds$CreditScore <- as.numeric(ds$CreditScore)
ds$Age <- as.numeric(ds$Age)
ds$Tenure <- as.numeric(ds$Tenure)
ds$NumOfProducts <- as.numeric(ds$NumOfProducts )                          
ds$HasCrCard  <- as.numeric(ds$HasCrCard)                           
ds$IsActiveMember <- as.numeric(ds$IsActiveMember )                           

str(ds)

knn.pred=knn(ds[train,-c(2,3,11)],ds[-train,-c(2,3,11)],ds$Exited[train],k=3)

#############################################################################################################
# LR  Churn Modelling 
#############################################################################################################

# Impo
dataset_reg = read.csv('C:/Users/pablomagnaghi/Desktop/Proyectos/r-egide/EGIDE/Churn_Modelling.csv')
dataset_reg = dataset_reg[-c(1:3)]

# factorizo
dataset_reg$Exited = factor(dataset_reg$Exited, levels = c(0, 1))


# validac
str(dataset_reg)
set.seed(5656)
train <- sample(10000, 8000)

training_set = dataset_reg[train,]
test_set = dataset_reg[-train,]

# modelo
classifier = glm(formula = Exited ~ .,
                 family = binomial,
                 data = training_set)
coef(classifier)

# Pred
prob_pred = predict(classifier, type = 'response', newdata = test_set[-11])
y_pred = ifelse(prob_pred > 0.5, 1, 0)



#############################################################################################################
# Cross Validation
#############################################################################################################

# CV
library(ISLR)
library(boot)
?cv.glm

attach(Auto)
plot(mpg,horsepower)

# 10 CV
modelo <- glm(mpg ~ poly(horsepower, 4),data=Auto,)
glm.fit

cv.error10 <- rep(0,5)
for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower, i),data=Auto,)
  cv.error10[i] <- cv.glm(Auto, glm.fit,K=10)$delta[1]
}

cv.error10

cv <- cv.glm(Auto, glm.fit,K=10)
str(cv)
error <- cv$delta[1]

# ploteo la evolución
plot(1:5,cv.error10,xlab="grado", ylab="error", type = "b", col="blue" )
