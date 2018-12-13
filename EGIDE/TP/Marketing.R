#######################################################################################################

######################################## Paquetes y librerias ######################################### 

#######################################################################################################

###### Funciones
installPackagesIfItIsNeeded <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dep=TRUE);
  }
}

###### Paquetes
installPackagesIfItIsNeeded("magrittr")
installPackagesIfItIsNeeded("pander")
installPackagesIfItIsNeeded("dplyr")
installPackagesIfItIsNeeded("ggplot2")
installPackagesIfItIsNeeded("cowplot")
installPackagesIfItIsNeeded("corrplot")
installPackagesIfItIsNeeded("VIM")
installPackagesIfItIsNeeded("mice")
installPackagesIfItIsNeeded("gdata")
installPackagesIfItIsNeeded("C50")
installPackagesIfItIsNeeded("caret")
installPackagesIfItIsNeeded("gmodels")
installPackagesIfItIsNeeded("ipred")
installPackagesIfItIsNeeded("randomForest")
installPackagesIfItIsNeeded("class")
installPackagesIfItIsNeeded("DMwR")
installPackagesIfItIsNeeded("ROCR")

###### Librerias
library(magrittr)
library(pander)
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(VIM)
library(mice)
library(gdata)
library(C50)
library(caret)
library(gmodels)
library(ipred)
library(randomForest)
library(class)
library(DMwR)
library(ROCR)
library(MASS)
#library(data.table) #for loading data
#library(caret) #for modelling
#library(vcd) #for visualizing categorical data
#library(GoodmanKruskal) #for association analysis of categorical variables
#library(DMwR) #for data mining
#library(ROCR) #for visualizing performance measures
#library(pROC) #to analyze roc curves
#library(xgboost) #to model xgboost algorithm
#library(e1071) #to model svm


#######################################################################################################

################################### Preparar el entorno de trabajo #################################### 

#######################################################################################################

# Limpiar el global environment
rm(list = ls())

# Obtener datos
input_file <- "./data/mktg-bank.csv" 

rawdata <- read.csv(input_file, sep=';')

mktg_bank_data <- rawdata 

#######################################################################################################

####################################### Estructura del dataset ####################################### 

#######################################################################################################

dim(mktg_bank_data)
str(mktg_bank_data)
names(mktg_bank_data)
# la muestra esta compuesta de 35010 observaciones (clientes) caracterizadas 
# por 21 atributos cada una (20 de entrada y 1 de salida).
summary(mktg_bank_data)
#######################################################################################################

########################################## Limpieza de datos ########################################## 

#######################################################################################################

# Eliminar la variable "duration" para obtener un modelo predictivo realista
mktg_bank_data$duration <- NULL

# Recuento de NAs por variable
sapply(X = mktg_bank_data, FUN = function(x) sum(length(which(is.na(x)))))

# Recuento de "unknown" por variable
sapply(X = mktg_bank_data, FUN = function(x) sum(x == "unknown"))

# Convertir los valores "unknown" en NAs
mktg_bank_data.na <- mktg_bank_data
mktg_bank_data.na[mktg_bank_data.na == "unknown"] = NA

# Distribución de missing values
mv_plot <- aggr(mktg_bank_data.na, col=c('lightblue','darkorange'), 
                numbers=TRUE, 
                prop=FALSE,
                sortVars=TRUE, 
                labels=names(mktg_bank_data.na), 
                cex.axis=0.7, 
                gap=1,
                varheight = FALSE,
                combined = FALSE,
                cex.numbers =0.5, 
                ylab=c("Histogram of missing data","Pattern"))

# Una vez hecho esto, vamos a imputar los missing values de cada atributo dandoles como 
# valor alguno de los posibles niveles de tal categoría. 

# Imputar los datos; 5 posibles imputaciones por NA y 5 iteraciones
seed.val <- 1492
mktg_bank_data.temp <- mice(mktg_bank_data.na, m = 5, maxit = 5,
                            method = "pmm", seed = seed.val)

# Reemplazar NAs con los valores imputados en el 1º de los 5 dataset (por ejemplo)
mktg_bank_data.imp <- complete(mktg_bank_data.temp, 1)
mktg_bank_data.imp <- drop.levels(mktg_bank_data.imp)

# Borrar cálculos intermedios
rm(mktg_bank_data, mktg_bank_data.na, mktg_bank_data.temp)

# No mas unknown
colSums(is.na(mktg_bank_data.imp))
dim(mktg_bank_data.imp)
str(mktg_bank_data.imp)

# Eliminar la variable "duration" para obtener un modelo predictivo realista
mktg_bank_data$duration <- NULL


#######################################################################################################
bank_data_testing <- mktg_bank_data.imp



dim(bank_data_testing)

set.seed(17)
modelo_randomforest <- randomForest(y ~ ., data = bank_data_testing, mtry = 2, ntree = 300,
                                    importance = TRUE, nodesize = 1,
                                    norm.votes = TRUE )
modelo_randomforest

#Se extrae el último valor OOB
tail(modelo_randomforest$err.rate[, 1], 1)

# identificación de los predictores más influyentes


library(tidyverse)
library(ggpubr)
importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data=importancia_pred, aes(x=reorder(variable, MeanDecreaseAccuracy),
                                        y = MeanDecreaseAccuracy,
                                        fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Reducción de Accuracy") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, MeanDecreaseGini),
                                          y = MeanDecreaseGini,
                                          fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Reducción de pureza (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)

# De entre todos los predictores empleados, RI, Mg, Al y Ca son los más influyentes. 
# De hecho, si se genera un modelo más simple que solo incluya estos predictores, 
# la estimación del error aumenta poco.

set.seed(17)
arbol_randomforest <- randomForest(y ~ RI + Mg + Al + Ca, data = fgl, mtry = 2,
                                   nodesize = 1, ntree = 300, importance = TRUE)
arbol_randomforest
#######################################################################################################


#######################################################################################################

# Outlier Detection and Treatment
logistic_model = glm(y~.,data=mktg_bank_data,family=binomial)
cooksd<-cooks.distance(logistic_model)
bank_dataset = mktg_bank_data[cooksd<4*mean(cooksd),]
str(bank_dataset)

#######################################################################################################

# Estudio de variables numéricas

# Calcular la matriz de correlación de las variables numéricas
matrix.corr <- cor(mktg_bank_data.imp[sapply(mktg_bank_data.imp, is.numeric)], use = "complete.obs")

corrplot(matrix.corr, method="ellipse", tl.col = "black", tl.srt = 45,
         order = "FPC", type = "lower", mar = c(0,0,2,0),
         main = substitute("Correlación ordenada por First Principal Component"))

# Borrar cálculos intermedios
rm(matrix.corr)

# Se observa que el a mayor fuerza de trabajo (“nr.employed”) y por tanto mayor tasa de 
# variación de empleo (“emp.var.rate”), crece el IPC (“cons.price.idx”) y el euribor (“euribor3m”), 
# principal tipo variable de referencia del mercado hipotecario.

# Insight: estos indicadores son inversamente proporcionales al número de contactos telefónicos 
# que hace este banco (“previous”), es decir, te contactan cuando por la situación económica 
# mundial y/o del país necesitan vender productos (en este caso créditos a plazo)

#######################################################################################################

# Train/Test datasets
# Una vez finalizado el pre-procesado de los datos, se generan los dataset de entrenamiento y 
# de test:
# Dividir los datos
split = 0.80
set.seed(seed.val)
ind = createDataPartition(mktg_bank_data.imp[,ncol(mktg_bank_data.imp)],
                          p = split, tim = 1, list = TRUE)

# Train y Test datasets
train.norm = mktg_bank_data.imp[ind$Resample1, ]
test.norm = mktg_bank_data.imp[-ind$Resample1, ]

# Train y Test labels
trainlabels = train.norm$y
testlabels = test.norm$y

# Eliminar datos intermedios
rm(ind, split)

# Generados los dataset de entrenamiento y de test, se comprueba que las 
# proporciones del target son semejantes en ambos dataset:

# Comprobar si el reparto de la target variable ha sido equitativo
train.prop <-round(prop.table(table(train.norm$y)), digits = 6)
test.prop <- round(prop.table(table(test.norm$y)), digits = 6)
rbind(train.prop, test.prop)
#######################################################################################################

############################################ Clasificacion ############################################

#######################################################################################################

# C5.0 por defecto


# Ajustar el modelo
train_label_index = grep(pattern = "^y$", colnames(train.norm))
set.seed(seed.val)
tic <- proc.time()
fit.C50 <- C5.0(x = train.norm[, -train_label_index],
                y = train.norm$y)
time <- proc.time() - tic

# Predecir sobre las muestras de test
test_label_index = grep(pattern = "^y$", colnames(test.norm))
C50.pred <- predict(fit.C50, test.norm[, -test_label_index], type = "class")

CrossTable(x = C50.pred, y = testlabels,
           prop.chisq = FALSE, prop.r = TRUE, prop.c = TRUE, digits = 2,
           dnn = c("PREDICCION", "REAL"))

# Performance metrics...
acc <- sum(C50.pred == testlabels)/length(testlabels)
prec <- posPredValue(C50.pred, testlabels, positive = "yes")
rec <- sensitivity(C50.pred, testlabels, positive = "yes")
f <- (2 * prec * rec) / (prec + rec) # = (2 * TP) / (2 * TP + FP + FN)

results <- data.frame(as.numeric(prec), as.numeric(rec), as.numeric(f),
                      as.numeric(time[3]),
                      "C5.0", "fit.C50",
                      stringsAsFactors = FALSE)
names(results) <- c("precision", "recall", "f-score", "time",
                    "algoritmo", "descripción")

results
#######################################################################################################

# Para mejorar este punto, voy a añadir al algoritmo ya utilizado (C5.0) el método ensemble 
# adaptive boosting…

# C5.0 con adaptive boosting
# Utilizando el mismo algoritmo de árboles de decisión que en el apartado anterior (C5.0), 
# se configura para realizar 10 iteraciones con el fin de mejorar el rendimiento del mismo:

# Ajustar el modelo
set.seed(seed.val)
tic <- proc.time()
fit.C50.boost10 <- C5.0(x = train.norm[, -train_label_index],
                        y = train.norm$y,
                        trials = 10)
time <- proc.time() - tic

# Predecir sobre las muestras de test
C50.boost10.pred <- predict(fit.C50.boost10, test.norm[, -test_label_index],
                            type = "class")

# Confusion Matrix
CrossTable(x = C50.boost10.pred, y = testlabels,
           prop.chisq = FALSE, prop.r = TRUE, prop.c = TRUE, digits = 2,
           dnn = c("PREDICCION", "REAL"))

# Performance metrics...
acc <- sum(C50.boost10.pred == testlabels)/length(testlabels)
prec <- posPredValue(C50.boost10.pred, testlabels, positive = "yes")
rec <- sensitivity(C50.boost10.pred, testlabels, positive = "yes")
f <- (2 * prec * rec) / (prec + rec) # = (2 * TP) / (2 * TP + FP + FN)
results <- rbind(results, c(as.numeric(prec), as.numeric(rec), as.numeric(f),
                            as.numeric(time[3]),
                            "C5.0", "fit.C50.boost10"))

results
#######################################################################################################


# C5.0 con penalización
# En esta nueva aproximación se penalizan los falsos negativos (FN) 5 veces más que que los 
# falsos positivos (FP). ¿Por qué? Porque en este caso es preferible contactar con un potencial 
# comprador que finalmente no lo sea, a que compradores reales no sean contactados.

# Montar la matriz de penalizaciones
matrix.dim <- list(c("no","yes"), c("no","yes"))
names(matrix.dim) <- c("PREDICCION", "REAL")
error.cost <- matrix(c(0,1,5,0), nrow = 2,
                     dimnames = matrix.dim)

# Ajustar el modelo
set.seed(seed.val)
tic <- proc.time()
fit.C50.cost <- C5.0(x = train.norm[, -train_label_index],
                     y = train.norm$y,
                     costs = error.cost)
time <- proc.time() - tic

# Predecir sobre las muestras de test
C50.cost.pred <- predict(fit.C50.cost, test.norm[, -test_label_index],
                         type = "class")

# Confusion Matrix
CrossTable(x = C50.cost.pred, y = testlabels,
           prop.chisq = FALSE, prop.r = TRUE, prop.c = TRUE, digits = 2,
           dnn = c("PREDICCION", "REAL"))

# Performance metrics...
acc <- sum(C50.cost.pred == testlabels)/length(testlabels)
prec <- posPredValue(C50.cost.pred, testlabels, positive = "yes")
rec <- sensitivity(C50.cost.pred, testlabels, positive = "yes")
f <- (2 * prec * rec) / (prec + rec) # = (2 * TP) / (2 * TP + FP + FN)
results <- rbind(results, c(as.numeric(prec), as.numeric(rec), as.numeric(f),
                            as.numeric(time[3]),
                            "C5.0", "fit.C50.cost"))

results
#######################################################################################################

# CART
# CART (Classification and Regression Trees), es el término introducido por Leo Breiman para 
# referirse a los algoritmos de árboles de decisión usados tanto en problemas predictivos de 
# clasificación como de regresión

# Ajustar el modelo
set.seed(seed.val)
tic <- proc.time()
fit.bagging10 <- bagging(y ~.,
                         data = train.norm,
                         nbagg = 10)
time <- proc.time() - tic

# Predecir sobre las muestras de test
bagging10.pred <- predict(fit.bagging10, test.norm[, -test_label_index],
                          type = "class")

# Confusion Matrix
CrossTable(x = bagging10.pred, y = testlabels,
           prop.chisq = FALSE, prop.r = TRUE, prop.c = TRUE, digits = 2,
           dnn = c("PREDICCION", "REAL"))

# Performance metrics...
acc <- sum(bagging10.pred == testlabels)/length(testlabels)
prec <- posPredValue(bagging10.pred, testlabels, positive = "yes")
rec <- sensitivity(bagging10.pred, testlabels, positive = "yes")
f <- (2 * prec * rec) / (prec + rec) # = (2 * TP) / (2 * TP + FP + FN)
results <- rbind(results, c(as.numeric(prec), as.numeric(rec), as.numeric(f),
                            as.numeric(time[3]),
                            "CART", "fit.bagging10"))

results
#######################################################################################################

# Random Forest

#######################################################################################################

# Entrenar el modelo
set.seed(seed.val)
tic <- proc.time()
fit.forest100 <- randomForest(x = train.norm[, -train_label_index],
                              y = train.norm$y,
                              ntree = 100, mtry = 5)
time <- proc.time() - tic

print(fit.forest100$confusion)

# Predecir sobre las muestras de test
forest100.pred = predict(fit.forest100, test.norm[, -test_label_index],
                         type = "class")

# Confusion Matrix
CrossTable(x = forest100.pred, y = testlabels,
           prop.chisq = FALSE, prop.r = TRUE, prop.c = TRUE, digits = 2,
           dnn = c("PREDICCION", "REAL"))

# Performance metrics...
acc <- sum(forest100.pred == testlabels)/length(testlabels)
prec <- posPredValue(forest100.pred, testlabels, positive = "yes")
rec <- sensitivity(forest100.pred, testlabels, positive = "yes")
f <- (2 * prec * rec) / (prec + rec) # = (2 * TP) / (2 * TP + FP + FN)
results <- rbind(results, c(as.numeric(prec), as.numeric(rec), as.numeric(f),
                            as.numeric(time[3]),
                            "Random Forest", "fit.forest100"))

results

#######################################################################################################
# k = 3; Entrenar el modelo y predecir sobre las muestras de test
# Para usar KNN normalizar
set.seed(seed.val)
tic <- proc.time()
knn3.pred <- knn(train = train.norm[,-train_label_index],
                 test = test.norm[,-test_label_index],
                 cl = train.norm$y,
                 k = 3)
time <- proc.time() - tic

# Confusion Matrix
CrossTable(x = knn3.pred, y = testlabels,
           prop.chisq = FALSE, prop.r = TRUE, prop.c = TRUE, digits = 2,
           dnn = c("PREDICCION", "REAL"))

# Performance metrics...
acc <- sum(knn3.pred == testlabels)/length(testlabels)
prec <- posPredValue(knn3.pred, testlabels, positive = "yes")
rec <- sensitivity(knn3.pred, testlabels, positive = "yes")
f <- (2 * prec * rec) / (prec + rec) # = (2 * TP) / (2 * TP + FP + FN)
results <- rbind(results, c(as.numeric(prec), as.numeric(rec), as.numeric(f),
                            as.numeric(time[3]),
                            "k-NN", "knn3.pred"))

#######################################################################################################



# Variable transformation

# age
for(i in 1 : nrow(mktg_bank_data)){
  if (mktg_bank_data$age[i] < 20){
    mktg_bank_data$age.cat[i] = 'Teenagers'
  } else if (mktg_bank_data$age[i] < 36 & mktg_bank_data$age[i] > 19){
    mktg_bank_data$age.cat[i] = 'Young Adults'
  } else if (mktg_bank_data$age[i] < 66 & mktg_bank_data$age[i] > 35){
    mktg_bank_data$age.cat[i] = 'Adults'
  } else if (mktg_bank_data$age[i] > 65){
    mktg_bank_data$age.cat[i] = 'Retired'
  }
}

table(mktg_bank_data$age.cat)

# job
for(i in 1 : nrow(mktg_bank_data)){
  if (mktg_bank_data$job[i] == 'retired' | mktg_bank_data$job[i] == 'student' | mktg_bank_data$job[i] == 'unemployed') {
    mktg_bank_data$job.cat[i] = 'unemployed'
  } else {
    mktg_bank_data$job.cat[i] = 'employed'
  }
}

table(mktg_bank_data$job.cat)
# marital
for(i in 1 : nrow(mktg_bank_data)){
  if (mktg_bank_data$marital[i] == 'single' | mktg_bank_data$marital[i] == 'divorced') {
    mktg_bank_data$marital.cat[i] = 'unmarried'
  } else {
    mktg_bank_data$marital.cat[i] = 'married'
  }
}
table(mktg_bank_data$marital.cat)

# education
for(i in 1 : nrow(mktg_bank_data)){
  if (mktg_bank_data$education[i] == 'basic.4y' | mktg_bank_data$education[i] == 'basic.6y' |
      mktg_bank_data$education[i] == 'basic.9y') {
    mktg_bank_data$education.cat[i] = 'primary'
  } else if (mktg_bank_data$education[i] == 'high.school') {
    mktg_bank_data$education.cat[i] = 'secondary'
  } else if (mktg_bank_data$education[i] == 'professional.course' |
             mktg_bank_data$education[i] == 'university.degree') {
    mktg_bank_data$education.cat[i] = 'tertiary'
  } else {
    mktg_bank_data$education.cat[i] = 'illiterate'
  }
}
table(mktg_bank_data$education.cat)

# pdays
# días que han pasado desde la última vez que se contactó al cliente por una campaña anterior
# Importante: 999 significa que el cliente no ha sido contactado con anterioridad
mktg_bank_data$pdays.cat <- cut(x = mktg_bank_data$pdays,
                                breaks = c(seq(0,7*4+1,7),999+1),
                                right = FALSE,
                                labels = c("Week 1","Week 2",
                                           "Week 3","Week 4","Never"))
table(mktg_bank_data$pdays.cat)


# Normalizar variables numéricas
if (!require("clusterSim")) {
  install.packages("clusterSim",
                   repos = "https://cloud.r-project.org/",
                   dependencies = TRUE)};
library(clusterSim)
data.norm <- data.Normalization(data.imp[sapply(data.imp, is.numeric)],
                                type = "n4", # (x-min)/range
                                normalization = "column")

# Crear matriz de las variables categóricas
form <- as.formula(
  paste0('~', paste(names(data.imp[sapply(data.imp,is.factor)][!names(
    data.imp[sapply(data.imp, is.factor)]) %in% 'y']),collapse = ' + ')))
matrix.cat <- model.matrix(form, data = data.imp)

# Crear datasets finales
y <- data.imp$y
data.norm <- cbind(data.norm, matrix.cat, y)

# Borrar cálculos intermedios
rm(form, y, matrix.cat)

##########################################################################

bank_data_combined <- mktg_bank_data.imp
str(bank_data_combined)
#balancing the data
nrow(bank_data_combined)

names(bank_data_combined)

ind = createDataPartition(mktg_bank_data.imp[,ncol(mktg_bank_data.imp)],
                          p = split, tim = 1, list = TRUE)

ind

# Train y Test datasets
train_data <- mktg_bank_data.imp[ind$Resample1, ]
test_data <- mktg_bank_data.imp[-ind$Resample1, ]

dim(train_data)
dim(test_data)
#further splitting of train data into validation set
train_data_split <- sample(2, nrow(train_data), replace =TRUE, prob = c(0.75, 0.25))
new_train_data <- train_data[train_data_split == 1,]
valid_data <- train_data[train_data_split == 2,]


#original proportion of the target class in the train data
prop.table(table(new_train_data$y))# yes - 0.1121559, no - 0.8878441;

#down sampling the train data
set.seed(178)
down_train_data <- downSample(x = new_train_data[, -18],
                              y = new_train_data$y)

#checking the proportion the target class after down sampling
prop.table(table(down_train_data$Class)) #yes 0.5, no-0.5;

#upsampling the train data
up_train_data <- upSample(x = new_train_data[, -18],
                          y = new_train_data$y)

#checking the proportion of the target class after up sampling
prop.table(table(up_train_data$Class)) #yes - 0.5, no - 0.5;

#smote training the train data
smote_train_data <- SMOTE(y ~ ., new_train_data)

#checking the proportion of target class after smoting
prop.table(table(smote_train_data$y)) #yes - 0.5714 , no - 0.4286;

#choosing the upsampled data
# model building

#logistic regression
glm_model <- glm(Class ~ ., family = binomial(link = "logit"), data = up_train_data)
summary(glm_model)

#running ANOVA chisq test to check the overall effect of variables on the target variable
anova(glm_model, test = 'Chisq')

#prediction on unseen data using the trained glm_model
glm_pred <- predict(glm_model, valid_data, type = "response")
#setting the probability threshol to 0.5 for now
glm_pred <- ifelse(glm_pred > 0.5, 1, 0)

#using AUC-ROC score to determine model fit
pred_glm <- prediction(glm_pred, valid_data$y)
perf_glm <- performance(pred_glm, measure = "tpr", x.measure = "fpr")
plot(perf_glm)
auc(valid_data$y, glm_pred)#0.7521
#the plot shows the model is predicting more negative values correctly

#interpreting the threshold value to be 0.6 and checking model performance
glm_response <- predict(glm_model, valid_data, type = "response")
glm_response <- ifelse(glm_response >0.6, 1, 0)

#determining the model fit
pred_glm <- prediction(glm_response, valid_data$y)
perf_glm <- performance(pred_glm, measure = "tpr", x.measure = "fpr")
plot(perf_glm)
auc(valid_data$y, glm_response)#0.7531

#obtaining the other measures

#user_defined function for fmeasure
measure_fscore <- function(predict, actual_labels){
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('f-measure:  ')
  cat(fmeasure)
  
}

valid_data$y = ifelse(valid_data$y == "no", 0, 1)
measure_fscore(glm_response, valid_data$y)#0.2263791

#evaluating the true negative rate
perf_glm1 <- performance(pred_glm, measure = "spec", x.measure = "cutoff")
perf_glm1@y.values[[1]][2]#0.9242641



#####################################################################################

#model building 

#random forest

#creating model using default parameters
control <- trainControl(method="repeatedcv", number=5, repeats=1)
seed <- 7
metric <- "auc"
set.seed(seed)
mtry <- sqrt(ncol(up_train_data))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Class~., data=up_train_data, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
#accuracy - 0.7956838

##prediction on unseen data using the trained rf_default model
rf_response <- predict(rf_default, valid_data, type = "prob")
rf_response <- rf_response[,2]
#setting the probability threshol to 0.5 for now
#rf_response <- ifelse(rf_default > 0.5, 1, 0)

#using AUC-ROC score to determine model fit
pred_rf <- prediction(rf_response, valid_data$y)
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
plot(perf_rf)
auc(valid_data$y, rf_response)#0.7964

#obtaining fscore
valid_data$y = ifelse(valid_data$y == "no", 0, 1)
measure_fscore(rf_response, valid_data$y)#0.6703518

#evaluating the true negative rate
perf_rf1 <- performance(pred_rf, measure = "spec", x.measure = "cutoff")
perf_rf1@y.values[[1]][2]#0.9994488

plot(varImp(rf_default))

###################################################################################

#model building

#xgboost

#encoding the categorical variables using one hot encoding
#one hot encoding of remaining categorical variables
train_ohe = dummyVars("~.", data = up_train_data[,], fullRank = T)
train_ohe_df = data.table(predict(train_ohe, up_train_data[,]))

valid_ohe = dummyVars("~.", data = valid_data[,], fullRank = T)
valid_ohe_df = data.table(predict(valid_ohe, valid_data[,]))

#creating parameter list
param_list = list(
  objective = "binary:logistic",
  eta=0.1,
  gamma = 1,
  max_depth=6,
  subsample=0.8,
  colsample_bytree=0.5
)

colnames(train_ohe_df)
colnames(valid_ohe_df)

#converting the data frame into Dmatrix
dtrain = xgb.DMatrix(data = as.matrix(train_ohe_df[,-ncol(train_ohe_df)]), label= train_ohe_df$Class.yes)
dvalid = xgb.DMatrix(data = as.matrix(valid_ohe_df))

#calculationg the nround for the model
xgbcv <- xgb.cv( params = param_list, data = dtrain, nrounds = 100, nfold = 3, 
                 showsd = T, stratified = T, print_every_n = 10, 
                 early_stop_round = 20, maximize = F)
#best iteration - [51]

#finding cv error
min(xgbcv$evaluation_log)

#building the model
xgb_model <- xgb.train (params = param_list, data = dtrain,
                        nrounds = 51,maximize = F , eval_metric = "error")

summary(xgb_model)

#prediction on unseen data using the trained model
xgb_response <- predict(xgb_model, dvalid)
View(xgb_response)

#using AUC-ROC score to determine model fit
pred_xgb <- prediction(xgb_response, valid_data$y)
perf_xgb <- performance(pred_xgb, measure = "tpr", x.measure = "fpr")
plot(perf_xgb)
auc(valid_data$y, xgb_response)#0.5766

#obtaining fscore
valid_data$y = ifelse(valid_data$y == "no", 0, 1)
measure_fscore(xgb_response, valid_data$y)#1.155165

#evaluating the true negative rate
perf_xgb <- performance(pred_xgb, measure = "spec", x.measure = "cutoff")
perf_xgb@y.values[[1]][2]#0.9994488

#viewing variable importance plot
mat <- xgb.importance (feature_names = colnames(up_train_data[,-ncol(up_train_data)]),model = xgb_model)
xgb.plot.importance (importance_matrix = mat[1:20]) 

#########################################################################################################

#model building

#support vector machines
#building model: radial kernel, default parameters
svm_model <- svm(Class~ ., data=up_train_data, method="C-classification", kernel="radial")

#prediction on unseen data using the trained model
svm_response <- predict(svm_model, valid_data, probability = TRUE)
View(svm_response)

#using AUC-ROC score to determine model fit
pred_svm <- prediction(svm_response, valid_data$y)
perf_svm <- performance(pred_svm, measure = "tpr", x.measure = "fpr")
plot(perf_svm)
auc(valid_data$y, svm_response)#0.7834

#obtaining fscore
valid_data$y = ifelse(valid_data$y == "no", 0, 1)
measure_fscore(svm_response, valid_data$y)#0.6512

#evaluating the true negative rate
perf_svm1 <- performance(pred_svm, measure = "spec", x.measure = "cutoff")
perf_svm1@y.values[[1]][2]#0.9992268

################################################################################

#prediction
#random forest has performed well than other algorithms
#predicting the test data using random forest

test_data <- read.csv("test_data.csv")
View(test_data)
test_data$X <- NULL

test_pred <- predict(rf_default, test_data, type = "prob")
write.csv(test_pred, "prediction_result.csv")
