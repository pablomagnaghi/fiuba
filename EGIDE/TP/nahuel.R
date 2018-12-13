#######################################################################################################

############################################## Librerias ##############################################

#######################################################################################################

# Limpiar el global environment
rm(list = ls())

library(mice)
library(magrittr)
library(pander)
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(VIM)
library(gdata)
library(GoodmanKruskal)
library(caret)
library(randomForest)
library(ggpubr)
library(rms)
library(ROCR)
library(pROC)
library(e1071)
library(C50)
#######################################################################################################

################################### Preparar el entorno de trabajo ####################################

#######################################################################################################

# Obtener datos
input_file <- "./data/mktg-bank.csv"

rawdata <- read.csv(input_file, sep=';')

mktg_bank_data <- rawdata

#######################################################################################################

########################################## Limpieza de datos ##########################################

#######################################################################################################

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
mktg_bank_data.temp <- mice(mktg_bank_data.na, m = 5, maxit = 5,
                            method = "pmm", seed = 1492)

# Reemplazar NAs con los valores imputados en el 1º de los 5 dataset (por ejemplo)
mktg_bank_data.imp <- complete(mktg_bank_data.temp, 1)
mktg_bank_data.imp <- drop.levels(mktg_bank_data.imp)

# Borrar cálculos intermedios
rm(mktg_bank_data, mktg_bank_data.na, mktg_bank_data.temp)

# No mas unknown
colSums(is.na(mktg_bank_data.imp))
dim(mktg_bank_data.imp)
str(mktg_bank_data.imp)
sum(is.na(mktg_bank_data.imp))
#######################################################################################################

####################################### Ingenieria de atributos #######################################

#######################################################################################################
mktg_bank_data.clean <- mktg_bank_data.imp

# Identificar características altamente correlacionadas entre variables numericas

set.seed(149)
str(mktg_bank_data.clean)

correlation_matrix <- cor(mktg_bank_data.clean[,c(1, 11, 12, 13, 14, 16:20)])
correlation_matrix
corrplot(correlation_matrix, method="number", type="upper")

# euribor3m and nr.employed so nr.employed. is removed.
# euribor3m and emp.var.rate so emp.var.rate is removed.
mktg_bank_data.clean[, c("nr.employed", "emp.var.rate")] <- NULL


# Identificar características altamente correlacionadas entre variables categoricas
vars <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month",
          "day_of_week", "poutcome")
data_wit_cat_vars <- subset(mktg_bank_data.clean, select = vars)
cat_corr_matrix <- GKtauDataframe(data_wit_cat_vars)
plot(cat_corr_matrix)

# no hay correlacion significativa

#######################################################################################################

# Conversion de variables

# age
mktg_bank_data.clean$age <- cut(mktg_bank_data.clean$age, c(0,20,35,65,100),
                                labels = c("Teenagers","Young Adults", "Adults", "Retired"))
table(mktg_bank_data.clean$age)

# marital
mktg_bank_data.clean$marital = factor(ifelse(mktg_bank_data.clean$marital == 'married', "married", "unmarried"))
table(mktg_bank_data.clean$marital)

# education
for(i in 1 : nrow(mktg_bank_data.clean)){
  if (mktg_bank_data.clean$education[i] == 'basic.4y' |
      mktg_bank_data.clean$education[i] == 'basic.6y' |
      mktg_bank_data.clean$education[i] == 'basic.9y') {
    mktg_bank_data.clean$new_education[i] = 'primary'
  } else if (mktg_bank_data.clean$education[i] == 'high.school') {
    mktg_bank_data.clean$new_education[i] = 'secondary'
  } else if (mktg_bank_data.clean$education[i] == 'professional.course' |
             mktg_bank_data.clean$education[i] == 'university.degree') {
    mktg_bank_data.clean$new_education[i] = 'tertiary'
  } else {
    mktg_bank_data.clean$new_education[i] = 'illiterate'
  }
}
table(mktg_bank_data.clean$new_education)
mktg_bank_data.clean$education <- as.factor(mktg_bank_data.clean$new_education)
table(mktg_bank_data.clean$education)
mktg_bank_data.clean$new_education <- NULL

# pdays
# días que han pasado desde la ultima vez que se contactó al cliente por una campaña anterior
# Importante: 999 significa que el cliente no ha sido contactado con anterioridad
mktg_bank_data.clean$pdays <- cut(x = mktg_bank_data.clean$pdays,
                                  breaks = c(seq(0,7*4+1,7),999+1),
                                  right = FALSE,
                                  labels = c("Week 1","Week 2",
                                             "Week 3","Week 4","Never"))
table(mktg_bank_data.clean$pdays)

# convertir int a numeric
mktg_bank_data.clean$campaign <- as.numeric(mktg_bank_data.clean$campaign)
mktg_bank_data.clean$previous <- as.numeric(mktg_bank_data.clean$previous)

# Eliminar la variable "duration" para obtener un modelo predictivo realista
mktg_bank_data.clean$duration <- NULL

colSums(is.na(mktg_bank_data.clean))
dim(mktg_bank_data.clean)
str(mktg_bank_data.clean)
sum(is.na(mktg_bank_data.clean))

#######################################################################################################

########################## Generacion de conjunto de prueba y entrenamiento ###########################

#######################################################################################################

idx <- createDataPartition(mktg_bank_data.clean$y, p = 0.8,list = FALSE)
train <- mktg_bank_data.clean[idx,]
test <- mktg_bank_data.clean[-idx,]

table(train$y)
table(test$y)

#######################################################################################################

############################################## Modelado ###############################################

#######################################################################################################

# 1. Regresion logistica

#######################################################################################################

set.seed(123)

# Creacion del modelo
glm_model <- glm(y ~.,family=binomial(link = "logit"), data = train)

# Estadisticas
summary(glm_model)
anova(glm_model, test="Chisq")

# Prediccion
pred_glm_model <- predict(glm_model,newdata=test,type='response')

# Creacion de parametros de prediccion y rendimiento para el ajuste
pred <- prediction(pred_glm_model, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Curva ROC
plot(perf)

# AUC
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Elegir el valor de corte optimo
cost.perf = performance(pred, "cost", cost.fp = 1 , cost.fn = 4 )
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# Valor de corte optimo
pred_glm_model_res_cm <- ifelse(pred_glm_model > 0.21,"yes","no")

# Matriz de confusion
cm <- confusionMatrix(table(pred_glm_model_res_cm, test$y), positive = "yes", mode="prec_recall")
cm
results <- data.frame(as.numeric(cm$overall['Accuracy']),
                      as.numeric(cm$byClass['Precision']),
                      as.numeric(cm$byClass['Recall']),
                      as.numeric(cm$byClass['F1'] ),
                      "Logistic Regression",
                      stringsAsFactors = FALSE)
names(results) <- c("Accuracy", "Precision", "Recall", "F1", "Algoritmo")

#######################################################################################################

# 2. Random forest

#######################################################################################################

# Creacion del modelo
rf_model<-randomForest(y ~.,data = train, importance=TRUE, ntree=2000)


# Estadisticas
summary(rf_model)

# Importancia
varImpPlot(rf_model)

# Prediccion
pred_rf_model <- predict(rf_model,newdata=test, type="prob")
pred_rf_model <- pred_rf_model[,2]

# Creacion de parametros de prediccion y rendimiento para el ajuste
pred <- prediction(pred_rf_model, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Curva ROC
plot(perf)

#AUC - Metric
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# Elegir el valor de corte optimo
cost.perf = performance(pred, "cost", cost.fp = 1 , cost.fn = 4)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# Valor de corte optimo
pred_rf_model_res_cm <- ifelse(pred_rf_model > 0.20,"yes","no")

# Matriz de confusion
cm <- confusionMatrix(table(pred_rf_model_res_cm, test$y), positive = "yes", mode="prec_recall")
cm

results <-rbind(results, c(as.numeric(cm$overall['Accuracy']),
                  as.numeric(cm$byClass['Precision']),
                  as.numeric(cm$byClass['Recall']),
                  as.numeric(cm$byClass['F1'] ),
                  "Random Forest"))
results
#######################################################################################################

# 3. SVM

#######################################################################################################

#Model Execution
svm_model <- svm(y~., data=train, kernel="radial", probability=TRUE)

#Summary Statistics
summary(svm_model)

#Prediction
pred_svm<- predict(svm_model, test, probability = TRUE)
pred_svm <- pred_svm$probabilities
pred_svm_prob <- attr(pred_svm, "probabilities")
pred_svm_prob <- pred_svm_prob[,2]

# Creacion de parametros de prediccion y rendimiento para el ajuste
pred <- prediction(pred_svm_prob, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Curva ROC
plot(perf)

#AUC - Metric
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Elegir el valor de corte optimo
cost.perf = performance(pred, "cost", cost.fp = 1 , cost.fn = 4)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

# Valor de corte optimo
pred_svm_model_res_cm <- ifelse(pred_svm_prob > 0.20,"yes","no")

# Matriz de confusion
cm_svm <- confusionMatrix(table(pred_svm_model_res_cm, test$y), positive = "yes", mode="prec_recall")
cm_svm

results <- rbind(results, c(as.numeric(cm_svm$overall['Accuracy']),
                 as.numeric(cm_svm$byClass['Precision']),
                 as.numeric(cm_svm$byClass['Recall']),
                 as.numeric(cm_svm$byClass['F1'] ),
                 "SVM"))
results

#######################################################################################################

# 4.1 C5.0 por defecto

#######################################################################################################

# Creacion del modelo
train_label_index = grep(pattern = "^y$", colnames(train))
c5_model <- C5.0(x = train[, -train_label_index], y = train$y)

# Estadisticas
summary(c5_model)

# Prediccion
pred_c5_model <- predict(c5_model, test, type="class")
pred_c5_model

# Matriz de confusion
cm <- confusionMatrix(table(pred_c5_model, test$y), positive = "yes", mode="prec_recall")
cm

results <-rbind(results, c(as.numeric(cm$overall['Accuracy']),
                           as.numeric(cm$byClass['Precision']),
                           as.numeric(cm$byClass['Recall']),
                           as.numeric(cm$byClass['F1'] ),
                           "C5.0"))
results

#######################################################################################################

# 4.2 C5.0 con adaptive boosting

#######################################################################################################

# Creacion del modelo
train_label_index = grep(pattern = "^y$", colnames(train))
c5_boost_10_model <- C5.0(x = train[, -train_label_index],
                        y = train$y,
                        trials = 10)

# Estadisticas
summary(c5_boost_10_model)

# Prediccion
pred_c5_boost_10__model <- predict(c5_boost_10_model, test, type="class")
pred_c5_boost_10__model

# Matriz de confusion
cm <- confusionMatrix(table(pred_c5_boost_10__model, test$y), positive = "yes", mode="prec_recall")
cm

results <-rbind(results, c(as.numeric(cm$overall['Accuracy']),
                           as.numeric(cm$byClass['Precision']),
                           as.numeric(cm$byClass['Recall']),
                           as.numeric(cm$byClass['F1'] ),
                           "C5.0 con adaptive boosting"))
results

#######################################################################################################

# 4.3 C5.0 con penalizaciones

#######################################################################################################

# Montar la matriz de penalizaciones
matrix.dim <- list(c("no","yes"), c("no","yes"))
names(matrix.dim) <- c("PREDICCION", "REAL")
error.cost <- matrix(c(0,1,5,0), nrow = 2,
                     dimnames = matrix.dim)

# Creacion del modelo
train_label_index = grep(pattern = "^y$", colnames(train))
c5_cost_model <- C5.0(x = train[, -train_label_index],
                          y = train$y,
                          costs = error.cost)

# Estadisticas
summary(c5_cost_model)

# Prediccion
pred_c5_cost_model <- predict(c5_cost_model, test, type="class")
pred_c5_cost_model

# Matriz de confusion
cm <- confusionMatrix(table(pred_c5_cost_model, test$y), positive = "yes", mode="prec_recall")
cm

results <-rbind(results, c(as.numeric(cm$overall['Accuracy']),
                           as.numeric(cm$byClass['Precision']),
                           as.numeric(cm$byClass['Recall']),
                           as.numeric(cm$byClass['F1'] ),
                           "C5.0 con penalizacion"))
results
