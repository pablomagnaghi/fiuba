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
correlation_matrix <- cor(mktg_bank_data.clean[,c(1, 11, 12, 13, 14, 16:20)])
correlation_matrix
corrplot(correlation_matrix, method="number", type="upper")

# euribor3m y nr.employed estan muy correlacionados, entonces nr.employed. es removido
# euribor3m y emp.var.rate estan muy correlacionados emp.var.rate es removido
mktg_bank_data.clean[, c("nr.employed", "emp.var.rate")] <- NULL

# Identificar características altamente correlacionadas entre variables categoricas
vars <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month",
          "day_of_week", "poutcome")
data_wit_cat_vars <- subset(mktg_bank_data.clean, select = vars)
cat_corr_matrix <- GKtauDataframe(data_wit_cat_vars)
plot(cat_corr_matrix)

# no hay correlacion significativa

# Eliminar la variable "duration" para obtener un modelo predictivo realista
mktg_bank_data.clean$duration <- NULL

#######################################################################################################

# Conversion de variables

# age
mktg_bank_data.clean$age <- cut(mktg_bank_data.clean$age, c(0,25,40,65,100),
                                labels = c("Teenagers","Young Adults", "Adults", "Retired"))
table(mktg_bank_data.clean$age)

# marital
levels(mktg_bank_data.clean$marital)[c(1,3)] <- "unmarried"
table(mktg_bank_data.clean$marital)

# education
levels(mktg_bank_data.clean$education)[c(1:4)] <- "lower.degree"
table(mktg_bank_data.clean$education)

# pdays
# días que han pasado desde la ultima vez que se contactó al cliente por una campaña anterior
# Importante: 999 significa que el cliente no ha sido contactado con anterioridad
mktg_bank_data.clean$pdays <- cut(x = mktg_bank_data.clean$pdays,
                                  breaks = c(seq(0,7*4+1,7),999+1),
                                  right = FALSE,
                                  labels = c("Week 1","Week 2",
                                             "Week 3","Week 4","Never"))
table(mktg_bank_data.clean$pdays)

# previous
mktg_bank_data.clean$previous = as.factor(mktg_bank_data.clean$previous)
levels(mktg_bank_data.clean$previous)[c(5:8)] <- "More than 3 times"
levels(mktg_bank_data.clean$previous)[c(2:4)] <- "Less than 4 times"
levels(mktg_bank_data.clean$previous)[c(1)] <- "Never contacted"
table(mktg_bank_data.clean$previous)

# convertir int a numeric
mktg_bank_data.clean$campaign <- as.numeric(mktg_bank_data.clean$campaign)

colSums(is.na(mktg_bank_data.clean))
dim(mktg_bank_data.clean)
str(mktg_bank_data.clean)
sum(is.na(mktg_bank_data.clean))

# remover valores aislados
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

mktg_bank_data.clean$campaign <- remove_outliers(mktg_bank_data.clean$campaign)
mktg_bank_data.clean$cons.price.idx <- remove_outliers(mktg_bank_data.clean$cons.price.idx)
mktg_bank_data.clean$cons.conf.idx <- remove_outliers(mktg_bank_data.clean$cons.conf.idx)
mktg_bank_data.clean$euribor3m <- remove_outliers(mktg_bank_data.clean$euribor3m)
mktg_bank_data.clean <- na.omit(mktg_bank_data.clean)

dim(mktg_bank_data.clean)
sum(is.na(mktg_bank_data.clean$age))
mktg_bank_data.clean <- na.omit(mktg_bank_data.clean)
dim(mktg_bank_data.clean)

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
add_profit <- function(profits, cm, title) {

  VN <- cm$table[1,1] # PRED NO => NO
  FN <- cm$table[1,2] # PRED NO => YES
  FP <- cm$table[2,1] # PRED YES => NO
  VP <- cm$table[2,2] # PRED YES => YES

  prob_p <- (VP + FN) / (VN + FN + FP + VP)
  prob_n <- (VN + FP) / (VN + FN + FP + VP)
  prob_vp <- VP / (VP + FN)
  prob_fp <- FP / (VN + FP)

  prob_p * prob_vp * 60
  profit = prob_p * prob_vp * 60 - prob_n * prob_fp * 3

  profits <- rbind(profits, c(as.numeric(profit),
                              as.numeric(VN),
                              as.numeric(FN),
                              as.numeric(FP),
                              as.numeric(VP),
                              title))
  profits
}

add_result <- function(results, cm, title) {
  results <- rbind(results, c(as.numeric(cm$overall['Accuracy']),
                             as.numeric(cm$byClass['Precision']),
                             as.numeric(cm$byClass['Recall']),
                             as.numeric(cm$byClass['F1'] ),
                             title))
  results
}
#######################################################################################################

# 1.1. Regresion logistica

#######################################################################################################

set.seed(123)

# Creacion del modelo
glm_model <- glm(y ~.,family=binomial(link = "logit"), data = train)

# Estadisticas
summary(glm_model)
anova(glm_model, test="Chisq")

# Prediccion
pred_glm_model <- predict(glm_model,newdata=test,type='response')
pred_glm_model_res_cm <- ifelse(pred_glm_model > 0.5,"yes","no")

# Matriz de confusion
cm_glm_model <- confusionMatrix(table(pred_glm_model_res_cm, test$y), positive = "yes", mode="prec_recall")
cm_glm_model

VN <- cm_glm_model$table[1,1] # PRED NO => NO
FN <- cm_glm_model$table[1,2] # PRED NO => YES
FP <- cm_glm_model$table[2,1] # PRED YES => NO
VP <- cm_glm_model$table[2,2] # PRED YES => YES

prob_p <- (VP + FN) / (VN + FN + FP + VP)
prob_n <- (VN + FP) / (VN + FN + FP + VP)
prob_vp <- VP / (VP + FN)
prob_fp <- FP / (VN + FP)

profit = prob_p * prob_vp * 60 - prob_n * prob_fp * 3

profits <- data.frame(as.numeric(profit),
                     as.numeric(VN),
                     as.numeric(FN),
                     as.numeric(FP),
                     as.numeric(VP),
                     "Logistic Regression",
                     stringsAsFactors = FALSE)

names(profits) <- c("Profit", "VN", "FN", "FP", "VP", "Algorithm")
profits

results <- data.frame(as.numeric(cm_glm_model$overall['Accuracy']),
                      as.numeric(cm_glm_model$byClass['Precision']),
                      as.numeric(cm_glm_model$byClass['Recall']),
                      as.numeric(cm_glm_model$byClass['F1'] ),
                      "Logistic Regression", 
                      stringsAsFactors = FALSE)
names(results) <- c("Accuracy", "Precision", "Recall", "F1", "Algorithm")

results

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

#######################################################################################################

# 1.2. Regresion logistica op

#######################################################################################################

# Creacion del modelo
glm_model_op <- glm(y ~.,family=binomial(link = "logit"), data = train)

# Estadisticas
summary(glm_model_op)
anova(glm_model_op, test="Chisq")

# Prediccion
pred_glm_model_op <- predict(glm_model_op,newdata=test,type='response')
pred_glm_model_op_res_cm <- ifelse(pred_glm_model_op > 0.2,"yes","no")

# Matriz de confusion
cm_glm_model_op <- confusionMatrix(table(pred_glm_model_op_res_cm, test$y), positive = "yes", mode="prec_recall")
cm_glm_model_op

profits <- add_profit(profits, cm_glm_model_op, "Logistic Regression op");
profits

results <- add_result(results, cm_glm_model_op, "Logistic Regression op")
results

# Prediccion
pred_glm_model_op <- predict(glm_model_op,newdata=test,type='response')

# Creacion de parametros de prediccion y rendimiento para el ajuste
pred <- prediction(pred_glm_model_op, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Curva ROC
plot(perf)

# AUC
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#######################################################################################################

# 2.1. SVM

#######################################################################################################

#Model Execution
svm_model <- svm(y~., data=train, kernel="radial", probability=TRUE)

#Summary Statistics
summary(svm_model)

pred_svm <- predict(svm_model, test, type = "response")

# Matriz de confusion
cm_svm <- confusionMatrix(table(pred_svm, test$y), positive = "yes", mode="prec_recall")
cm_svm

profits <- add_profit(profits, cm_svm, "SVM");
profits

results <- add_result(results, cm_svm, "SVM")
results

#######################################################################################################

# 2.2. SVM con penalizacion

#######################################################################################################

#Model Execution
svm_model_costs <- svm(y~., data=train, class.weights = c("yes"=3, "no"=1),
                       kernel="radial", probability=TRUE)

#Summary Statistics
summary(svm_model_costs)

pred_svm_costs <- predict(svm_model_costs, test, type = "response")

# Matriz de confusion
cm_svm_costs <- confusionMatrix(table(pred_svm_costs, test$y), positive = "yes", mode="prec_recall")
cm_svm_costs

profits <- add_profit(profits, cm_svm_costs, "SVM costs");
profits

results <- add_results(results, cm_svm_costs, "SVM costs")
results

#######################################################################################################

# 3.1 C5.0 por defecto

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
cm_c5 <- confusionMatrix(table(pred_c5_model, test$y), positive = "yes", mode="prec_recall")
cm_c5

profits <- add_profit(profits, cm_c5, "C5.0");
profits

results <- add_results(results, cm_c5, "C5.0")
results

#######################################################################################################

# 3.2 C5.0 con adaptive boosting

#######################################################################################################

# Creacion del modelo
train_label_index = grep(pattern = "^y$", colnames(train))
c5_boost_10_model <- C5.0(x = train[, -train_label_index],
                          y = train$y,
                          trials = 10)

# Estadisticas
summary(c5_boost_10_model)

# Prediccion
pred_c5_boost_10_model <- predict(c5_boost_10_model, test, type="class")
pred_c5_boost_10_model

# Matriz de confusion
cm_c5_boost_10 <- confusionMatrix(table(pred_c5_boost_10_model, test$y), positive = "yes", mode="prec_recall")
cm_c5_boost_10

profits <- add_profit(profits, cm_c5_boost_10, "C5.0 with adaptive boosting");
profits

results <- add_results(results, cm_c5_boost_10, "C5.0 with adaptive boosting")
results

#######################################################################################################

# 3.3 C5.0 con penalizaciones

#######################################################################################################

# Montar la matriz de penalizaciones
error.cost <- matrix(c(0,1,3,0), nrow = 2)

# Creacion del modelo
train_label_index = grep(pattern = "^y$", colnames(train))
c5_model_costs <- C5.0(x = train[, -train_label_index],
                       y = train$y,
                       costs = error.cost)

# Estadisticas
summary(c5_model_costs)

# Prediccion
pred_c5_model_costs <- predict(c5_model_costs, test, type="class")
pred_c5_model_costs

# Matriz de confusion
cm_c5_model_costs  <- confusionMatrix(table(pred_c5_model_costs, test$y), positive = "yes", mode="prec_recall")
cm_c5_model_costs

profits <- add_profit(profits, cm_c5_model_costs, "C5.0 costs");
profits

results <- add_results(results, cm_c5_model_costs , "C5.0 costs")
results


#######################################################################################################

# 4.1. Random forest

#######################################################################################################

# Creacion del modelo
rf_model<-randomForest(y ~.,data = train, importance=TRUE, ntree=500)

# Estadisticas
summary(rf_model)

# Importancia
varImpPlot(rf_model)

# Prediccion
pred_rf_model <- predict(rf_model, test, type="class")

# Matriz de confusion
cm_rf_model <- confusionMatrix(table(pred_rf_model, test$y), positive = "yes", mode="prec_recall")
cm_rf_model

profits <- add_profit(profits, cm_rf_model, "Random Forest");
profits

results <- add_results(results, cm_rf_model, "Random Forest")
results

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

#######################################################################################################

# 4.2. Random forest con penalizacion

#######################################################################################################

# Creacion del modelo
error.cost <- matrix(c(0,1,5,0), nrow = 2)
rf_model_costs <-randomForest(y ~ ., parms = list(loss=error.cost),
                              data = train, importance=TRUE, ntree=500, mtry = 2)

# Prediccion
pred_rf_model_costs <- predict(rf_model_costs, test, type="class")

# Matriz de confusion
cm_rf_model_costs <- confusionMatrix(table(pred_rf_model_costs, test$y), positive = "yes", mode="prec_recall")
cm_rf_model_costs

profits <- add_profit(profits, cm_rf_model_costs, "Random Forest costs");
profits

results <- add_results(results, cm_rf_model_costs, "Random Forest costs")
results

# Prediccion
pred_rf_model_costs <- predict(rf_model_costs,newdata=test, type="prob")
pred_rf_model_costs <- pred_rf_model_costs[,2]

# Creacion de parametros de prediccion y rendimiento para el ajuste
pred <- prediction(pred_rf_model_costs, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Curva ROC
plot(perf)

#AUC - Metric
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#######################################################################################################

# 4.3. Random forest optimizado

#######################################################################################################

### Optimización de hiperparametros
# Identificación del valor optimo del hiperparámetro mtry.
tuning_rf_mtry <- function(df, y, ntree = 500){
  # Esta función devuelve el out-of-bag clasification error de un modelo RandomForest
  # en función del número de predictores evaluados (mtry)

  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = numero de árboles creados en el modelo randomForest

  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_err_rate    <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictores[i] <- i
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
  }
  results <- data_frame(n_predictores, oob_err_rate)
  return(results)
}

hiperparametro_mtry <-  tuning_rf_mtry(df = train, y = "y")
hiperparametro_mtry %>% arrange(oob_err_rate)

ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry",
       x = "nº predictores empleados") +
  theme_bw()

# Se identifica el valor 2 como número óptimo de predictores a evaluar en cada división.
# Identificación del valor óptimo del hiperparámetro nodesize.

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # Esta función devuelve el out-of-bag clasification error de un modelo RandomForest
  # en función del tamaño mínimo de los nodos terminales (nodesize).

  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   sizes = tamaños evaluados
  #   ntree = número de árboles creados en el modelo randomForest

  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_err_rate <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(321)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = 2, ntree = ntree,
                              nodesize = i)
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
  }
  results <- data_frame(size, oob_err_rate)
  return(results)
}

hiperparametro_nodesize <-  tuning_rf_nodesize(df = train, y = "y", size = c(1:17))
hiperparametro_nodesize %>% arrange(oob_err_rate)


ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparametro_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

# Se identifica el 4 como número óptimo de observaciones mínimas que deben contener los nodos terminales.
modelo_randomforest <- randomForest(y ~ ., data = train, mtry = 2, ntree = 500,
                                    importance = TRUE, nodesize = 4)

oob_err_rate <- data.frame(oob_err_rate = modelo_randomforest$err.rate[, 1],
                           arboles = seq_along(modelo_randomforest$err.rate[, 1]))
ggplot(data = oob_err_rate, aes(x = arboles, y = oob_err_rate )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

# Alcanzados en torno a 200 árboles el error del modelo se estabiliza.

### Ajuste final y out-of-bag (OOB) error

# En este caso, en lugar de calcular el error de test con un conjunto de datos
# independiente, se emplean todas las observaciones para entrenar el modelo final
# y se extrae el OOB-error que, como se ha descrito anteriormente, es una buena
# estimación del error de test.
set.seed(17)
error.cost <- matrix(c(0,1,10,0), nrow = 2)
rf_model_tunned <- randomForest(y ~ ., data = train, mtry = 2, ntree = 200,
                                    importance = TRUE, nodesize = 4,
                                    parms = list(loss=error.cost))

# Estadisticas
summary(rf_model_tunned)

# Prediccion
pred_rf_model_tunned <- predict(rf_model_tunned, test, type="class")
pred_rf_model_tunned

# Matriz de confusion
cm_tunned <- confusionMatrix(table(pred_rf_model_tunned, test$y), positive = "yes", mode="prec_recall")
cm_tunned

profits <- add_profit(profits, cm_tunned, "Random Forest tunned");
profits

results <- add_result(results, cm_tunned, "Random Forest tunned");
results

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

# Prediccion
pred_rf_model_tunned <- predict(rf_model_tunned,newdata=test, type="prob")
pred_rf_model_tunned <- pred_rf_model_tunned[,2]

# Creacion de parametros de prediccion y rendimiento para el ajuste
pred <- prediction(pred_rf_model_tunned, test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

# Curva ROC
plot(perf)

#AUC - Metric
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Elegir el valor de corte optimo
cost.perf = performance(pred, "cost", cost.fp = 1 , cost.fn = 12)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


#######################################################################################################

# 5. Modelo utlizado - Random forest optimizado

#######################################################################################################

# Creacion del modelo
rf_model_op <- randomForest(y ~.,data = train,
                            importance=TRUE,
                            ntree=200,
                            mtry=2,
                            nodesize = 4,
                            cutoff = c(0.98,0.02))

# Estadisticas
summary(rf_model_op)

# Importancia
varImpPlot(rf_model_op)

# Prediccion
pred_rf_model_op <- predict(rf_model_op, test, type="class")


# Matriz de confusion
cm_rf_model_op <- confusionMatrix(table(pred_rf_model_op , test$y), positive = "yes", mode="prec_recall")
cm_rf_model_op

profits <- add_profit(profits, cm_rf_model_op, "Random Forest op");
profits

results <- add_result(results, cm_rf_model_op, "Random Forest op")
results
