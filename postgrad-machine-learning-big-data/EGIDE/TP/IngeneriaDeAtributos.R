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
