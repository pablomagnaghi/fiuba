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
