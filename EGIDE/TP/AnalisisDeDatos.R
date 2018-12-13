#######################################################################################################

############################################## Librerias ##############################################

#######################################################################################################

# Limpiar el global environment
rm(list = ls())

library(magrittr)
library(pander)
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(VIM)
library(mice)
library(gdata)

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
seed.val <- 1492
mktg_bank_data.temp <- mice(mktg_bank_data.na, m = 5, maxit = 5,
                            method = "pmm", seed = seed.val)

# Reemplazar NAs con los valores imputados en el 1º de los 5 dataset (por ejemplo)
mktg_bank_data.imp <- complete(mktg_bank_data.temp, 1)
mktg_bank_data.imp <- drop.levels(mktg_bank_data.imp)

# Borrar calculos intermedios
rm(mktg_bank_data, mktg_bank_data.na, mktg_bank_data.temp)

# No mas unknown
colSums(is.na(mktg_bank_data.imp))
dim(mktg_bank_data.imp)
str(mktg_bank_data.imp)
sum(is.na(mktg_bank_data.imp))

#######################################################################################################

########################################## Analisis de datos  #########################################

#######################################################################################################

mktg_bank_data <- mktg_bank_data.imp

# Edad
p1 <- ggplot() +
  geom_bar(aes(y = (..count..), x = age, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Edad vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Edad [age]")
p1

b1 <- ggplot(mktg_bank_data, aes(factor(y), age)) +
  geom_boxplot(aes(fill = factor(y)))
b1

# Tipo de trabajo vs Toma de deposito
p2 <- ggplot() +
  geom_bar(aes(y = (..count..), x = job, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Tipo de trabajo vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Tipo de trabajo [job]")
p2

# Estado civil vs Toma de deposito
p3 <- ggplot() +
  geom_bar(aes(y = (..count..), x = marital, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Estado civil vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Estado civi [marital]")
p3

# Nivel de educacion vs Toma de deposito
p4 <- ggplot() +
  geom_bar(aes(y = (..count..), x = education, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Nivel de educacion vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Nivel de educacion [education]")
p4

# Credito incumplido vs Toma de deposito
p5 <- ggplot() +
  geom_bar(aes(y = (..count..), x = default, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Credito incumplido vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Credito incumplido [default]")
p5

# Prestamo para vivienda vs Toma de deposito
p6 <- ggplot() +
  geom_bar(aes(y = (..count..), x = housing, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Prestamo para vivienda vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Prestamo para vivienda [housing]")
p6

# Prestamo personal vs Toma de deposito
p7 <- ggplot() +
  geom_bar(aes(y = (..count..), x = loan, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Prestamo personal vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Prestamo personal [loan]")
p7

# Tipo de comunicación de contacto vs Toma de deposito
p8 <- ggplot() +
  geom_bar(aes(y = (..count..), x = contact, fill = y), data = mktg_bank_data, stat="count" ) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Tipo de comunicación de contacto vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Tipo de comunicación de contacto [contact]")
p8

# Mes del ultimo contacto vs Toma de deposito
p9 <- ggplot() +
  geom_bar(aes(y = (..count..), x = month, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Mes del ultimo contacto vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Mes del ultimo contacto [month]")
p9

# Dia de la semana del ultimo contacto vs Toma de deposito
p10 <- ggplot() +
  geom_bar(aes(y = (..count..), x = day_of_week, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Dia de la semana del ultimo contacto vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Dia de la semana del ultimo contacto [day_of_week]")
p10

# Duracion del ultimo contacto vs Toma de deposito
p11 <- ggplot() +
  geom_bar(aes(y = (..count..), x = duration, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Duracion del ultimo contacto vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Duracion del ultimo contacto [duration]")
p11

# ln_duration <- log(train$duration)
ln_duration <- log(1 + (mktg_bank_data$duration))
p11Log <- ggplot() +
  geom_histogram(aes(x = ln_duration, fill = y), data = mktg_bank_data, binwidth = 0.1) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Duracion del ultimo contacto vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Duracion del ultimo contacto [ln(duration)]")
p11Log

# Numero de contactos realizados vs Toma de deposito
b12 <- ggplot() +
  geom_bar(aes(y = (..count..), x = campaign, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Numero de contactos realizados vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Numero de contactos realizados [campaign]")
b12

# Numero de días que pasaron desde la campaña anterior vs Toma de deposito
p13 <- ggplot() +
  geom_histogram(aes(y = (..count..), x = pdays, fill = y), data = mktg_bank_data, binwidth = 50) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad") +
  ggtitle("Numero de días que pasaron desde la campaña anterior vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
          element_text(size  = 10,
                       angle = 45,
                       hjust = 1,
                       vjust = 1)) +
  xlab("Numero de días que pasaron desde la campaña anterior [pdays]")
p13

# Numero de contactos realizados a este cliente vs Toma de deposito
p14 <- ggplot() +
  geom_histogram(aes( x = previous, fill = y), data = mktg_bank_data, binwidth = 1) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Numero de contactos realizados a este cliente vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x =
        element_text(size  = 10,
                     angle = 45,
                     hjust = 1,
                     vjust = 1)) +
  xlab("Numero de contactos realizados a este cliente [previous]")
p14

# Resultado de la campaña de marketing anterior vs Toma de deposito
p15 <- ggplot() +
  geom_bar(aes(y = (..count..), x = poutcome, fill = y), data = mktg_bank_data, stat="count") +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Resultado de la campaña de marketing anterior vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Resultado de la campaña de marketing anterior [poutcome]")
p15

# Tasa de variación del empleo vs Toma de deposito
p16 <- ggplot() +
  geom_histogram(aes( x = emp.var.rate, fill = y), data = mktg_bank_data, binwidth = 1) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Tasa de variación del empleo vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tasa de variación del empleo [emp.var.rate]")
p16

b16 <- ggplot(mktg_bank_data, aes(factor(y), emp.var.rate)) +
  geom_boxplot(aes(fill = factor(y)))
b16

# Indice de precios al consumidor vs Toma de deposito
p17 <- ggplot() +
  geom_histogram(aes( x = cons.price.idx, fill = y), data = mktg_bank_data, binwidth = 1) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Indice de precios al consumidor vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Indice de precios al consumidor [cons.price.idx]")
p17

b17 <- ggplot(mktg_bank_data, aes(factor(y), cons.price.idx)) +
  geom_boxplot(aes(fill = factor(y)))
b17

# Indice de confianza del consumidor vs Toma de deposito
p18 <- ggplot() +
  geom_histogram(aes( x = cons.conf.idx, fill = y), data = mktg_bank_data, binwidth = 5) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Indice de confianza del consumidor vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Indice de confianza del consumidor [cons.conf.idx]")
p18

b18 <- ggplot(mktg_bank_data, aes(factor(y), cons.conf.idx)) +
  geom_boxplot(aes(fill = factor(y)))
b18

# Tasa de interes interbancario vs Toma de deposito
p19 <- ggplot() +
  geom_histogram(aes( x = euribor3m, fill = y), data = mktg_bank_data, binwidth =0.01) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Cantidad")+
  ggtitle("Tasa de interes interbancario vs Toma de deposito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tasa de interes interbancario [euribor3m]")
p19

b19 <- ggplot(mktg_bank_data, aes(factor(y), euribor3m)) +
  geom_boxplot(aes(fill = factor(y)))
b19

# Promedio trimestral del numero de ciudadanos activos vs Toma de deposito
p20 <- ggplot() +
  geom_histogram(aes( x = nr.employed, fill = y), data = mktg_bank_data, binwidth =100) +
  labs(fill="Toma de deposito") +
  scale_y_continuous(name = "Count") +
  ggtitle("Promedio trimestral del numero de ciudadanos activos vs Toma de depósito") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Promedio trimestral del numero de ciudadanos activos [nr.employed]")
p20

b20 <- ggplot(mktg_bank_data, aes(factor(y), nr.employed)) +
  geom_boxplot(aes(fill = factor(y)))
b20

# Toma de deposito (objetivo)
p21 <- ggplot(mktg_bank_data %>% group_by(y) %>% summarise(Count = n())) +
  geom_bar(aes(y, Count), stat = "identity", fill = "indianred1") +
  scale_y_continuous(name = "Cantidad") +
  geom_label(aes(y, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Toma de deposito") +
  xlab("")
p21

#######################################################################################################
