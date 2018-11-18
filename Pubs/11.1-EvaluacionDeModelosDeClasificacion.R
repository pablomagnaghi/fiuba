library(ISLR)
library(MASS)
library(dplyr)

names(Default)

summary(Default)

# Creacion del dataset train y test
nrow(Default)

ntrain <- nrow(Default)*0.8
ntest <- nrow(Default)*0.2
c(ntrain,ntest)

set.seed(161)
index_train<-sample(1:nrow(Default),size = ntrain)
train<-Default[index_train,]
test<-Default[-index_train,]

summary(train)

summary(test)

logit_reg <- glm(default~balance+student+income,data = train,family = "binomial")

lda_fit<-lda(default~balance+student+income,data=train)

predicted_value <- predict(logit_reg,test,type = "response")
predicted_class <- ifelse(predicted_value>0.5, "Yes","No")
performance_data<-data.frame(observed=test$default,
           predicted= predicted_class)


positive <- sum(performance_data$observed=="Yes")
negative <- sum(performance_data$observed=="No")
predicted_positive <- sum(performance_data$predicted=="Yes")
predicted_negative <- sum(performance_data$predicted=="No")
total <- nrow(performance_data)
data.frame(positive, negative,predicted_positive,predicted_negative)


tp<-sum(performance_data$observed=="Yes" & performance_data$predicted=="Yes")
tn<-sum(performance_data$observed=="No" & performance_data$predicted=="No")
fp<-sum(performance_data$observed=="No" & performance_data$predicted=="Yes")
fn<-sum(performance_data$observed=="Yes" & performance_data$predicted=="No")
data.frame(tp,tn,fp,fn)

accuracy <- (tp+tn)/total
error_rate <- (fp+fn)/total
sensitivity <- tp/positive
especificity <- tn/negative
precision <- tp/predicted_positive
npv <- tn / predicted_negative
data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)

# Cambiando el cutoff
predicted_value <- predict(logit_reg,test,type = "response")
predicted_class <- ifelse(predicted_value>0.25, "Yes","No")
performance_data<-data.frame(observed=test$default,
           predicted= predicted_class)

positive <- sum(performance_data$observed=="Yes")
negative <- sum(performance_data$observed=="No")
predicted_positive <- sum(performance_data$predicted=="Yes")
predicted_negative <- sum(performance_data$predicted=="No")
total <- nrow(performance_data)
tp<-sum(performance_data$observed=="Yes" & performance_data$predicted=="Yes")
tn<-sum(performance_data$observed=="No" & performance_data$predicted=="No")
fp<-sum(performance_data$observed=="No" & performance_data$predicted=="Yes")
fn<-sum(performance_data$observed=="Yes" & performance_data$predicted=="No")
accuracy <- (tp+tn)/total
error_rate <- (fp+fn)/total
sensitivity <- tp/positive
especificity <- tn/negative
precision <- tp/predicted_positive
npv <- tn / predicted_negative
data.frame(tp,tn,fp,fn)

data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)

# Curva ROC
roc_data <- function(model, test, step=0.01) {
  out<-data.frame()
  cut <- seq(step, 1, by = step)
  for (i in cut) {
  predicted_value <- predict(logit_reg, test, type = "response")
  predicted_class <- ifelse(predicted_value > i, "Yes", "No")
  performance_data <- data.frame(observed = test$default,predicted = predicted_class)
  predicted_positive <- sum(performance_data$predicted=="Yes")
  predicted_negative <- sum(performance_data$predicted=="No")
  positive <- sum(performance_data$observed == "Yes")
  negative <- sum(performance_data$observed == "No")
  total <- nrow(performance_data)
  tp <-sum(performance_data$observed == "Yes" &performance_data$predicted == "Yes")
  tn <-sum(performance_data$observed == "No" & performance_data$predicted == "No")
  fn<-sum(performance_data$observed=="Yes" & performance_data$predicted=="No")
  sensitivity <- tp / positive
  especificity <- tn / negative
  precision <- tp / predicted_positive
  npv <- tn / predicted_negative
  fpr <- fn/negative

  out<-rbind(out,c(i,1-especificity,sensitivity,especificity,precision,npv,fpr))
  }
  names(out)<-c("cut","1-Especificity","Sensivity","Especificity","Precision","npv","fpr")
  return(out)
}

roc_graph <- roc_data(logit_reg,test,step = 0.01)
plot(roc_graph$`1-Especificity`,roc_graph$Sensivity,xlab = "1-especificity", ylab = "Sensitivity",type = "l")
index<-seq(1,nrow(roc_graph), by=nrow(roc_graph)*0.05)
text(roc_graph$`1-Especificity`[index],roc_graph$Sensivity[index], labels = roc_graph$cut[index], cex=0.6, pos=4)
