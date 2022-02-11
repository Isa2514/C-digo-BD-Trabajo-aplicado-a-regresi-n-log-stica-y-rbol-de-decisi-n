##Cargar librerias

library(rattle)
library(rpart)
library(rpart.plot)
library(ROSE)
library(dplyr)
library(fastDummies)
library(ellipsis)
library(caTools)
library(corrplot)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(readr)
library(caret)




###SELECCIONAR VARIABLES NÚMERICAS
##cONVERSION VARIABLE RESPUESTA EN DUMMY
Base_nueva2  <- bd_fin %>% dplyr::select(Attrition,EnvironmentSatisfaction,
                                JobInvolvement,JobRole,JobSatisfaction ,NumCompaniesWorked,OverTime,WorkLifeBalance,YearsInCurrentRole,
                                YearsSinceLastPromotion,YearsWithCurrManager,RelationshipSatisfaction,DistanceFromHome,BusinessTravel)










###################################################################################
#Base_nueva2$OverTime<-ifelse(Base_nueva2$OverTime=="No",0,1)
###DIVIDIR BD EN TEST Y TRAINING
set.seed(1234)
options(scipen = 999)
split <- sample.split(Base_nueva2$Attrition, SplitRatio = 0.75)
training_set <- subset(Base_nueva2, split == TRUE)
test_set <- subset(Base_nueva2, split == FALSE)


##Arbol de decision con training_set
tree<- rpart(Attrition ~ ., data =   training_set,minbucket=5)
rpart.plot(tree,cex=0.6)
fancyRpartPlot(tree,cex=0.8)


pred_train <- predict(tree, newdata  = test_set)
pred_train<-as.data.frame(pred_train)
pred_train$pred_train<- ifelse(pred_train > 0.6, "1", "0")
pred_train$pred_train<-as.factor(pred_train$pred_train)
test_set$Attrition<-as.factor(test_set$Attrition)


#confusionMatrix(pred_train$pred_train,test_set$Attrition)
confusionMatrix(pred_train$pred_train,test_set$Attrition)




