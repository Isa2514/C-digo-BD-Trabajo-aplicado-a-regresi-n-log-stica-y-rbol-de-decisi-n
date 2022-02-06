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






bd_fin$Attrition<-ifelse(bd_fin$Attrition=="No",0,1)
bd_fin$Attrition<-as.numeric(bd_fin$Attrition)
x=data.frame(bd_fin)
options(scipen = 999)
numericas = names(dplyr::select_if(x,is.numeric))
numericas = x %>% select(numericas)
numericas = data.frame(numericas)

#####
###DIVIDIR BD EN TEST Y TRAINING
set.seed(1234)
split <- sample.split(Base_nueva2$Attrition, SplitRatio = 0.75)
training_set <- subset(Base_nueva2, split == TRUE)
test_set <- subset(Base_nueva2, split == FALSE)

##Arbol de decision
tree<- rpart(Attrition ~ ., data =   test_set,minbucket=5)
rpart.plot(tree,cex=0.6)
fancyRpartPlot(tree,cex=0.6)

pred_train <- predict(tree, type = 'response', newdata  = test_set)
pred_train$pred_train<- ifelse(pred_train > 0.6, "1", "0")
pred_train$pred_train<-as.factor(pred_train$pred_train)
test_set$Attrition<-as.factor(test_set$Attrition)



#confusionMatrix(pred_train$pred_train,test_set$Attrition)
confusionMatrix(pred_train$pred_train,test_set$Attrition)






