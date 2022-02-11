##Cargar librerias
install.packages("Boruta")
install.packages("mlbench")
install.packages("randomForest")
install.packages("Rcpp")
install.packages("MASS")

library(Rcpp)
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)
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
library(factoextra)
library(corrplot)
library(corrplot)
library(ggplot2)
library(janitor)
library(readxl)
library(plotly)
library(dplyr)
library(DT)
library(BSDA)
library(reshape2)
library(MASS)


base_t11 <- read_delim("C:/Postgrado/Seminario de grado/base_t11.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
bd_fin<-base_t11

###CONVERSION VARIABLE RESPUESTA EN DUMMY
bd_fin$Attrition<-ifelse(bd_fin$Attrition=="No",0,1)

###Seleccion de variables
set.seed(1234)
boruta <- Boruta(Attrition ~ ., data = bd_fin, doTrace = 2, maxRuns = 500)

print(boruta)
plot(boruta, las = 2, cex.axis = 0.55, xlab= NULL)


bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)


##Otro metodo de selección de variables (Vamos a crear un modelo saturado, es decir, el modelo mayor a considerar)
full.model <- lm(Attrition ~ ., data=bd_fin)
summary(full.model)

##Aplicación de método backward
modback <- stepAIC(full.model, trace=TRUE, direction="backward")
modback$anova
summary(modback)


##Aplicación modelo Forward
empty.model <- lm(Attrition ~ 1, data=bd_fin)
horizonte <- formula(Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome + 
                       Education + EducationField + EmployeeNumber + EnvironmentSatisfaction + 
                       Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + 
                       JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + 
                       NumCompaniesWorked + OverTime + PercentSalaryHike + PerformanceRating + 
                       RelationshipSatisfaction + StockOptionLevel + TotalWorkingYears + 
                       TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + 
                       YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager)

modforw <- stepAIC(empty.model, trace=FALSE, direction="forward", scope=horizonte)
modforw$anova
summary(modforw)


###Base con la seleccion de variables
Base_nueva <- bd_fin %>% dplyr::select(Attrition, Age, Department, EnvironmentSatisfaction,
                                       JobInvolvement, JobLevel, JobRole, JobSatisfaction , MaritalStatus, MonthlyIncome,
                                       NumCompaniesWorked, OverTime, StockOptionLevel, TotalWorkingYears, WorkLifeBalance, YearsAtCompany, YearsInCurrentRole,
                                       YearsSinceLastPromotion, YearsWithCurrManager, RelationshipSatisfaction, YearsWithCurrManager, Gender, DistanceFromHome, BusinessTravel, TrainingTimesLastYear)



####Data Partition
split <- sample.split(Base_nueva$Attrition, SplitRatio = 0.75)
training_set <- subset(Base_nueva, split == TRUE)
test_set <- subset(Base_nueva, split == FALSE)



###GENERAR MODELO CON TRAINING
training_set$Attrition<-as.factor(training_set$Attrition)
modelo <- glm(Attrition ~ ., data = training_set, family = "binomial")
summary(modelo)


pred_train <- predict(modelo, type = 'response', newdata  = test_set)
pred_train$pred_train<- ifelse(pred_train > 0.6, "1", "0")
pred_train$pred_train<-as.factor(pred_train$pred_train)
test_set$Attrition<-as.factor(test_set$Attrition)
confusionMatrix(pred_train$pred_train,test_set$Attrition)

