## Load the Data
Attrition <- read.csv("C:/Users/Gaurav Jetley/Desktop/Attrition/WA_Fn-UseC_-HR-Employee-Attrition.csv")
View(Attrition)

names(Attrition)

# ## Clean Data / Transform Variables
# Attrition$Age <- as.numeric(Attrition$Age)
# Attrition$Attrition <- as.factor(Attrition$Attrition)
# Attrition$BusinessTravel <- as.factor(Attrition$BusinessTravel)
# Attrition$DailyRate <- as.numeric(Attrition$DailyRate)
# Attrition$Department <- as.factor(Attrition$Department)
# Attrition$DistanceFromHome <- as.numeric(Attrition$DistanceFromHome)
# Attrition$Education <- as.numeric(Attrition$Education)
# Attrition$EducationField <- as.factor(Attrition$EducationField)
# Attrition$EmployeeCount <- as.numeric(Attrition$EmployeeCount)
# Attrition$EmployeeNumber <- as.numeric(Attrition$EmployeeCount)
# Attrition$EnvironmentSatisfaction <- as.numeric(Attrition$EnvironmentSatisfaction)


class(Attrition)
class(Attrition$Age)
class(Attrition$BusinessTravel)
(as.numeric(Attrition$Attrition))
(as.numeric(Attrition$Attrition)-1)

now(Attrition)

runif(n = nrow(Attrition)*0.7, min = 1, max = nrow(Attrition))

#########################
## Logistic Regression ##
#########################
fit <- glm((as.numeric(Attrition)-1) ~ . ,
           data = Attrition[ ,c(-22,-27)])

summary(fit)

## Visit the following website for how to interpret the results
## https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

# This fitted model says that, holding math and reading at a fixed value, the odds of getting into an 
# honors class for females (female = 1)over the odds of getting into an honors class for males (female = 0) 
# is exp(.979948) = 2.66.  In terms of percent change, we can say that the odds for females are 166% higher 
# than the odds for males.  The coefficient for math says that, holding female and reading at a fixed value, 
# we will see 13% increase in the odds of getting into an honors class for a one-unit increase in math 
# score since exp(.1229589) = 1.13.

exp(-4.040e-02) # Increases chance of attrition by -(1-0.96)%
exp(1.081e-02) # Increases chance of attrition by -(1-1.01)%
exp(-5.800e-02) # 

-(1 - exp(-4.040e-02))
-(1 - exp(1.081e-02))
-(1 - exp(-5.800e-02))


predict(object = fit, newdata = Attrition[1 ,c(-2, -22,-27)])
Attrition[1,2]


predict.glm(object = fit, newdata = Attrition[2 ,c(-2, -22,-27)])
Attrition[2,2]


predict.glm(object = fit, newdata = Attrition[3 ,c(-2, -22,-27)])
Attrition[3,2]


predict.glm(object = fit, newdata = Attrition[4 ,c(-2, -22,-27)])
Attrition[4,2]


predict.glm(object = fit, newdata = Attrition[5 ,c(-2, -22,-27)])
Attrition[5,2]

predicted <- predict.glm(object = fit, newdata = Attrition[ ,c(-2, -22,-27)])
predicted[predicted >= 0.5] <- 1
predicted[predicted < 0.5] <- 0

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
confusionMatrix(data = as.factor(predicted), 
                reference=as.factor((as.numeric(Attrition$Attrition)-1)))


###################
## Decision Tree ##
###################
install.packages("rpart")
library(rpart)
install.packages("rpart.utils")
library(rpart.utils)
install.packages("rpart.plot")
library(rpart.plot)


fit_tree <- rpart(Attrition ~ .,
                    data = Attrition[ ,c(-22,-27)])
summary(fit_tree)
rpart.plot(fit_tree,cex = 0.8)


predicted_tree <- predict(object = fit_tree, newdata = Attrition[ ,c(-2, -22,-27)])
confusionMatrix(data = as.factor(predicted_tree), 
                reference=as.factor(Attrition$Attrition))

###################
## Random Forest ##
###################
install.packages("randomForest")
library(randomForest)

fit_rf <- randomForest(Attrition ~ . ,
                       data = Attrition[ ,c(-22,-27)])
summary(fit_rf)

predicted_rf <- predict(object = fit_rf, newdata = Attrition[ ,c(-2, -22,-27)])
confusionMatrix(data = as.factor(predicted_rf), 
                reference=as.factor(Attrition$Attrition))




