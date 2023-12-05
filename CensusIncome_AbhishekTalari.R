#title: "CensusReport_AbhishekTalari"
#author: "Abhishek Talari"
#date: "2023-12-05"
# Introduction 

if(!require(rpart.plot)) 
install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(rattle)) 
install.packages("rattle", repos = "http://cran.us.r-project.org")
library(readr)
library(rpart)
library(tidyverse)
library(ggplot2)
library(caret)
library(dplyr)
library(knitr)
library(magrittr)
library(rpart.plot)
library(rattle)
library(randomForest)
adult <- read.csv("adult_data.csv")

#Let us look at the structure of the data.
str(adult)

#Let us first search for any 'NA' values present in the data set.
adult %>% anyNA()

#For simplicity of this analysis, i) the weighting factor and ii) relationship (Role in the family can be assessed from gender and marital status) are discarded. 
#Thus, the following 2 variables are deleted - relationship and fnlwgt.
adult <- subset(adult,select = -c(fnlwgt,relationship))

# Explotory Analysis 

#boxplot for all continuous variables. 

p1 <- adult %>% ggplot(aes(income , age, fill= income)) + geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p2 <- adult %>% ggplot(aes(income , hours.per.week,fill= income)) + geom_boxplot(alpha=0.3)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p3 <- adult %>% ggplot(aes(income , education.num,fill= income)) + geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p4 <- adult %>% ggplot(aes(income , capital.gain,fill= income)) + geom_boxplot(alpha=0.3)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
p5 <- adult %>% ggplot(aes(income , capital.loss,fill= income)) + geom_boxplot(alpha=0.3)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
gridExtra::grid.arrange(p1 , p2 , p3, p4 ,p5, ncol=2)

# plot for work class variable comparision 
adult %>% ggplot(aes(x = workclass ,fill= income)) + geom_bar() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

# plot for education variable comparision 
adult %>% ggplot(aes(education)) + geom_bar(aes(fill = income)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

# plot for occupations variable comparison 
adult %>% ggplot(aes(x= occupation)) + geom_bar(aes(fill = income ))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

# Plot for sex comparison in relation to income
 adult %>% ggplot(aes(adult$sex)) + geom_bar(aes(fill = income ))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  scale_fill_brewer(palette="Paired")

# Data Partition

trainIndex <- createDataPartition(adult$income, times=1,p =0.8, list=FALSE)
train <- adult[trainIndex,]
test <-  adult[-trainIndex,]


# Machine Learning Techniques - Model Fitting 

###Logistic Regression Model

train$income = factor(train$income)
censusglm <- glm( income ~ . , family = binomial , data = train )
summary(censusglm)

# confusion matrix
prob <- predict(censusglm, test , type = 'response')
pred <- rep('<=50K', length(prob))
pred[prob>=.5] <- '>50K'
tb <- table(pred, test$income)
kable(tb)

#Computed accuracy 
glm_accuracy <- sum(diag(tb))/sum(tb)
glm_accuracy
auc_results <- data_frame(Model = "Generalized Linear Model", Accuracy = glm_accuracy)

kable(auc_results)


### Decision Tree Model

censustree <- rpart( income ~ . , method="class", data = train)

# tree plot 
rpart.plot(censustree)

censustree$variable.importance

#Confusion matrix:
t_prob <- predict(censustree , test , type = "class" )
tb2 <- confusionMatrix(t_prob,as.factor(test$income))
tb2

#Computed accuracy:
tree_auc <- tb2$overall['Accuracy']
auc_results <- data_frame(Model = "Decision Tree Model", Accuracy = tree_auc)
kable(auc_results, row.names = 0 )


### Random Forest Model 
train$income = factor(train$income)
censusforest <- randomForest(income ~ . ,data = train,importance = TRUE)
censusforest

#Confusion matrix:
r_pred <- predict(censusforest , test )
tb3<- confusionMatrix(r_pred,as.factor(test$income))
tb3
rf_auc <- tb3$overall['Accuracy']

#Computed accuracy:
auc_results <- rbind(auc_results, data.frame(Model = "Random Forest Model", Accuracy = rf_auc ))
kable(auc_results,row.names = 0)