setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(dplyr)
library(pscl)
library(missMethods)
library(randomForest)
library(missForest)
#The file "DataSceintist-Case-Dataset.xlsx" is saved as a csv file under the name "case_data.csv"

#Load data and exclude the parameter "customer_id"
dataset <- read.csv("case_data.csv", header = T)
dataset <- as.data.frame(dataset[,2:10])

#make sure, that the target variable is a boolean
dataset[,1] <- as.factor(dataset[,1])

#Investigate the dataset
nrow(dataset)
sapply(dataset, n_distinct)

data.frame(table(dataset[,8]))
#Exclude the parameter "credit_account_id" according to the resons mentioned in the report
dataset <- dataset[,-8]

#Handle missing data
colnames(dataset)[colSums(is.na(dataset)) > 0]
nrow(dataset[is.na(dataset$age),])
#dataset <- impute_mean(dataset, type = "columnwise")
set.seed(1234)
dataset <- missForest(dataset)$ximp

#Build the logistic regression model
model <- glm(dataset$converted~., data = dataset, family = binomial(link = "logit"))

summary(model)
anova(model, test = "Chisq")

#Find the McFadden R^2 index for when all parameters are used
McFadden_full <- pR2(model)[4]

#Initiate a table for the increasements in the McFadden R^2 indexes
r_sqrd_increasement <- matrix(0, nrow=1, ncol = ncol(dataset)-1)
colnames(r_sqrd_increasement) <- colnames(dataset[,2:ncol(dataset)])

#Loop that build a logistic regression model where each of the parameters are excluded and then find the increasement i the McFadden R^2 index
for(i in 2:(ncol(dataset)-1)){
  reduced_model <- glm(converted~., data = dataset[,-i], family = binomial(link = "logit"))
  #print( McFadden_full - pR2(reduced_model)[4])
  r_sqrd_increasement[1,i-1] <- McFadden_full - pR2(reduced_model)[4]
}

r_sqrd_increasement

