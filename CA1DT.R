# Install libraries
if (!require("rpart")) install.packages("rpart")
library(rpart)
if (!require("partykit")) install.packages("partykit")
library(partykit)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

###################Prepare Data################### 
rm(list=ls())

# Read CSV file, seperate by ';'
Data <- read.csv("ProjectData.csv", header=TRUE, sep= ";")

# See categories of Variables using STR function 
str(Data)
summary(Data)

# Convert categorical variables into factors & remove column 1
Data=Data[,-1]
cols <- c(1:9)
Data[cols] <- lapply(Data[cols], factor)

# Amount of rows with NA values
sum(!complete.cases(Data))

################### Partition Data into training and validation datasets ###################
n=nrow(Data) 
indexes = sample(n, n*(80/100)) 
trainset= Data[indexes,] 
testset = Data[-indexes,] 

###################  Model 1 - Decision Tree model - X's for all data #####################################################

# Build, Plot and print  DT model
DT_Model <-rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

###Build Model using trainset###
rpart_model <- rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion Matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 1 =', accuracy * 100))

###################  Model 2 - Decision Tree model over - Y's for all data ########################################


# Build, Plot and print  DT model
DT_Model <-rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

###Build Model using trainset###
rpart_model <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 2 =', accuracy * 100))

###################  Model 3 - Decision Tree model - X's and Y's for all data ########################################

# Remove Column "Group"
Data1 <- Data %>% select(-c(Group))

# Create Decision tree model on entire dataset minus group column, plot & print
DT_Model <-rpart(Response~., data=Data1)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Create new test and train set without group
n=nrow(Data1) 
indexes = sample(n, n*(80/100)) 
trainset= Data1[indexes,] 
testset = Data1[-indexes,] 


###Build Model using trainset###
rpart_model <- rpart(Response~., data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 3 =', accuracy * 100))

###################  Model 4 - Decision Tree model - X's for Group 0 ########################################

#select rows where Group=0
Data2 <- Data [Data$Group==0,]

# Build, Plot and print  DT model
DT_Model <-rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data2)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Create new test and train set
n=nrow(Data2) 
indexes = sample(n, n*(80/100)) 
trainset= Data2[indexes,] 
testset = Data2[-indexes,] 

###Build Model using trainset###
rpart_model <- rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 4 =', accuracy * 100))


###################  Model 5 - Decision Tree model - Y's for Group 0 ########################################

# Build, Plot and print  DT model
DT_Model <-rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data2)#,  maxdepth=4)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Create new test and train set
n=nrow(Data2) 
indexes = sample(n, n*(80/100)) 
trainset= Data2[indexes,] 
testset = Data2[-indexes,] 

###Build Model using trainset###
rpart_model <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)


# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 5 =', accuracy * 100))

###################  Model 6 - Decision Tree model - X's and Y's for Group=0 ########################################

# Remove Column "Group"
Data2 <- Data2 %>% select(-c(Group))

# Build, Plot and print  DT model
DT_Model <-rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data2)#,  maxdepth=4) 
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Create new test and train set
n=nrow(Data2) 
indexes = sample(n, n*(80/100)) 
trainset= Data2[indexes,] 
testset = Data2[-indexes,] 

###Build Model using trainset###
rpart_model <- rpart(Response~., data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 6 =', accuracy * 100))

###################  Model 7 - Decision Tree model - X's for Group=1 ########################################

#select only rows where Group=1
Data3 <- Data [Data$Group==1,]

# Build, Plot and print  DT model
DT_Model <-rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data3)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Create new test and train set
n=nrow(Data3) 
indexes = sample(n, n*(80/100)) 
trainset= Data3[indexes,] 
testset = Data3[-indexes,] 

###Build Model using trainset###
rpart_model <- rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 7 =', accuracy * 100))

###################  Model 8 - Decision Tree model - Y's for Group=1 ########################################

# Build, Plot and print  DT model
DT_Model <-rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data3)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Create new test and train set
indexes = sample(n, n*(80/100)) 
trainset= Data3[indexes,] 
testset = Data3[-indexes,] 

###Build Model using trainset###
rpart_model <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 8 =', accuracy * 100))

###################  Model 9 - Decision Tree model - X's and Y's for Group=1 ########################################

# Remove Column "Group"
Data3 <- Data3 %>% select(-c(Group))

# Build, Plot and print  DT model
DT_Model <-rpart(Response~., data=Data3)
plot(as.party(DT_Model))
print(DT_Model)

# Pruning a decision tree
print(DT_Model$cptable)

# This line of code identifies tree with least error
opt <- which.min(DT_Model$cptable [, "xerror"])
cp <- DT_Model$cptable [opt,"CP"]

# this line uses the complexity parameter to select the optimal tree size, Plot and Print results
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

# Create new test and train set
n=nrow(Data3) 
indexes = sample(n, n*(80/100)) 
trainset= Data3[indexes,] 
testset = Data3[-indexes,] 

rpart_model <- rpart(Response~., data=trainset, method="class")
plot(as.party(rpart_model))
print(rpart_model)

# Make a prediction using the test dataset
pred = predict(rpart_model, testset, type = "class")   

# Confusion matrix
matrix_table <- table(testset$Response, pred)
matrix_table

# Accuracy
accuracy <- sum(diag(matrix_table)) / sum(matrix_table)
print(paste('Accuracy for model 9 =', accuracy * 100))

## END




