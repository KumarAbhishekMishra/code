#######################################################################################
#Example 1: mtcars
# Decision Tree
data(mtcars)
str(mtcars)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)

# Using rpart function of rpart package
# Step 1:Split data in train and test data
library(caTools)
set.seed(1278) 

split <- sample.split(mtcars, SplitRatio = 0.8)
split

train <- subset(mtcars, split== "TRUE")
test <- subset(mtcars, split== "FALSE")

str(train)
str(test)

# Step 2: Train model using rpart function of rpart package 
install.packages("rpart")
library("rpart") ## recursive partitioning
?rpart
DecTreeModel <- rpart(am ~ ., data = train, method = "class")

# See the decision tree
#install.packages("rpart.plot")
library("rpart.plot")
rpart.plot(DecTreeModel)

# Step 3:predict on test data
fitted.value <- predict(DecTreeModel, newdata = test, type = "class")

#Step 4:Evaluate the model accuracy
table(test$am, fitted.value)
misClassError <- mean(fitted.value != test$am)
print(paste('Accuracy =',1-misClassError))

#######################################################################################
#Example 3: More Than 2 levels
#######################################################################################
## Decision Trees IRIS
#install.packages("rpart")
#install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(e1071)


head(iris)
iris
decision_tree_model<- rpart(Species ~ ., data = iris, method = "class")
decision_tree_model
rpart.plot(decision_tree_model)

iris$Species_Predicted<-predict(decision_tree_model,newdata=iris,type="class")


table(iris$Species,iris$Species_Predicted)

##############################################################################
## Running the model in test and train
library(caTools)
set.seed(123) 

split <- sample.split(iris, SplitRatio = 0.7)
split


train <- subset(iris, split== "TRUE")
test <- subset(iris, split== "FALSE")
train
test

# Train model on train data
decision_tree_model<-rpart(Species ~ ., data = train, method = "class")

#Detailed information about the each node
summary(decision_tree_model)

#Plot the decision tree using plot fucntion
plot(decision_tree_model, uniform = TRUE, branch = 0.6, margin = 0.1)
text(decision_tree_model, all = TRUE, use.n = TRUE)

#Plot the decision tree using rpart.plot fucntion
rpart.plot(decision_tree_model)


# Predict Species on test data
test$Species_Predicted<-predict(decision_tree_model,newdata=test,type="class")
table(test$Species,test$Species_Predicted)

library(caret)
confusionMatrix(table(test$Species,test$Species_Predicted))

####################################################################################
#Tree Pruning

#If we look at the summary of decision_tree_model in the above code snippet, 
#it shows the statistics for all splits. 
decision_tree_model

#The printcp and plotcp functions provide the cross-validation error for each nsplit and can be used to prune the tree. 
#The one with least cross-validated error (xerror) is the optimal value of CP given by the printcp() function. 
#Minimum error occurs when the CP = 0.01
printcp(decision_tree_model) 
?printcp

#Minimum error occurs when the tree size is = 3
plotcp(decision_tree_model)
?plotcp
#Find the value of CP for which cross validation error is minimum
min(decision_tree_model$cptable[,"xerror"])
which.min(decision_tree_model$cptable[,"xerror"])
cpmin <- decision_tree_model$cptable[3, "CP"]

#Prune the tree by setting the CP parameter as =  cpmin
?prune
decision_tree_pruned = prune(decision_tree_model, cp = cpmin)
rpart.plot(decision_tree_pruned)

# Predict Species on test data
test$Species_Predicted<-predict(decision_tree_pruned,newdata=test,type="class")
table(test$Species,test$Species_Predicted)

library(caret)
confusionMatrix(table(test$Species,test$Species_Predicted))

