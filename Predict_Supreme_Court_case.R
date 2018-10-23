#Read data file to predict case of stevens.
stevens = read.csv("stevens.csv")
str(stevens)
#Now split data to training set and test set.
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse,SplitRatio = 0.7)
train = subset(stevens,spl == TRUE )
test = subset(stevens,spl == FALSE)
#To prediction decision tree model we need to use library below.
library(rpart)
library(rpart.plot)
stevens_tree = rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method = "class",minbucket = 25)
#Use all features to predict except Docket and Term and set method = "class" for classification tree.
#If not this model will predict regresstion tree.
#And set minbucket = 25 to limits the tree that it doesn't overfit to our trainning set.
summary(stevens_tree)
#Plot tree by prp function to see our tree.
prp(stevens_tree)
#Now predict CART
pred_cart = predict(stevens_tree,newdata = test,type = "class")
table(test$Reverse,pred_cart)
#Compute accuracy
(41+71)/(41+36+22+71)
#Find baseline accuracy from
table(test$Reverse)
93/(77+93)
#Lastly evaluate model by ROCR to find best threshold.
library(ROCR)
pred_roc = predict(stevens_tree,newdata = test)
#Now generate ROCR curve by second column of pred_roc.
pred = prediction(pred_roc[,2],test$Reverse)
#Now use perform function.
perf = performance(pred,"tpr","fpr")
plot(perf,colorize = TRUE,print.cutoffs.at =seq(0,1,0.1),text.adj=c(-0.2,1.7))
#Now let compute AUC.
as.numeric(performance(pred, "auc")@y.values)
#Build model random forest
library(randomForest)
#Before build model we need to make sure outcome is a factor.
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
stevens_forest = randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,nodesize = 25,ntree = 200)
#Set ntree = 200 this is a number of tree to build.
#Compute prediction.
pred_forest = predict(stevens_forest,newdata = test)
table(test$Reverse,pred_forest)
#Compute accuracy.
(40+76)/(40+37+17+76)
#Accuracy of CART around 65.9%.
#Accuracy of logistic regression around 66.5%
#Accuracy of random forest around 68.2%
#Now we'll define cross validation experiment.
library(caret)
library(e1071)
#Set cv mean cross validation and number 10 is number of folds.
num_folds = trainControl(method="cv",number = 10)
#We need to pick possible values for cp.
cp_grid = expand.grid(.cp=seq(0.01,0.5,0.01)) # from 0.01 to 0.5 step by 0.01
train(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method="rpart",trControl=num_folds,tuneGrid=cp_grid)
#From result we can see final value is cp = 0.18
#Create new model by use cp
stevens_tree_cv = rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data = train,method = "class",cp = 0.18)
pred_cv = predict(stevens_tree_cv,newdata = test,type = "class")
table(test$Reverse,pred_cv)
#Accuracy of cv is 72.3% 
(59+64)/(59+18+29+64)
#Cross validation help to make sure good parameter value.
#And often this will significant increase the accuracy.
#Plot decision tree.
prp(stevens_tree_cv)
#It available only one split because we already choose best parameter by cross validation. 
