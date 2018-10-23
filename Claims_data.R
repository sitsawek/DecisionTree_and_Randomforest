#Load data set published by the United States Centers for Medicare and Medicaid Services.
#Structured to represent a sample of patients in the Medicare program.
#Which provides health insurance to Americans age 65 and older.
claims = read.csv("ClaimsData.csv")
str(claims)
#Compute percentage of patients in each cost bucket.
table(claims$bucket2009)/nrow(claims)
#Our goal will be to predict the cost bucket the patient fell into in 2009 using a CART model.
#First we need to split data to training set and testing set.
library(caTools)
set.seed(88)
split = sample.split(claims$bucket2009,SplitRatio = 0.6)
train = subset(claims,split == TRUE)
test = subset(claims,split == FALSE)
#Find average age of patient in training set.
mean(train$age)
#What proportion of people in the training set had at least one diagnosis code for diabetes?
table(train$diabetes)/nrow(train)
#See how the baseline method perform on this data set.
#Cost bucket on 2009 will be the same as it was in 2008.
mb = table(test$bucket2009,test$bucket2008)
#Observation it correct is
(110138+10721+2774+1539+104)/nrow(test) 
#How penalty error?
#Create penalty matrix.
p_matrix = matrix(c(0,1,2,3,4
                    ,2,0,1,2,3
                    ,4,2,0,1,2
                    ,6,4,2,0,1
                    ,8,6,4,2,0),byrow=TRUE,nrow = 5)
#Row as outcome and column as predict.
#If we want to change axis we can use transpose t() and change first in table to bucket2008.
#Now let change table as matrix for multiplication with p_matrix.
m = as.matrix(table(test$bucket2009,test$bucket2008))*p_matrix
#Now compute penalty error.
sum(m)/nrow(test)
#Baseline of all 1
m1 = table(test$bucket2009)
m1[1]
#Error of all 1
me1 = (122978*0+34840*2+16390*4+7937*6+1057*8)
me1/sum(m1)
mb
#Build model CART.
library(rpart)
library(rpart.plot)
str(claims)
claims_tree = rpart(bucket2009~age+alzheimers+arthritis+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke+reimbursement2008+bucket2008,data = train,method = "class",cp = 0.00005)
prp(claims_tree)
pred_test = predict(claims_tree,newdata = test,type = "class")
ct = as.matrix(table(test$bucket2009,pred_test))
#Now find accuracy from model.
(114141+16102+118+201)/nrow(test)
#Find penalty matrix.
sum(ct*p_matrix)/nrow(test)
#When we increase accuracy the penalty error also went up.
#Because by default rpart will try to maximize the overall accuracy.
#And every type of error is seen as having a penalty of one.
#The rpart function allows us to specify a parameter called loss.
claims_tree = rpart(bucket2009~age+alzheimers+arthritis+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke+reimbursement2008+bucket2008,data = train,method = "class",cp = 0.00005,parms = list(loss=p_matrix))
pred_test = predict(claims_tree,newdata = test,type = "class")
ct = as.matrix(table(test$bucket2009,pred_test))
#Find accuracy on new model.
(94310+18942+4692+636+2)/nrow(test)
#Find penalty matrix.
sum(ct*p_matrix)/nrow(test)
