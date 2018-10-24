#Load data set from UCI Machine Learning Repository.
#To predict house price in boston.
boston = read.csv("boston.csv")
str(boston)
#Each entry corresponds to cencus tract, a statistical division of the area that is use by resercher to break down towns and cities.
#There will usually be multiple cencus tracts per town.
#LON and LAT are longtitude and latitude of the center of the cencus tract.
#MEDV is a median value of owner-occupied homes,in thousands of dollars.
#CRIM is the per capita crime rate.
#ZN is related to how much of the land is zoned for large residential properties.
#INDUS is proportion of area used for industry.
#CHAS is 1 if cencus tract is next to the Charles River.
#NOX is the concentration of nitrous oxides in the air.
#RM is average number of rooms per dwelling.
#AGE is the proportion of owner-occupied units builts before 1940.
#DIS is measure of how far the tract is from centers of employment in Boston.
#RAD is a measure of closeness to important hihtways.
#TAX is the property tax rate per $10000 of value.
#PTRATIO is the pupil-teacher ratio by town.
#How price vary by location across a region. 
plot(boston$LON,boston$LAT)
#Find poins are near Chares River.
points(boston$LON[boston$CHAS=="1"],boston$LAT[boston$CHAS=="1"],col = "blue",pch = 19)
#Find poins of MIT is cencus tract number 3531.
points(boston$LON[boston$TRACT=="3531"],boston$LAT[boston$TRACT=="1"],col = "red",pch = 19)
#Now investigate how air pollution effect house prices.
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col = "green",pch = 19)
#Clear and plot again.
plot(boston$LON,boston$LAT)
summary(boston$MEDV)
#Plot to find price under average.
points(boston$LON[boston$MEDV >= 21.2],boston$LAT[boston$MEDV>= 21.2],col = "red",pch = 19)
#Plot correlation MEDV between LON and LAT.
plot(boston$LON,boston$MEDV)
plot(boston$LAT,boston$MEDV)
#Try to fit model with linear regression.
latlonlm = lm(MEDV~LON+LAT,data = boston)
summary(lat1onlm)
#Result quite bad at R square = 0.101
#And we can see LON has significant.
#Replot again.
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV >= 21.2],boston$LAT[boston$MEDV>= 21.2],col = "red",pch = 19)
#What does the linear regression model think is above median.
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2],boston$LAT[latlonlm$fitted.values>=21.2],col = "blue",pch = "$")
#We can see points of linear regression pretty vertical because LAT not significant in linear regression. 
#Now build model by regression tree(not set method = "class").
library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV~LAT+LON,data = boston)
prp(latlontree)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV >= 21.2],boston$LAT[boston$MEDV>= 21.2],col = "red",pch = 19)
fittedvalue = predict(latlontree)
points(boston$LON[fittedvalue>=21.2],boston$LAT[fittedvalue>=21.2],col = "blue",pch = "$")
#We can see all done much better than linear regression.
#Try to get most of this effect with a much of simpler tree.
#By set minbucket.
latlontree = rpart(MEDV~LAT+LON,data = boston,minbucket = 50)
prp(latlontree)
# Or by plot.
plot(latlontree)
text(latlontree)
#First we split on longtitude and it was negative 71.07
#We focus on lowest price (at left from tree is 42.21).
#Mean under 42.21 is a lowest price we can see from tree(and near Charles River).
plot(boston$LON,boston$LAT)
abline(v=-71.07,h=42.21)
#Final split is bottom left corner 1 is 42.17.
abline(h=42.17)
#If we look at the righ side of middle of the tree.
#Rectangle on the righ side that is bucket we were predict.
#That is south low price boston area that we saw before.
points(boston$LON[boston$MEDV >= 21.2],boston$LAT[boston$MEDV>= 21.2],col = "red",pch = 19)
#Let's predict model with all variable.
#First split data to training set and test set.
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV,SplitRatio=0.7)
train = subset(boston,split==TRUE)
test = subset(boston,split==FALSE)
#Build model linear regression.
linreg = lm(MEDV~LON+LAT+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data = train)
summary(linreg)
#We can see LON and LAT is not significant.
#Now use linreg to predict.
pred_linreg = predict(linreg,newdata = test)
linreg_sse = sum((pred_linreg-test$MEDV)^2)
#Build model by tree.
tree = rpart(MEDV~LON+LAT+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data = train)
prp(tree)
tree_pred = predict(tree,newdata = test)
tree_sse = sum((tree_pred-test$MEDV)^2)
#If we look at sum square root tree is not good.
#Now use cross validation.
library(caret)
library(e1071)
tr_control = trainControl(method="cv",number = 10)
#cp_grid = expand.grid(.cp=seq(0.00,0.01,0.001))
cp_grid = expand.grid(.cp=(0:10)*0.001)
tr = train(MEDV~LON+LAT+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data = train,method="rpart",trControl=tr_control,tuneGrid = cp_grid)
#Best cp is 0.001
best_tree = tr$finalModel
prp(best_tree)
best_tree_pred = predict(best_tree,newdata = test)
best_tree_sse = sum((best_tree_pred-test$MEDV)^2)
#Compare sum square error with another model.
linreg_sse
tree_sse
best_tree_sse
#We can see tree not good more than linear regression but can improve by cross validation.