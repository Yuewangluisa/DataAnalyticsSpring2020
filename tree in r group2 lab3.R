library(ggplot2)
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
# regression tree
help('msleep')
data('msleep')
str(msleep)
mSleepDF1 <- msleep[,c(3,6,10,11)] 
str(mSleepDF1)
head(mSleepDF1)
sleepModel_1 <- rpart(sleep_total ~ ., data=mSleepDF1, method = "anova")
sleepModel_1
rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)
rpart.plot(sleepModel_1, type = 3,digits = 3, fallen.leaves = TRUE) # with 3 digits 
rpart.plot(sleepModel_1, type = 3,digits = 4, fallen.leaves = TRUE)

# classification tree
install.packages("C50")
require(C50)
data("iris")
head(iris)
str(iris)
table(iris$Species)
set.seed(9850)
#rows are now randomly shuffled
grn <-runif(nrow(iris))
irisrand <-iris[order(grn),]
str(irisrand)
classificationmodel1 <-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1
table(irisrand[101:150,5],prediction1)
help("table")
table(irisrand[101:150,5],Predicted = prediction1)
plot(classificationmodel1)
# naive bayers
library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green") 
#lab 3 ctree 1 
require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(Swiss_rpart) # try some different plot options
text(Swiss_rpart) # try some different text options

require(party)
treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)
cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))
treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
install.packages('tree')
library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr,main="CI Tree")
text(tr)
# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)
#
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")
fitK <- rpart(Kyphosis ~ Age + Number + Start, method="class", data=kyphosis)
printcp(fitK) # display the results
plotcp(fitK) # visualize cross-validation results
summary(fitK) # detailed summary of splits
# plot tree
plot(fitK, uniform=TRUE, main="Classification Tree for Kyphosis")
text(fitK, use.n=TRUE, all=TRUE, cex=.8)
# create attractive postscript plot of tree
post(fitK, file = "kyphosistree.ps", title = "Classification Tree for Kyphosis") # might need to convert to PDF (distill)

pfitK<- prune(fitK, cp=   fitK$cptable[which.min(fitK$cptable[,"xerror"]),"CP"])
plot(pfitK, uniform=TRUE, main="Pruned Classification Tree for Kyphosis")
text(pfitK, use.n=TRUE, all=TRUE, cex=.8)
#post(pfitK, file = â€œptree.ps", title = "Pruned Classification Tree for Kyphosis")

# RANDOM FOREST
install.packages('randomForest')
require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results
importance(fitKF) # importance of each predictor
#
fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data = swiss)
print(fitSwiss) # view results
importance(fitSwiss) # importance of each predictor
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss,1, labelVar=TRUE)

help(randomForest) # look at all the package contents and the randomForest method options

# look at rfcv - random forest cross-validation - 
help(rfcv)

