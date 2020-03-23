rm(list=ls())
# mar 23th
# random forest
require(randomForest)
data("kyphosis",package="gam") 
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF)
importance(fitKF) 
#PCA
data("USArrests")
states=row.names(USArrests) 
states
names(USArrests )
apply(USArrests , 2, mean)
apply(USArrests , 2, var)
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
biplot(pr.out, scale=0)
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve
#pca on iris dataset
data("iris")
head(iris)
irisdata1 <- iris[,1:4]
irisdata1
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
# cor = a logical value indicating whether the calculation should 
#use the correlation matrix or the covariance matrix.
# (The correlation matrix can only be used if there are no constant variables.)
# score = a logical value indicating whether the score on  each principal component should be calculated.
summary(principal_components)
# in the summary you can see that it has four Principal Components it is because the input data has
# four different features.
plot(principal_components)
plot(principal_components, type = "l")
biplot(principal_components)


#PCA ON BOSTON DAtaset
install.packages('MASS')
data(Boston, package="MASS")

pca_out <- prcomp(Boston,scale. = T)
# pca_out shows the loadings that used.
pca_out
plot(pca_out)

# plotting using the biplot()
# Read the documentation for biplot() function in RStudio
help(biplot)

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
# boston_pc has the Princial Components having the same number of rows in the original dataset
head(boston_pc)
summary(boston_pc)
#
data(Titanic)

rpart, ctree, hclust, etc. for Survived ~ .
