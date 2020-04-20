#lda
library(MASS)
names(iris)
dim(iris) # check the dimensions of the iris dataset, you will see 150 rows and 5 columns
head(iris)
set.seed(555)
Train <- sample(1:nrow(iris), nrow(iris)/2)
iris_Train <- iris[Train,] # Traning dataset
irist_Test <- iris[-Train,] # Testing dataset
fit1 <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris_Train)
predict1 <- predict(fit1, iris_Train)
predict1_class <- predict1$class
table1 <- table(predict1_class, iris_Train$Species)
table1
# Calculating the Accuracy of the prediction
sum(diag(table1))/sum(table1)


# LOESS 
# http://r-statistics.co/Loess-Regression-With-R.html
data(economics, package="ggplot2")  # load data
economics$index <- 1:nrow(economics)  # create index variable
economics <- economics[1:80, ]  # retail 80rows for better graphical understanding
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

# Predict Loess
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 
# From above plot, you would notice that as the span increases, the smoothing of the curve also increases.
# Code for Plot
# Plot it
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction", xlab="Date", ylab="Unemployment (Median)")
lines(smoothed10, x=economics$date, col="red")
lines(smoothed25, x=economics$date, col="green")
lines(smoothed50, x=economics$date, col="blue")
#lowess
data("cars")
plot(speed ~ dist, data = cars)
help("lowess")
lowess(cars$speed ~ cars$dist)
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue") 
lines(lowess(cars$speed ~ cars$dist, f=0.75), col="gray")  # f = 0.75
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="red")  # f = 0.8
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="green")  # f = 0.9
lines(lowess(cars$speed ~ cars$dist, f=0.1), col= 5)  # f = 0.1
lines(lowess(cars$speed ~ cars$dist, f=0.01), col= 6)  # f = 0.01 

