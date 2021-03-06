# FIRST CLASS PRACTICE
print('DATA ANAKYTICS')
install.packages("ISLR") # installing the ISLR package
library(ISLR)
data("Auto")
head(Auto)
head(Auto,10)
tail(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$cylinders)
boxplot(Auto$mpg)
attach(Auto)
str(Auto)
help(read.csv)
data1<-read.csv(file.choose(),header=TRUE)
data1
is.na(data1)
hist(Auto$mpg)
install.packages('MASS')
library(MASS)
data('Boston')
attach(Boston)
??Bosotn
head(Boson)
dim(Boston)
names(Boston)
str(Boston)
nrow(Boston)
ncol(Boston)
summary(Boston)
summary(Boston$crim)
summary(crim)
help(data.frame)
days<-c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp<-c(28,30.5,32,31.2,29.3,27.9,26.3)
Weather_week=data.frame(days,temp)
head(Weather_week)
str(Weather_week)
summary(Weather_week)
Weather_week[1,]
#EPI DATA PART
EPI_data<-read.csv(file.choose(),header=TRUE,skip=1)
View(EPI_data)
attach(EPI_data)
help("fix")
fix(EPI_data)
EPI
tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI)
fivenum(EPI,na.rm = TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
help(hist)
lines(density(EPI,na.rm = TRUE,bw='SJ'))
rug(EPI)
#Cumulative Density Function
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
#Quantile-Quantile?
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x<-seq(30.95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for tdsn')
qqline(x)
# DALY
DALY
tf1 <- is.na(DALY)
hist(DALY)
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
boxplot(EPI,DALY)
qqplot(EPI,DALY)
qqplot(AIR_H,WATER_H)
EPILand<-EPI[!Landlock]
ELAND<-EPILand[!is.na(EPILand)]
hist(ELAND)
hist(ELAND,seq(30.,95.,1.0),prob=TRUE)
summary(EPI) 	# stats
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
rug(EPI) 

