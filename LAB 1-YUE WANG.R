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
