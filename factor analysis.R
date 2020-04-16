
#Factor analysis

data <- read.csv("~/Desktop/dataset_EFA.csv")
#install.packages("psych")
library(psych)
corMat  <- cor(data)
corMat
solution <- fa(r = corMat, nfactors = 2, rotate = "oblimin", fm = "pa")
solution
# example 1
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")$scores
#example 2 
install.packages("Hmisc")
library(Hmisc)
AthleticsData <- spss.get("~/Desktop/AthleticsData.sav")
attach(AthleticsData)
names(AthleticsData)
cor(AthleticsData)
prcomp(AthleticsData)
fit.2 <- factanal(AthleticsData,factors=2,rotation="varimax")
print(fit.2)
fit.3 <- factanal(AthleticsData,factors=3,rotation="varimax")
print(fit.3)
print(fit.3, digits = 2, cutoff = .2, sort = TRUE)
install.packages("GPArotation")
library(GPArotation)
fit <- principal(AthleticsData, nfactors=3, rotate="varmax")
fit # print results
fit.3.promax <- update(fit.3,rotation="promax") 
colnames(fit.3.promax$loadings)<-c("Endurance","Strength","Hand-Eye") 
print(loadings(fit.3.promax), digits = 2, cutoff = .2, sort = TRUE)
AssignFactorNames <- function(fit.object,names)
{
  colnames(fit.object$promax.loadings)<-names
  colnames(fit.object$varimax.loadings)<-names
  rownames(fit.object$corr.factors)<-names
  colnames(fit.object$corr.factors)<-names
}
fit.3.Enzmann <- fa(AthleticsData,factors=3, digits=2, sort=TRUE,rotation="promax") 
AssignFactorNames(fit.3.Enzmann,factor.names)
fit.3.Enzmann


# example 5
set.seed(1.234)
N <- 200                             # number of observations
P <- 6                               # number of variables
Q <- 2                               # number of factors

# true P x Q loading matrix -> variable-factor correlations
Lambda <- matrix(c(0.7,-0.4, 0.8,0, -0.2,0.9, -0.3,0.4, 0.3,0.7, -0.8,0.1),
                 nrow=P, ncol=Q, byrow=TRUE)

library(mvtnorm)                      # for rmvnorm()
FF  <- rmvnorm(N, mean=c(5, 15), sigma=diag(Q))    # factor scores (uncorrelated factors)
E   <- rmvnorm(N, rep(0, P), diag(P)) # matrix with iid, mean 0, normal errors
X   <- FF %*% t(Lambda) + E           # matrix with variable values
Xdf <- data.frame(X)                  # data also as a data frame


library(psych) # for fa(), fa.poly(), factor.plot(), fa.diagram(), fa.parallel.poly, vss()
fa(X, nfactors=2, rotate="varimax")$loadings     # factor analysis continuous data

# dichotomize variables into a list of ordered factors
Xdi    <- lapply(Xdf, function(x) cut(x, breaks=c(-Inf, median(x), Inf), ordered=TRUE))
Xdidf  <- do.call("data.frame", Xdi) # combine list into a data frame
XdiNum <- data.matrix(Xdidf)         # dichotomized data as a numeric matrix

library(polycor)                     # for hetcor()
pc <- hetcor(Xdidf, ML=TRUE)         # polychoric corr matrix -> component correlations

#

faPC <- fa(r=pc$correlations, nfactors=2, n.obs=N, rotate="varimax")
faPC$loadings
#
faPCdirect <- fa.poly(XdiNum, nfactors=2, rotate="varimax")    # polychoric FA
faPCdirect$fa$loadings        # loadings are the same as above ...
#### NB: For factor scores, look at package ltm which has a factor.scores() function specifically for polytomous outcome data. An example is provided on this page -> "Factor Scores - Ability Estimates".

factor.plot(faPCdirect$fa, cut=0.5)
fa.diagram(faPCdirect)

fa.parallel.poly(XdiNum)      # parallel analysis for dichotomous data
vss(pc$correlations, n.obs=N, rotate="varimax")   # very simple structure
#
library(random.polychor.pa)    # for random.polychor.pa()
random.polychor.pa(data.matrix=XdiNum, nrep=5, q.eigen=0.99)



