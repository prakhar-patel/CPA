library(rpart)
library(rpart.plot)
library(pracma)
library(scatterplot3d)
data <- read.csv('MarketingData')

graduation.data <- data$Income[data$Education=="Graduation"]

higherstudies.data.income <- data$Income[data$Education %in% c("Master", "PhD")]

t.test(graduation.data,higherstudies.data.income,var.equal=TRUE,conf.level=0.95)
#Null hypothesis - Mean of the income of people who have opted for higher studies is same as to those who have not

higherstudies.data <- data[data$Education %in% c("Graduation", "Master"),c(4,30)]

### MLE

summary(data$NumStorePurchases)

hist(data$NumStorePurchases)

pois.lik<-function(theta,y){
  logl<- sum(dpois(y,theta,log=TRUE))
  return(-logl)
}

lam <- optim(0.1,pois.lik,y=data$NumStorePurchases,method="BFGS")$par

c <- numeric(14)
for(i in data$NumStorePurchases){
  c[i+1] <- c[i+1]+1
}
c <- c/sum(c)
barplot(c,names.arg = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))
x <- seq(0,15,1)
lines(x,dpois(x,lam),col="red")


# Creates a qqplot
par(mfrow=c(1,2))
qqplot(rpois(2216,lam),data$NumStorePurchases,xlab = "Theoretical quantiles of Poiss(lam)",ylab="Quantiles of our actual data",main = "Using MLE")
abline(0,1,col="red",lwd=3)
qqplot(rpois(2216,10),data$NumStorePurchases,xlab = "Theoretical quantiles of Poiss(lam)",ylab="Quantiles of our actual data",main = "Random lambda=10")
abline(0,1,col="red",lwd=3)

#Prop testing
table(higherstudies.data)
prop.test(c(964,309),c(1116,365),conf.level=0.95, correct=FALSE)

#orthogonal regression
res <- odregress(as.matrix(data[,c(11,16)]), as.matrix(data$Income))
res$coeff 

