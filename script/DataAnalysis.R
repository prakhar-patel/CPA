library(ggplot2)
library(corrplot)
data <- read.csv('MarketingData')

head(data)

dim(data)

par(mfrow=c(1,2))
boxplot(data$MntFishProducts,main="No. of fish purchases")
abline(mean(data$MntFishProducts),0,col="red",lwd=3)
summary(data$MntFishProducts)
boxplot(data$MntFruits,main="No. of fruit purchases")
abline(mean(data$MntFruits),0,col="red",lwd=3)
summary(data$MntFruits)

boxplot(data$MntWines,main="No. of wine purchases")
abline(mean(data$MntWines),0,col="red",lwd=3)
summary(data$MntWines)
boxplot(data$MntMeatProducts,main="No of meat purchases")
abline(mean(data$MntMeatProducts),0,col="red",lwd=3)
summary(data$MntMeatProducts)

par(mfrow=c(1,1))

table(data$Education,data$Marital_Status)
options(digits=2)
prop.table(table(data$Education,data$Marital_Status),margin=1) #Formargin=1, proportions
mosaicplot(table(data$Education[data$Marital_Status %in% c("Divorced", "Married","Single","Together")],data$Marital_Status[data$Marital_Status %in% c("Divorced", "Married","Single","Together")]),
           main="Mosaic plot Education vs Marital status",
           xlab = "Qualifications",ylab = "Marital Status",color = c("red","blue","yellow","green"))

ggplot(data[data$Kidhome %in% c("0","1")], aes(gender, number, fill = friend_or_not)) +
  geom_bar(stat="identity", position = "dodge") + 
  labs(title="Multiple Bar plots")

bar.mat <- matrix(0,2,6)
for(i in 0:1){
  for(j in 1:6){
    bar.mat[i+1,j] <- sum(data[data$Kidhome==i,j+10])/sum(data$Kidhome==i)
  }
}
barplot(bar.mat,
        main="Kid vs No Kids", 
        ylab="Total Amount spend per customer", 
        beside=TRUE, 
        names.arg = c("Wines","Fruits","Meat","Fish","Sweet","Gold"),
        col = c("pink","blue")
)

#Correlation PLots
M = cor(data[11:16])
corrplot(M, method = 'number')

Q = cor(data[17:21])
corrplot(Q,method='number')

ggplot(data[data$Income<1e5,], aes(x = MntWines, y = MntSweetProducts, color=Income,size=NumWebVisitsMonth)) +
  geom_point(alpha=0.5) +
  scale_color_gradient(low = "yellow", high = "black", name = "Income")




