#Loading required packages

library(caret)

dat=read.csv("MarketingData.csv")

#checking NA vlaues

sum(is.na(dat))

#Removing NA values

dat=na.omit(dat)

# Combining the required columns

num_purchase=dat$NumDealsPurchases+dat$NumWebPurchases+dat$NumCatalogPurchases+dat$NumStorePurchases
num_child=dat$Kidhome+dat$Teenhome
amt_spent=dat$MntWines+dat$MntFruits+dat$MntMeatProducts+dat$MntFishProducts+dat$MntSweetProducts+dat$MntGoldProds

dat1=cbind(dat[,c(2,5,9,20)],num_purchase,num_child,amt_spent)

#Scaling the Data

dat1=as.data.frame(scale(dat1))

# train test split

set.seed(123) 

train_index <- createDataPartition(dat1$amt_spent, p = 0.8, list = FALSE)

# Create training and testing sets
train_data <- dat1[train_index, ]
test_data <- dat1[-train_index, ]

# Fitting linear regression model with predictors Year_Birth, Income,
# Recency, NumWebVisitsMonth, num_purchase, num_child

mod1=lm(amt_spent~Year_Birth+Income+Recency+NumWebVisitsMonth+num_purchase+num_child,data=train_data)
summary(mod1)

# As the p values of birth year and recency is so high, we'll try a model without them.

mod2=lm(amt_spent~Income+NumWebVisitsMonth+num_purchase+num_child,data=train_data)
summary(mod2)

##there's no major differnce in adjusted R square of the previous model and this one.

# Making predictions on the test set
predictions <- predict(mod2, newdata = test_data)

# Evaluating the model performance
mse <- mean((test_data$amt_spent - predictions)^2)
r_squared <- cor(test_data$amt_spent, predictions)^2

# Printing model performance metrics

print(paste("Mean Squared Error:", mse))
print(paste("R-squared:", r_squared))

# An R-squared of 0.7492 suggests that the model explains a 
# substantial portion of the variability in the target variable,
# but there is still some unexplained variance.

# The MSE value of 0.2738 suggests that the model's predictions are generally
# close to the actual values of the target variable.
















