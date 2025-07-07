# Install required libraries

library(cluster)
library(factoextra)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)



data=read_excel('MarketingData.xlsx')

dim(data)
head(data)

#Adding new features

data <- data %>%mutate(age_at_enroll = year(Dt_Customer) - Year_Birth)

data = data %>% mutate(days_customer_for = as.numeric(difftime(max(Dt_Customer),Dt_Customer, units = "days")))

### PCA 

# seperating out categorical and numerical columns
categorical_columns <- data %>%
  select(where(is.character))
numerical_columns <- data %>%
  select(-all_of(colnames(categorical_columns)))

# Identify and filter out columns with zero variance or constant values 
required_columns <- numerical_columns[, sapply(numerical_columns, function(col) var(col, na.rm = TRUE) != 0)]    

# Check for any missing values in the entire dataset  
sum(is.na(required_columns))

# Remove rows with missing values
required_columns <- na.omit(required_columns) 

# Now perform PCA on the filtered data  
pc <- prcomp(required_columns, scale. = TRUE)  

# Check the summary of the PCA  
summary(pc)  

# plotting the result
plot(pc)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))

# retaining components which explain upto 80% variance
k <- which(cumsum(pc$sdev^2/sum(pc$sdev^2)) >= .8)[1]
reduced_compo <- pc$x[,1:3]

autoplot(pc, data=required_columns)


##CLUSTERING



n_data = reduced_compo


# Elbow method to find optimal number of clusters and Plotting the elbow curve
fviz_nbclust(n_data, kmeans, method = "wss") +
  labs(title = "Elbow Method", x = "Number of clusters", y = "WCSS")


# Perform k-means clustering with 4 clusters
kmeans_model <- kmeans(n_data, centers = 4, nstart = 25)

# Create a data frame with cluster assignments and PCA coordinates
cluster_data <- data.frame(
  cluster = as.factor(kmeans_model$cluster),
  pca_1 = n_data[, 1],
  pca_2 = n_data[, 2]
)

# Plot the clusters
ggplot(cluster_data, aes(x = pca_1, y = pca_2, color = cluster)) +
  geom_point(size = 3) +
  geom_point(data = data.frame(pca_1 = kmeans_model$centers[, 1], pca_2 = kmeans_model$centers[, 2]),
             aes(x = pca_1, y = pca_2), color = "black", size = 5, shape = "x") +
  scale_color_manual(values = c("orange", "green","pink","blue")) +
  labs(title = "Clusters of customers", x = "pca_1", y = "pca_2") +
  theme_minimal()

cluster = cluster_data[,1]
final_data = cbind(required_columns,cluster)
dim(final_data)
head(final_data)
View(final_data)

final_data = final_data %>% mutate(total_Mnt = MntWines+MntFruits+MntMeatProducts+MntFishProducts+MntSweetProducts+MntGoldProds)
final_data = final_data %>% mutate(num_children = Teenhome+Kidhome)
final_data = final_data %>% mutate(num_total_purchases = NumDealsPurchases+NumWebPurchases+NumCatalogPurchases+NumStorePurchases+NumWebVisitsMonth)

# Creating a scatter plot displaying income and amount spend by different clusters
ggplot(final_data, aes(x = Income, y = total_Mnt, color = factor(cluster))) +
  geom_point() +
  labs(color = "Cluster") + coord_cartesian(xlim =c(0, 160000), ylim = c(0, 2500))

# Remove outliers from the data
df_no_outliers <- subset(final_data, Income <= 250000)

View(df_no_outliers)
# Define basic information columns
basic_info <- c('Income', 'num_children', 'Kidhome', 'Teenhome', 'num_total_purchases', 'total_Mnt', 'cluster')
# Group by cluster and calculate mean for basic information
basic_info_summary <- t(aggregate(cbind(Income, num_children, Kidhome, Teenhome, num_total_purchases, total_Mnt) ~ cluster, data = df_no_outliers, FUN = mean))

basic_info_summary = as.data.frame(basic_info_summary)

#from summary we can see that arrangement of cluster in decreasing order of income is 1,4,2,3







####################################################################################################

#studying nature of spending and income by different clusters

#####################################################################################################

p1 <- ggplot(df_no_outliers, aes(x = Income, y = total_Mnt, color = factor(cluster))) +
  geom_point() +
  ggtitle("Income vs Total Spending") + coord_cartesian(xlim =c(0, 160000), ylim = c(0, 2500))

plot(p1)

p2 <- ggplot(df_no_outliers, aes(x = factor(cluster), y = Income, fill = factor(cluster))) + geom_boxplot()+theme(legend.position="none")+
  ggtitle("Income by Cluster")+coord_cartesian( ylim = c(0, 160000))
plot(p2)

p3 <- ggplot(df_no_outliers, aes(x = factor(cluster), y = total_Mnt, fill = factor(cluster))) +
  geom_boxplot()+theme(legend.position="none")+ggtitle("Total Spending by Cluster")+coord_cartesian( ylim = c(0, 2500))
plot(p3)






#####################################################################################################

# Count plots for num_children, Kidhome, Teenhome

##################################################################################################

p4 <- ggplot(df_no_outliers, aes(x = factor(cluster), fill = factor(num_children))) + geom_bar(position = "dodge")+labs(title = "Count of Clusters by Number of Children")
p5 <- ggplot(df_no_outliers, aes(x = factor(cluster), fill = factor(Kidhome))) + geom_bar(position = "dodge")+labs(title = "Count of Clusters by Kidhome")
p6 <- ggplot(df_no_outliers, aes(x = factor(cluster), fill = factor(Teenhome))) + geom_bar(position = "dodge") + labs(title = "Count of Clusters by Teenhome")


plot(p4)
plot(p5)
plot(p6)






#############################################################################################

# Percentage of spending by category within each cluster

##############################################################################################

spending_columns <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')
spend <- aggregate(. ~ cluster, data = df_no_outliers[, c(spending_columns, 'cluster')], FUN = sum)
spend <- spend[, -which(names(spend) == "cluster")] 
spend_matrix <- as.matrix(spend)


spend_pct_by_category <- prop.table(spend_matrix, 1)

# View the percentage data
View(spend_pct_by_category) 

# Plotting percentage of spending by category and cluster
cluster <- c(rep("1" , 6) , rep("2" , 6) , rep("3" , 6) , rep("4" , 6) )
category <- rep(spending_columns , 4)
spend_pct_by_category = as.numeric(spend_pct_by_category )
spending_data <- data.frame(cluster, category, spend_pct_by_category)

ggplot(spending_data, aes(y = spend_pct_by_category, fill = category, x=cluster)) +
geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Percentage of Spending by Category and Cluster") +
  xlab("Cluster") +
  ylab("Percentage")






################################################################################################
# Web visits by cluster
####################################################################################################

web_visits_by_cluster <- aggregate(NumWebVisitsMonth ~ cluster, data = df_no_outliers, FUN = sum)
web_visits_by_cluster





#######################################################################################################
# Promotion analysis
########################################################################################################

promotion_list <- c('NumDealsPurchases', 'AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5', 'Response', 'cluster')
promotion_summary <- aggregate(cbind(NumDealsPurchases, AcceptedCmp1, AcceptedCmp2, AcceptedCmp3, AcceptedCmp4, AcceptedCmp5, Response) ~ cluster, data = df_no_outliers, FUN = sum)
promotion_summary





###########################################################################################
# Recency distribution and boxplot
#################################################################################################

ggplot(df_no_outliers, aes(x = Recency, fill = factor(cluster))) + geom_histogram(position = "dodge")
ggplot(df_no_outliers, aes(x = factor(cluster), y = Recency, fill = factor(cluster))) + geom_boxplot()




######################################################################################################
# Days as customer distribution and barplot
########################################################################################################

ggplot(df_no_outliers, aes(x = days_customer_for, fill = factor(cluster))) + geom_density(alpha = 0.5)
ggplot(df_no_outliers, aes(x = factor(cluster), y = days_customer_for, fill = factor(cluster))) + geom_bar(stat = "summary", fun = "mean", position = "dodge")


################################################################################################

#Age Distribution of customers in different clusters

#################################################################################################

p1 <- ggplot(df_no_outliers, aes(x = age_at_enroll, color = factor(cluster))) +
  geom_density(kernel = "gaussian", adjust = 2,linewidth = 2) 


p2 <- ggplot(df_no_outliers, aes(x = factor(cluster), y = age_at_enroll)) +
  geom_boxplot(aes(color = factor(cluster))) +
  scale_color_brewer(palette = "Pastel1")
plot(p1)
plot(p2)

##############################################################################################

#Understanding Source of Purchases of Customers

#################################################################################################

p1 <- ggplot(df_no_outliers, aes(x = cluster, y = NumDealsPurchases),color = factor(cluster)) + geom_boxplot()
plot(p1)
p2 <- ggplot(df_no_outliers, aes(x = cluster, y = NumStorePurchases)) + geom_boxplot()
plot(p2)
p3 <- ggplot(df_no_outliers, aes(x = cluster, y = NumWebPurchases)) + geom_boxplot()
plot(p3)
p4 <- ggplot(df_no_outliers, aes(x = cluster, y = NumCatalogPurchases)) + geom_boxplot()
plot(p4)
p5 <- ggplot(df_no_outliers, aes(x = cluster, y = NumWebVisitsMonth)) + geom_boxplot()
plot(p5)



