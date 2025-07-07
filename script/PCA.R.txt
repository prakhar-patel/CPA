library(readxl)
library(dplyr)
library(ggfortify)
data <- read_excel("../Data/MarketingData.xlsx")
head(data)
dim(data)

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
reduced_compo <- pc$x[,1:k]

autoplot(pc, data=required_columns)
