library(readxl)
library(dplyr)

data <- read_excel("MarketingData.xlsx")
duplicated(data)
data <- na.omit(data)
data <- distinct(data)
write.csv(data,'MarketingData')

