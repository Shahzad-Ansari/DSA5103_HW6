
library(mlbench)
library(dplyr)
library(ibd)
library(tidyverse)
library(reshape2)
library(datasets)
library(devtools)
library(scatterplot3d)
library(rgl)
library(ggplot2)
library(ggbiplot)
library(caret)
library(MASS)
library(HSAUR2)
library(outliers)
library(plyr)
library(readr)
library(car)
library(klaR)
library(caTools)
library(tidyverse)
library(pls)
library(AppliedPredictiveModeling)
library(visdat)
library(DataExplorer)
library(gridExtra)
library(grid)
library(ggcorrplot)
library(mice)
library(FactoMineR)
library(factoextra)
library(Hmisc)




# load and factorize/mutate input data
training_data <- read.csv("School/DSA5103/Homework 6/Train.csv")
testing_data <- read.csv("School/DSA5103/Homework 6/Test.csv")

training_data = data.frame(read.csv(file = "School/DSA5103/Homework 6/Train.csv", stringsAsFactors = TRUE)) %>% mutate_all(na_if,"")
# how much of the data is missing?
percentagesDf = data.frame(colnames(training_data))
for(i in 1:ncol(training_data)) {       # for-loop over columns
  percentagesDf[i,2] = (round((sum(is.na(training_data[,i])))
                             /dim(training_data)[1],5))*100
}
names(percentagesDf)[1] = "Column_Name"
names(percentagesDf)[2] = "Missing_Percentage"
percentagesDf = as.data.frame(percentagesDf)

ggplot(data = percentagesDf, mapping = aes(x = reorder(Column_Name, -Missing_Percentage), Missing_Percentage)) + 
  geom_bar(stat = "identity",aes(fill=Missing_Percentage), position = 'dodge') + 
  coord_flip()+
  geom_label(aes(label = Missing_Percentage,fill=Missing_Percentage),color = "Red")+
  xlab("Column Name")+
  ylab("Missing Percentage")

sum(is.na(training_data$country))


# It seems that newVisits to adContent may not be usable.

training_data = dplyr::select(training_data,-c(adwordsClickInfo.isVideoAd,
                                               campaign,
                                               adContent,
                                               metro,
                                               adwordsClickInfo.page,
                                               city,
                                               adwordsClickInfo.slot,
                                               adwordsClickInfo.gclId,
                                               adwordsClickInfo.adNetworkType,
                                               keyword,
                                               referralPath,
                                               region,
                                               networkDomain,
                                               topLevelDomain,
                                               bounces
                                               ))
  


# Check if there is a empty country or region or subContinent are all fields empty?
emptyLocation <- training_data[is.na(training_data$country),] 
emptyLocation

###################################################

#not working

levels <- levels(training_data$country)
levels[length(levels) + 1] <- "Unknown"

training_data$country <- factor(training_data$country, levels = levels)
training_data$country[is.na(training_data$country)] <- "None"




training_data$country[is.na(training_data$country)] = as.factor("Unknown")

emptyLocation <- training_data[is.na(training_data$country),] 
emptyLocation

###################################################
#not working

res.famd <- FAMD(training_data, 
                 graph = TRUE, 
                 ncp=25)

## Inspect principal components
print(get_eigenvalue(res.famd))
###################################################

training_data$browser[is.na(training_data$browser)] <- getmode(training_data$browser) 
training_data$operatingSystem[is.na(training_data$operatingSystem)] <- getmode(training_data$operatingSystem) 
training_data$newVisits[is.na(training_data$newVisits)] = 0
percentagesDf = data.frame(colnames(training_data))

for(i in 1:ncol(training_data)) {       # for-loop over columns
  percentagesDf[i,2] = sum(is.na(training_data[,i]))
                              
}

percentagesDf


sum(is.na(training_data$isMobile))
######################################
#not working


md.pattern(training_data)

imputed_data = mice(training_data,m=5,method="rf")

imputed_data

training_data = as.data.frame(training_data)

typeof(training_data)

training_data

my_imp = mice(training_data[,"medium"],method="rf")

miceOutput <- complete(my_imp)

anyNA(miceOutput)

is.na(testing_data$subContinent)

######################################
# old code
#Mean value imputation on NA values
for(i in 1:ncol(training_data)){
  training_data[is.na(training_data[,i]), i] <- mean(training_data[,i], na.rm = TRUE)
}
for(i in 1:ncol(testing_data)){
  testing_data[is.na(testing_data[,i]), i] <- mean(testing_data[,i], na.rm = TRUE)
}

head(training_data)



#Convert all character values and numeric values to ints and factors
training_data[sapply(training_data, is.character)] <- lapply(training_data[sapply(training_data, is.character)],as.factor)
training_data[sapply(training_data, is.numeric)] <- lapply(training_data[sapply(training_data, is.numeric)],as.integer)


testing_data[sapply(testing_data, is.character)] <- lapply(testing_data[sapply(testing_data, is.character)],as.factor)
testing_data[sapply(testing_data, is.numeric)] <- lapply(testing_data[sapply(testing_data, is.numeric)],as.integer)


# Confrim no N/A Values 
count = 0
for(i in 1:ncol(training_data)){
  count = count + (sum(is.na(training_data[1,i])))
}
print(count)

count = 0
for(i in 1:ncol(testing_data)){
  count = count + (sum(is.na(testing_data[1,i])))
}
print(count)


# Confirm the changes have taken place 
str(training_data)
training_data$revenue <- log(training_data$revenue)




















