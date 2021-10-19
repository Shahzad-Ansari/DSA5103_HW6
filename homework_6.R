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
library(fastDummies)

# load and factorize/mutate input data
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
str(training_data)

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
                                               bounces,
                                               sessionId
                                               ))

################################################################################################################################
# IMPUTATION                                                                                                                   #
################################################################################################################################
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# imputing the missing values of medium using probability 
set.seed(1)
mediumFreq = na.omit(count(training_data,'medium'))
totalMedium = sum(operatingSystemFreq$freq[1:5])
mediumFreq[,3] = mediumFreq[,2]/totalMedium
names(mediumFreq)[3] = "Probability"
training_data$medium[is.na(training_data$medium)] <-as.factor(sample(mediumFreq$medium[1:5], 
                                                                size=1, replace=TRUE, prob=mediumFreq$Probability[1:5]
                                                              ))
sum(is.na(training_data$medium))

# browser has only 1 na value, just going to use mode imputation on this
browserFreq = count(training_data,'browser')
browserFreq
training_data$browser[is.na(training_data$browser)] <- getmode(training_data$browser) 
# impute operating System via probability
operatingSystemFreq = na.omit(count(training_data,'operatingSystem'))
totaloperatingSystem = sum(operatingSystemFreq$freq[1:15])
operatingSystemFreq[,3] = operatingSystemFreq[,2]/totaloperatingSystem
names(operatingSystemFreq)[3] = "Probability"
training_data$operatingSystem[is.na(training_data$operatingSystem)] <-as.factor(sample(operatingSystemFreq$operatingSystem[1:15], 
                 size=1, replace=TRUE, prob=operatingSystemFreq$Probability[1:15]
                 ))
sum(is.na(training_data$operatingSystem))
# No way to know if its not or is a new visit as every non NA value is a 1. Cannot compute a probability so i will assume
# that if it is NA that it is not a new visit. 
training_data$newVisits[is.na(training_data$newVisits)] = 0
# imputing page views with rounded down average
training_data$pageviews[is.na(training_data$pageviews)] = round(mean(!is.na(training_data$pageviews)))


# Check if there is a empty country or region or subContinent are all fields empty?
emptyLocation <- training_data[is.na(training_data$country),] 
emptyLocation

levels(training_data$country) = c(levels(training_data$country),"Unknown")
levels(training_data$continent) = c(levels(training_data$continent),"Unknown")
levels(training_data$subContinent) = c(levels(training_data$subContinent),"Unknown")

training_data$country[is.na(training_data$country)] = as.factor("Unknown")
training_data$continent[is.na(training_data$continent)] = as.factor("Unknown")
training_data$subContinent[is.na(training_data$subContinent)] = as.factor("Unknown")

# Since source has only 2 missing values i will just use mode imputation 
sourceFreq = count(training_data,'source')
sourceFreq
training_data$source[is.na(training_data$source)] <- getmode(training_data$source)

# show NA for all columns 

percentagesDf = data.frame(colnames(training_data))

for(i in 1:ncol(training_data)) {       # for-loop over columns
  percentagesDf[i,2] = sum(is.na(training_data[,i]))
  
}
names(percentagesDf)[1] = "Column name"
names(percentagesDf)[2] = "Missing Count"

percentagesDf


################################################################################################################################
# END OF IMPUTATION                                                                                                            # 
################################################################################################################################

#Convert all character values and numeric values to ints and factors
training_data[sapply(training_data, is.numeric)] <- lapply(training_data[sapply(training_data, is.numeric)],as.integer)

# Confirm the changes have taken place 
str(training_data)
################################################################################################################################
# CREATING DUMMY VARIABLES 
################################################################################################################################
test = fastDummies::dummy_cols(training_data,select_columns = c())

test$browser_



###################################################
























