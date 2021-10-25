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
library(outliers)

# load and factorize/mutate input data
training_data = data.frame(read.csv(file = "School/DSA5103/Homework 6/Train.csv", stringsAsFactors = TRUE)) %>% mutate_all(na_if,"")
testing_data = data.frame(read.csv(file = "School/DSA5103/Homework 6/Test.csv", stringsAsFactors = TRUE)) %>% mutate_all(na_if,"")

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


percentagesDf = data.frame(colnames(testing_data))
for(i in 1:ncol(testing_data)) {       # for-loop over columns
  percentagesDf[i,2] = (round((sum(is.na(testing_data[,i])))
                              /dim(testing_data)[1],5))*100
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
                                               sessionId,
                                               visitStartTime,
                                               date
                                               ))

testing_data = dplyr::select(testing_data,-c(adwordsClickInfo.isVideoAd,
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
                                              sessionId,
                                              visitStartTime,
                                              date
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
totalMedium = sum(mediumFreq$freq[1:5])
mediumFreq[,3] = mediumFreq[,2]/totalMedium
names(mediumFreq)[3] = "Probability"
training_data$medium[is.na(training_data$medium)] <-as.factor(sample(mediumFreq$medium[1:5], 
                                                                size=1, replace=TRUE, prob=mediumFreq$Probability[1:5]
                                                              ))
sum(is.na(training_data$medium))

set.seed(1)
mediumFreq = na.omit(count(testing_data,'medium'))
totalMedium = sum(mediumFreq$freq[1:5])
mediumFreq[,3] = mediumFreq[,2]/totalMedium
names(mediumFreq)[3] = "Probability"
testing_data$medium[is.na(testing_data$medium)] <-as.factor(sample(mediumFreq$medium[1:5], 
                                                                     size=1, replace=TRUE, prob=mediumFreq$Probability[1:5]
))
sum(is.na(training_data$medium))
#################################################################
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

browserFreq = count(testing_data,'browser')
browserFreq
testing_data$browser[is.na(testing_data$browser)] <- getmode(testing_data$browser) 
# impute operating System via probability
operatingSystemFreq = na.omit(count(testing_data,'operatingSystem'))
totaloperatingSystem = sum(operatingSystemFreq$freq[1:15])
operatingSystemFreq[,3] = operatingSystemFreq[,2]/totaloperatingSystem
names(operatingSystemFreq)[3] = "Probability"
testing_data$operatingSystem[is.na(testing_data$operatingSystem)] <-as.factor(sample(operatingSystemFreq$operatingSystem[1:15], 
                                                                                       size=1, replace=TRUE, prob=operatingSystemFreq$Probability[1:15]
))
sum(is.na(testing_data$operatingSystem))
################################################
# No way to know if its not or is a new visit as every non NA value is a 1. Cannot compute a probability so i will assume
# that if it is NA that it is not a new visit. 
training_data$newVisits[is.na(training_data$newVisits)] = 0
# imputing page views with rounded down average
training_data$pageviews[is.na(training_data$pageviews)] = round(mean(!is.na(training_data$pageviews)))
##############################################
testing_data$newVisits[is.na(testing_data$newVisits)] = 0
# imputing page views with rounded down average
testing_data$pageviews[is.na(testing_data$pageviews)] = round(mean(!is.na(testing_data$pageviews)))
###############################################
# Check if there is a empty country or region or subContinent are all fields empty?
emptyLocation <- training_data[is.na(training_data$country),] 
emptyLocation

levels(training_data$country) = c(levels(training_data$country),"Unknown")
levels(training_data$continent) = c(levels(training_data$continent),"Unknown")
levels(training_data$subContinent) = c(levels(training_data$subContinent),"Unknown")

training_data$country[is.na(training_data$country)] = as.factor("Unknown")
training_data$continent[is.na(training_data$continent)] = as.factor("Unknown")
training_data$subContinent[is.na(training_data$subContinent)] = as.factor("Unknown")

#######################################
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

################################################################################################################################
# OUTLIER RESOLUTION                                                                                                          # 
################################################################################################################################
grubbs.test(training_data$revenue)

outliers = training_data[training_data$revenue >= 15980.79,] 

# just one person is an outlier in revenue. 
dim(outliers)

training_data = training_data[training_data$revenue < 15980.79,]

################################################################################################################################
# END OF OUTLIER RESOLUTION                                                                                                    # 
################################################################################################################################

#Convert all character values and numeric values to ints and factors
training_data[sapply(training_data, is.numeric)] <- lapply(training_data[sapply(training_data, is.numeric)],as.integer)
testing_data[sapply(testing_data, is.numeric)] <- lapply(testing_data[sapply(testing_data, is.numeric)],as.integer)

# Confirm the changes have taken place 
str(training_data)


t1 = training_data %>%  
  mutate(subContinent = fct_lump(as.factor(subContinent),n=1)) %>%
  mutate(browser = fct_lump(as.factor(browser),n=8)) %>%
  mutate(country = fct_lump(as.factor(country),n=1))%>%
  mutate(operatingSystem = fct_lump(as.factor(operatingSystem),n=6)) %>% 
  mutate(continent = fct_lump(as.factor(continent),n=1)) %>% 
  
  group_by(custId) %>%
  dplyr::summarise(n = n(),
                   totalRevenue = sum(revenue),
                   pageviews = mean(pageviews, na.rm=TRUE),
                   total_visit= max(visitNumber),
                   visitGap = mean(timeSinceLastVisit)/3600
                   ,subContinent = getmode(subContinent)
                   ,browser = getmode(browser)
                   ,operatingSystem = getmode(operatingSystem)
                   ,country = getmode(country)
                   ,continent = getmode(continent)
                   
                   
                   

                  )

t2 = testing_data %>%  
  mutate(subContinent = fct_lump(as.factor(subContinent),n=1)) %>%
  mutate(browser = fct_lump(as.factor(browser),n=8)) %>%
  mutate(country = fct_lump(as.factor(country),n=1))%>%
  mutate(continent = fct_lump(as.factor(continent),n=1)) %>% 
  mutate(operatingSystem = fct_lump(as.factor(operatingSystem),n=6)) %>% 
  group_by(custId) %>%
  dplyr::summarise(n = n(),
                   pageviews = mean(pageviews, na.rm=TRUE),
                   total_visit= max(visitNumber),
                   visitGap = mean(timeSinceLastVisit)/3600
                   ,subContinent = getmode(subContinent)
                   ,browser = getmode(browser)
                   ,operatingSystem = getmode(operatingSystem)
                   ,country = getmode(country)
                   ,continent = getmode(continent)
                   
                   
  )
t2$subContinent[is.na(t2$subContinent)] = as.factor("Other")
t2$browser[is.na(t2$browser)] = as.factor("Other")
t2$operatingSystem[is.na(t2$operatingSystem)] = as.factor("Other")
t2$country[is.na(t2$country)] = as.factor("Other")

levels(t2$continent) = c(levels(t2$continent),"Other")
t2$continent[is.na(t2$continent)] = as.factor("Other")


t2$country[is.na(t2$country)]

table(t2$continent)
table(t1$continent)


glimpse(t2)  

tempFit <- lm(data=t1, log(totalRevenue + 1) ~ .-custId)
summary(tempFit)


sqrt(mean((tempFit$fitted.values-log(t1$totalRevenue+1))^2))


out1 = data.frame(t2$custId,predict(tempFit,newdata=t2))
write.csv(out1,"out3.csv", row.names = FALSE)

sum(is.na(out1))

fitControl <- trainControl(method="repeatedcv",number=20, repeats = 5)
lassoGrid <- expand.grid(fraction=seq(0.1,0.99,length=100))
fit_ols <- train(data=t1, log(totalRevenue + 1) ~ .-custId,
                 method="lm",
                 trControl = fitControl,
                 na.action = na.pass)
fit_ols
# RMSE 0.9320438
# with only united states 0.929571
pred<-predict(fit_ols,newdata=t2)
pred
dim(pred)

submissionDf1 <- data.frame(custId=t2$custId, predRevenue=pred)
#create csv file for uploading to kaggle
write.csv(submissionDf1, "submission3.csv", row.names=FALSE)
View(t2)

fit_lasso <- train(data=t1, log(totalRevenue + 1) ~ .-custId,
                   method="lasso",
                   trControl = fitControl,
                   tuneGrid = lassoGrid,
                   na.action = na.pass)

fit_lasso
# lasso has RMSE of 0.9387684
# 0.9330399

custom = trainControl(method = "repeatedcv", number = 5, verboseIter = T, repeats = 5)

set.seed(1234)

ridge <- train(data=t1, log(totalRevenue + 1) ~ .-custId,method ='glmnet',
               tuneGrid = expand.grid(alpha = 0,lambda = seq(.0000,1,length=5 )),
               trControl = custom)
plot(ridge)
print(ridge)
plot(ridge$finalModel,xvar="lambda",label = T)
plot(varImp(ridge,scale = T))

#  0.9363085
# 0.9364989  

library(caret)
library(elasticnet)

grid <- expand.grid(
  lambda   = seq(0.5, 0.7, by=0.1),
  fraction = seq(0, 1, by=0.1)
)

ctrl <- trainControl(
  method     = 'repeatedcv',
  number     = 5,  #folds
  repeats    = 10, #repeats
  classProbs = FALSE
)


enetTune <- train(data=t1, log(totalRevenue + 1) ~ .-custId,
  method = 'enet',
  metric = 'RMSE',
  tuneGrid = grid,
  trControl = ctrl
)

plot(enetTune)

enetTune$bestTune

enetTune

# 0.9476270
# 0.9481803



################################################################################################################################
# CREATING DUMMY VARIABLES 
################################################################################################################################
training_data_test = fastDummies::dummy_cols(training_data,remove_selected_columns = TRUE)
dim(training_data)
###################################################

tD <- training_data %>% 
  mutate(subContinent = fct_lump(as.factor(subContinent),n=3)) %>%
  mutate(browser = fct_lump(as.factor(browser),n=8))
  mutate(operatingSystem = fct_lump(as.factor(operatingSystem),n=6))

table(tD$subContinent)

fit = lm(data=tD,revenue~.-custId-country-source-medium)

summary(fit)

factor

table(training_data$medium,training_data$channelGrouping)

predict(tempFit,training_data)


predict(fit,training_data)

head(training_data$custId)

#LOOK AT FACTOR COLAPSING
#https://forcats.tidyverse.org/










