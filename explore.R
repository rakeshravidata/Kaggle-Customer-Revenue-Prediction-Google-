# SYS 6018
# Kaggle Competition - Google 

##########
# Part 1 #
##########
# Set working directory

wd = "C:/Users/Tommy/Desktop/kaggle4"
setwd(wd)

# Read in data
train = read.csv("train.csv",colClasses=c("fullVisitorId"="character"))
test = read.csv("test.csv",colClasses=c("fullVisitorId"="character"))

##########
# Part 2 #
##########
# Some preliminary cleaning

# Start looking through the data
names(train); names(test)

# Notice that some of these variables are in JSON format

library(jsonlite)
library(tidyverse)
library(magrittr)

# Set columns which are in JSON

col = c("device","geoNetwork","totals","trafficSource")

train.json = train[,col]
train[,col] = NULL

# Let's get some help on going from JSON to data.frame from:
# https://www.kaggle.com/kailex/r-eda-for-gstore-glm-keras-xgb/code
flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- . %>% 
  bind_cols(flatten_json(.$device)) %>%
  bind_cols(flatten_json(.$geoNetwork)) %>% 
  bind_cols(flatten_json(.$trafficSource)) %>% 
  bind_cols(flatten_json(.$totals)) %>% 
  select(-device, -geoNetwork, -trafficSource, -totals)

# Parse through the json columns
train.json.parsed = parse(train.json)

# Now bring it all together
train.new = cbind(train,train.json.parsed)

# Check which columns are not avalible via Kaggle
cols = train.new[10,] == "not available in demo dataset"
cols[is.na(cols)] = 0
cols

# These are the variables which are not available via Kaggle
name = names(train.new)[cols==1]
train.new[,name] = NULL

# Check the final output
dim(train.new)
names(train.new)

# Fix classes
unlist(lapply(train.new, class))

# Char
char = c("sessionId")

# Factors
fac = c("browser","operatingSystem","deviceCategory","continent","subContinent","country","networkDomain","campaign","source","medium","keyword","referralPath","adContent","campaignCode","adwordsClickInfo.page","adwordsClickInfo.slot","adwordsClickInfo.gclId","adwordsClickInfo.adNetworkType","adwordsClickInfo.isVideoAd")

# Date
dat = c("date")

# Numeric
nums = c("transactionRevenue","visits","hits","pageviews","bounces","newVisits")

# Apply
train.new[,char] = as.character(train.new[,char])
for (i in fac) {
  train.new[,i] = as.factor(train.new[,i])
}
train.new[,dat] = as.Date(as.character(train.new[,dat]), format="%Y%m%d")
for (i in nums) {
  train.new[,i] = as.numeric(train.new[,i])
}
train.new$visitStartTime = as.POSIXct(train.new$visitStartTime, origin="1970-01-01") 
unlist(lapply(train.new, class))

# Note that there are some columns with NA values which are not denoted by NA
nas = c("(not provided)","(not set)","<NA>","unknown.unknown","(none)")
for (i in colnames(train.new)) {
  train.new[,i][train.new[,i] %in% nas] = NA
}

# Set all values for transaction revenue to 0 if NA
train.new$transactionRevenue[is.na(train.new$transactionRevenue)] = 0

# Now check for missing value
miss = unlist(lapply(train.new, function(x) sum(is.na(x))/length(x)))
miss90 = miss[miss>0.90]
# These columns are missing lots of information, drop them
train.new[,names(miss90)] = NULL

miss = unlist(lapply(train.new, function(x) sum(is.na(x))/length(x)))
miss10 = miss[miss>0.10]
intnames = names(miss10)
# [1] "networkDomain" "medium"        "isTrueDirect"  "referralPath"  "bounces"       "newVisits"  

# Now let's see if we can interpolate some of these variables
plot(train.new$networkDomain)
table(train.new$networkDomain)
levels(train.new$networkDomain)[table(train.new$networkDomain)>20000]
# The most common is comcast.net
# Interpolate missing values to mode
train.new$networkDomain[is.na(train.new$networkDomain)] = "comcast.net"

plot(train.new$medium)
# Interpolate to mode
train.new$medium[is.na(train.new$medium)] = "organic"

# Set NA to FALSE
train.new$isTrueDirect[is.na(train.new$isTrueDirect)] = FALSE
sum(train.new$isTrueDirect)/length(train.new$isTrueDirect)
# 0.3032 true

plot(train.new$referralPath)
levels(train.new$referralPath)[table(train.new$referralPath)>60000]
# The two most popular referrals are "/" and "/yt/about/"
# Interpolate to "/"
train.new$referralPath[is.na(train.new$referralPath)] = "/"

summary(train.new$bounces)
# Interpolate to 0 (?)
train.new$bounces[is.na(train.new$bounces)] = 0

summary(train.new$newVisits)
# Interpolate to 0 (?)
train.new$newVisits[is.na(train.new$newVisits)] = 0

# Finish up with last variables that need interpolation
miss = unlist(lapply(train.new, function(x) sum(is.na(x))/length(x)))
miss0 = miss[miss!=0]

# Needed a mode function
# https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (i in names(miss0)) {
  train.new[,i][is.na(train.new[,i])] = Mode(train.new[,i])
}

miss = unlist(lapply(train.new, function(x) sum(is.na(x))/length(x)))
miss

############
# Part 2.5 #
############
# Continue to filter the data. Some factors are too rare!

facs = which(unlist(lapply(train.new, is.factor)),TRUE)
for (i in facs) {
  nam = colnames(train.new)[i]
  len = length(levels(train.new[,i]))
  print(nam)
  print(len)
}

# Plots
for (i in facs) {
  plot(train.new[,i],main=as.character(colnames(train.new)[i]))
}

# Drop socialEngagementType and networkDomain
train.new$socialEngagementType = NULL
train.new$networkDomain = NULL
# Drop sessionId
train.new$sessionId = NULL

# Change browser, operating system, country, source, referralPath
tab=table(train.new$browser)[order(-table(train.new$browser))]
tab
# Let's just use the top 15 browsers
nam=names(tab)[1:15]
levels(train.new$browser) = c(levels(train.new$browser), "Other")
train.new$browser[!train.new$browser %in% nam] ="Other"
plot(factor(train.new$browser))

tab=table(train.new$operatingSystem)[order(-table(train.new$operatingSystem))]
tab
# Let's use the top 8 OS
nam=names(tab)[1:8]
levels(train.new$operatingSystem) = c(levels(train.new$operatingSystem), "Other")
train.new$operatingSystem[!train.new$operatingSystem %in% nam] ="Other"
plot(factor(train.new$operatingSystem))

tab=table(train.new$country)[order(-table(train.new$country))]
plot(tab)
# Let's use the top 50 countries
nam=names(tab)[1:50]
levels(train.new$country) = c(levels(train.new$country), "Other")
train.new$country[!train.new$country %in% nam] ="Other"
plot(factor(train.new$country))

tab=table(train.new$source)[order(-table(train.new$source))]
tab
# Let's use the top 10 sources
nam=names(tab)[1:10]
levels(train.new$source) = c(levels(train.new$source), "Other")
train.new$source[!train.new$source %in% nam] ="Other"
plot(factor(train.new$source))

# There's something off about referralPath right now, drop for now
train.new$referralPath = NULL

# tab=table(train.new$referralPath)[order(-table(train.new$referralPath))]
# head(tab,30)
# # Let's use the top 5 referralPaths
# nam=names(tab)[1:5]
# levels(train.new$referralPath) = c(levels(train.new$referralPath), "Other")
# train.new$referralPath[!train.new$referralPath %in% nam] ="Other"
# plot(factor(train.new$referralPath))

# Refactor data and made final data frame
train.final = as.data.frame(lapply(train.new, function(x) if (is.factor(x)) factor(x) else x))

# Apply everything to test

test.json = test[,col]
test[,col] = NULL
test.json.parsed = parse(test.json)
test.new = cbind(test, test.json.parsed)
name = names(test.new)[cols==1]
test.new[,name] = NULL
test.new[,char] = as.character(test.new[,char])

# Take only the columns which are relevant
select = names(train.final)
select = select[1:length(select)-1]
test.new = test.new[,select]
unlist(lapply(test.new, class))

for (i in colnames(test.new)) {
  test.new[,i][test.new[,i] %in% nas] = NA
}

miss = unlist(lapply(test.new, function(x) sum(is.na(x))/length(x)))
miss0 = miss[miss!=0]
miss0

test.new$isTrueDirect[is.na(test.new$isTrueDirect)] = FALSE
test.new$referralPath[is.na(test.new$referralPath)] = "/"

for (i in names(miss0)) {
  test.new[,i][is.na(test.new[,i])] = Mode(test.new[,i])
}

miss = unlist(lapply(test.new, function(x) sum(is.na(x))/length(x)))
miss0 = miss[miss!=0]
miss0

# Change browser, operating system, country, source, referralPath
tab=table(test.new$browser)[order(-table(test.new$browser))]
tab
nam=levels(train.final$browser)
levels(test.new$browser) = c(levels(test.new$browser), "Other")
test.new$browser[!test.new$browser %in% nam] ="Other"
plot(factor(test.new$browser))

tab=table(test.new$operatingSystem)[order(-table(test.new$operatingSystem))]
tab
nam=levels(train.final$operatingSystem)
levels(test.new$operatingSystem) = c(levels(test.new$operatingSystem), "Other")
test.new$operatingSystem[!test.new$operatingSystem %in% nam] ="Other"
plot(factor(test.new$operatingSystem))

tab=table(test.new$country)[order(-table(test.new$country))]
tab
nam=levels(train.final$country)
levels(test.new$country) = c(levels(test.new$country), "Other")
test.new$country[!test.new$country %in% nam] ="Other"
plot(factor(test.new$country))

tab=table(test.new$source)[order(-table(test.new$source))]
tab
nam=levels(train.final$source)
levels(test.new$source) = c(levels(test.new$source), "Other")
test.new$source[!test.new$source %in% nam] ="Other"
plot(factor(test.new$source))

# tab=table(test.new$referralPath)[order(-table(test.new$referralPath))]
# tab
# nam=levels(train.final$referralPath)
# levels(test.new$referralPath) = c(levels(test.new$referralPath), "Other")
# test.new$referralPath[!test.new$referralPath %in% nam] ="Other"
# plot(factor(test.new$referralPath))

# Refactor data
test.final = as.data.frame(lapply(test.new, function(x) if (is.factor(x)) factor(x) else x))

# Dummy encoding
library(dummies)
library(Matrix)

dum = names(train.final)[unlist(lapply(train.final, is.factor))]
dum = dum[!dum %in% "fullVisitorId"]

for (i in dum) {
  print(i)
  print(length(levels(train.final[,i])))
  print(length(levels(test.final[,i])))
  print("")
}

# Remove some columns for later
train.id = train.final$fullVisitorId
test.id = test.final$fullVisitorId
train.final$fullVisitorId = NULL
test.final$fullVisitorId = NULL
  
train.list = Matrix(nrow=dim(train.final)[1], ncol=0)
for (i in dum) {
  train.list = cbind(train.list, Matrix(dummy(train.final[,i])))
}

test.list = Matrix(nrow=dim(test.final)[1], ncol=0)
for (i in dum) {
  test.list = cbind(test.list, Matrix(dummy(test.final[,i])))
}

dim(train.list)
dim(test.list)

##########
# Part 3 #
##########
# Ridge regression

library(e1071)

# Take log transform of Y
skewness(train.final$transactionRevenue)
skewness(log(train.final$transactionRevenue+1))
Y = log(train.final$transactionRevenue+1)
train.final$transactionRevenue = NULL
all.train = cbind(train.list,Y)

# Parallel processing
library(glmnet)
library(doParallel)
library(parallel)
registerDoParallel(detectCores())

glm_model = cv.glmnet(train.list,Y,alpha=1,parallel=TRUE)
plot(glm_model)
min(glm_model$cvm)
# Get min lambda
lam=glm_model$lambda.min
lam

# Predict
pred=predict(glm_model, test.list, type="response", s=lam)
hist(pred, breaks=100)

# Transform back
pred.trans = exp(pred)
hist(pred.trans, breaks=100)

# Return the data frame in the format kaggle likes
out = data.frame(fullVisitorId=test.id, PredictedLogRevenue=pred)
names(out) = c("fullVisitorId","PredictedLogRevenue")

# Aggregate mean log
library(data.table)
out.dt = data.table(out)
out.agg = out.dt[,median(PredictedLogRevenue), by=list(out$fullVisitorId)]
out.agg = data.frame(out.agg)
names(out.agg) = c("fullVisitorId","PredictedLogRevenue")
head(out.agg)
dim(out.agg)

write.csv(out.agg, "lin_1_agg.csv", row.names = FALSE)

# Try performing some naive estimations?
out.agg2 = out.agg
out.agg2$PredictedLogRevenue[out.agg2$PredictedLogRevenue<1.6] = 0
hill = mean(log(train.new$transactionRevenue[train.new$transactionRevenue>0]))
out.agg2$PredictedLogRevenue[out.agg2$PredictedLogRevenue!=0] = hill

write.csv(out.agg, "lin_2_agg.csv", row.names = FALSE)