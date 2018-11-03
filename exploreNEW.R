# SYS 6018
# Kaggle Competition - Google 

##########
# Part 1 #
##########
# Set working directory

wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/04_Google/kaggle4"
setwd(wd)

# Read in data
train = read.csv("train.csv",colClasses=c("fullVisitorId"="character"))
test = read.csv("test.csv",colClasses=c("fullVisitorId"="character"))

##########
# Part 2 #
##########
# Some preliminary cleaning

# Start looking through the data
summary(train)
summary(test)
names(train)
names(test)

# Notice that some of these variables are in JSON format

library(jsonlite)
library(tidyverse)
library(magrittr)

# Set columns which are in JSON

col = c("device","geoNetwork","totals","trafficSource")

# Temporarily remove the json colunns into its own dataframe to work on it

train.json = train[,col]
train[,col] = NULL
head(train.json)

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
rm(train.json)
gc()

# Now bring it all together
train.new = cbind(train,train.json.parsed)
rm(train.json.parsed)
rm(train)
gc()

# Check which columns are not avalible via Kaggle
cols = train.new[10,] == "not available in demo dataset"
cols[is.na(cols)] = 0
cols

# These are the variables which are not available via Kaggle
name = names(train.new)[cols==1]
train.new[,name] = NULL
rm(cols)

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

# Classes are now fixed
unlist(lapply(train.new, class))

# Check number of unique values
name = names(train.new)
for (n in name) {
  pt = length(unique(train.new[,n]))
  pst = paste(n,"has",pt,"classes")
  print(pst)
}

# Social engagement type and visits are completely homogenous
train.new$socialEngagementType = NULL
train.new$visits = NULL

name = names(train.new)
# Note that there are some columns with missing values, let's fix them
miss = c()
for (n in name) {
  misssum = sum(is.na(train.new[,n]))
  if (misssum>0) {
    miss = c(miss, n)
  }
}
miss

# Check number of unique levels
for (m in miss) {
  pst = paste(m, "has classes", length(unique(train.new[,m])))
  print(pst)
}

# "keyword"
# Manually fix them... some have overlapping NA values!
head(sort(table(train.new$keyword), decreasing=TRUE), 50)
# Based on the data it looks like youtube, google, and other should be the categories
is.goo = unlist(lapply(as.character(train.new$keyword), function(x) grepl("goo",x)))
is.you = unlist(lapply(as.character(train.new$keyword), function(x) grepl("you",x)))
is.not = !(is.goo|is.you)
train.new$keyword = as.character(train.new$keyword)
train.new$keyword[is.goo] = "Google"
train.new$keyword[is.you] = "YouTube"
train.new$keyword[is.not] = "Other"
head(train.new$keyword, 100)
table(train.new$keyword)

# "isTrueDirect"
head(train.new$isTrueDirect,100)
train.new$isTrueDirect[is.na(train.new$isTrueDirect)] = FALSE
head(train.new$isTrueDirect,100)
table(train.new$isTrueDirect)

# "referralPath"
head(train.new$referralPath,100)
head(sort(table(train.new$referralPath), decreasing=TRUE), 50)
# google and permissions = Google
# yt = Youtube
# analytics = Analytics
# / = Other
is.goo = unlist(lapply(as.character(train.new$referralPath), function(x) grepl("goo",x)))
is.goo2 =unlist(lapply(as.character(train.new$referralPath), function(x) grepl("permissions",x)))
is.goo = is.goo|is.goo2
is.ana = unlist(lapply(as.character(train.new$referralPath), function(x) grepl("analytics",x)))
is.you = unlist(lapply(as.character(train.new$referralPath), function(x) grepl("yt",x)))
is.not = !(is.goo|is.ana|is.you)
train.new$referralPath = as.character(train.new$referralPath)
train.new$referralPath[is.ana] = "Analytics"
train.new$referralPath[is.goo & !is.ana] = "Google"
train.new$referralPath[is.you & !is.goo & !is.ana] = "YouTube"
train.new$referralPath[is.not] = "Other"
table(train.new$referralPath)

# "adContent"      
head(train.new$adContent,100)
head(sort(table(train.new$adContent), decreasing=TRUE), 20)
# Google, ad, and other
is.goo = unlist(lapply(as.character(train.new$adContent), function(x) grepl("goog",x)))
is.ad = unlist(lapply(as.character(train.new$adContent), function(x) grepl("ad",x)))
is.ad = is.ad & !is.goo
is.not = !(is.goo | is.ad)
train.new$adContent = as.character(train.new$adContent)
train.new$adContent[is.goo] = "Google"
train.new$adContent[is.ad] = "Ad"
train.new$adContent[is.not] = "Other"
table(train.new$adContent)

# "campaignCode"                  
head(train.new$campaignCode,100)
head(sort(table(train.new$campaignCode), decreasing=TRUE), 20)
# Useless, drop
train.new$campaignCode = NULL

# "adwordsClickInfo.page"         
head(train.new$adwordsClickInfo.page,100)
head(sort(table(train.new$adwordsClickInfo.page), decreasing=TRUE), 20)
# Is numeric
train.new$adwordsClickInfo.page = as.numeric(train.new$adwordsClickInfo.page)
not=is.na(train.new$adwordsClickInfo.page)
train.new$adwordsClickInfo.page[not] = 0
table(train.new$adwordsClickInfo.page)

# "adwordsClickInfo.slot"         
head(train.new$adwordsClickInfo.slot,100)
head(sort(table(train.new$adwordsClickInfo.slot), decreasing=TRUE), 20)
train.new$adwordsClickInfo.slot = as.character(train.new$adwordsClickInfo.slot)
not=is.na(train.new$adwordsClickInfo.slot)
train.new$adwordsClickInfo.slot[not] = "Other"
table(train.new$adwordsClickInfo.slot)

# "adwordsClickInfo.gclId"        
head(train.new$adwordsClickInfo.gclId,100)
head(sort(table(train.new$adwordsClickInfo.gclId), decreasing=TRUE), 5)
# This is really messy. Drop
train.new$adwordsClickInfo.gclId = NULL

# "adwordsClickInfo.adNetworkType"
head(train.new$adwordsClickInfo.adNetworkType,100)
head(sort(table(train.new$adwordsClickInfo.adNetworkType), decreasing=TRUE), 20)
train.new$adwordsClickInfo.adNetworkType = as.character(train.new$adwordsClickInfo.adNetworkType)
not=is.na(train.new$adwordsClickInfo.adNetworkType)
train.new$adwordsClickInfo.adNetworkType[not] = "Other"
table(train.new$adwordsClickInfo.adNetworkType)

# "adwordsClickInfo.isVideoAd"    
head(train.new$adwordsClickInfo.isVideoAd,100)
head(sort(table(train.new$adwordsClickInfo.isVideoAd), decreasing=TRUE), 20)
istrue = is.na(train.new$adwordsClickInfo.isVideoAd)
train.new$adwordsClickInfo.isVideoAd = as.logical(train.new$adwordsClickInfo.isVideoAd)
train.new$adwordsClickInfo.isVideoAd[istrue] = TRUE
table(train.new$adwordsClickInfo.isVideoAd)

# "pageviews"                     
head(train.new$pageviews,100)
head(sort(table(train.new$pageviews), decreasing=TRUE), 20)
sum(is.na(train.new$pageviews))
not = is.na(train.new$pageviews)
train.new$pageviews[not] = 0
table(train.new$pageviews)

# "bounces"                       
head(train.new$bounces,100)
head(sort(table(train.new$bounces), decreasing=TRUE), 20)
not = is.na(train.new$bounces)
train.new$bounces[not] = 0 
table(train.new$bounces)

# "newVisits"
head(train.new$newVisits,100)
head(sort(table(train.new$newVisits), decreasing=TRUE), 20)
not = is.na(train.new$newVisits)
train.new$newVisits[not] = 0 
table(train.new$newVisits)

# "transactionRevenue"
# Missing revenue is just 0
not = is.na(train.new$transactionRevenue)
train.new$transactionRevenue[not] = 0
hist(log(train.new$transactionRevenue+1))
hist(log(train.new$transactionRevenue))

# Check for missing data
miss = unlist(lapply(train.new, function(x) sum(is.na(x))/length(x)))
miss

############
# Part 2.5 #
############
# Continue to filter the data. Some factors are too rare!

# Needed a mode function for later
# https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

facs = which(unlist(lapply(train.new, is.factor)),TRUE)
largefacs = c()
for (i in facs) {
  nam = colnames(train.new)[i]
  len = length(levels(train.new[,i]))
  if (len>=10) {
    largefacs = c(largefacs, nam)
  }
  pst = paste(nam,"has",len,"levels.")
  print(pst)
}
largefacs

# Start with browser. See which levels are the best predictors 
plot(train.new$browser,log(train.new$transactionRevenue+1))
tab=sort(table(train.new$browser[train.new$transactionRevenue>0]), decreasing=TRUE)
tab
# It looks like only 9 browsers actually had any sales on them.
nam=names(head(tab, 9))
train.new$browser = as.character(train.new$browser)
is.nam = lapply(train.new$browser, function(x) any(nam==x))
is.nam = unlist(is.nam)  
train.new$browser[!is.nam] = "Other"
train.new$browser = as.factor(train.new$browser)

# Operating system
plot(train.new$operatingSystem,log(train.new$transactionRevenue+1))
tab=sort(table(train.new$operatingSystem[train.new$transactionRevenue>0]), decreasing=TRUE)
tab
# It looks like 7 OS had any sales on them
nam=names(head(tab, 7))
train.new$operatingSystem = as.character(train.new$operatingSystem)
is.nam = lapply(train.new$operatingSystem, function(x) any(nam==x))
is.nam = unlist(is.nam)  
train.new$operatingSystem[!is.nam] = "Other"
train.new$operatingSystem = as.factor(train.new$operatingSystem)

# Subcontinent
plot(train.new$subContinent,log(train.new$transactionRevenue+1))
tab=sort(table(train.new$subContinent[train.new$transactionRevenue>0]), decreasing=TRUE)
tab
# There area some regions with no sales but this is fine, the majority of levels are filled
train.new$subContinent = as.factor(train.new$subContinent)

# Country
plot(train.new$country,log(train.new$transactionRevenue+1))
tab=sort(table(train.new$country[train.new$transactionRevenue>0]), decreasing=TRUE)
tab
# Arbitrarily select the top 10
nam=names(head(tab, 10))
train.new$country = as.character(train.new$country)
is.nam = lapply(train.new$country, function(x) any(nam==x))
is.nam = unlist(is.nam)  
train.new$country[!is.nam] = "(not set)"
train.new$country = as.factor(train.new$country)

# Network Domain
# Warning plotting takes too long
# plot(train.new$networkDomain,log(train.new$transactionRevenue+1))
tab=sort(table(train.new$networkDomain[train.new$transactionRevenue>0]), decreasing=TRUE)
head(tab,100)
# Arbitrarily select the top 10
nam=names(head(tab, 10))
train.new$networkDomain = as.character(train.new$networkDomain)
is.nam = lapply(train.new$networkDomain, function(x) any(nam==x))
is.nam = unlist(is.nam)  
train.new$networkDomain[!is.nam] = "(not set)"
train.new$networkDomain[train.new$networkDomain=="unknown.unknown"] = "(not set)"
train.new$browser = as.factor(train.new$browser)

# Campaign
plot(train.new$campaign,log(train.new$transactionRevenue+1))
tab=sort(table(train.new$campaign[train.new$transactionRevenue>0]), decreasing=TRUE)
tab
# This doesn't look like a good predictor. Drop
train.new$campaign = NULL

# Source
tab=sort(table(train.new$source[train.new$transactionRevenue>0]), decreasing=TRUE)
head(tab,100)
# It looks like popular sources are: google, (direct), yahoo, facebook
train.new$source = as.character(train.new$source)

is.goo = unlist(lapply(train.new$source, function(x) grepl("google",x)))
is.dir = unlist(lapply(train.new$source, function(x) grepl("direct",x)))
is.yah = unlist(lapply(train.new$source, function(x) grepl("yah",x)))
is.fb = unlist(lapply(train.new$source, function(x) grepl("faceb",x)))
is.not = !(is.goo|is.dir|is.yah|is.fb)

train.new$source[is.goo] = "Google"
train.new$source[is.dir] = "(direct)"
train.new$source[is.yah] = "Yahoo"
train.new$source[is.fb] = "Facebook"
train.new$source[is.not] = "Other"

table(as.factor(train.new$source))
train.new$source = as.factor(train.new$source)

# Make the final train
train.final = train.new

# Apply everything to test

test.json = test[,col]
test[,col] = NULL
test.json.parsed = parse(test.json)
test.new = cbind(test, test.json.parsed)

# First drop any columns we have dropped previously
nam = names(test.new) %in% names(train.final)
test.new = test.new[,nam]

# Fix the missing values
miss = unlist(lapply(test.new, function(x) sum(is.na(x))/length(x)))
miss


#
#
#

# "keyword"
# Manually fix them... some have overlapping NA values!
head(sort(table(test.new$keyword), decreasing=TRUE), 50)
# Based on the data it looks like youtube, google, and other should be the categories
is.goo = unlist(lapply(as.character(test.new$keyword), function(x) grepl("goo",x)))
is.you = unlist(lapply(as.character(test.new$keyword), function(x) grepl("you",x)))
is.not = !(is.goo|is.you)
test.new$keyword = as.character(test.new$keyword)
test.new$keyword[is.goo] = "Google"
test.new$keyword[is.you] = "YouTube"
test.new$keyword[is.not] = "Other"
head(test.new$keyword, 100)
table(test.new$keyword)

# "isTrueDirect"
head(test.new$isTrueDirect,100)
test.new$isTrueDirect[is.na(test.new$isTrueDirect)] = FALSE
head(test.new$isTrueDirect,100)
table(test.new$isTrueDirect)

# "referralPath"
head(test.new$referralPath,100)
head(sort(table(test.new$referralPath), decreasing=TRUE), 50)
# google and permissions = Google
# yt = Youtube
# analytics = Analytics
# / = Other
is.goo = unlist(lapply(as.character(test.new$referralPath), function(x) grepl("goo",x)))
is.goo2 =unlist(lapply(as.character(test.new$referralPath), function(x) grepl("permissions",x)))
is.goo = is.goo|is.goo2
is.ana = unlist(lapply(as.character(test.new$referralPath), function(x) grepl("analytics",x)))
is.you = unlist(lapply(as.character(test.new$referralPath), function(x) grepl("yt",x)))
is.not = !(is.goo|is.ana|is.you)
test.new$referralPath = as.character(test.new$referralPath)
test.new$referralPath[is.ana] = "Analytics"
test.new$referralPath[is.goo & !is.ana] = "Google"
test.new$referralPath[is.you & !is.goo & !is.ana] = "YouTube"
test.new$referralPath[is.not] = "Other"
table(test.new$referralPath)

# "adContent"      
head(test.new$adContent,100)
head(sort(table(test.new$adContent), decreasing=TRUE), 20)
# Google, ad, and other
is.goo = unlist(lapply(as.character(test.new$adContent), function(x) grepl("goog",x)))
is.ad = unlist(lapply(as.character(test.new$adContent), function(x) grepl("ad",x)))
is.ad = is.ad & !is.goo
is.not = !(is.goo | is.ad)
test.new$adContent = as.character(test.new$adContent)
test.new$adContent[is.goo] = "Google"
test.new$adContent[is.ad] = "Ad"
test.new$adContent[is.not] = "Other"
table(test.new$adContent)

# "campaignCode"                  
head(test.new$campaignCode,100)
head(sort(table(test.new$campaignCode), decreasing=TRUE), 20)
# Useless, drop
test.new$campaignCode = NULL

# "adwordsClickInfo.page"         
head(test.new$adwordsClickInfo.page,100)
head(sort(table(test.new$adwordsClickInfo.page), decreasing=TRUE), 20)
# Is numeric
test.new$adwordsClickInfo.page = as.numeric(test.new$adwordsClickInfo.page)
not=is.na(test.new$adwordsClickInfo.page)
test.new$adwordsClickInfo.page[not] = 0
table(test.new$adwordsClickInfo.page)

# "adwordsClickInfo.slot"         
head(test.new$adwordsClickInfo.slot,100)
head(sort(table(test.new$adwordsClickInfo.slot), decreasing=TRUE), 20)
test.new$adwordsClickInfo.slot = as.character(test.new$adwordsClickInfo.slot)
not=is.na(test.new$adwordsClickInfo.slot)
test.new$adwordsClickInfo.slot[not] = "Other"
table(test.new$adwordsClickInfo.slot)

# "adwordsClickInfo.gclId"        
head(test.new$adwordsClickInfo.gclId,100)
head(sort(table(test.new$adwordsClickInfo.gclId), decreasing=TRUE), 5)
# This is really messy. Drop
test.new$adwordsClickInfo.gclId = NULL

# "adwordsClickInfo.adNetworkType"
head(test.new$adwordsClickInfo.adNetworkType,100)
head(sort(table(test.new$adwordsClickInfo.adNetworkType), decreasing=TRUE), 20)
test.new$adwordsClickInfo.adNetworkType = as.character(test.new$adwordsClickInfo.adNetworkType)
not=is.na(test.new$adwordsClickInfo.adNetworkType)
test.new$adwordsClickInfo.adNetworkType[not] = "Other"
table(test.new$adwordsClickInfo.adNetworkType)

# "adwordsClickInfo.isVideoAd"    
head(test.new$adwordsClickInfo.isVideoAd,100)
head(sort(table(test.new$adwordsClickInfo.isVideoAd), decreasing=TRUE), 20)
istrue = is.na(test.new$adwordsClickInfo.isVideoAd)
test.new$adwordsClickInfo.isVideoAd = as.logical(test.new$adwordsClickInfo.isVideoAd)
test.new$adwordsClickInfo.isVideoAd[istrue] = TRUE
table(test.new$adwordsClickInfo.isVideoAd)

# "pageviews"                     
head(test.new$pageviews,100)
head(sort(table(test.new$pageviews), decreasing=TRUE), 20)
sum(is.na(test.new$pageviews))
not = is.na(test.new$pageviews)
test.new$pageviews[not] = 0
table(test.new$pageviews)

# "bounces"                       
head(test.new$bounces,100)
head(sort(table(test.new$bounces), decreasing=TRUE), 20)
not = is.na(test.new$bounces)
test.new$bounces[not] = 0 
table(test.new$bounces)

# "newVisits"
head(test.new$newVisits,100)
head(sort(table(test.new$newVisits), decreasing=TRUE), 20)
not = is.na(test.new$newVisits)
test.new$newVisits[not] = 0 
table(test.new$newVisits)

#
#
#

# Start with browser. See which levels are the best predictors 
nam=levels(train.final$browser)
test.new$browser = as.character(test.new$browser)
is.nam = lapply(test.new$browser, function(x) any(nam==x))
is.nam = unlist(is.nam)  
test.new$browser[!is.nam] = "Other"
test.new$browser = as.factor(test.new$browser)

# Operating system
nam=levels(train.final$operatingSystem)
test.new$operatingSystem = as.character(test.new$operatingSystem)
is.nam = lapply(test.new$operatingSystem, function(x) any(nam==x))
is.nam = unlist(is.nam)  
test.new$operatingSystem[!is.nam] = "Other"
test.new$operatingSystem = as.factor(test.new$operatingSystem)

# Subcontinent
test.new$subContinent = as.factor(test.new$subContinent)

# Country
nam=levels(train.final$country)
test.new$country = as.character(test.new$country)
is.nam = lapply(test.new$country, function(x) any(nam==x))
is.nam = unlist(is.nam)  
test.new$country[!is.nam] = "(not set)"
test.new$country = as.factor(test.new$country)

# Network Domain
nam=levels(train.final$networkDomain)
test.new$networkDomain = as.character(test.new$networkDomain)
is.nam = lapply(test.new$networkDomain, function(x) any(nam==x))
is.nam = unlist(is.nam)  
test.new$networkDomain[!is.nam] = "(not set)"
test.new$networkDomain[test.new$networkDomain=="unknown.unknown"] = "(not set)"
test.new$browser = as.factor(test.new$browser)

# Campaign
test.new$campaign = NULL

# Source
test.new$source = as.character(test.new$source)

is.goo = unlist(lapply(test.new$source, function(x) grepl("google",x)))
is.dir = unlist(lapply(test.new$source, function(x) grepl("direct",x)))
is.yah = unlist(lapply(test.new$source, function(x) grepl("yah",x)))
is.fb = unlist(lapply(test.new$source, function(x) grepl("faceb",x)))
is.not = !(is.goo|is.dir|is.yah|is.fb)

test.new$source[is.goo] = "Google"
test.new$source[is.dir] = "(direct)"
test.new$source[is.yah] = "Yahoo"
test.new$source[is.fb] = "Facebook"
test.new$source[is.not] = "Other"

table(as.factor(test.new$source))
test.new$source = as.factor(test.new$source)

# Repeat
test.final = test.new

# Fix some last errors
test.final$medium = as.factor(test.final$medium)
test.final$deviceCategory = as.factor(test.final$deviceCategory)
test.final$continent = as.factor(test.final$continent)

# Remove some columns for later
train.id = train.final$fullVisitorId
test.id = test.final$fullVisitorId
train.final$fullVisitorId = NULL
test.final$fullVisitorId = NULL

############
# Part 2.7 #
############

# Skip if running this file as a stand-alone

# Write as csv
# write.csv(train.final, file="train_final.csv")
# write.csv(test.final, file="test_final.csv")
 
# write.csv(train.id, file="train_id.csv")
# write.csv(test.id, file="test_id.csv")

# # Full cleanup
# rm(list=ls())
# gc()

# Read in to save ram
# wd = "F:/2018 Fall/SYS 6018, Data Mining/assignments/kaggle/04_Google/kaggle4"
# setwd(wd)
# train.final = read.csv("train_final.csv")
# test.final = read.csv("test_final.csv")
# train.id = read.csv("train_id.csv")
# test.id = read.csv("test_id.csv")
# train.final$X = NULL
# test.final$X = NULL

# unlist(lapply(train.final,class))

# train.final$date = as.Date(as.character(train.final$date), format="%Y%m%d")
# train.final$sessionId = as.character(train.final$sessionId)
# train.final$visitStartTime = as.character(train.final$visitStartTime)
# test.final$date = as.Date(as.character(test.final$date), format="%Y%m%d")
# test.final$sessionId = as.character(test.final$sessionId)
# test.final$visitStartTime = as.character(test.final$visitStartTime) 

# unlist(lapply(train.final,class))

############
# Part 2.8 #
############

# # Dummy encoding
# library(dummies)
# library(Matrix)
# 
# dum = names(train.final)[unlist(lapply(train.final, is.factor))]
# dum = dum[!dum %in% "fullVisitorId"]
# 
# train.list = Matrix(nrow=dim(train.final)[1], ncol=0)
# for (i in dum) {
#   train.list = cbind(train.list, Matrix(dummy(train.final[,i])))
# }
# 
# test.list = Matrix(nrow=dim(test.final)[1], ncol=0)
# for (i in dum) {
#   test.list = cbind(test.list, Matrix(dummy(test.final[,i])))
# }
# 
# dim(train.list)
# dim(test.list)

##########
# Part 3 #
##########
# Ridge regression

# library(e1071)
# 
# # Take log transform of Y
# skewness(train.final$transactionRevenue)
# skewness(log(train.final$transactionRevenue+1))
# Y = log(train.final$transactionRevenue+1)
# train.final$transactionRevenue = NULL
# all.train = cbind(train.list,Y)
# 
# # Parallel processing
# library(glmnet)
# library(doParallel)
# library(parallel)
# registerDoParallel(detectCores())
# 
# glm_model = cv.glmnet(train.list,Y,alpha=1,parallel=TRUE)
# 
# plot(glm_model)
# min(glm_model$cvm)
# # Get min lambda
# lam=glm_model$lambda.min
# lam
# 
# # Predict
# pred=predict(glm_model, test.list, type="response", s=lam)
# hist(pred, breaks=100)
# 
# # Transform back
# pred.trans = exp(pred)
# hist(pred.trans, breaks=100)
# 
# # Return the data frame in the format kaggle likes
# out = data.frame(fullVisitorId=test.id, PredictedLogRevenue=pred)
# names(out) = c("fullVisitorId","PredictedLogRevenue")
# 
# # Aggregate mean log
# library(data.table)
# out.dt = data.table(out)
# out.agg = out.dt[,median(PredictedLogRevenue), by=list(out$fullVisitorId)]
# out.agg = data.frame(out.agg)
# names(out.agg) = c("fullVisitorId","PredictedLogRevenue")
# head(out.agg)
# dim(out.agg)
# 
# write.csv(out.agg, "lin_1_agg.csv", row.names = FALSE)
# 
# # Try performing some naive estimations?
# out.agg2 = out.agg
# out.agg2$PredictedLogRevenue[out.agg2$PredictedLogRevenue<1.6] = 0
# hill = mean(log(train.new$transactionRevenue[train.new$transactionRevenue>0]))
# out.agg2$PredictedLogRevenue[out.agg2$PredictedLogRevenue!=0] = hill
# 
# write.csv(out.agg, "lin_2_agg.csv", row.names = FALSE)
# 
# # Not very good!
# 
# # Try out random forest
# # but first cleanup!
# 
# rm(all.train)
# rm(glm_model)
# rm(out)
# rm(out.agg)
# rm(out.agg2)
# rm(out.df)
# rm(pred)
# rm(pred.trans)
# rm(test)
# rm(test.json)
# rm(test.json.parsed)
# rm(test.list) 
# rm(test.new)
# rm(train.list)
# rm(train.new)
# gc()

##########
# Part 4 #
##########
# Random forest

library(randomForest)
library(caret)

# Create temporary df
train.rf = train.final
test.rf = test.final

# Drop certain columns that don't make sense
train.rf$date = NULL
train.rf$sessionId = NULL
train.rf$visitStartTime  = NULL
train.rf$visitId = NULL
test.rf$date = NULL
test.rf$sessionId = NULL
test.rf$visitStartTime  = NULL
test.rf$visitId = NULL

# We have a big problem, the vector size is too large. Reduce by selecting only
# a few predictors.

names(train.rf)
train.rf$visitNumber = NULL
train.rf$isMobile = NULL
train.rf$deviceCategory = NULL
train.rf$continent = NULL
train.rf$country = NULL
train.rf$networkDomain = NULL
train.rf$isTrueDirect = NULL
train.rf$adwordsClickInfo.isVideoAd = NULL
train.rf$hits = NULL
train.rf$bounces = NULL
train.rf$newVisits = NULL
train.rf$adwordsClickInfo.slot = NULL
train.rf$adwordsClickInfo.adNetworkType = NULL
train.rf$pageviews = NULL

test.rf$visitNumber = NULL
test.rf$isMobile = NULL
test.rf$deviceCategory = NULL
test.rf$continent = NULL
test.rf$country = NULL
test.rf$networkDomain = NULL
test.rf$isTrueDirect = NULL
test.rf$adwordsClickInfo.isVideoAd = NULL
test.rf$hits = NULL
test.rf$bounces = NULL
test.rf$newVisits = NULL
test.rf$adwordsClickInfo.slot = NULL
test.rf$adwordsClickInfo.adNetworkType = NULL
test.rf$pageviews = NULL

# This is still too large, maybe try one or two predictors that make sense?

train2 = train.rf[c("subContinent","adwordsClickInfo.page","transactionRevenue")]

# Do the validation set method
library(doParallel)
mtry = sqrt(ncol(train.rf))
grid = expand.grid(.mtry=mtry)
# Unfortunately we have way too many obs. Try to use as many for the training set and use the rest for validation
ind = sample(1:nrow(train.rf), 200000)
train = train2[ind,]
valid = train2[-ind,]
train$transactionRevenue = log(train$transactionRevenue+1)

rf = randomForest(transactionRevenue~., data=train)

# Predict validation
predval = predict(rf, valid, type="response")
mse = (predval-valid$transactionRevenue)**2
mean(mse)

# The crossvalidation score isn't very good. This is because we omitted so much
# information. The datasize is too large and we cannot process this volume of data.
# The methodology is the same, regardless.

# Get the predictions on the test set
pred = predict(rf, test.rf, type="response")

# Plot the predictions
par(mfrow=c(1,1))
hist(pred)
summary(pred)

# Use some naive transformations
# pred.2 = pred
# pred.2[pred.2<1] = 0
# pred.2[pred.2>1] = log(median(train.rf$transactionRevenue[train.rf$transactionRevenue>0]))
# hist(pred.2)
# These lowered accuracy by quite a bit, let's not do this

pred.2=pred

# Return the data frame in the format kaggle likes
library(stringr)
out = data.frame(fullVisitorId=test.id, predictedLogRevenue=pred.2)
names(out) = c("fullVisitorId","predictedLogRevenue")

# Aggregate by min value
library(data.table)
out.dt = data.table(out)
out.agg = out.dt[,min(predictedLogRevenue), by=list(out$fullVisitorId)]
out.agg = data.frame(out.agg)
names(out.agg) = c("fullVisitorId","predictedLogRevenue")
head(out.agg)
dim(out.agg)

write.csv(out.agg, "rf_1_min.csv", row.names = FALSE)