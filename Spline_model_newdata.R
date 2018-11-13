train <- read.csv("train_final.csv")

# Drop the columns obviously useless for predicting
train.final[,"date"] <- NULL
train.final[,"fullVisitorId"] <- NULL
train.final[,"visitStartTime"] <- NULL
train.final[,c("visitId", "visitNumber","newVisits")] <- NULL
train.final[,c("country","continent", "subContinent")] <- NULL
train.final[,c("operatingSystem","browser", "dev")] <- NULL
train.final[,c("transactions","transactionRevenue")] <- NULL
train.final[,c("hits.1","pageviews","isTrueDirect")] <- NULL
train.final[,"deviceCategory"] <- NULL

# Drop the columns not useful for spline model
train.final[,"keyword"] <- NULL
train.final[,"channelGrouping"] <- NULL
train.final[,c("referralPath","networkDomain")] <- NULL
train.final[,c("adwordsClickInfo.slot","adwordsClickInfo.adNetworkType")] <- NULL
train.final[,"adContent"] <- NULL
train.final[,c("source","medium")] <- NULL
train.final[,"sessionQualityDim"] <- NULL

# Make everything numeric
which.true <- which(train.final[,"isMobile"] == TRUE)
which.false <- which(train.final[,"isMobile"] == FALSE)
train.final[which.true, "isMobile"] <- 1
train.final[which.false, "isMobile"] <- 0

which.true <- which(train.final[,"adwordsClickInfo.isVideoAd"] == TRUE)
which.false <- which(train.final[,"adwordsClickInfo.isVideoAd"] == FALSE)
train.final[which.true, "adwordsClickInfo.isVideoAd"] <- 1
train.final[which.false, "adwordsClickInfo.isVideoAd"] <- 0

# Delete rows where timeOnSite is not available
which.na <- which(is.na(train.final[,"timeOnSite"]))
train.final <- train.final[-which.na,]

# Create a spline model for the train data
library(splines)

# Generate response and predicting variables
library(SciViews)
y <- train.final$totalTransactionRevenue
X <- train.final[,-c("totalTransactionRevenue")]

# Choose the degree of freedom
dfs <- seq(2,10)
MSEs <- rep(0,9)
n <- length(y)
for(i in dfs){
  df <- dfs[i]
  train.spline <- lm(y~ns(X,df))
  y.fit <- as.numeric(fitted(train.spline))
  MSEs <- sum((y - y.fit)^2) / n
}

plot(dfs, MSEs, xlab="degree of freedom", ylab="Mean Squared Error", pch=16, type="l")

df <- 4
train.spline <- lm(y~ns(X,df))

# Import the test data
test <- read.csv("test_final.csv")
test.X <- test[,c("hits","isMobile","adwordsClickInfo.page","adwordsClickInfo.isVideoAd",
                  "bounces", "timeOnSite")]
test.id <- test$fullVisitorId
y.pred <- predict(train.spline, newdata=test.X, se=T)
output <- as.data.frame(cbind(test.id, y.pred))
write.csv(output, "result.csv")
