train.data <- read.csv("train.csv")
train.response <- read.csv("train_tvals.csv")

# Generate the response variable
y <- train.response[,1]

# Generate the regressor variables
colnames(train.data)

# Drop obvisouly useless columns
train.final <- train.data
useless_cols <- c("channelGrouping","visitNumber","device_browser","device_deviceCategory",
                  "device_operatingSystem","geoNetwork_city","geoNetwork_continent","geoNetwork_country",
                  "geoNetwork_metro","geoNetwork_networkDomain","geoNetwork_region","geoNetwork_subContinent",
                  "totals_newVisits","trafficSource_medium","trafficSource_referralPath", "trafficSource_keyword",
                  "trafficSource_adContent","trafficSource_campaign","trafficSource_isTrueDirect",
                  "trafficSource_source","dayofweek","month","day","hour","count_hits_new_domain",
                  "browser_category","browser_operatingSystem","source_country")
train.final[,useless_cols] <- NULL

# As furthur test demonstrates, spline models can only work with continuous data
uncontinuous_col <- c("device_isMobile","totals_bounces","totals_hits","totals_pageviews")
train.final[,uncontinuous_col] <- NULL
names(train.final)

# Create regressors
X <- as.data.frame(train.final)
X <- X[-nrow(X),]
colnames(X) <- c("x1","x2","x3","x4")
x1 <- X$x1
x2 <- X$x2
x3 <- X$x3
x4 <- X$x4

# Create a spline model for the train data
library(splines)

# Choose the degree of freedom
dfs <- seq(2,4)
MSEs <- rep(0,3)
n <- length(y)

df <- 3
train.spline <- lm(y~ns(x1,df=df)+ns(x2,df=df)+ns(x3,df=df)+ns(x4,df=df))
y.fit <- as.numeric(fitted(train.spline))
MSE <- sum((y - y.fit)^2) / n
MSE

# Import the test data
test <- read.csv("test.csv")
test[,useless_cols] <- NULL
test[,uncontinuous_col] <- NULL
colnames(test) <- c("x1","x2","x3","x4")
test.result <- as.numeric(predict(train.spline, newdata=test, se=T))
test.result <- unlist(test.result, use.names=F)
# Import the test ids
test.id <- read.csv("test_user_id.csv", colClasses="character")

# Final table
test.final <- as.data.frame(cbind(test.id, test.result))

y.pred <- predict(train.spline, newdata=test.X, se=T)
output <- as.data.frame(cbind(test.id, y.pred))
write.csv(output, "result.csv")
