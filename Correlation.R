install.packages("corrplot")
library(corrplot)
library(PerformanceAnalytics)
library(readr)
library(xts)

#load prices and securites
prices <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/OPCM DE.csv")
date <- as.Date(prices$Date, format = "%m/%d/%Y")
prices$Date <- date
prices.xts <- xts(prices[,-1], order.by = prices[,1])

#calculate returns
returns.xts <- Return.calculate(prices.xts, method = c("log"))
returns.xts <- returns.xts[-1,]

#sub set years to look at
returns2yr <- returns.xts['2017-11::']
returns4yr <- returns.xts['2015-11::']
returns5yr <- returns.xts['2014-11::']

#find missing data
M <- sapply(returns2yr, function(x) sum(is.na(x))); M[M>0]
M <- sapply(returns4yr, function(x) sum(is.na(x))); M[M>0]
M <- sapply(returns5yr, function(x) sum(is.na(x))); M[M>0]
M <- sapply(returns.xts, function(x) sum(is.na(x))); M[M>0]

#remove tickers with missing data
returns5yr <- subset(returns5yr, select = -c(dow, flow, jo, goog))
returns4yr <- subset(returns4yr, select = -c(dow))
returns2yr <- subset(returns2yr, select = -c(dow))

#calculate and display correlations
corr <- cor(returns2yr) 
corrplot(corr, method= "circle", order = "hclust", addrect = 5)

#with p-values
res1 <- cor.mtest(returns2yr, conf.level = .95)
res2 <- cor.mtest(returns2yr, conf.level = .99)

corrplot(corr, p.mat = res1$p, sig.level = .05)
