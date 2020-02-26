library(PerformanceAnalytics)
library(readr)
library(xts)
library(ggplot2)
library(reshape2)

prices <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/SP500.csv")
date <- as.Date(prices$Date, format = "%m/%d/%Y")
prices$Date <- date
prices.xts <- xts(prices[,-1], order.by = prices[,1])

returns.xts <- Return.calculate(prices.xts, method = c("discrete"))
returns.xts <- returns.xts[-1,]

chart.Drawdown(returns.xts["2010::",], legend.loc = "bottomleft", plot.engine = "ggplot2")
table.Drawdowns(returns.xts["2010::",1], top = 10)

?chart.Drawdown

SharpeRatio.annualized(returns.xts["2016::",])
charts.PerformanceSummary(returns.xts)
charts.PerformanceSummary(returns.xts["2010::",])


Return.annualized(returns.xts)
StdDev.annualized(returns.xts)
