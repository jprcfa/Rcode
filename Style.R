install.packages("FactorAnalytics", repos="http://R-Forge.R-project.org")
library(FactorAnalytics)
library(PerformanceAnalytics)
library(readr)
library(xts)
library(ggplot2)
library(reshape2)

style <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/Style.csv", stringsAsFactors=FALSE)
date <- as.Date(style$Date, format = "%m/%d/%Y")
style$Date <- date
style.xts <- xts(style[,-1], order.by = style[,1])

chart.Drawdown(style.xts["2005::",c(1:4, 16, 17)], legend.loc = "bottomleft", plot.engine = "ggplot2")
table.Drawdowns(style.xts["2000::",1], top = 10)

chart.RollingStyle(style.xts["2005::",17],style.xts[,1:4], method="constrained", width=24, cex.legend = .7, colorset=tol12qualitative, las=1)
chart.RollingStyle(style.xts["2005::",17],style.xts[,1:4], method="unconstrained", width=24, cex.legend = .7, colorset=tol12qualitative, las=1)
chart.RollingStyle(style.xts["2005::",17],style.xts[,1:4], method="normalized", width=24, cex.legend = .7, colorset=tol12qualitative, las=1)

chart.RollingStyle(style.xts["2005::",17],style.xts["2005::",c(6:11,13:15,5,12)], method="unconstrained", width=24, cex.legend = .7, colorset=tol12qualitative, las=1)
chart.RollingStyle(style.xts["2005::",17],style.xts[,c(5:15)], method="constrained", width=24, cex.legend = .7, colorset=tol12qualitative, las=1)

chart.RollingStyle(style.xts["2005::",16],style.xts[,1:4], method="constrained", width=24, cex.legend = .7, colorset=tol12qualitative, las=1)
chart.RollingStyle(style.xts["2005::",16],style.xts[,c(6:11,13:15,5,12)], method="constrained", width=18, cex.legend = .7, colorset=tol12qualitative, las=1)
chart.RollingStyle(style.xts["2005::",16],style.xts[,c(5:15)], method="constrained", width=24, cex.legend = .7, colorset=tol12qualitative, las=1)

style.QPfit(style.xts["2009::2010",17],style.xts["2009::2010",c(6:11,13:15,5,12)], method="constrained")
chart.Style(style.xts["2009-12::2010-12",17],style.xts["2009-12::2010-12",c(6:11,13:15,5,12)], method="constrained")
