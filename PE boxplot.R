install.packages("reshape2")
install.packages("tidyverse")
library(PerformanceAnalytics)
library(readr)
library(xts)
library(ggplot2)
library(reshape2)
library(tidyverse)

PE <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/PE.csv")
date <- as.Date(PE$Date, format = "%m/%d/%Y")
PE$Date <- date
PE.xts <- xts(PE[,-1], order.by = PE[,1])
PE5yr <- PE.xts["2014::"]


PE5yr.df <- fortify(PE5yr)
PE5yr.df <- subset(PE5yr.df, select = -c(C , MYL , TER, ATVI))


mPE <- melt.data.frame(PE5yr.df, measure.vars = 2:41, na.rm = TRUE)
head(mPE)

mPE1 <- mPE %>% 
  group_by(variable) %>% 
  summarise(LastVal = last(value))

head(mPE1)

P <- ggplot(mPE, aes(x= reorder(variable, value, FUN = median), y = value)) + 
  geom_boxplot(outlier.alpha = 0) + 
  coord_cartesian(ylim = c(0, 50)) + 
  geom_point(data = mPE1, aes(x = variable, y = LastVal), shape = 18, size = 4) +
  theme_classic() 
P 
