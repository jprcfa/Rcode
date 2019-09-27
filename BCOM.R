library(ggplot2)
library(readxl)
library(reshape)

BCOM <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/BCOM.csv")
BCOM6 <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/BCOM6.csv")

BCOM$Year <- as.factor(BCOM$Year)
BCOM6$Year <- as.factor(BCOM6$Year)

g <- ggplot(BCOM6, aes(x = X., y = BCOM, color = Year, shape = Year))
g + geom_point() + theme_light() +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_smooth(method = lm) + facet_grid(~ Year) +
  labs(x = "Dollar % Change", y = "BCOM % Change", title = "Rolling 6m")

lm <- lm(X. ~ BCOM, subset(BCOM, Year == "90"))
summary(lm)$r.squared
