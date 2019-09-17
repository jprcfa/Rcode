####
## Last updated on 9/13/19 by Jason Rodnick -uploaded to GitHub 9/17/19
####
##only need to run once 
install.packages("readxl")
install.packages("colorspace")
install.packages("ggplot2")
install.packages("tidyverse")

##run at the beginning of each session
library(ggplot2)
library(tidyverse)
library(readxl)
library(colorspace)

## read the R data from teh DE Risk Reward Reference v4 spreadsheet
RankSPRaw <- read_excel("//osb-sfo-dc01/data/APPDATA/ResearchUpdates/DE Risk Reward Reference v5.xlsx", sheet = "RTableData")
## pull out the required columns into new df RankSP (rank scatter plot)
RankSP <- RankSPRaw[c(1,2,4,10)]
## rename columns
colnames(RankSP) <- c("Ticker", "target", "rank", "W")

## calcuatle ranks from 1 to n for Up Down, Upside, and Weight
RankSP <- RankSP %>% 
  mutate(UDRank = dense_rank(desc(rank))) %>% 
  mutate(Upside = dense_rank(desc(target))) %>% 
  mutate(Weight = dense_rank(desc(W)))

## setting the numbers for the shaded areas
chartY <- (RankSP %>% summarise(max(RankSP$Weight)) - 1) / 2
chartXUS <- (RankSP %>% summarise(max(RankSP$Upside))) / 2  #for upside chart
chartXUD <- (RankSP %>% summarise(max(RankSP$UDRank))) / 2  #for updown chart
dotline <- RankSP %>% summarise(max(RankSP$Weight)) - 1

## change the previous numbers from data.frame to numeric... there is probably a simpler way to do this
chartY <- as.numeric(chartY)
chartXUS <- as.numeric(chartXUS)
chartXUD <- as.numeric(chartXUD)
dotline <- as.numeric(dotline)
 
## check that the data looks as expected
head(RankSP)

## X axis is Upside 
g <- ggplot(RankSP, aes(x =  Upside, y = Weight, label = Ticker))

g + geom_text(aes(color = UDRank)) + scale_y_reverse() + scale_x_reverse() + 
  coord_cartesian(ylim = c(0, chartY * 2), xlim = c(0, chartXUS * 2)) +
  geom_rect(aes(xmin = chartXUS, xmax = Inf, ymin = chartY - 1, ymax = -Inf), fill = "pink", alpha = 0.01) +
  geom_rect(aes(xmin = -Inf, xmax = chartXUS, ymin = dotline, ymax = chartY - 1), fill = "seagreen1", alpha = 0.01) +
  theme_classic() +  
  annotate("text", x = 32, y = chartY - chartY / 2, label = "Low Upside/High Weight", fontface = "italic") + #using the chartY variable to center the title
  annotate("text", x = 10, y = chartY + chartY / 2, label = "High Upside/Low Weight", fontface = "italic") + #using the chartY variable to center the title
  annotate("text", x = 10, y = dotline + 0.5, label = "Candidates", fontface = "bold") +
  labs(caption = paste(Sys.time())) +
  geom_hline(yintercept = dotline, linetype="dashed") +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank()) +
  xlab("Upside: Low to High") + ylab("Weight: Low to High")


## X axis is UpDown Rank
b <- ggplot(RankSP, aes(x =  UDRank, y = Weight, label = Ticker))

b + geom_text(aes(color = UDRank)) + scale_y_reverse() + scale_x_reverse() + 
  coord_cartesian(ylim = c(0, chartY * 2), xlim = c(0, chartXUD * 2)) +
  geom_rect(aes(xmin = chartXUD, xmax = Inf, ymin = chartY - 1, ymax = -Inf), fill = "pink", alpha = 0.01) +
  geom_rect(aes(xmin = -Inf, xmax = chartXUD, ymin = dotline, ymax = chartY - 1), fill = "seagreen1", alpha = 0.01) +
  theme_classic() +  
  annotate("text", x = 32, y = chartY - chartY / 2, label = "Low UpDown Rank/High Weight", fontface = "italic") + #using the chartY variable to center the title
  annotate("text", x = 10, y = chartY + chartY / 2, label = "High UpDown Rank/Low Weight", fontface = "italic") + #using the chartY variable to center the title
  annotate("text", x = 10, y = dotline + 0.5, label = "Candidates", fontface = "bold") +
  labs(caption = paste(Sys.time())) +
  geom_hline(yintercept = dotline, linetype="dashed") +
  theme(legend.position="none", axis.ticks = element_blank(), axis.text = element_blank()) +
  xlab("Up/Down Rank: Low to High") + ylab("Weight: Low to High")

