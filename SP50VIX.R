library(ggplot2)
library(reshape2)


VIXSPX <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/SP50VIX.csv", stringsAsFactors=FALSE)
head(VIXSPX)
VIXSPX$Year <-as.factor(VIXSPX$Year)

chart <- ggplot(VIXSPX, aes(x =  SP50, y = VIX))
                
chart + geom_point(aes(color = Year)) + theme_bw()