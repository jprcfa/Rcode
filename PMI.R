library(ggplot2)
library(readxl)
library(reshape)

PMI <- read.csv("O:/Administration/Employee Folders/Jason's Folder/R Data/PMI.csv")
lastPMI <- PMI[1,1:25]

mlastPMI <- melt(lastPMI)
mPMI <- melt(PMI)

mPMI$Date <- as.Date(mPMI$Date, format = "%m/%d/%Y")
mlastPMI$Date <- as.Date(mlastPMI$Date, format = "%m/%d/%Y")

g <- ggplot(mPMI, aes(x = Date, y = value))

g + geom_line() + facet_wrap(~ variable) +
  theme_classic() + geom_hline(yintercept = 50, linetype = "dashed") +
  geom_text(aes(x = Date, y = value, label = value), mlastPMI, size = 4, vjust = 0, hjust = 1) +
  ylab("Markit PMI")

          
         
