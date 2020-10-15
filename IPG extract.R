install.packages("pdftools")
install.packages("pdftools")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("textdata")
install.packages("tesseract") 
install.packages("dplyr")

library(pdftools)
library(tesseract)
library(tidyverse)
library(tidyr)         # For data cleaning
library(tidytext)      # For data cleaning of text corpus
library(ggplot2)
library(stringr)
library(dplyr)

setwd("O:/Administration/Employee Folders/Jason's Folder/R Data/IPG")
files <- list.files() #No parameter necessary now since you're in the proper directory
files1 <- list.files(path="O:/Administration/Employee Folders/Jason's Folder/R Data/IPG", pattern="*.pdf", full.names=TRUE, recursive=FALSE)
f <- list()
t <- data.frame()


IPG <- pdf_text("O:/Administration/Employee Folders/Jason's Folder/R Data/IPG/20190617 Lindskog Foundation IPG Signed and Executed.pdf")


setwd("O:/Administration/Employee Folders/Jason's Folder/R Data/IPG Image")

for (i in 1:length(files1)) 
  f[[i]] <- pdftools::pdf_convert(files1[i])

files2 <- list.files(path="O:/Administration/Employee Folders/Jason's Folder/R Data/IPG Image", pattern="*.png", full.names=TRUE, recursive=FALSE)


for (i in 1:length(files2))
  text <- tesseract::ocr(files2[i])

files1 <- list.files(path="O:/Administration/Employee Folders/Jason's Folder/R Data/IPG", pattern="*.pdf", full.names=TRUE, recursive=FALSE)

for (i in 1:length(files1)) {
  t[i,1] <- files1[i]
  x <- pdftools::pdf_convert(files1[i], dpi = 600)
  text <- tesseract::ocr(x)
  cat(text)
  text2 <- strsplit(text, "\n") %>% unlist()
  allocation <- which(grepl("ASSET ALLOCATION GUIDELINES", text2, ignore.case = T))
  FI <- which(grepl("Fixed Income", text2, ignore.case = T))
  if (length(FI) > 1) 
    FI2 <- FI[length(FI)]
  else
    FI2 <-FI
  allocationlines <- c(allocation:FI2+1)
  allocation_extracted <- text2[allocationlines]
  t[i,2] <- allocation_extracted[which(grepl("Domestic", allocation_extracted, ignore.case = TRUE))]
  t[i,3] <- allocation_extracted[which(grepl("Foreign", allocation_extracted, ignore.case = TRUE))]
  t[i,4] <- allocation_extracted[which(grepl("Natural Resources", allocation_extracted, ignore.case = TRUE))]
  t[i,5] <- allocation_extracted[which(grepl("REal Estate", allocation_extracted, ignore.case = TRUE))]
  t[i,6] <- allocation_extracted[which(grepl("Alternative Investments", allocation_extracted, ignore.case = TRUE))]
  t[i,7] <- allocation_extracted[which(grepl("Fixed Income", allocation_extracted, ignore.case = TRUE))]
  t[i,8] <- allocation_extracted[which(grepl("Cash", allocation_extracted, ignore.case = TRUE))]
}


files2
test<-cat(t)


##########
eng <- tesseract("eng")

pngfile <- pdftools::pdf_convert("O:/Administration/Employee Folders/Jason's Folder/R Data/IPG/20190617 Lindskog Foundation IPG Signed and Executed.pdf", dpi = 600)
text <- tesseract::ocr(pngfile)
cat(text)
text

length(FI)

text2 <- strsplit(text, "\n") %>% unlist()
allocation <- which(grepl("ASSET ALLOCATION GUIDELINES", text2, ignore.case = T))
FI <- which(grepl("Fixed Income", text2, ignore.case = T))
FI2 <- FI[2]  ##need to fix this so only works
#cash <- which(grepl("Cash", text2, ignore.case = T))
allocationlines <- c(allocation:FI2+1)
allocationlines
allocation_extracted <- text2[allocationlines]
allocation_extracted 
allocation2 <- paste(allocation_extracted, collapse = " ")
allocation2
DF <- data.frame(allocation2)
DF

DE <- allocation_extracted[which(grepl("Domestic", allocation_extracted, ignore.case = TRUE))]
FE <- allocation_extracted[which(grepl("Foreign", allocation_extracted, ignore.case = TRUE))]
NR <- allocation_extracted[which(grepl("Natural Resources", allocation_extracted, ignore.case = TRUE))]
RE <- allocation_extracted[which(grepl("REal Estate", allocation_extracted, ignore.case = TRUE))]
AA <- allocation_extracted[which(grepl("Alternative Investments", allocation_extracted, ignore.case = TRUE))]
FI <- allocation_extracted[which(grepl("Fixed Income", allocation_extracted, ignore.case = TRUE))]
C <- allocation_extracted[which(grepl("Cash", allocation_extracted, ignore.case = TRUE))]


