install.packages("pdftools")
install.packages("pdftools")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("textdata")
install.packages("tesseract") 
install.packages("dplyr")
install.packages("magick")

library(pdftools)
library(tesseract)
library(tidyverse)
library(tidyr)         # For data cleaning
library(tidytext)      # For data cleaning of text corpus
library(ggplot2)
library(stringr)
library(dplyr)
library(magick)

setwd("O:/Administration/Employee Folders/Jason's Folder/R Data/IPG Image")
setwd()
files <- list.files() #No parameter necessary now since you're in the proper directory
files1 <- list.files(path="O:/Administration/Employee Folders/Jason's Folder/R Data/IPG", pattern="*.pdf", full.names=TRUE, recursive=FALSE)
f <- list()
t <- data.frame()


files1 <- list.files(path="O:/Operations/AdvisorPeak Onboarding/onboarding work in progress docs/IPGs", pattern="*.pdf", full.names=TRUE, recursive=FALSE)
t <- data.frame()

for (i in 1:length(files1)) {
  t[i,1] <- files1[i]
  x <- pdftools::pdf_convert(files1[i], dpi = 600)
  text <- tesseract::ocr(x)
  #cat(text)
  text2 <- strsplit(text, "\n") %>% unlist()
  allocation <- which(grepl("ASSET ALLOCATION GUIDELINES", text2, ignore.case = TRUE))
  if (length(allocation) == 0) {
    allocation <- 70
    allocationend <- allocation + 50
  } else {
    allocation <- allocation - 5
    allocationend <- allocation + 15
  }
  #allocation <- allocation - 5
  #allocationend <- allocation + 15
  #FI <- which(grepl("Fixed Income", text2, ignore.case = TRUE))
  #if (length(FI) > 1) {
  #  FI2 <- FI[length(FI)] +5
  #} else {
  #  FI2 <-FI +5
  #}
  allocationlines <- c(allocation:allocationend)
  allocation_extracted <- text2[allocationlines]
  acct <- which(grepl("ACCOUNTS PERTAINING", text2, ignore.case = TRUE))
  acctend <- which(grepl("COMBINING", text2, ignore.case = TRUE))
  if (length(acct) == 0) {
    acct <- 70
  } 
  if (length(acctend) == 0) {
    acctend <- 120
  } 
  acctlines <- c(acct:acctend)
  acctextract <- text2[acctlines]
  t[i,2] <- paste(allocation_extracted[which(grepl("Domestic", allocation_extracted, ignore.case = TRUE))], collapse = " ")
  t[i,3] <- paste(allocation_extracted[which(grepl("Foreign", allocation_extracted, ignore.case = TRUE))], collapse = " ")
  t[i,4] <- paste(allocation_extracted[which(grepl("Natural Resources", allocation_extracted, ignore.case = TRUE))], collapse = " ")
  t[i,5] <- paste(allocation_extracted[which(grepl("Real Estate", allocation_extracted, ignore.case = TRUE))], collapse = " ")
  t[i,6] <- paste(allocation_extracted[which(grepl("Alternative Investments", allocation_extracted, ignore.case = TRUE))], collapse = " ")
  t[i,7] <- paste(allocation_extracted[which(grepl("Fixed Income", allocation_extracted, ignore.case = TRUE))], collapse = " ")
  t[i,8] <- paste(allocation_extracted[which(grepl("Cash", allocation_extracted, ignore.case = TRUE))], collapse = " ")
  t[i,9] <- paste(acctextract, collapse = " ")
}


write.csv(t, "O:/Administration/Employee Folders/Jason's Folder/R Data/IPG.csv")

files2
test<-cat(t)


##########
eng <- tesseract("eng")

pngfile <- pdftools::pdf_convert("O:/Operations/AdvisorPeak Onboarding/onboarding work in progress docs/IPGs/20190417 Goldberg, David IPG Signed and Executed (trust and IRAs).pdf", dpi = 600)


text <- tesseract::ocr(pngfile)
cat(text)
text

length(FI)

text2 <- strsplit(text, "\n") %>% unlist()
allocation <- which(grepl("ASSET ALLOCATION GUIDELINES", text2, ignore.case = T))
FI <- which(grepl("Fixed Income", text2, ignore.case = T))
acct <- which(grepl("Pertaining", text2, ignore.case = T))
acctend <- acct + 10
acctlines <- c(acct:acctend)

if (length(FI) > 1) {
  FI2 <- FI[length(FI)]
} else {
  FI2 <-FI
}
#cash <- which(grepl("Cash", text2, ignore.case = T))
allocationlines <- c(allocation:FI2+5)
allocationlines
allocation_extracted <- text2[allocationlines]
allocation_extracted 
allocation2 <- paste(allocation_extracted, collapse = " ")
allocation2
DF <- data.frame(allocation2)
DF

acct <- which(grepl("Pretaining", text2, ignore.case = T))
acctlines <- c(acct:5)

DE <- allocation_extracted[which(grepl("Domestic", allocation_extracted, ignore.case = TRUE))]
FE <- allocation_extracted[which(grepl("Foreign", allocation_extracted, ignore.case = TRUE))]
NR <- allocation_extracted[which(grepl("Natural Resources", allocation_extracted, ignore.case = TRUE))]
RE <- allocation_extracted[which(grepl("REal Estate", allocation_extracted, ignore.case = TRUE))]
AA <- allocation_extracted[which(grepl("Alternative Investments", allocation_extracted, ignore.case = TRUE))]
FIp <- allocation_extracted[which(grepl("Fixed Income", allocation_extracted, ignore.case = TRUE))]
C <- allocation_extracted[which(grepl("Cash", allocation_extracted, ignore.case = TRUE))]


