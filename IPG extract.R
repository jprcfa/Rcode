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

setwd("O:/Administration/Employee Folders/Jason's Folder/R Data/IPG")
files <- list.files() #No parameter necessary now since you're in the proper directory
f <- list()

eng <- tesseract("eng")

IPG <- pdf_text("O:/Administration/Employee Folders/Jason's Folder/R Data/PDF test.pdf")
    
pngfile <- pdftools::pdf_convert("O:/Administration/Employee Folders/Jason's Folder/R Data/PDF test.pdf", dpi = 600)
text <- tesseract::ocr(pngfile)
cat(text)
text


text2 <- strsplit(text, "\n") %>% unlist()
allocation <- which(grepl("ASSET ALLOCATION GUIDELINES", text2, ignore.case = T))
FI <- which(grepl("Fixed Income", text2, ignore.case = T))
allocationlines <- c(allocation:FI+3)
allocation_extracted <- text2[allocationlines]
allocation_extracted 
allocation2 <- paste(allocation_extracted, collapse = " ")
allocation2
cash
