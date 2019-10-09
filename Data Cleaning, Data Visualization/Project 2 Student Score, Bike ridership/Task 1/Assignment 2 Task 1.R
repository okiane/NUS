#Set working directory
setwd("E:/NUS/EBAC/EBA5001 Management of Business Analytics Project/1 Data Analytics Process and Best Practices/Assignment/Assignment 2/Task 1")

#Install package to read .xlsx file
#install.packages("gdata")

pacman::p_load(tidyverse, rebus,IRdisplay,data.table, 
               stringr, lubridate, editrules, gdata)

#Read .xlsx files 
key <- readxl::read_xlsx("./DABP_D3_Assignment_T1_Data/data/key.xlsx",col_names=TRUE)

response <- data.frame(readxl::read_xlsx("./DABP_D3_Assignment_T1_Data/data/response.xlsx",col_names=TRUE))

#Check file

##head(key)
##dim(key)
  
##head(response)
##dim(response)

#Summarize all answers for each question and check if further data clean up is necessary
##sapply(response,unique)

#match(response$Q.1, key[1,1], nomatch = 0)
#response$Q.1 %in% key[1,1]

#Data cleaning and preparation: Remove extra whitespaces and convert to lowercase
##key <- lapply(key, function(x) str_to_lower(str_squish(x)))

##response <- sapply(response, function(x) str_to_lower(str_squish(x)))

#Match student response with answer key
#identical(key[,2:ncol()],response)

#checkAns <- function(r,c){
#  identical(response[r,c],key[1,c])
#}

#Calculate frequency of correct answers

#Generate column of bar chart of Correct or Wrong
#Mode of wrong answers (proportion)

#score of whole cohort

#In educational context, there's a discrimination index if we want to calculate that

#Generate stacked bar chart for each question