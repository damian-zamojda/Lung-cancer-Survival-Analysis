# ========================================
# 1. Project setup
# ========================================

# Installing packages 
install.packages("survival")
install.packages("survminer")
install.packages("tidyverse")
install.packages("devtools")

# Loading packages
library(survival)
library(survminer)
library(tidyverse)
library(devtools)

# Loading raw data
data_raw <- read.csv("https://www.key2stats.com/NCCTG_Lung_Cancer_Data_535_29.csv", stringsAsFactors = T)

data("lung")
head(lung)
head(data_raw)

# Deselecting redundant columns, changing depended variable to binary form, changing sex to binary 
data <- data_raw %>% select(3:12) %>% mutate(status = ifelse(.$status==1,0,1),
                                             sex = ifelse(.$sex==1,0,1)) #Male=1, Female=2

# Saving data for further use
save(data, file = "data/data.RData")
