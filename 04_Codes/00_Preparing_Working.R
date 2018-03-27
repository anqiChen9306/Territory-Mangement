# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Bayer PromoReturn
# Purpose:      Setup the Directories and Reference
# programmer:   Xin Huang
# Date:         11-20-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# loading the required packages
options(java.parameters = "-Xmx2048m")

library(openxlsx)
library(RODBC)
library(dplyr)
library(plm)
library(dynlm)
library(randomForest)

## setup the directories
system("mkdir 01_Background 02_Inputs 03_Outputs 04_Codes 05_Internal_Review
       06_Deliveries")

