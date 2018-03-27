# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TMIST
# Purpose:      testing
# programmer:   Anqi Chen
# Date:         20-11-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(progress)
pb <- progress_bar$new(format = "[:bar] :percent finished in :elapsed",
                       total = 50,
                       clear = FALSE,
                       width= 60)
for (i in 1:2) { 
  
  pb$tick()
  
  Phase = 1
  R_Json_Path <- "1dc11b20-f653-421a-bd88-091cdeb4e098"
  source("stp_handler.R")
  rm(list=setdiff(ls(),"pb"))
  Phase = 2
  R_Json_Path <- "1dc11b20-f653-421a-bd88-091cdeb4e098"
  source("stp_handler.R")
  rm(list=ls())
}
