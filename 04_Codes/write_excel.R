# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TMIST
# Purpose:      write excel
# programmer:   Anqi Chen
# Date:         20-11-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(DT)
library(plyr)
library(dplyr)
library(tidyr)
library(digest)
library(openxlsx)
library(mongolite)
library(jsonlite)
library(utf8)
library(uuid)

options(scipen=200,
        mongodb = list(
          "host" = "0000000"
          # "username" = "root",
          # "password" = "root"
        ))

## receive signal
# argss[1] :  R_File_Path
# argss[2] :  filekey of json
# argss[3] :  reports save path
argss <- commandArgs(TRUE)
R_Json_Path <- argss[1]
Phase <- argss[2]

writeDown <- function(report){

  wb <- createWorkbook()

  ## 1
  addWorksheet(wb, rsd_sheet_names[1])
  report7_1 <- cbind("name"="",report$report1_finalreport)
  colnames(report7_1)[1] <- report_sep_names[1]
  writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, report7_1,
                 startCol = 1,rowNames = F,colNames = T)
  report7_2 <- cbind("name"="",report$report1_sales_report)
  colnames(report7_2)[1] <- report_sep_names[2]
  writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, report7_2,
                 startCol = 1,startRow = 8,rowNames = F,colNames = T)

  ## 2
  addWorksheet(wb, rsd_sheet_names[2])
  report1_1 <- cbind("name"="",report$report2_staff_timetable)
  colnames(report1_1)[1] <- report_sep_names[3]
  writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, report1_1,
                 startCol = 1,rowNames = F,colNames = T)
  report1_2 <- bind_rows(report$report2_product_knowledge,
                     report$report2_experience,
                     report$report2_sales_skills,
                     report$report2_motivation)
  report1_2 <- cbind("name"="",report1_2)
  colnames(report1_2)[1] <- report_sep_names[4]
  writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, report1_2,
                 startCol = 1,startRow = 8,rowNames = F,colNames = T)

  ## 3
  addWorksheet(wb, rsd_sheet_names[3])
  report2_1 <- cbind("name"="",report$report3_staff_cost)
  colnames(report2_1)[1] <- report_sep_names[5]
  writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, report2_1,
                 startCol = 1,rowNames = F,colNames = T)
  report2_2 <- cbind("name"="",report$report3_flm_timetable)
  colnames(report2_2)[1] <- report_sep_names[6]
  writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, report2_2,
                 startCol = 1,startRow = 9,rowNames = F,colNames = T)

  ## 4
  addWorksheet(wb, rsd_sheet_names[4])
  report3_1 <- cbind("name"="",report$report4_resource)
  colnames(report3_1)[1] <- report_sep_names[7]
  writeDataTable(wb, sheet = rsd_sheet_names[4],withFilter = F, report3_1,
                 startCol = 1,rowNames = F,colNames = T)



  ## 7
  addWorksheet(wb, rsd_sheet_names[5])
  report6_1 <- cbind("name"=rep("",50),report$report5_sales_by_hosp)
  colnames(report6_1)[1] <- report_sep_names[8]
  writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report6_1,
                 startCol = 1,rowNames = F,colNames = T)
  report6_2 <-cbind("name"=rep("",nrow(report$report5_sales_by_salesmen)),report$report5_sales_by_salesmen)
  colnames(report6_2)[1] <- report_sep_names[9]
  writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report6_2,
                 startCol = 1,startRow = 53,rowNames = F,colNames = T)
  report6_3 <- cbind("name"="",report$report5_sales_by_prod)
  colnames(report6_3)[1] <- report_sep_names[10]
  writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report6_3,
                 startCol = 1,startRow = sum(53,3,nrow(report6_2)),rowNames = F,colNames = T)
  return(wb)}

db_inter <- mongo(collection = "intermedia",
                   url = sprintf(
                     "mongodb://%s/%s",
                     options()$mongodb$host,
                     "TMIST"))

background <- db_inter$find( paste('{"uuid" : ', '"', "all", '"}',sep = ""))

rsd_sheet_names <- background$rsd_sheet_names[[1]]
report_sep_names <- background$report_sep_names[[1]]
report_names <- background$report_names[[1]]
names_box <- background$names_box[[1]]


db_report <- mongo(collection = "report",
                       url = sprintf(
                         "mongodb://%s/%s",
                         options()$mongodb$host,
                         "TMIST"))

info <- db_report$find( paste('{"uuid" : ', '"', R_Json_Path, '"}',sep = ""))
info_report <- info$report[[1]] %>%
  filter(phase==Phase)

report_data <- lapply(1:nrow(info_report), function(x) {
  out <- info_report$result[[x]] %>%
    dplyr::select(-one_of(c("hosp_code", "prod_code")))
  

  colnames(out) <- 
    sapply(colnames(out),function(x) {
      names_box[which(x==names_box$names_english),]$names_chinese
    })
  print(colnames(out))
  out
})

names_data <- lapply(1:nrow(info_report), function(x) {
  out <- info_report$report_name[[x]]
  out <- report_names[which(report_names$names_chinese==out),]$names_english
  out
})

names(report_data) <- unlist(names_data)

file_name <-  paste(UUIDgenerate(),".xlsx",sep="")

saveWorkbook(writeDown(report_data),
               file_name,
               overwrite = T)

print(paste(getwd(),"/",file_name,sep=""))
