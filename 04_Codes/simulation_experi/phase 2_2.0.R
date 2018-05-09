install.packages("gtools")
library(gtools)
library(tidyr)
library(dplyr)
library(DT)
library(data.table)
library(reshape2)
library(stringr)
library(parallel)
library(foreach)
library(doSNOW)

setwd("D:\\Rachel\\WorkMaterial\\Pharbers_git\\Territory Mangement\\04_Codes\\simulation_experi")
source("global.R")
load("backupofdata_0.RData")
## permutate salesmen allocation combs
set.seed(1234)
salesmen_allocation_container <- permutations(n = 5, r = 10, v = letters[1:5], repeats.allowed = T)
chk_for_all_salesmen <- apply(salesmen_allocation_container, 1, function(x) all(letters[1:5] %in% x))
salesmen_allocation_container_m <- salesmen_allocation_container[chk_for_all_salesmen,]
random_100000_comb_phase2 <- sample(1:nrow(salesmen_allocation_container_m), 100000, replace = F)
salesmen_allocation_container_m1_phase2 <- as.data.frame(salesmen_allocation_container_m[random_100000_comb,], stringsAsFactors = F)

universe <- list(target_factor = c(0.95,1,1.1,1.2),
                 management_comb = seq(4,12,2),
                 target_factor_prod4 = seq(0.05,0.4,0.05))


## formal permutation
cl <- makeCluster(4, outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)
iterations_phase2 <- nrow(salesmen_allocation_container_m1_phase2)
pb <- txtProgressBar(min = 1, max = iterations_phase2, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

system.time(
  contain_phase2 <- foreach(i = 1:iterations, #.combine = rbind,
                            .options.snow = opts,
                            .packages = c("dplyr", "tidyr", "DT", 
                                          "data.table", "reshape2",
                                          "stringr"))  %dopar% {
                                            
                                            salesmen_comb <- salesmen_allocation_container_m1_phase2[i,]
                                            man_admin_work <- sample(universe[[2]], 1)
                                            man_kpi_analysis <- sample(universe[[2]], 1)
                                            man_meetings_with_team <- sample(universe[[2]], 1)
                                            
                                            flm_alled_time <- sum(man_admin_work,
                                                                  man_kpi_analysis,
                                                                  man_meetings_with_team)
                                            
                                            sr1_alled_time <- man_meetings_with_team
                                            sr2_alled_time <- man_meetings_with_team
                                            sr3_alled_time <- man_meetings_with_team
                                            sr4_alled_time <- man_meetings_with_team
                                            sr5_alled_time <- man_meetings_with_team
                                            
                                            sr_alled_time <- list("sr1_alled_time" = sr1_alled_time,
                                                                  "sr2_alled_time" = sr2_alled_time,
                                                                  "sr3_alled_time" = sr2_alled_time,
                                                                  "sr4_alled_time" = sr2_alled_time,
                                                                  "sr5_alled_time" = sr2_alled_time)
                                            
                                            alled_budget <- 0
                                            
                                            decision_input <- data.frame(hosp_code = NULL,
                                                                         budget = NULL,
                                                                         salesmen = NULL,
                                                                         prod_code = NULL,
                                                                         prod_hours = NULL,
                                                                         target_factor = NULL,
                                                                         phase = NULL)
                                            
                                            management_input <- data.frame(salesmen = NULL,
                                                                           field_work = NULL,
                                                                           product_training = NULL,
                                                                           sales_training = NULL,
                                                                           admin_work = NULL,
                                                                           kpi_analysis = NULL,
                                                                           meetings_with_team = NULL,
                                                                           phase = NULL)
                                            
                                            sr1_contact_time <- 0
                                            sr2_contact_time <- 0
                                            sr3_contact_time <- 0
                                            sr4_contact_time <- 0
                                            sr5_contact_time <- 0
                                            
                                            sr_cont_time <- list("sr1_cont_time" = sr1_contact_time,
                                                                 "sr2_cont_time" = sr1_contact_time,
                                                                 "sr3_cont_time" = sr1_contact_time,
                                                                 "sr4_cont_time" = sr1_contact_time,
                                                                 "sr5_cont_time" = sr1_contact_time)
                                            
                                            for (hosp_num in 1:10) {
                                              hosp_salesmen <- salesmen_comb[[hosp_num]]
                                              hosp_budget <- sample(0:(100-alled_budget), 1)
                                              alled_budget <- alled_budget + hosp_budget
                                              
                                              for (prod_num in 1:4) {
                                                
                                                salesmen_chk <- switch(EXPR = hosp_salesmen,
                                                                       a = "sr1_alled_time",
                                                                       b = "sr2_alled_time",
                                                                       c = "sr3_alled_time",
                                                                       d = "sr4_alled_time",
                                                                       e = "sr5_alled_time")
                                                
                                                salesmen_alled_chk <- sr_alled_time[[salesmen_chk]]
                                                perm_prod_hours <- sample(0:(100-salesmen_alled_chk), 1)
                                                if (prod_num == 4) {
                                                  perm_target_factor <- sample(universe[[3]], 1)
                                                } else {
                                                  perm_target_factor <- sample(universe[[1]], 1)
                                                }
                                                
                                                sr_alled_time[[salesmen_chk]] <- sr_alled_time[[salesmen_chk]]+ perm_prod_hours
                                                hosp_salesmen_m <- switch(hosp_salesmen,
                                                                          a = "小宋",
                                                                          b = "小兰",
                                                                          c = "小木",
                                                                          d = "小白",
                                                                          e = "小青")
                                                
                                                decision_input <- bind_rows(decision_input,
                                                                            data.frame(hosp_code = hosp_num,
                                                                                       budget = hosp_budget,
                                                                                       salesmen = hosp_salesmen_m,
                                                                                       prod_code = prod_num,
                                                                                       prod_hours = perm_prod_hours,
                                                                                       target_factor = perm_target_factor,
                                                                                       phase = 2, 
                                                                                       stringsAsFactors = F))
                                                
                                                
                                                
                                              }
                                            }
                                            
                                            
                                            for (sales_num in 1:5) {
                                              flm_left_time <- 100 - flm_alled_time
                                              sr_cont_time_chk <-  sr_cont_time[[paste0("sr",sales_num,"_cont_time")]]
                                              for_field_work_time <- ifelse(flm_left_time >= sr_cont_time_chk,
                                                                            sr_cont_time_chk,
                                                                            flm_left_time)
                                              perm_field_work <- sample(0:for_field_work_time, 1)
                                              flm_alled_time <- flm_alled_time + perm_field_work
                                              sr_left_time_chk <- 100-sr_alled_time[[paste0("sr",sales_num,"_alled_time")]]
                                              flm_left_time <- 100 - flm_alled_time
                                              for_sales_train_time <- ifelse(flm_left_time >= sr_left_time_chk,
                                                                             sr_left_time_chk,
                                                                             flm_left_time)
                                              perm_sales_train <- sample(0:for_sales_train_time, 1)
                                              sr_alled_time[[paste0("sr",sales_num,"_alled_time")]] <- sr_alled_time[[paste0("sr",sales_num,"_alled_time")]] +perm_sales_train
                                              flm_alled_time <- flm_alled_time + perm_sales_train
                                              perm_prod_train <- sample(0:(100-sr_alled_time[[paste0("sr",sales_num,"_alled_time")]]), 1)
                                              management_input <- bind_rows(management_input,
                                                                            data.frame(salesmen = salesmen_list$salesmen[sales_num],
                                                                                       field_work = perm_field_work,
                                                                                       product_training = perm_prod_train,
                                                                                       sales_training = perm_sales_train,
                                                                                       admin_work = man_admin_work,
                                                                                       kpi_analysis = man_kpi_analysis,
                                                                                       meetings_with_team = man_meetings_with_team,
                                                                                       phase = 1,
                                                                                       stringsAsFactors =  F))
                                              print(flm_left_time)
                                              
                                            }
                                            
                                            ## filter
                                            decision_input <- decision_input %>%
                                              group_by(hosp_code) %>%
                                              mutate(prod_hour_perhosp = sum(prod_hours)) %>%
                                              ungroup() %>%
                                              mutate(salesmen = ifelse(prod_hour_perhosp==0, "0", salesmen),
                                                     target_factor = ifelse(prod_hour_perhosp==0, 0, target_factor),
                                                     budget = ifelse(prod_hour_perhosp==0, 0, budget)) %>%
                                              select(-prod_hour_perhosp)
                                            
                                            ## promotional fee <= 100
                                            restrict_test1 <- decision_input %>%
                                              select(hosp_code, budget) %>%
                                              distinct()
                                            
                                            ## worktime <= 100
                                            decision_part <- decision_input %>%
                                              group_by(salesmen) %>%
                                              summarise(contact_time = sum(as.numeric(prod_hours, na.rm = T))) %>%
                                              filter(salesmen %in% salesmen_list$salesmen)
                                            
                                            management_part <- data.frame(salesmen = management_input$salesmen,
                                                                          other_time = apply(subset(management_input,
                                                                                                    select = c(salesmen,
                                                                                                               product_training,
                                                                                                               sales_training,
                                                                                                               meetings_with_team)), 1, function(x) sum(as.numeric(x[2:4]))))
                                            total_time <- decision_part %>%
                                              left_join(management_part, by = "salesmen") %>%
                                              mutate(total_worktime = contact_time +other_time) %>%
                                              filter(salesmen %in% salesmen_list$salesmen)
                                            
                                            ## field worktime <= contract time
                                            field_work_part <- data.frame(salesmen = management_input$salesmen,
                                                                          field_work_time = management_input$field_work) %>%
                                              left_join(decision_part, by = "salesmen") %>%
                                              mutate(time_chk = field_work_time >contact_time)
                                            
                                            ## flm worktime <= 100
                                            flm_worktime <- sum(as.numeric(management_input$field_work),
                                                                as.numeric(management_input$sales_training),
                                                                as.numeric(management_input$admin_work[1]),
                                                                as.numeric(management_input$kpi_analysis[1]),
                                                                as.numeric(management_input$meetings_with_team[1]))
                                            
                                            if (any(!salesmen_list$salesmen %in% decision_input$salesmen)) {
                                              out <- "salesmen restrict" 
                                            } else if (sum(as.numeric(restrict_test1$budget)) > 100) {
                                              out <- "budget restrict"
                                            } else if (any(total_time$total_worktime > 100)) {
                                              out <- "work time restrict"
                                            } else if (any(field_work_part$time_chk)) {
                                              out <- "field work restrict"
                                            } else if (flm_worktime > 100) {
                                              out <- "flm work time"
                                            } else {
                                              out <- list("decision_input" = decision_input,
                                                          "management_input" = management_input)
                                            } 
                                          })
close(pb)
stopCluster(cl)
contain_phase2m <- contain_phase2[sapply(contain_phase2, is.list)]

## calculation

cl <- makeCluster(4, outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)
iterations_containter <- length(contain_phase2m)
iterations_phase1out <- length(tmist_output_phase1)
pb <- txtProgressBar(min = 1, max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


system.time(tmist_output_phase1 <- foreach(i = 1:iterations_phase1out, #.combine = c, 
                                           .options.snow = opts,
                                           .packages = c("dplyr", "tidyr", "DT", 
                                                         "data.table", "reshape2",
                                                         "stringr")) %dopar% {
                                                           
                                                           inter_data <- tmist_output_phase1[[i]]$inter_data
                                                           last_acc_success_value <- tmist_output_phase1[[i]]$result$score
                                                           pre_out <-  tmist_output_phase1[[i]]$result
                                                           
                                                           ## inter_data from phase 1
                                                           pp_data1 <- inter_data %>% select(hosp_name,
                                                                                             hosp_code,
                                                                                             prod_name,
                                                                                             prod_code,
                                                                                             real_revenue,
                                                                                             real_volume,
                                                                                             sr_sales_performance,
                                                                                             deployment_quality_index,
                                                                                             customer_relationship_index,
                                                                                             promotional_support_index,
                                                                                             sales_performance,
                                                                                             offer_attractiveness,
                                                                                             acc_offer_attractiveness) %>%
                                                             mutate(acc_success_value = last_acc_success_value)%>%
                                                             distinct()
                                                           
                                                           colnames(pp_data1)[5:14] <- paste("pp_",colnames(pp_data1)[5:14],sep="")
                                                           
                                                           pp_data2 <- inter_data %>% select(salesmen,
                                                                                             sales_level,
                                                                                             real_revenue_by_sr,
                                                                                             real_volume_by_sr,
                                                                                             sr_acc_revenue,
                                                                                             sales_skills_index,
                                                                                             product_knowledge_index,
                                                                                             motivation_index,
                                                                                             sr_acc_field_work,
                                                                                             target_revenue_realization_by_sr) %>%
                                                             distinct() %>%
                                                             filter(salesmen %in% salesmen_list$salesmen)
                                                           
                                                           colnames(pp_data2)[3:10] <- paste("pp_",colnames(pp_data2)[3:10],sep="")
                                                           pp_data2 <- rbind(pp_data2,
                                                                             0)
                                                           
                                                           foreach(j = 1:iterations_containter, #.combine = c,
                                                                   .options.snow = opts,
                                                                   .packages = c("dplyr", "tidyr", "DT", 
                                                                                 "data.table", "reshape2",
                                                                                 "stringr")) %dopar% {
                                                           
                                                           tmp <- contain_phase2m[[i]]
                                                           cp_data1 <- get.data1(tmp$decision_input)
                                                           cp_data2 <- get.data2(tmp$management_input)
                                                           flm_data <- get.data3(cp_data2)
                                                           data_to_use <- calculation(pp_data1,
                                                                                      pp_data2,
                                                                                      cp_data1,
                                                                                      cp_data2)
                                                           out <- run_for_results(data_to_use, flm_data)
                                                           out$phase =1 
                                                           out$comb_no = i
                                                           inter_data_m <- data_to_use %>%
                                                             select(hosp_name,
                                                                    hosp_code,
                                                                    prod_name,
                                                                    prod_code,
                                                                    real_revenue,
                                                                    real_volume,
                                                                    sr_sales_performance,
                                                                    deployment_quality_index,
                                                                    customer_relationship_index,
                                                                    promotional_support_index,
                                                                    sales_performance,
                                                                    offer_attractiveness,
                                                                    acc_offer_attractiveness,
                                                                    salesmen,
                                                                    sales_level,
                                                                    real_revenue_by_sr,
                                                                    real_volume_by_sr,
                                                                    sr_acc_revenue,
                                                                    sales_skills_index,
                                                                    product_knowledge_index,
                                                                    motivation_index,
                                                                    sr_acc_field_work,
                                                                    target_revenue_realization_by_sr)
                                                           
                                                           
                                                           list(inter_data = inter_data_m,
                                                                result = out)}})

tmist_output_phase1m <- lapply(tmist_output_phase1, function(x) {
  x$result})
tmist_output_phase1m1 <- bind_rows(tmist_output_phase1m)
