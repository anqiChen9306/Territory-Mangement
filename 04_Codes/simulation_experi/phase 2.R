library(tidyr)
library(dplyr)
library(DT)
library(data.table)
library(reshape2)
library(stringr)




## set the random number generator seed
set.seed(1000)
## you can lay the 60 options as a list with length equal to 60 here
universe <- list(a = letters[1:5],
                 b = seq(0,15,3), #6
                 c = c(0.95,1,1.1,1.2),
                 d = c(4, 6, 8),
                 e = seq(4,16,4), # 4
                 f = seq(0.05,0.2,0.05), #4
                 g = seq(15,25,5)) #5
## please set a container to store the combinations
contain_phase2 <- matrix(ncol = 118)

cl <- makeCluster(4, outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)
iterations <- 10000
pb <- txtProgressBar(min = 1, max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

## here is the loop to do the work
system.time(
  contain_phase2 <- foreach(i = 1:iterations, .combine = rbind,
                            .options.snow = opts)  %dopar% {
                              tmp<- c(universe[[1]][sample(1:5, 1)], ##hosp 1
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[7]][sample(1:3, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[6]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[1]][sample(1:5, 1)], ##hosp 2
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[6]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[1]][sample(1:5, 1)], ##hosp 3
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[7]][sample(1:3, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[6]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[1]][sample(1:5, 1)], ##hosp 4
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      0,
                                      0,
                                      universe[[1]][sample(1:5, 1)], ##hosp 5
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      0,
                                      0,
                                      universe[[1]][sample(1:5, 1)], ##hosp 6
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      0,
                                      0,
                                      universe[[1]][sample(1:5, 1)], ##hosp 7
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      0,
                                      0,
                                      universe[[1]][sample(1:5, 1)], ##hosp 8
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[6]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[1]][sample(1:5, 1)], ##hosp 9
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      0,
                                      0,
                                      universe[[1]][sample(1:5, 1)], ##hosp 10
                                      universe[[2]][sample(1:6, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[7]][sample(1:3, 1)],
                                      universe[[3]][sample(1:4, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      universe[[3]][sample(1:3, 1)],
                                      universe[[5]][sample(1:4, 1)],
                                      0,
                                      0,
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)], ## field work
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)], ##product training
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)], ##sales training
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)],
                                      universe[[4]][sample(1:3, 1)])
                              # if (any(apply(contain_phase1, 2, function(x) identical(x, tmp)))) {
                              #   next
                              # } else {
                              #   contain_phase1 <- rbind(contain_phase1, tmp)
                              # }
                              # # if you want 100000, you can change the 100 to 100000
                              # if (nrow(contain_phase1) == 10000) break
                              # contain_phase1 <- rbind(contain_phase1, tmp)
                              setTxtProgressBar(pb, i) 
                              return(tmp)
                            }
)

close(pb)
stopCluster(cl)


### filter 
tmist_container_phase2 <- 
  as.data.frame(contain_phase2, stringsAsFactors = FALSE)


part1 <- NULL
for (i in 1:10) {
  for (j in 1:4) {
    if ( j == 1) {
      tmp <- c(paste("hosp_",i,"_salesmen",sep=""),
               paste("hosp_",i,"_budget",sep=""),
               paste("hosp_",i,"_prod_",j,"_target_factor",sep=""),
               paste("hosp_",i,"_prod_",j,"_prod_hours",sep=""))
    } else {
      tmp <- c(paste("hosp_",i,"_prod_",j,"_target_factor",sep=""),
               paste("hosp_",i,"_prod_",j,"_prod_hours",sep=""))
    }
    
    part1 <- c(part1, tmp)
    
  }
}

part2 <- NULL
for (i in 1:5) {
  tmp <- c(paste("man_salesmen_",i,"_field_work",sep=""),
           paste("man_salesmen_",i,"_product_training",sep=""),
           paste("man_salesmen_",i,"_sales_training",sep=""))
  part2 <- c(part2,tmp)
}
part2 <- c(part2,"man_admin_work","man_kpi_analysis","man_meetings_with_team")

colnames(tmist_container_phase2) <-  c(part1, part2)

### processing each combination, including data manipulation & filtering & calculation
## filter combinations within restriction
# restriction 1 : sum of arranged worktime of each salesmen &flm <=100
# restriction 2 : no arrangement of time & budget in hospital is without salesmen
# restriction 3 : sum of arranged budget <=100
# restriction 4 : field work time <= arranged contact time
# restriction 5 : contact time of each salesmen >0

# tmist_container_phase1m <- vector("list", nrow(tmist_container_phase1))

cl <- makeCluster(4, outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)
iterations <- nrow(tmist_container_phase2)
pb <- txtProgressBar(min = 1, max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


tmist_container_phase2m <- foreach(i = 1:iterations, #.combine = c, 
                                   .options.snow = opts,
                                   .packages = c("dplyr", "tidyr", "DT", 
                                                 "data.table", "reshape2",
                                                 "stringr")) %dopar% {
                                                   tmp <- tmist_container_phase2[i,] %>%
                                                     gather(name, value) %>%
                                                     mutate(split = ifelse(substr(name, 1, 4) == "hosp", 1, 2))
                                                   
                                                   ## extract account decision part
                                                   # extract hosp code
                                                   decision_input_m <- subset(tmp, split == "1", select = c(name, value)) %>%
                                                     mutate(hosp_code = substr(name, 
                                                                               sapply(name,function(x)str_locate_all(x, "_")[[1]][1,1]+1),
                                                                               sapply(name,function(x)str_locate_all(x, "_")[[1]][2,1]-1)),
                                                            others = substr(name,sapply(name,function(x)str_locate_all(x, "_")[[1]][2,1]+1), str_length(name)))
                                                   
                                                   # extract salesment & budget 
                                                   decision_input_m1 <- subset(decision_input_m, others %in% c("salesmen","budget"), 
                                                                               select = c(hosp_code, others, value)) %>%
                                                     spread(others, value) 
                                                   
                                                   # extract others
                                                   decision_input_m2 <- subset(decision_input_m, !(others %in% c("salesmen","budget")), 
                                                                               select = c(hosp_code, others, value)) %>%
                                                     mutate(prod_code = substr(others, 6, 6),
                                                            others = substr(others, 8, str_length(others))) %>%
                                                     spread(others, value)
                                                   
                                                   decision_input <- decision_input_m1 %>%
                                                     left_join(decision_input_m2, by = "hosp_code")
                                                   
                                                   decision_input$salesmen <- sapply(decision_input$salesmen, function(x) {
                                                     switch(x,
                                                            a = "小宋",
                                                            b = "小兰",
                                                            c = "小木",
                                                            d = "小白",
                                                            e = "小青")
                                                   })
                                                   
                                                   decision_input$budget <- as.numeric(decision_input$budget)
                                                   decision_input$prod_hours <- as.numeric(decision_input$prod_hours)
                                                   decision_input$target_factor <- as.numeric(decision_input$target_factor)
                                                   decision_input$phase <- 2
                                                   decision_input$hosp_code <- as.numeric(decision_input$hosp_code)
                                                   decision_input$prod_code <- as.numeric(decision_input$prod_code)
                                                   
                                                   ## extract management decision part
                                                   # extract salesmen
                                                   management_input_m <- subset(tmp, split == "2", select = c(name,value)) %>%
                                                     mutate(is.personel = ifelse(substr(name,5,9) == "sales", 1, 2),
                                                            value = as.numeric(value))
                                                   
                                                   # extract personal training
                                                   management_input_m1 <- subset(management_input_m, is.personel == 1,
                                                                                 select = c(name, value)) %>%
                                                     mutate(salesmen = substr(name, 14, 14),
                                                            name = substr(name, 16, str_length(name))) %>%
                                                     spread(name, value)
                                                   
                                                   # extract shared team time
                                                   management_input_m2 <- subset(management_input_m, is.personel != 1,
                                                                                 select = c(name, value)) %>%
                                                     mutate(name = substr(name, 5, str_length(name))) %>%
                                                     spread(name, value)
                                                   
                                                   management_input <- data.frame(management_input_m1,
                                                                                  management_input_m2)
                                                   management_input$phase <- 2
                                                   management_input$salesmen <- sapply(management_input$salesmen, function(x) {
                                                     switch(x,
                                                            "1" = "小宋",
                                                            "2" = "小兰",
                                                            "3" = "小木",
                                                            "4" = "小白",
                                                            "5" = "小青")})
                                                   
                                                   ## promotional fee <= 100
                                                   restrict_test1 <- decision_input %>%
                                                     select(hosp_code, budget) %>%
                                                     distinct()
                                                   
                                                   ## worktime <= 100
                                                   decision_part <- decision_input %>%
                                                     group_by(salesmen) %>%
                                                     summarise(contact_time = sum(as.numeric(prod_hours, na.rm = T)))
                                                   
                                                   management_part <- data.frame(salesmen = management_input$salesmen,
                                                                                 other_time = apply(subset(management_input,
                                                                                                           select = -field_work), 1, function(x) sum(as.numeric(x[2:7]))))
                                                   total_time <- decision_part %>%
                                                     left_join(management_part, by = "salesmen") %>%
                                                     mutate(total_worktime = contact_time +other_time)
                                                   
                                                   ## field worktime <= contract time
                                                   field_work_part <- data.frame(salesmen = management_input$salesmen,
                                                                                 field_work_time = management_input$field_work)
                                                   
                                                   ## flm worktime <= 100
                                                   flm_worktime <- sum(as.numeric(management_input$field_work),
                                                                       as.numeric(management_input$sales_training),
                                                                       as.numeric(management_input$admin_work[1]),
                                                                       as.numeric(management_input$kpi_analysis[1]),
                                                                       as.numeric(management_input$meetings_with_team[1]))
                                                   if (length(unique(decision_input$salesmen[decision_input$salesmen%in%salesmen_list$salesmen])) != 5 ) {
                                                     tmist_container_phase2m <- "salesmen restrict" 
                                                   } else if (sum(as.numeric(restrict_test1$budget)) > 100) {
                                                     tmist_container_phase2m <- "budget restrict"
                                                   } else if (any(total_time$total_worktime > 100)) {
                                                     tmist_container_phase2m <- "contact time restrict"
                                                   } else if (any(as.numeric(field_work_part$field_work_time) > as.numeric(decision_part$contact_time))) {
                                                     tmist_container_phase2m <- "field work restrict"
                                                   } else if (flm_worktime > 100) {
                                                     tmist_container_phase2m <- "flm work time"
                                                   } else {
                                                     tmist_container_phase2m <- list("decision_input" = decision_input,
                                                                                     "management_input" = management_input)
                                                   }
                                                   
                                                   return(tmist_container_phase2m)
                                                 }


close(pb)
stopCluster(cl)
tmist_container_phase2m1 <- tmist_container_phase2m[sapply(tmist_container_phase2m, is.list)]

## calculation

cl <- makeCluster(4, outfile="") # number of cores. Notice 'outfile'
registerDoSNOW(cl)
iterations_containter <- length(tmist_container_phase2m1)
iterations_phase1out <- length(tmist_output_phase1)

pb <- txtProgressBar(min = 1, max = iterations_phase1out, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)


tmist_output_phase2 <- foreach(i = 1:2, #.combine = c, 
                                   .options.snow = opts,
                                   .packages = c("dplyr", "tidyr", "DT", 
                                                 "data.table", "reshape2",
                                                 "stringr","foreach")) %dopar% {
                                                   
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
                                                      distinct()

                                                    colnames(pp_data2)[3:10] <- paste("pp_",colnames(pp_data2)[3:10],sep="")
                                                   
                                                  foreach(j = 1:iterations_containter, #.combine = c,
                                                          .options.snow = opts,
                                                          .packages = c("dplyr", "tidyr", "DT", 
                                                                        "data.table", "reshape2",
                                                                        "stringr")) %dopar% {
                                                    
                                                    
                                                    
                                                    tmp <- tmist_container_phase2m1[[j]]
                                                    
                                                 cp_data1 <- get.data1(tmp$decision_input)
                                                 cp_data2 <- get.data2(tmp$management_input)
                                                 flm_data <- get.data3(cp_data2)
                                                 data_to_use <- calculation(pp_data1,
                                                                            pp_data2,
                                                                            cp_data1,
                                                                            cp_data2)
                                                 
                                                 out <- run_for_results(data_to_use, flm_data)
                                                 out$phase = 2
                                                 out$comb_no = j
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
       result = bind_rows(pre_out,out))
                                                  }
                                                   
                                                 }

tmist_output_phase1m <- lapply(tmist_output_phase1, function(x) {
  x$result})
tmist_output_phase1m1 <- bind_rows(tmist_output_phase1m)

