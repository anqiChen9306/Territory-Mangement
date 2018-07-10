# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  T***T
# Purpose:      Calculation
# programmer:   Anqi Chen
# Date:         02-07-2018
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
  
  options(scipen=200,
          mongodb = list(
            "host" = "192.168.100.142:27017"
            # "username" = "root",
            # "password" = "root"
          ))
  

  
  ## receive signal
  # argss[1] :  R_File_Path
  # argss[2] :  filekey of json
  argss <- commandArgs(TRUE)
  R_Json_Path <- argss[1]
  Phase <- as.integer(argss[2])
  #file_path <- argss[3]
  #R_File_Path <- "resource/pre_data_linux.RData"
  #load(R_File_Path)
  
  ## R_Json_Path <- "219776aa-6d5a-4bcf-bc99-74857e86ec7a"
  ## Phase <-1


  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                              curve function
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  curve <- function(curve.no,input_x){
    data <- curve.no
    if (input_x<=min(data$x)) {
      y <- data$y[which.min(data$x)]
    } else if (input_x>=max(data$x)) {
      y <- data$y[which.max(data$x)]
    } else {
      left <- data[which.min(abs(input_x-data$x)),]
      tmp <- data[-which.min(abs(input_x-data$x)),]
      right <- tmp[which.min(abs(input_x-tmp$x)),]
      y <- ifelse(left$x <= right$x,
                  (1-(input_x-left$x)/(right$x-left$x))*left$y + 
                    (1-(right$x-input_x)/(right$x-left$x))*right$y,
                  (1-(input_x-right$x)/(left$x-right$x))*right$y + 
                    (1-(left$x-input_x)/(left$x-right$x))*left$y)}
    
    y
  }
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      data cleaning part
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  ## to collect sales decisions
  get.data1 <- function(input) {
    tmp1 <- input %>%
      left_join(hospital_info, by= c("phase",
                                     "hosp_code",
                                     "prod_name",
                                     "hosp_name")) %>%   ## need adjust to pro_code in 2.0 version
      left_join(promotioal_budget_list, by="phase") %>%
      dplyr::mutate(budget = budget/100*total_budget,
                    prod_hours = prod_hours)
    return(tmp1)
      
  }
  
  
  
  ## to collect management decisions
  get.data2 <- function(input) {
    tmp2 <- data.frame(input,
                       work_time = worktime)
    tmp2 <- rbind(tmp2,
                  rep(0,ncol(tmp2)))
    tmp2$phase <- input$phase[1]
    return(tmp2)
  }
  
  
  ## to collect flm time management
  get.data3 <- function(data) {
    flm_decision <- data %>%
      dplyr::select(salesmen ,phase, sales_training, field_work) %>%
      filter(salesmen!="0") %>%
      gather(project_name,
             days,
             -salesmen,
             -phase) %>%
      group_by(project_name) %>%
      dplyr::summarise(days = sum(days, na.rm = T)) %>%
      spread(project_name,days)
    
    flm_decision2 <- data %>%
      dplyr::select(-product_training, -sales_training, -field_work, -work_time) %>%
      filter(salesmen!="0") %>%
      gather(project_name,
             days,
             -salesmen,
             -phase) %>%
      group_by(project_name) %>%
      filter(row_number()==1) %>%
      dplyr::summarise(days = sum(days, na.rm = T)) %>%
      spread(project_name,days)
    
    out <-bind_cols(flm_decision,flm_decision2)
    
    return(out)
  }
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                              read data
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  ##-- product info
  db_product <- mongo(collection = "products",
               url = sprintf(
                 "mongodb://%s/%s",
                 options()$mongodb$host,
                 "TMIST"))
  
  product_info <- db_product$find() %>%
    dplyr::select(-date) %>%
    dplyr::mutate(prod_unit_price = as.numeric(prod_unit_price),
                  prod_unit_cost = as.numeric(prod_unit_cost),
                  prod_code = as.integer(prod_code))
  
  ##-- hospital info
  db_hosp <- mongo(collection = "hospitals",
              url = sprintf(
                "mongodb://%s/%s",
                options()$mongodb$host,
                "TMIST"))
  hosp_info <- db_hosp$find() %>%
    dplyr::select(hosp_name,hosp_code) %>%
    dplyr::mutate(hosp_code = as.integer(hosp_code))
  
  db_potential <- mongo(collection = "constrats",
               url = sprintf(
                 "mongodb://%s/%s",
                 options()$mongodb$host,
                 "TMIST"))
  
  hospital_info <- db_potential$find() %>%
    dplyr::select(-date) %>%
    gather(phase, potential, -hosp_code, -prod_code) %>%
    dplyr::mutate(phase = as.numeric(substr(phase, 8, 8)),
                  potential = as.numeric(potential),
                  potential = ifelse(is.na(potential), 0, potential),
                  hosp_code = as.integer(hosp_code),
                  prod_code = as.integer(prod_code)) %>%
    left_join(hosp_info, by = "hosp_code") %>%
    left_join(dplyr::select(product_info,prod_code,prod_name), by ="prod_code")
  
  ##-- salesmen list
  db_salesmen <- mongo(collection = "salesmen",
               url = sprintf(
                 "mongodb://%s/%s",
                 options()$mongodb$host,
                 "TMIST"))
  
  salesmen_list <- db_salesmen$find()
  
  colnames(salesmen_list)[which(colnames(salesmen_list)=="name")] <- "salesmen"
  
  ##-- promotional budget list
  db_budget <- mongo(collection = "budget",
               url = sprintf(
                 "mongodb://%s/%s",
                 options()$mongodb$host,
                 "TMIST"))
  
  promotioal_budget_list <- db_budget$find() 
  colnames(promotioal_budget_list)[which(colnames(promotioal_budget_list)=="phrase")] <- "phase"
  promotioal_budget_list <- promotioal_budget_list %>%
    dplyr::mutate(phase = as.integer(phase),
                  total_budget = as.numeric(one_product_budget)) %>%
    dplyr::select(-date, -one_product_budget,-budget) 
  
  
  db_inter <- mongo(collection = "intermedia",
               url = sprintf(
                 "mongodb://%s/%s",
                 # options()$mongodb$username,
                 # options()$mongodb$password,
                 options()$mongodb$host,
                 "TMIST"))
  
  info_pre <- db_inter$find(paste('{"uuid" : "all_new"}',sep = ""))
  curves <- info_pre$curves[[1]]
  over_ch <- info_pre$over_ch
  report_names <- info_pre$report_names[[1]]
  final_report_add <- info_pre$colnames$final_report_add[[1]]
  staff_time_add <- info_pre$colnames$staff_time_add[[1]]
  product_knowledge_add <- info_pre$colnames$product_knowledge_add[[1]]
  experience_add <- info_pre$colnames$experience_add[[1]]
  sales_skills_add <- info_pre$colnames$sales_skills_add[[1]]
  motivation_add <-info_pre$colnames$motivation_add[[1]]
  flm_time_add <- info_pre$colnames$flm_time_add[[1]]
  resource_allocation_add <- info_pre$colnames$resource_allocation_add[[1]]
  weightages <- info_pre$weightage[[1]] 
  project_list <- info_pre$project_list[[1]]
  worktime <-info_pre$worktime
  overhead <- info_pre$overhead
  big_hosp_list <- info_pre$big_hosp_list[[1]]
  resource_allocation_rank <- info_pre$ranks$resource_allocation_rank[[1]]
  final_report_rank <- info_pre$ranks$final_report_rank[[1]]
  resource_allocation_variable_list <- info_pre$variable_list$resource_allocation_variable_list[[1]]
  
  ## dig info from database input
  find_sta <- function(name,data,result) {
    if ( result == "curves") {
      position <- which(data$curve_name==name)
      out<-data$curve_data[[position]]
    } else {
      position <- which(data$up_index==name)
      out<-data$weight[[position]]
    }
    
    return(out)
  }
  
  json_inputs <- mongo(collection = "inputs",
                      url = sprintf(
                        "mongodb://%s/%s",
                        # options()$mongodb$username,
                        # options()$mongodb$password,
                        options()$mongodb$host,
                        "TMIST"))
  
  best_allocations <- info_pre$best_allocations[[1]]
  
  mongodb_target <- mongo(collection = "target",
                          url = sprintf(
                            "mongodb://%s/%s",
                            # options()$mongodb$username,
                            # options()$mongodb$password,
                            options()$mongodb$host,
                            "TMIST"))
  
  set_target_revenue_info <- mongodb_target$find()
  
  mongodb_ass <- mongo(collection = "assessment",
                       url = sprintf(
                         "mongodb://%s/%s",
                         # options()$mongodb$username,
                         # options()$mongodb$password,
                         options()$mongodb$host,
                         "TMIST"))
  
  
  # Read all the entries
  transfer <- json_inputs$find(paste('{"uuid" : "',R_Json_Path,'"}',sep = ""))
  decision <- transfer$decision[[1]]
  management <- transfer$management[[1]]
  
  ## arg in need
  user_name <- transfer$user_id
  
  
  
  
  
  if (Phase ==1) {
    
    inter_data <- info_pre$inter$data[[1]]
    last_report1_1 <- info_pre$inter$report[[1]]
    last_acc_success_value <- info_pre$inter$acc_success_value[[1]]
    
  } else {
    
    
    transfer1 <- db_inter$find('{"uuid" : "cebe92eb-ad0f-4053-a1c6-8aaa1594a480"}')
    transfer1_m <- filter(transfer1$inter[[1]], phase == Phase-1)
    inter_data <- transfer1_m$data[[1]]
    last_report1_1 <- transfer1_m$report[[1]]
    last_acc_success_value <- 0
    
    assess_info_need <- mongodb_ass$find(paste('{"uuid" : "',R_Json_Path,'"}',sep = ""))
    # pp_assess_info <- assess_info_need$result$phase_1
    
    
  }
  
  last_report1_1[,2:6] <- apply(last_report1_1[,2:6], 2, function(x) as.numeric(gsub(",","",x)))
  
  
  decision_input <- apply(decision, 1, function(x) {
   
    part1 <- x$sales
    part2 <- x$visit_hours
    part <- part1 %>%
      left_join(part2, by="prod_name")
    out<-data.frame("hosp_code"=x$hosp_code,
               "hosp_name"=x$hosp_name,
               "phase"=x$phase,
               "budget"=x$budget,
               "salesmen"=x$salesmen,
               part,
               stringsAsFactors = F)
    out <- out %>%
      dplyr::mutate(salesmen = ifelse(salesmen=="","0",salesmen),
                    budget = ifelse(salesmen=="0",0,budget),
                    prod_value = ifelse(salesmen=="0",0,prod_value),
                    prod_hours = ifelse(salesmen=="0",0,prod_hours))
    return(out)
  })
  
  decision_input <- bind_rows(decision_input) %>%
    filter(phase==Phase)
  
  management_input <- apply(management, 1, function(x) {

    if (x$project_code %in% c(0,1,5)) {
      part <- x$apply
      colnames(part)[which(colnames(part)=="personal")] <- "salesmen"
      part$project_code <-  x$project_code
      part$phase <- x$phase
      
    } else{
      part_m <- x$apply
      part <- data.frame(phase = x$phase,
                         days = part_m$days,
                         salesmen = salesmen_list$salesmen,
                         project_code = x$project_code,
                         stringsAsFactors = F)
    }

    return(part)
  })
  
  management_input <- bind_rows(management_input) %>%
    left_join(project_list, by = "project_code") %>%
    dplyr::select(-project_code) %>%
    spread(project_name,days) %>%
    filter(phase==Phase)
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      begin computation
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  
  
  cp_data1 <- get.data1(decision_input)
  cp_data2 <- get.data2(management_input)
  flm_data <- get.data3(cp_data2)
  
  pp_potential_info <- hospital_info %>%
    filter(phase == Phase-1) %>%
    mutate(pp_potential = potential) %>%
    select(-hosp_name, -prod_name, -phase, -potential) 
  
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
    distinct() %>%
    left_join(pp_potential_info, by = c("hosp_code", "prod_code"))
  
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
  
  
  
  ##----------------------------------------------------------------------------
  ##--                 data processing
  ##----------------------------------------------------------------------------
  calculation <- function(pp_data1,
                          pp_data2,
                          cp_data1,
                          cp_data2){
    #
    #
    tmp1 <- left_join(cp_data1,dplyr::select(pp_data1,-prod_code,-hosp_code),by=c("hosp_name","prod_name"))
    tmp2 <- left_join(cp_data2,pp_data2,by=c("salesmen"))
    
    tmp <- left_join(tmp1,tmp2,by=c("phase","salesmen")) %>%
      filter(prod_code == 1) %>%
      dplyr::mutate(salesmen = ifelse(prod_hours==0,"0",salesmen),
                    sales_level = ifelse(prod_hours==0,"0",sales_level),
                    pp_real_volume_by_sr = ifelse(prod_hours==0,0,pp_real_volume_by_sr),
                    pp_real_revenue_by_sr = ifelse(prod_hours==0,0,pp_real_revenue_by_sr),
                    pp_sr_acc_revenue = ifelse(prod_hours==0,0,pp_sr_acc_revenue),
                    pp_sales_skills_index = ifelse(prod_hours==0,0,pp_sales_skills_index),
                    pp_product_knowledge_index = ifelse(prod_hours==0,0,pp_product_knowledge_index),
                    pp_motivation_index = ifelse(prod_hours==0,0,pp_motivation_index),
                    pp_sr_acc_field_work = ifelse(prod_hours==0,0,pp_sr_acc_field_work),
                    pp_target_revenue_realization_by_sr = ifelse(prod_hours==0,0,pp_target_revenue_realization_by_sr),
                    sales_training = ifelse(prod_hours==0,0,sales_training),
                    product_training = ifelse(prod_hours==0,0,product_training),
                    field_work = ifelse(prod_hours==0,0,field_work),
                    meetings_with_team = ifelse(prod_hours==0,0,meetings_with_team),
                    kpi_analysis = ifelse(prod_hours==0,0,kpi_analysis),
                    admin_work = ifelse(prod_hours==0,0,admin_work),
                    work_time = ifelse(prod_hours==0,0,work_time)) %>%
      dplyr::mutate(product_price = sapply(prod_name,function(x) product_info[which(product_info$prod_name==x),]$prod_unit_price),
                    target_revenue= prod_value,
                    target_volume = round(target_revenue/product_price)) %>%
      group_by(phase,salesmen) %>%
      dplyr::mutate(sr_time=prod_hours,
                    no_hospitals = n_distinct(hosp_code),
                    sr_time_total=sum(sr_time,na.rm=T),
                    last_revenue_by_sr = sum(pp_real_revenue,na.rm=T),
                    overhead_proportion = sr_time/sr_time_total,
                    overhead_proportion = ifelse(is.nan(overhead_proportion),0,overhead_proportion)) %>%
      ungroup %>%
      group_by(phase,hosp_name) %>%
      dplyr::mutate(sr_time_by_hosp=sum(sr_time,na.rm=T)) %>%
      ungroup() %>%
      dplyr::mutate(product_time_proportion=round(sr_time/ifelse(sr_time_by_hosp==0,0.0001,sr_time_by_hosp),2),
                    budget = round(budget*product_time_proportion),
                    promotional_factor = ifelse(target_revenue==0,0,round(budget/pp_real_revenue*100,2)),
                    sr_acc_field_work = pp_sr_acc_field_work+field_work,
                    overhead_factor = sapply(pp_motivation_index,
                                             function(x) curve(find_sta("curve12",curves,"curves"),x)),
                    overhead_time = round(overhead_factor*overhead,0),
                    real_sr_time = round(sr_time-overhead_time*overhead_proportion,2),
                    real_sr_time = ifelse(real_sr_time < 0, 0, real_sr_time),
                    real_sr_time_to_cal = real_sr_time/2.5,
                    pp_experience_index = round(sapply(pp_sr_acc_revenue,function(x) round(curve(find_sta("curve11",curves,"curves"),x),2))),
                    field_work_peraccount = field_work/ifelse(no_hospitals==0,0.0001,no_hospitals),
                    product_knowledge_addition_current_period = sapply(product_training,function(x)curve(find_sta("curve26",curves,"curves"),x)),
                    product_knowledge_transfer_value = sapply(pp_product_knowledge_index,function(x)curve(find_sta("curve28",curves,"curves"),x)),
                    ss_accumulated_field_work_delta = sapply(sr_acc_field_work,function(x)curve(find_sta("curve42",curves,"curves"),x)),
                    ss_accumulated_sales_training_delta = sapply(sales_training,function(x)curve(find_sta("curve43",curves,"curves"),x)),
                    ss_experience_index_pp = sapply(pp_experience_index,function(x)curve(find_sta("curve44",curves,"curves"),x)),
                    m_sales_training_delta = sapply(sales_training,function(x)curve(find_sta("curve17",curves,"curves"),x)),
                    m_admin_work_delta = sapply(admin_work,function(x)curve(find_sta("curve18",curves,"curves"),x)))%>%
      dplyr::mutate(sales_skills_index = round(
        (ss_accumulated_field_work_delta+pp_sales_skills_index)*((find_sta("sales_skills",weightages,"1"))$field_work)+
          (ss_accumulated_sales_training_delta+pp_sales_skills_index)*((find_sta("sales_skills",weightages,"1"))$sales_training)+
          (ss_experience_index_pp+pp_sales_skills_index)*((find_sta("sales_skills",weightages,"1"))$experience)),
        product_knowledge_index = round(
          product_knowledge_addition_current_period+
            pp_product_knowledge_index)) %>%
      dplyr::mutate(srsp_motivation_delta = sapply(pp_motivation_index,function(x)curve(find_sta("curve32",curves,"curves"),x)),
                    srsp_sales_skills_delta = sapply(sales_skills_index,function(x)curve(find_sta("curve34",curves,"curves"),x)),
                    srsp_product_knowledge_delta = sapply(product_knowledge_index,
                                                          function(x)curve(find_sta("curve33",curves,"curves"),x)),
                    srsp_time_with_account_delta =  mapply(function(x,y,z){ if(
                      x==as.character(product_info$prod_name[2])){
                      curve(find_sta("curve36",curves,"curves"),y)} else if (
                        x==as.character(product_info$prod_name[3])) {
                        curve(find_sta("curve37",curves,"curves"),y)} else if (x==as.character(product_info$prod_name[4])) {
                          curve(find_sta("curve38",curves,"curves"),y)} else if (x==as.character(product_info$prod_name[1])&
                                                     z %in% big_hosp_list){
                              curve(find_sta("curve39",curves,"curves"),y)}else{curve(find_sta("curve35",curves,"curves"),y)}},
                      prod_name,real_sr_time_to_cal,hosp_name)) %>%
      dplyr::mutate(sr_sales_performance =
                      (srsp_motivation_delta+pp_sr_sales_performance)*
                      ((find_sta("sr_sales_performance",weightages,"1"))$motivation)+
                      (srsp_sales_skills_delta+pp_sr_sales_performance)*
                      ((find_sta("sr_sales_performance",weightages,"1"))$sales_skills)+
                      (srsp_product_knowledge_delta+pp_sr_sales_performance)*
                      ((find_sta("sr_sales_performance",weightages,"1"))$product_knowledge)+
                      (srsp_time_with_account_delta+pp_sr_sales_performance)*
                      ((find_sta("sr_sales_performance",weightages,"1"))$time_with_account))%>%
      dplyr::mutate(sr_sales_performance = ifelse(sr_sales_performance<0,0,sr_sales_performance),
                    dq_admin_work_delta = sapply(admin_work,function(x)curve(find_sta("curve5",curves,"curves"),x)),
                    dq_meetings_with_team_delta =sapply(meetings_with_team,function(x)curve(find_sta("curve7",curves,"curves"),x)),
                    dq_kpi_analysis_factor = sapply(kpi_analysis,function(x)curve(find_sta("curve8",curves,"curves"),x)))%>%
      dplyr::mutate(deployment_quality_index = round(
        (pp_deployment_quality_index+dq_admin_work_delta)*
          ((find_sta("deployment_quality",weightages,"1"))$admin_work)+
          (pp_deployment_quality_index+dq_meetings_with_team_delta)*
          ((find_sta("deployment_quality",weightages,"1"))$meetings_with_team)+
          pp_deployment_quality_index*dq_kpi_analysis_factor*
          ((find_sta("deployment_quality",weightages,"1"))$kpi_report_analysis)))%>%
      dplyr::mutate(deployment_quality_index = ifelse(deployment_quality_index<0,0,deployment_quality_index),
                    ps_promotional_budget_factor = sapply(promotional_factor,function(x)curve(find_sta("curve30",curves,"curves"),x))) %>%
      dplyr::mutate(promotional_support_index = 
                      pp_promotional_support_index*ps_promotional_budget_factor) %>%
      dplyr::mutate(promotional_support_index = ifelse(promotional_support_index<0,0,promotional_support_index),
                    sp_field_work_delta = sapply(field_work_peraccount,function(x)curve(find_sta("curve40",curves,"curves"),x)),
                    sp_deployment_quality_delta = sapply(deployment_quality_index,function(x)curve(find_sta("curve41",curves,"curves"),x))) %>%
      dplyr::mutate(sales_performance = 
                      sr_sales_performance*((find_sta("sales_performance",weightages,"1"))$sr_sales_performance)+
                      (pp_sales_performance+sp_field_work_delta)*
                      ((find_sta("sales_performance",weightages,"1"))$field_work)+
                      (pp_sales_performance+sp_deployment_quality_delta)*
                      ((find_sta("sales_performance",weightages,"1"))$deployment_quality))%>%
      dplyr::mutate(sales_performance = ifelse(sales_performance<0,0,sales_performance),
                    cr_product_knowledge_delta = 
                      sapply(product_knowledge_index,function(x)curve(find_sta("curve2",curves,"curves"),x)),
                    cr_promotional_support_delta = 
                      sapply(ps_promotional_budget_factor,function(x)curve(find_sta("curve3",curves,"curves"),x)),
                    cr_pp_customer_relationship_index = 
                      sapply(pp_customer_relationship_index,function(x)curve(find_sta("curve4",curves,"curves"),x)))%>%
      dplyr::mutate(customer_relationship_index = 
                      (cr_pp_customer_relationship_index+cr_product_knowledge_delta)*
                      (find_sta("customer_relaitonship",weightages,"1"))$product_knowledge+
                      (cr_pp_customer_relationship_index+cr_promotional_support_delta)*
                      (find_sta("customer_relaitonship",weightages,"1"))$promotional_support) %>%
      dplyr::mutate(customer_relationship_index=ifelse(customer_relationship_index<0,0,customer_relationship_index))%>%
      dplyr::mutate(oa_customer_relationship_factor = 
                      mapply(function(x,y){if (x==as.character(product_info$prod_name[1])){
                        curve(find_sta("curve19",curves,"curves"),y)} else if(
                          x==as.character(product_info$prod_name[2])){
                          curve(find_sta("curve20",curves,"curves"),y)} else if (
                            x==as.character(product_info$prod_name[3])) {
                            curve(find_sta("curve21",curves,"curves"),y)} else {
                              curve(find_sta("curve22",curves,"curves"),y)}},
                        prod_name,customer_relationship_index),
                    oa_sales_performance_factor = sapply(sales_performance,function(x)curve(find_sta("curve25",curves,"curves"),x))) %>%
      dplyr::mutate(cp_offer_attractiveness = 
                      oa_customer_relationship_factor*100*
                      (find_sta("cp_offer_attractiveness",weightages,"1"))$customer_relationship+
                      oa_sales_performance_factor*100*
                      (find_sta("cp_offer_attractiveness",weightages,"1"))$sales_performance) %>%
      dplyr::mutate(cp_offer_attractiveness = ifelse(salesmen==0,0,cp_offer_attractiveness),
                    offer_attractiveness = 
                      cp_offer_attractiveness*(find_sta("total_attractiveness",weightages,"1"))$cp_offer_attractiveness+
                      pp_offer_attractiveness*(find_sta("total_attractiveness",weightages,"1"))$pp_offer_attractiveness,
                    acc_offer_attractiveness = round(pp_acc_offer_attractiveness+offer_attractiveness),
                    market_share =  mapply(function(x,y){if (x==as.character(product_info$prod_name[1])){
                      curve(find_sta("curve51",curves,"curves"),y)} else if(
                        x==as.character(product_info$prod_name[2])){
                        curve(find_sta("curve52",curves,"curves"),y)} else if (
                          x==as.character(product_info$prod_name[3])) {
                          curve(find_sta("curve53",curves,"curves"),y)} else {
                            curve(find_sta("curve54",curves,"curves"),y)}},
                      prod_name,offer_attractiveness),
                    real_revenue = round(market_share/100*potential),
                    real_volume = round(real_revenue/product_price)) %>%
      ungroup() %>%
      dplyr::group_by(phase,salesmen) %>%
      dplyr::mutate(target_revenue_by_sr = sum(target_revenue,na.rm=T),
                    target_revenue_percent = target_revenue_by_sr/last_revenue_by_sr,
                    bonus_factor = sapply(target_revenue_percent,function(x) {if (x==0|is.nan(x)) {
                      0 }  else {1}}),
                    real_revenue_by_sr = sum(real_revenue,na.rm=T),
                    target_revenue_realization_by_sr = round(real_revenue_by_sr/target_revenue_by_sr*100,2),
                    target_volume_by_sr = sum(target_volume,na.rm=T),
                    real_volume_by_sr = sum(real_volume,na.rm=T),
                    target_volume_realization_by_sr = round(real_volume_by_sr/target_volume_by_sr*100,2),
                    bonus_tmp = mapply(function(x,y) {if(is.nan(x)) {
                      0} else if (x >= 90 & x <= 120){
                        round(x/100*y*0.03)} else if(x >120) {
                          round(1.2*y*0.03)} else {0}},
                      target_revenue_realization_by_sr,real_revenue_by_sr),
                    bonus = round(bonus_tmp*bonus_factor),
                    sr_acc_revenue = real_revenue_by_sr+pp_sr_acc_revenue,
                    experience_index = round(sapply(sr_acc_revenue, function(x) round(curve(find_sta("curve11",curves,"curves"),x),2))),
                    m_meeting_with_team_delta =  mapply(function(x,y){
                      if (x == "junior") {
                        curve(find_sta("curve13",curves,"curves"),y)
                      } else if(x=="middle"){
                        curve(find_sta("curve14",curves,"curves"),y)
                      } else if(x=="senior"){
                        curve(find_sta("curve15",curves,"curves"),y)
                      } else{0}
                    },sales_level,
                    meetings_with_team,SIMPLIFY=T),
                    m_sales_target_realization_delta = sapply(target_revenue_realization_by_sr,function(x) 
                      if (!is.nan(x)) {curve(find_sta("curve16",curves,"curves"),x)} else {0}),
                    motivation_index = round(
                      (pp_motivation_index+m_admin_work_delta)*
                        ((find_sta("motivation",weightages,"1"))$admin_work)+
                        (pp_motivation_index+m_sales_target_realization_delta)*
                        ((find_sta("motivation",weightages,"1"))$sales_target_realization)+
                        (pp_motivation_index+m_meeting_with_team_delta)*
                        ((find_sta("motivation",weightages,"1"))$meetings_with_team)+
                        (pp_motivation_index+m_sales_training_delta)*
                        ((find_sta("motivation",weightages,"1"))$sales_training))) %>%
      dplyr::mutate(motivation_index=ifelse(salesmen==0,0,motivation_index)) %>%
      ungroup()
    
    
    tmp
  }
  
  
  data_to_use <- calculation(pp_data1,
                             pp_data2,
                             cp_data1,
                             cp_data2)
  
  ##----------------------------------------------------------------------------
  ##--                 making assessment
  ##----------------------------------------------------------------------------
  
  assessment_func <- function(input_phase,
                              data,
                              management_part,
                              decision_part,
                              target_info,
                              best_allocations_info
  ) {
    # input_phase <- Phase
    # data <- data_to_use
    # management_part <- management_input
    # decision_part <- decision_input
    # target_info <- set_target_revenue_info
    # best_allocations_info <- best_allocations
    
    ## Part 1 
    total_revenue <- sum(data$real_revenue)
    pp_total_revenue <- sum(data$pp_real_revenue)
    total_revenue_uplift_ratio <- total_revenue/pp_total_revenue - 1 
    
    business_info_in_need <- data %>%
      left_join(target_info, by = c("phase", "prod_code")) %>%
      group_by(phase,prod_code) %>%
      summarise(real_revenue = sum(real_revenue, na.rm = T),
                target_revenue = sum(target_revenue, na.rm = T),
                pp_real_revenue = sum(pp_real_revenue, na.rm = T),
                pp_potential = sum(pp_potential, na.rm = T),
                potential = sum(potential, na.rm = T),
                set_target_revenue = first(set_target_revenue)) %>%
      mutate(achievement_ratio = ifelse(target_revenue==0,
                                        0,
                                        real_revenue/target_revenue),
             pp_market_share = pp_real_revenue/pp_potential,
             market_share = real_revenue/potential,
             uplift_ratio = market_share - pp_market_share) 
    
    achievement_info <- business_info_in_need %>%
      select(phase, prod_code, real_revenue, target_revenue, achievement_ratio)
    
    market_share_info <- business_info_in_need %>%
      select(phase, prod_code, pp_market_share, market_share, uplift_ratio)
    
    team_ability_info <- data %>%
      filter(salesmen !=0) %>%
      select(phase,
             salesmen,
             sales_skills_index, 
             product_knowledge_index,
             motivation_index,
             pp_sales_skills_index,
             pp_product_knowledge_index,
             pp_motivation_index) %>%
      distinct() %>%
      summarise_if(is.numeric, mean) %>%
      dplyr::mutate(team_ability = round((sales_skills_index+product_knowledge_index+motivation_index)/3),
                    pp_team_ability = round((pp_sales_skills_index+pp_product_knowledge_index+pp_motivation_index)/3),
                    uplift_ratio = team_ability-pp_team_ability) %>%
      dplyr::select(phase, pp_team_ability, team_ability, uplift_ratio)
    
    
    ## point A
    overall_target_realization <- business_info_in_need %>%
      summarise(real_revenue = sum(real_revenue),
                set_target_revenue = first(set_target_revenue)) %>%
      mutate(overall_target_realization = real_revenue/set_target_revenue) 
    
    overall_target_realization <- overall_target_realization$overall_target_realization
    
    ### KPI 1 当期市场份额增长率
    
    kpi_1_info_1 <- data %>%
      filter(prod_code!=4) %>%
      arrange(-potential) %>%
      mutate(pp_market_share = ifelse(pp_potential == 0,
                                      pp_real_revenue/(pp_potential+1), 
                                      pp_real_revenue/pp_potential),
             cp_market_share = ifelse(potential == 0,
                                      prod_value/(potential+1),
                                      prod_value/potential),
             cum_proportion = cumsum(potential)/sum(potential),
             cluster = ifelse(cum_proportion<=0.8,1,2),
             chk = ifelse(cp_market_share- pp_market_share>0,
                          1,
                          0)) %>%
      group_by(cluster) %>%
      summarise(good_num = sum(chk),
                total_num = n()) %>%
      mutate(cluster_pro = good_num/total_num,
             cluster_score = ifelse(cluster==1,
                                    0.6*cluster_pro,
                                    0.4*cluster_pro)) %>%
      select(cluster,
             cluster_pro, cluster_score)
    
    kpi_1_1_1 <- diff(c(6,unique(management_input$kpi_analysis)))  # check KPI analysis
    kpi_1_1_2 <- sum(kpi_1_info_1$cluster_score) #check estimated market share&current share
    
    if ((kpi_1_1_1>4) | kpi_1_1_2<0.6) {
      kpi_1_1 <- 1
    } else if (kpi_1_1_2>= 0.6 & kpi_1_1_2<0.7) {
      kpi_1_1 <- 2
    } else if (kpi_1_1_2>= 0.7 & kpi_1_1_2<0.8) {
      kpi_1_1 <- 3
    } else if (kpi_1_1_2>= 0.8 & kpi_1_1_2<0.9) {
      kpi_1_1 <- 4
    } else { kpi_1_1 <- 5}
    
    
    ### KPI 2 市场增长率对比指标增长率
    kpi_1_info_2 <- data %>%
      mutate(market_growth = ifelse(pp_potential == 0,
                                    potential/(pp_potential+1),
                                    potential/pp_potential),
             tgrevenue_growth = ifelse(pp_real_revenue == 0,
                                       prod_value/(pp_real_revenue+1),
                                       prod_value/pp_real_revenue),
             pp_market_share = ifelse(pp_potential==0,
                                      0,
                                      pp_real_revenue/pp_potential),
             wgt = potential/sum(potential)) %>%
      filter(prod_code != 4) %>%
      mutate(rank_1 = dense_rank(-market_growth),
             rank_2 = dense_rank(pp_market_share),
             rank_12 = (rank_1+rank_2)/2,
             rank_3 = dense_rank(-tgrevenue_growth)) %>%
      select(hosp_code, prod_code, pp_market_share, 
             market_growth, tgrevenue_growth,
             rank_12, rank_3,wgt) %>%
      mutate(chk = rank_12 - rank_3,
             summ = sum(rank_12),
             numm = n()-1)
    
    kpi_1_info_2$chk_m <- scale(abs(kpi_1_info_2$chk),center = F)
    kpi_1_info_2 <- kpi_1_info_2 %>%
      mutate(chk_m1 = wgt*chk_m)
    kpi_1_2_1 <- sum(kpi_1_info_2$chk_m1)
    
    if (kpi_1_2_1 > 1) {
      kpi_1_2 <- 1
    } else if (kpi_1_2_1 >0.8& kpi_1_2_1 <= 1) {
      kpi_1_2 <- 2  
    } else if (kpi_1_2_1 >0.6& kpi_1_2_1 <=0.8) {
      kpi_1_2 <- 3
    } else if (kpi_1_2_1 >0.5& kpi_1_2_1 <=0.6) {
      kpi_1_2 <- 4
    } else {kpi_1_2 <- 5}
    
    
    
    ### KPI 3 指标贡献度
    kpi_1_info_3 <- data %>%
      group_by(hosp_code) %>%
      summarise(target_revenue = sum(prod_value, na.rm = T),
                potential = sum(potential, na.rm = T)) %>%
      arrange(-target_revenue) %>%
      mutate(cum_prob_tgrevenue = cumsum(target_revenue)/sum(target_revenue, na.rm = T),
             rank_target = row_number()) %>%
      arrange(-potential) %>%
      mutate(cum_prob_potential = cumsum(potential)/sum(potential, na.rm = T)) %>%
      select(hosp_code,cum_prob_tgrevenue, cum_prob_potential,
             rank_target)
    
    kpi_1_3_1 <- dist(rbind(kpi_1_info_3$cum_prob_tgrevenue, kpi_1_info_3$cum_prob_potential))
    
    if (input_phase == 1) {
      chk_data <- data %>%
        filter(prod_code != 4) %>%
        group_by(hosp_code) %>%
        summarise(target_revenue =sum(target_revenue, na.rm = T),
                  pp_real_revenue = sum(pp_real_revenue, na.rm = T)) %>%
        mutate(total_target_revenue = sum(target_revenue, na.rm = T),
               total_pp_real_revenue = sum(pp_real_revenue, na.rm = T),
               chk = ifelse(total_target_revenue==0,
                            0,
                            target_revenue/total_target_revenue),
               chk_m = ifelse(chk==0,
                              0,
                              chk*log10(chk)))
      
    } else {
      
      chk_data <- data %>%
        group_by(hosp_code) %>%
        summarise(target_revenue =sum(target_revenue, na.rm = T),
                  pp_real_revenue = sum(pp_real_revenue, na.rm = T)) %>%
        mutate(total_target_revenue = sum(target_revenue, na.rm = T),
               total_pp_real_revenue = sum(pp_real_revenue, na.rm = T),
               chk = ifelse(total_target_revenue==0,
                            0,
                            target_revenue/total_target_revenue),
               chk_m = ifelse(chk==0,
                              0,
                              chk*log10(chk)))
      
    }
    
    kpi_1_3_2 <- -sum(chk_data$chk_m)
    kpi_1_3_3 <- unique(chk_data$total_target_revenue)/unique(chk_data$total_pp_real_revenue)
    
    users_target_hosp <- kpi_1_info_3 %>%
      filter(rank_target <= 4)
    
    if (kpi_1_3_1 > 0.8 ) {
      kpi_1_3 <- 1
    } else if (kpi_1_3_1 >0.6& kpi_1_3_1 <= 0.8) {
      kpi_1_3 <- 2 
    } else if (kpi_1_3_1 >0.4& kpi_1_3_1 <=0.6) {
      kpi_1_3 <- 3
    } else if (kpi_1_3_1 >0.15& kpi_1_3_1 <=0.4) {
      kpi_1_3 <- 4
    } else {kpi_1_3 <- 5}
    
    if (overall_target_realization >0.9){
      kpi_1_1 <- ifelse(kpi_1_1<3, 3, kpi_1_1)
      kpi_1_2 <- ifelse(kpi_1_3<3, 3, kpi_1_3)
      kpi_1_3 <- ifelse(kpi_1_3<3, 3, kpi_1_3)
    } else  {
      kpi_1_1 <- ifelse(kpi_1_1<3, kpi_1_1, 3)
      kpi_1_2 <- ifelse(kpi_1_2<3, kpi_1_2, 3)
      kpi_1_3 <- ifelse(kpi_1_3<3, kpi_1_3, 3)
    }
    
    if (kpi_1_3_2 < 0.2|kpi_1_3_2>0.9|kpi_1_3_3<0.8) {
      kpi_1_3 <- 2
    } else {kpi_1_3 <- kpi_1_3}
    
    ## point B
    overall_revenue_distance <- 
      sum(data$real_revenue)/filter(best_allocations_info,
                                    phase == input_phase)$best_revenue
    
    ### KPI 1 
    kpi_2_info_1 <- data %>%
      group_by(phase, prod_code) %>%
      summarise(pp_real_revenue = sum(pp_real_revenue, na.rm = T),
                target_revenue = sum(target_revenue, na.rm = T),
                real_revenue = sum(real_revenue, na.rm = T)) %>%
      left_join(set_target_revenue_info, by = c("phase", "prod_code")) %>%
      mutate(target_revenue_const = ifelse(target_revenue==0&set_target_revenue==0,
                                           1,
                                           target_revenue/set_target_revenue),
             target_realization = ifelse(target_revenue==0,
                                         10,
                                         real_revenue/target_revenue))
    
    kpi_2_1_1 <- dist(rbind(kpi_2_info_1$target_revenue_const,rep(1,length(nrow(kpi_2_info_1))))) 
    kpi_2_1_2 <- sum(kpi_2_info_1$target_realization >=0.9 &kpi_2_info_1$target_realization <=1.2)
    
    if (kpi_2_1_1 >0.2) {
      kpi_2_1 <- 1
    } else if (kpi_2_1_2 == 0) {
      kpi_2_1 <- 2
    } else if (kpi_2_1_2 == 1) {
      kpi_2_1 <- 3
    } else if (kpi_2_1_2 == 2) {
      kpi_2_1 <- 4
    } else {kpi_2_1 <- 5}
    
    ### KPI 2
    kpi_2_info_2 <- data %>%
      group_by(hosp_code) %>%
      summarise(salesmen = first(salesmen),
                prod_hours = sum(prod_hours, na.rm = T)) %>%
      left_join(unique(select(decision_input, hosp_code, budget)), by = c("hosp_code")) %>%
      mutate(rank_time = dense_rank(-prod_hours),
             rank_budget = dense_rank(-budget)) %>%
      left_join(kpi_1_info_3, by = "hosp_code")
    
    kpi_2_2_1 <- kpi_2_info_2 %>%
      filter(rank_target <= 4) 
    kpi_2_2_1_1 <- dist(rbind(kpi_2_2_1$rank_time,kpi_2_2_1$rank_target))
    kpi_2_2_1_2 <- dist(rbind(kpi_2_2_1$rank_budget,kpi_2_2_1$rank_target))
    
    kpi_2_2_2 <- kpi_2_info_2 %>%
      filter(rank_target > 4)
    kpi_2_2_2_1 <- dist(rbind(kpi_2_2_2$rank_time,kpi_2_2_2$rank_target))
    kpi_2_2_2_2 <- dist(rbind(kpi_2_2_2$rank_budget,kpi_2_2_2$rank_target))
    
    kpi_2_2_2_3 <- 0.8*kpi_2_2_1_1 + 0.2*kpi_2_2_2_1
    kpi_2_2_2_4 <- 0.8*kpi_2_2_1_2 + 0.2*kpi_2_2_2_2
    kpi_2_2_2_5 <- mean(c(kpi_2_2_2_3, kpi_2_2_2_4))
    
    if(kpi_2_2_2_5 > 6) {
      kpi_2_2 <- 1
    } else if (kpi_2_2_2_5 >4& kpi_2_2_2_5 <=6) {
      kpi_2_2 <- 2
    } else if (kpi_2_2_2_5 >3& kpi_2_2_2_5 <=4) {
      kpi_2_2 <- 3
    } else if (kpi_2_2_2_5 >2& kpi_2_2_2_5 <=3) {
      kpi_2_2 <- 4
    } else {
      kpi_2_2 <- 5
    }
    ### KPI 3
    kpi_2_info_3 <- data %>%
      select(hosp_code, prod_code,
             prod_hours)
    
    highest_revenue <- filter(best_allocations_info,phase == input_phase)$best_revenue
    
    kpi_2_3_1 <- var(kpi_2_info_3$prod_hours)
    kpi_2_3_2 <- var(unique(select(decision_input, hosp_code, budget))$budget)
    kpi_2_3_3 <- sum(data$real_revenue)/highest_revenue 
    
    if (kpi_2_3_3 < 0.6|kpi_2_3_1<60|kpi_2_3_2<10) {
      kpi_2_3 <- 1
    } else if (kpi_2_3_3 >=0.6 & kpi_2_3_3 <0.8) {
      kpi_2_3 <- 2
    } else if (kpi_2_3_3 >=0.8 & kpi_2_3_3 <0.9) {
      kpi_2_3 <- 3
    } else if (kpi_2_3_3 >=0.9 & kpi_2_3_3 <1) {
      kpi_2_3 <- 4
    } else {kpi_2_3 <- 5}
    
    # if (overall_revenue_distance >0.8){
    #   kpi_2_1 <- ifelse(kpi_2_1<3, 3, kpi_2_1)
    #   kpi_2_2 <- ifelse(kpi_2_2<3, 3, kpi_2_2)
    #   kpi_2_3 <- ifelse(kpi_2_3<3, 3, kpi_2_3)
    # } else {
    #   kpi_2_1 <- ifelse(kpi_2_1<3, kpi_2_1, 3)
    #   kpi_2_2 <- ifelse(kpi_2_2<3, kpi_2_2, 3)
    #   kpi_2_3 <- ifelse(kpi_2_3<3, kpi_2_3, 3)
    # }
    
    ## point C
    team_ability_delta <- mean(c(data$product_knowledge_index,
                                 data$sales_skills_index,
                                 data$motivation_index)) -
      mean(c(data$pp_product_knowledge_index,
             data$pp_sales_skills_index,
             data$pp_motivation_index))    
    
    
    ### KPI 1
    kpi_3_info_1 <- data %>%
      filter(salesmen != 0) %>%
      group_by(salesmen) %>%
      summarise(pp_real_revenue = sum(pp_real_revenue, na.rm = T),
                target_revenue = sum(prod_value, na.rm = T),
                real_revenue = sum(real_revenue, na.rm = T),
                field_work = first(field_work),
                pp_experience_index = first(pp_experience_index)) %>%
      mutate(target_realization = ifelse(is.nan(real_revenue/target_revenue),
                                         0,real_revenue/target_revenue),
             target_increase = target_revenue/pp_real_revenue,
             rank_increase = dense_rank(-target_increase),
             rank_pp_exper = dense_rank(pp_experience_index),
             focus_sr = ifelse(rank_increase%in%1|rank_pp_exper%in%1:2,
                               1,
                               0)) 
    
    kpi_3_1_1 <- sum(kpi_3_info_1$real_revenue)/sum(kpi_3_info_1$target_revenue)
    kpi_3_1_2 <- sum(kpi_3_info_1$target_realization <=1.2&kpi_3_info_1$target_realization>0.9)
    kpi_3_1_3 <- sum(kpi_3_info_1$field_work>=5&kpi_3_info_1$field_work<=10)
    kpi_3_1_4 <- sum(filter(kpi_3_info_1, focus_sr == 1)$field_work>=8)/nrow(filter(kpi_3_info_1, focus_sr == 1))
    
    if (kpi_3_1_1 >1.2 | kpi_3_1_1 <0.9 |kpi_3_1_3<3 |kpi_3_1_4<0.4) {
      kpi_3_1 <- 1
    } else if (kpi_3_1_2 ==1|
               (kpi_3_1_4>=0.4&kpi_3_1_4<0.6)|
               (kpi_3_1_3==3)) {
      kpi_3_1 <- 2
    } else if (kpi_3_1_2 ==2) {
      kpi_3_1 <- 3
    } else if (kpi_3_1_2 ==3) {
      kpi_3_1 <- 4
    } else {kpi_3_1 <- 5}
    
    
    ### KPI 2 
    kpi_3_info_2 <- data %>%
      filter(salesmen != 0) %>%
      select(salesmen, motivation_index,
             pp_motivation_index) %>%
      distinct() %>%
      mutate(motivation_delta = motivation_index - pp_motivation_index)
    
    kpi_3_2_1 <- mean(kpi_3_info_2$motivation_delta)
    
    if (kpi_3_2_1 <= 0) {
      kpi_3_2 <- 1
    } else if (kpi_3_2_1 >0& kpi_3_2_1<=2) {
      kpi_3_2 <- 2
    } else if (kpi_3_2_1 >2& kpi_3_2_1<=4) {
      kpi_3_2 <- 3
    } else if (kpi_3_2_1 >4& kpi_3_2_1<=6) {
      kpi_3_2 <- 4
    } else { kpi_3_2 <- 5}
    
    ### KPI 3
    kpi_3_info_3 <- data %>%
      filter(salesmen != 0) %>%
      select(salesmen, sales_training,
             pp_sales_skills_index,
             product_training,
             pp_product_knowledge_index) %>%
      distinct() %>%
      mutate(rank_skills = dense_rank(pp_sales_skills_index),
             rank_sales_train = dense_rank(-sales_training),
             rank_knowledge = dense_rank(pp_product_knowledge_index),
             rank_prod_train = dense_rank(-product_training),
             wht_sales_train = sales_training<8,
             wht_prod_train = product_training>4)
    
    kpi_3_3_1 <- dist(rbind(kpi_3_info_3$rank_prod_train,kpi_3_info_3$rank_knowledge))
    kpi_3_3_2 <- dist(rbind(kpi_3_info_3$rank_sales_train,kpi_3_info_3$rank_skills))
    kpi_3_3_3 <- mean(c(kpi_3_3_1,kpi_3_3_2))
    
    if (kpi_3_3_3 >10) {
      kpi_3_3 <- 1
    } else if (kpi_3_3_3>6&kpi_3_3_3<=10) {
      kpi_3_3 <- 2
    } else if (kpi_3_3_3>4&kpi_3_3_3<6) {
      kpi_3_3 <- 3
    } else if (kpi_3_3_3>2&kpi_3_3_3<=4) {
      kpi_3_3 <- 4
    } else {r <- 5}
    
    if (team_ability_delta >2){
      kpi_3_1 <- ifelse(kpi_3_1<3, 3, kpi_3_1)
      kpi_3_2 <- ifelse(kpi_3_2<3, 3, kpi_3_2)
      kpi_3_3 <- ifelse(kpi_3_3<3, 3, kpi_3_3)
    } else {
      kpi_3_1 <- ifelse(kpi_3_1<3, kpi_3_1, 3)
      kpi_3_2 <- ifelse(kpi_3_2<3, kpi_3_2, 3)
      kpi_3_3 <- ifelse(kpi_3_3<3, kpi_3_3, 3)
    }
    
    part2 <- data.frame(phase = rep(input_phase,length.out=9),
                        ability_code = rep(1:3,each=3),
                        kpi_code = rep(1:3,times=3)) %>%
      mutate(basic_score = mapply(function(x,y) get(paste("kpi",x, y, sep="_")),
                                  ability_code,kpi_code),
             revenue_distance = overall_revenue_distance,
             basic_score = mapply(function(x,y) {
               if (y < 0.5) {
                 1 } else if ((y >=0.5&y<0.7)& x >2) {
                   2 } else { x}},basic_score, revenue_distance),
             chk = paste("kpi",ability_code, kpi_code, sep="_"),
             second_score = ifelse(basic_score >3,3,NA),
             second_score = ifelse(basic_score ==1,1,second_score),
             second_score = ifelse(basic_score >1&basic_score<=3,2,second_score))
    
    score <- median(part2$second_score)
    
    if (score >= 3) {
      overall_score <- "gold"
    } else if (score <3&score>=2) {
      overall_score <- "silver"
    } else {
      overall_score <- "bronze"
    }
    
    out <- list("phase" = input_phase,
                "overall_score" = overall_score,
                "assess_results" = part2,
                "final_revenue_info" = list("pp_revenue"=pp_total_revenue,
                                            "revenue"= total_revenue,
                                            "uplift_ratio" = total_revenue_uplift_ratio),
                "achievement_info" = achievement_info,
                "market_share_info" = market_share_info,
                "team_ability_info" = list("pp_team_ability" = team_ability_info$pp_team_ability,
                                           "team_ability"=team_ability_info$team_ability,
                                           "uplift_ratio"=team_ability_info$uplift_ratio))
    
    out$assess_results$basic_score <- sapply(out$assess_results$basic_score , function(x) { 
      if (x <=1 ){
        1
      } else if (x <=2 &x >1) {
        sample(2:3,1)
      } else if (x <3 &x >2) {
        sample(2:3,1) 
      } else if (x <4 &x >=3) {
        sample(3:4,1)
      } else { sample(4:5,1)}
    })
    
    
    return(out)
  }
  
  assess_info <- assessment_func(Phase,
                                 data_to_use,
                                 management_input,
                                 decision_input,
                                 set_target_revenue_info,
                                 best_allocations)
  
  assess_info_mongo <- mongodb_ass$find()
  
  if ( R_Json_Path%in%assess_info_mongo$uuid ) {
    if (Phase == 1) {
      out <- list("phase_1" = assess_info)
    } else {
      

      out <- list("phase_1" = -1,
                  "phase_2" = assess_info,
                  "final" = -1)
    }
    
    mongo_assess_tmp <- paste('{"uuid" : ', '"', R_Json_Path, '"}',sep = "")
    mongo_assess_tmp2 <- paste('{"$set":{"time":',as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),',"result":',toJSON(out,auto_unbox = T),'}}', sep = "")
    mongodb_ass$update(mongo_assess_tmp, mongo_assess_tmp2)
    
    
  } else {
    
    mongodb_ass$insert(list("uuid"= R_Json_Path,
                            "user_id"= user_name,
                            "time" = as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),
                            "result"= list("phase_1" = -1,
                                           "phase_2" = assess_info,
                                           "final" = -1)),
                       na="string",
                       auto_unbox = T)}
  

  

  
  ##----------------------------------------------------------------------------
  ##--                 making reports
  ##----------------------------------------------------------------------------
  
  report_data <- function(tmp,flm_data) {
    
    tmp1 <- tmp %>% 
      dplyr::mutate(prod_name = factor(prod_name,levels=product_info$prod_name)) 
    tmp2 <- tmp %>%
      filter(salesmen!="0") %>%
      dplyr::mutate(salesmen=factor(salesmen,levels = salesmen_list$salesmen))
    
    ## report 1
    profit_tmp <- tmp1 %>%
      select(real_revenue,
             real_volume,
             budget,
             prod_name) %>%
      dplyr::mutate(production_cost = sapply(prod_name,function(x)product_info[which(product_info$prod_name==x),]$prod_unit_cost),
                    production_fee = round(production_cost*real_volume),
                    total_revenue =round(sum(real_revenue,na.rm=T)),
                    total_production_fee =round(sum(production_fee,na.rm=T)),
                    total_promotional_budget = round(sum(budget,na.rm=T)),
                    total_profit = total_revenue-total_production_fee-total_promotional_budget)  %>%
      select(total_profit) %>%
      distinct()
    
    
    report1_mod1 <- tmp1 %>%
      select(phase,
             salesmen,
             hosp_name,
             product_knowledge_index,
             sales_skills_index,
             customer_relationship_index,
             motivation_index,
             real_revenue,
             pp_acc_success_value) %>%
      distinct() %>%
      dplyr::mutate(total_revenue = round(sum(real_revenue,na.rm=T),2)) %>%
      filter(salesmen!="0") %>%
      dplyr::mutate(average_customer_relationship_index = round(mean(customer_relationship_index,na.rm=T),2),
                    average_sales_skills_index = round(mean(sales_skills_index,na.rm=T),2),
                    average_product_knowledge_index = round(mean(product_knowledge_index,na.rm=T),2),
                    average_motivation_index = round(mean(motivation_index,na.rm=T),2),
                    team_capability = round((average_motivation_index +
                                               average_product_knowledge_index +
                                               average_sales_skills_index)/3)) %>%
      select(phase,
             total_revenue,
             average_customer_relationship_index,
             team_capability,
             pp_acc_success_value) %>%
      distinct() %>%
      dplyr::mutate(profit=as.numeric(profit_tmp),
                    inter1=find_sta("success_value",weightages,"1")$total_sales*curve(find_sta("curve50",curves,"curves"),total_revenue),
                    inter2=find_sta("success_value",weightages,"1")$team_capability*curve(find_sta("curve46",curves,"curves"),team_capability),
                    inter3=find_sta("success_value",weightages,"1")$contribution_margin*curve(find_sta("curve49",curves,"curves"),profit),
                    success_value = round(inter1+inter2+inter3),
                    acc_success_value = success_value + pp_acc_success_value) %>%
      dplyr::mutate(success_value = ifelse(phase==0,"",success_value),
                    acc_success_value = ifelse(phase==0,"",acc_success_value)) %>%
      select(phase,
             total_revenue,
             profit,
             team_capability,
             success_value,
             acc_success_value) %>%
      distinct() %>%
      dplyr::mutate(phase = paste("phase",phase,sep=""))
    
    acc_success_value <- ifelse(is.na(as.numeric(report1_mod1$acc_success_value)),0,
                                as.numeric(report1_mod1$acc_success_value))
    
    colnames(report1_mod1) <- final_report_add
    

    
    ## report 1——2
    report1_mod2 <- tmp1 %>%
      select(hosp_name,
             hosp_code,
             prod_name,
             real_revenue,
             pp_real_revenue) %>%
      group_by(hosp_name) %>%
      dplyr::mutate(hospital_revenue = round(sum(real_revenue,na.rm=T)),
                    pp_hospital_revenue = round(sum(pp_real_revenue,na.rm=T))) %>%
      ungroup() %>%
      select(hosp_name,
             hosp_code,
             pp_hospital_revenue,
             hospital_revenue) %>%
      distinct() %>%
      arrange(hosp_code) 
    
    colnames(report1_mod2) <- c("hosp_name", "hosp_code" ,"last_total_revenue", "current_total_revenue")

    
    ## report 2——1
    report2_mod1 <- tmp2 %>%
      group_by(salesmen) %>%
      dplyr::mutate(visit_time=round(sum(real_sr_time,na.rm=T)),
                    total_sr_time=round(overhead_time+
                                          sales_training+
                                          product_training+
                                          meetings_with_team+
                                          visit_time)) %>%
      ungroup() %>%
      select(overhead_time,
             sales_training,
             product_training,
             meetings_with_team,
             visit_time,
             total_sr_time,
             salesmen) %>%
      distinct()
    
    colnames(report2_mod1) <- staff_time_add
    
    report2_mod1 <- report2_mod1 %>%
      gather(variable,value,-salesmen) %>%
      spread(salesmen,value) 
    
    report2_rank1 <- data.frame(
      variable=staff_time_add[1:6],
      rank=1:6,
      stringsAsFactors = F
    )
    
    report2_mod1 <- report2_mod1 %>%
      left_join(report2_rank1,by="variable") %>%
      arrange(rank) %>%
      dplyr::select(variable,
             one_of(salesmen_list$salesmen))
    
    colnames(report2_mod1) <- c("general_names","salesmen_first","salesmen_second",
                                "salesmen_third", "salesmen_fourth",
                                "salesmen_fifth")
      
  
    
    
    
    ## report 2——2
    report2_mod2 <- tmp2 %>%
      select(salesmen,
             pp_product_knowledge_index,
             product_knowledge_index) %>%
      distinct()
    
    colnames(report2_mod2) <- product_knowledge_add
    
    report2_mod2 <- report2_mod2 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value) %>%
      select(variable,
            one_of(salesmen_list$salesmen))
    
    colnames(report2_mod2) <- c("general_names","salesmen_first","salesmen_second",
                                "salesmen_third", "salesmen_fourth",
                                "salesmen_fifth")
    
    
    ## report 2——3
    report2_mod3 <- tmp2 %>%
      select(pp_experience_index,
             experience_index,
             salesmen) %>%
      distinct()
    
    colnames(report2_mod3) <- experience_add
    
    report2_mod3 <- report2_mod3 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value)  %>%
      select(variable,
             one_of(salesmen_list$salesmen))
    
    colnames(report2_mod3) <- c("general_names","salesmen_first","salesmen_second",
                                "salesmen_third", "salesmen_fourth",
                                "salesmen_fifth")
    
   
    
    ## report 2——4
    report2_mod4 <- tmp2 %>%
      select(salesmen,
             pp_sales_skills_index,
             sales_skills_index) %>%
      distinct()
    
    colnames(report2_mod4) <- sales_skills_add
    
    report2_mod4 <- report2_mod4 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value)  %>%
      select(variable,
             one_of(salesmen_list$salesmen))
    
    colnames(report2_mod4) <- c("general_names","salesmen_first","salesmen_second",
                                "salesmen_third", "salesmen_fourth",
                                "salesmen_fifth")
    
    
    ## report 2——5
    report2_mod5 <- tmp2 %>%
      select(salesmen,
             pp_motivation_index,
             motivation_index) %>%
      distinct()  
    
    colnames(report2_mod5) <- motivation_add
    
    report2_mod5 <- report2_mod5 %>%
      gather(variable,value,-sr_rep) %>%
      spread(sr_rep,value)  %>%
      select(variable,
             one_of(salesmen_list$salesmen))
    
    colnames(report2_mod5) <- c("general_names","salesmen_first","salesmen_second",
                                "salesmen_third", "salesmen_fourth",
                                "salesmen_fifth")
    
    
    ## report 3——1
    flm_report <- tmp1 %>%
      select(salesmen,
             bonus) %>%
      distinct() %>%
      dplyr::mutate(all_sr_bonus=sum(bonus,na.rm=T)) %>%
      select(all_sr_bonus) %>%
      distinct()
    
    flm_report <- flm_data %>%
      dplyr::mutate(all_sr_bonus = flm_report$all_sr_bonus,
                    work_time=sales_training+
                      field_work+
                      meetings_with_team+
                      kpi_analysis+
                      admin_work)
    
    report3_mod1 <- tmp1 %>%
      filter(salesmen!=0) %>%
      select(salesmen,bonus) %>%
      distinct() %>%
      dplyr::mutate(salesmen=factor(salesmen,levels = salesmen_list$salesmen)) %>%
      do(plyr::rbind.fill(.,data.frame(salesmen=over_ch,
                                       bonus = sum(.$bonus)))) %>%
      arrange(salesmen) 
    
    colnames(report3_mod1) <- c("salesmen", "bonus")
    
    report3_mod2 <- flm_report %>%
      select(-all_sr_bonus) 
    
    colnames(report3_mod2) <- flm_time_add
    
    report3_mod2 <- report3_mod2 %>%
      gather(variable,value)  %>%
      select(variable,
             value)
    
    colnames(report3_mod2) <- c("general_names","values")
    
    
    
    ## report 4——1
    report4_mod1 <- tmp1 %>%
      select(hosp_name,
             prod_name,
             salesmen,
             sr_time,
             real_sr_time,
             budget,
             real_revenue,
             real_volume,
             hosp_code) %>%
      group_by(hosp_name) %>%
      dplyr::mutate(production_cost = sapply(prod_name,function(x)product_info[which(product_info$prod_name==x),]$prod_unit_cost),
                    production_fee = round(production_cost*real_volume),
                    profit = real_revenue - budget - production_fee) %>%
      select(hosp_name,
             prod_name,
             salesmen,
             sr_time,
             real_sr_time,
             real_revenue,
             budget,
             production_fee,
             profit,
             hosp_code) %>%
      do(plyr::rbind.fill(.,data.frame(hosp_name = first(.$hosp_name),
                                       prod_name = over_ch,
                                       salesmen = first(.$salesmen),
                                       sr_time = sum(.$sr_time,na.rm=T),
                                       real_sr_time = sum(.$real_sr_time,na.rm=T),
                                       real_revenue = sum(.$real_revenue,na.rm=T),
                                       budget = sum(.$budget,na.rm=T),
                                       production_fee = sum(.$production_fee,na.rm=T),
                                       profit = sum(.$profit,na.rm=T),
                                       hosp_code = first(.$hosp_code)))) %>%
      ungroup()
    
    
    colnames(report4_mod1) <- resource_allocation_add
    
    report4_mod1 <- report4_mod1 %>%
      gather(variable,value,one_of(resource_allocation_add[3:9])) %>%
      spread(prod_name,value) %>%
      left_join(resource_allocation_rank,by="variable") %>%
      arrange(hosp_code,rank) %>%
      select(one_of(resource_allocation_variable_list))
    
    colnames(report4_mod1) <- c("hosp_code","hosp_name","factor","product_first","overall")
    
 
    
    
    ## report 5——1
    report5_mod1 <- tmp1 %>%
      select(hosp_name,
             hosp_code,
             prod_name,
             prod_code,
             real_revenue,
             pp_real_revenue,
             target_revenue)%>%
      group_by(hosp_name) %>%
      # do(plyr::rbind.fill(.,data.frame(hosp_name = first(.$hosp_name),
      #                                  hosp_code = first(.$hosp_code),
      #                                  prod_name = over_ch,
      #                                  prod_code = 5,
      #                                  real_revenue=sum(.$real_revenue,na.rm=T),
      #                                  pp_real_revenue=sum(.$pp_real_revenue,na.rm=T),
      #                                  target_revenue = sum(.$target_revenue,na.rm=T)))) %>%
      dplyr::mutate(real_revenue_increase = real_revenue - pp_real_revenue,
                    real_revenue_increase_ratio = ifelse(is.nan(round(real_revenue_increase/pp_real_revenue*100,0)),
                                                         0,
                                                         round(real_revenue_increase/pp_real_revenue*100,0)),
                    target_revenue_realization = ifelse(is.nan(round(real_revenue/target_revenue*100,0)),
                                                        0,
                                                        round(real_revenue/target_revenue*100,0))) %>%
      arrange(hosp_code) %>%
      select(hosp_code,
             hosp_name,
             prod_code,
             prod_name,
             target_revenue,
             pp_real_revenue,
             real_revenue,
             real_revenue_increase,
             real_revenue_increase_ratio,
             target_revenue_realization)
    
    
    
    colnames(report5_mod1) <- c("hosp_code","hosp_name","prod_code","prod_name","current_target",
                                "last_revenue","current_revenue","increase_revenue",
                                "increase_ratio","target_realization")
    
    
 
    report5_mod2 <- tmp1 %>%
      filter(salesmen!=0) %>%
      select(salesmen,
             prod_name,
             real_revenue,
             pp_real_revenue,
             target_revenue) %>%
      group_by(salesmen,prod_name) %>%
      dplyr::summarise(real_revenue_by_sr = sum(real_revenue,na.rm=T),
                       pp_real_revenue_by_sr = sum(pp_real_revenue,na.rm=T),
                       target_revenue_by_sr = sum(target_revenue,na.rm=T)) %>%
      # do(plyr::rbind.fill(.,data.frame(salesmen=first(.$salesmen),
      #                                  prod_name =over_ch,
      #                                  real_revenue_by_sr=sum(.$real_revenue_by_sr,na.rm=T),
      #                                  pp_real_revenue_by_sr =sum(.$pp_real_revenue_by_sr,na.rm=T),
      #                                  target_revenue_by_sr=sum(.$target_revenue_by_sr,na.rm=T)))) %>%
      dplyr::mutate(sr_target_revenue_realization = ifelse(is.nan(round(real_revenue_by_sr/target_revenue_by_sr*100,0)),0,
                                                           round(real_revenue_by_sr/target_revenue_by_sr*100,0))) %>%
      select(salesmen,
             prod_name,
             target_revenue_by_sr,
             pp_real_revenue_by_sr,
             real_revenue_by_sr,
             sr_target_revenue_realization) %>%
      arrange(salesmen)
    
    
    colnames(report5_mod2) <- c("salesmen","prod_name","current_target",
                                "last_revenue","current_revenue","target_realization")
    
    report5_mod3 <- tmp1 %>%
      select(prod_name,
             real_revenue,
             pp_real_revenue,
             target_revenue) %>%
      group_by(prod_name) %>%
      dplyr::summarise(real_revenue_by_product = round(sum(real_revenue,na.rm=T)),
                       pp_real_revenue_by_product = round(sum(pp_real_revenue,na.rm=T)),
                       real_revenue_increase = round(real_revenue_by_product - pp_real_revenue_by_product),
                       target_revenue_by_product = round(sum(target_revenue,na.rm=T))) %>%
      do(plyr::rbind.fill(.,data.frame(prod_name=over_ch,
                                       real_revenue_by_product=round(sum(.$real_revenue_by_product,na.rm=T)),
                                       pp_real_revenue_by_product=round(sum(.$pp_real_revenue_by_product,na.rm=T)),
                                       real_revenue_increase=sum(.$real_revenue_increase,na.rm=T),
                                       target_revenue_by_product=round(sum(.$target_revenue_by_product,na.rm=T))))) %>%
      dplyr::mutate(real_revenue_increase_ratio = ifelse(is.nan(round(real_revenue_increase/pp_real_revenue_by_product*100,0)),0,
                                                         round(real_revenue_increase/pp_real_revenue_by_product*100,0)),
                    target_revenue_realization_by_product = ifelse(is.nan(round(real_revenue_by_product/target_revenue_by_product*100,0)),0,
                                                                   round(real_revenue_by_product/target_revenue_by_product*100,0))) %>%
      select(prod_name,
             target_revenue_by_product,
             pp_real_revenue_by_product,
             real_revenue_by_product,
             real_revenue_increase,
             real_revenue_increase_ratio,
             target_revenue_realization_by_product)
    
    
    report5_mod3 <- report5_mod3 %>%
      left_join(dplyr::select(product_info,prod_code,prod_name),by="prod_name") %>%
      arrange(prod_code) %>%
      dplyr::select(-prod_code)

    colnames(report5_mod3) <- c("prod_name","current_target",
                                "last_revenue","current_revenue","increase_revenue",
                                "increase_ratio","target_realization")
    
   
    
    
    out<-list("report1_mod1"=report1_mod1,
              "report1_mod2"=report1_mod2,
              "report2_mod1"=report2_mod1,
              "report2_mod2"=report2_mod2,
              "report2_mod3"=report2_mod3,
              "report2_mod4"=report2_mod4,
              "report2_mod5"=report2_mod5,
              "report3_mod1"=report3_mod1,
              "report3_mod2"=report3_mod2,
              "report4_mod1"=report4_mod1,
              "report5_mod1"=report5_mod1,
              "report5_mod2"=report5_mod2,
              "report5_mod3"=report5_mod3,
             "acc_success_value"=acc_success_value
              
    )
    
    out
    
  }
  
  data_to_use2 <- report_data(data_to_use,flm_data)
  
  
  
  if (Phase == 1) {
    last_report1_1$phase <- c("phase0","phase1","phase2")
    report1_1_tmp <- rbind(last_report1_1[1,],data_to_use2$report1_mod1)
  } else {
    last_report1_1$phase <- c("phase0","phase1")
    report1_1_tmp <- rbind(last_report1_1[1:2,],data_to_use2$report1_mod1)
  }
  
  
  report1_mod1 <- report1_1_tmp %>%
    gather(name,value,-phase) %>%
    spread(phase,value)  %>%
    left_join(final_report_rank,by="name") %>%
    arrange(rank) %>%
    select(-variable,-rank) 
  
  if (Phase == 1) {
    report1_mod1 <- data.frame(name = report1_mod1$name,
                               phase0 = report1_mod1$phase0,
                               phase1= report1_mod1$phase1,
                               phase2= rep(NA,5))
    
    
  } else {
    report1_mod1 <- data.frame(name = report1_mod1$name,
                               phase0 = report1_mod1$phase0,
                               phase1= report1_mod1$phase1,
                               phase2= report1_mod1$phase2)
  }
 
  colnames(report1_mod1) <- c("general_names","phase0","phase1","phase2")
  report1_mod1[is.na(report1_mod1)] <- -1
  
####-- report data  
  tmp_data <- list(
    "report1_finalreport" = report1_mod1,
    "report1_sales_report" = data_to_use2$report1_mod2,
    "report2_staff_timetable" = data_to_use2$report2_mod1,
    "report2_product_knowledge" = data_to_use2$report2_mod2,
    "report2_experience" = data_to_use2$report2_mod3,
    "report2_sales_skills" = data_to_use2$report2_mod4,
    "report2_motivation" = data_to_use2$report2_mod5,
    "report3_staff_cost" = data_to_use2$report3_mod1,
    "report3_flm_timetable" = data_to_use2$report3_mod2,
    "report4_resource" = data_to_use2$report4_mod1,
    "report5_sales_by_hosp" = data_to_use2$report5_mod1,
    "report5_sales_by_salesmen" = data_to_use2$report5_mod2,
    "report5_sales_by_prod" = data_to_use2$report5_mod3)
  
  names(tmp_data) <- report_names$names_chinese
  
  to_mongo_tmp <- lapply(1:length(tmp_data), function(x) {
    report_name <- names(tmp_data)[x]
    tmp <- tmp_data[[x]]
    colname <- colnames(tmp)
    # colnames(tmp) <- 
    #   sapply(colname,function(x) names(names_box[which(x==names_box)]))
    out_new <- list("phase"=Phase,
                    "report_name"=report_name,
                    "result"=tmp)
    
    return(out_new)
  })
  
  to_mongo <- list(uuid=R_Json_Path,
                   user_id=user_name,
                   "time" = as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),
                   "report"=to_mongo_tmp)
  
  
#####-- intermedia data
  if (R_Json_Path %in% db_inter$find()$uuid) {
    
    transfer1 <- db_inter$find(paste('{"uuid" : "',R_Json_Path,'"}',sep = ""))
    inter_info <- transfer1$inter[[1]]
    
    if (Phase %in% inter_info$phase) {
      
      inter_out <- lapply(1:nrow(inter_info), function(x) {
        
        inter_chk <- inter_info[x,]
        
        if (inter_chk$phase == Phase) {
          
          list("phase" = Phase,
               "data" = data_to_use,
               "report" = report1_1_tmp,
               "acc_success_value" = data_to_use2$acc_success_value)
        } else {
          
          list("phase" = inter_chk$phase,
               "data" = inter_chk$data[[1]],
               "report" = inter_chk$report[[1]],
               "acc_success_value" = inter_chk$acc_success_value)
        }
      })
      
    } else {
      
      inter_out <- lapply(1:2, function(x) {
        

        if (x == 1) {
          
          list("phase" = inter_info$phase,
               "data" = inter_info$data[[1]],
               "report" = inter_info$report[[1]],
               "acc_success_value" = inter_info$acc_success_value)
          
        } else {
          
          list("phase" = Phase,
               "data" = data_to_use,
               "report" = report1_1_tmp,
               "acc_success_value" = data_to_use2$acc_success_value)
        }
      })
    }

     mongo_tmp <- paste('{"uuid" : ', '"', R_Json_Path, '"}',sep = "")
     mongo_tmp1 <- paste('{"$set":{"inter":',
                         toJSON(inter_out,
                                auto_unbox = T),'}}', sep = "")
     db_inter$update(mongo_tmp, mongo_tmp1)
     
  } else {
    
    db_inter$insert(list("uuid"=R_Json_Path,
                         "user_id"=user_name,
                         "inter"=list(list("phase" = Phase,
                                      "data" = data_to_use,
                                      "report" = report1_1_tmp,
                                      "acc_success_value" = data_to_use2$acc_success_value))),
                    na="string",
                    auto_unbox = T) 
      
    }
  
  
  
  
  
  
  
  
  
  
  ##----------------------------------------------------------------------------
  ##--               write the output results to the mongodb
  ##----------------------------------------------------------------------------
  
  #- create connection, database and collection
  
    mongodb_con <- mongo(collection = "report",
        url = sprintf(
          "mongodb://%s/%s",
          # options()$mongodb$username,
          # options()$mongodb$password,
          options()$mongodb$host,
          "TMIST"))
  
    names_report <- unlist(lapply(to_mongo_tmp, function(x) x$report_name))
    transfer2 <- mongodb_con$find()
    
    if ( R_Json_Path%in%transfer2$uuid ) {
      
      rownn2 <- which(transfer2$uuid==R_Json_Path)
      info <- transfer2[rownn2,]$report[[1]]
      phase_in_mongo <- info$phase
      
      if (Phase %in% phase_in_mongo) {
        
      out <-lapply(1:nrow(info), function(x) {
          
          report_name1 <- info$report_name[x]
          
          if (info$phase[x]==Phase) {
            chk <- which(names_report==report_name1)
            list("phase"=Phase,
                 "report_name"=report_name1,
                 "result"=to_mongo_tmp[[chk]]$result)
          } else {
            list("phase"=info$phase[x],
                 "report_name"= report_name1,
                 "result"=info$result[[x]])
          }}) 
      } else {
        
        out <-lapply(1:26, function(x) {
          
          if( is.na(info$phase[x])) {
            
            rownn_x <- x-13
            report_name1 <- info$report_name[rownn_x]
            chk <- which(names_report==report_name1)
            list("phase"=Phase,
                 "report_name"=report_name1,
                 "result"=to_mongo_tmp[[chk]]$result)
            
          } else {
            
            list("phase"=info$phase[x],
                 "report_name"= info$report_name[x],
                 "result"=info$result[[x]])
          }    
        })
      }
        
      mongo_tmp <- paste('{"uuid" : ', '"', R_Json_Path, '"}',sep = "")
      mongo_tmp2 <- paste('{"$set":{"time":',as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),',"report":',toJSON(out,auto_unbox = T),'}}', sep = "")
      mongodb_con$update(mongo_tmp, mongo_tmp2)
      
        
    } else {
      
      mongodb_con$insert(to_mongo, auto_unbox = T, na = "string")
      
    }
    
    
    
   

    
  
  

  