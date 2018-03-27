library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(plyr)
library(dplyr)
library(tidyr)
library(digest)
library(openxlsx)
library(mongolite)
options(mongodb = list(
  "host" = "xxxxxx"
  # "username" = "root",
  # "password" = "root")
),
scipen=200)

databaseName <- "STP"
collectionName <- "register"
# options(scipen=200)


load("initial_setting1.RData")

## curve funcion
curve <- function(name,input_x){
  data <- name
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

calculator <- function(input,phase){
  phase1_promotional_budget=0
  phase1_total_time_arrangement1 <- 0 
  phase1_total_time_arrangement2 <- 0 
  phase1_total_time_arrangement3 <- 0 
  phase1_total_time_arrangement4 <- 0
  phase1_total_time_arrangement5 <- 0
  
  for(i in 1:10){

    phase1_promotional_budget <-
      sum(c(phase1_promotional_budget, 
            as.numeric(input[[paste("p",phase,"_promotional_budget_hosp",i,sep="")]])),
          na.rm = TRUE)

    tmp <- sum(c(as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_1",sep="")]]),
                 as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_2",sep="")]]),
                 as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_3",sep="")]]),
                 as.numeric(input[[paste("p",phase,"_hosp",i,"_worktime_4",sep="")]])),
               na.rm = TRUE)
    if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
        sr_info_list$sales_rep[1]){
      phase1_total_time_arrangement1 <- 
        phase1_total_time_arrangement1 +tmp
    } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
               sr_info_list$sales_rep[2]) {
      phase1_total_time_arrangement2 <- 
        phase1_total_time_arrangement2 +tmp
    } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
               sr_info_list$sales_rep[3]) {
      phase1_total_time_arrangement3 <- 
        phase1_total_time_arrangement3 +tmp
    } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
               sr_info_list$sales_rep[4]) {
      phase1_total_time_arrangement4 <- 
        phase1_total_time_arrangement4 +tmp
    } else if (input[[paste("p",phase,"_sr_hosp",i,sep = "")]]==
               sr_info_list$sales_rep[5]) {
      phase1_total_time_arrangement5 <- 
        phase1_total_time_arrangement5 +tmp
    }
  }
  
  # team_meeting <- as.numeric(input[[paste("p",phase,"_flm_team_meeting",sep="")]])
  # 
  # phase1_total_time_arrangement1 <- sum(c(phase1_total_time_arrangement1,
  #   as.numeric(input[[paste("p",phase,"_sr1_sales_training",sep="")]]),
  #   as.numeric(input[[paste("p",phase,"_sr1_product_training",sep="")]]),
  #   team_meeting),na.rm=T)
  # 
  # phase1_total_time_arrangement2 <- sum(c(phase1_total_time_arrangement2,
  #   as.numeric(input[[paste("p",phase,"_sr2_sales_training",sep="")]]),
  #   as.numeric(input[[paste("p",phase,"_sr2_product_training",sep="")]]),
  #   team_meeting),na.rm=T)
  # 
  # phase1_total_time_arrangement3 <- sum(c(phase1_total_time_arrangement3,
  #   as.numeric(input[[paste("p",phase,"_sr3_sales_training",sep="")]]),
  #   as.numeric(input[[paste("p",phase,"_sr3_product_training",sep="")]]),
  #   team_meeting),na.rm=T)
  # 
  # phase1_total_time_arrangement4 <- sum(c(phase1_total_time_arrangement4,
  #   as.numeric(input[[paste("p",phase,"_sr4_sales_training",sep="")]]),
  #   as.numeric(input[[paste("p",phase,"_sr4_product_training",sep="")]]),
  #   team_meeting),na.rm=T)
  # 
  # phase1_total_time_arrangement5 <- sum(c(phase1_total_time_arrangement5,
  #   as.numeric(input[[paste("p",phase,"_sr5_sales_training",sep="")]]),
  #   as.numeric(input[[paste("p",phase,"_sr5_product_training",sep="")]]),
  #   team_meeting),na.rm=T)
  
  data <- c(phase1_promotional_budget,
            phase1_total_time_arrangement1,
            phase1_total_time_arrangement2,
            phase1_total_time_arrangement3,
            phase1_total_time_arrangement4,
            phase1_total_time_arrangement5)
  data
  
}

sales_training <- function(input,phase){sum(c(
  as.numeric(input[[paste("p",phase,"_sr1_sales_training",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr2_sales_training",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr3_sales_training",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr4_sales_training",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr5_sales_training",sep = "")]])),
  na.rm = T)}

field_work <- function(input,phase){sum(c(
  as.numeric(input[[paste("p",phase,"_sr1_field_work",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr2_field_work",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr3_field_work",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr4_field_work",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_sr5_field_work",sep = "")]])),
  na.rm = T)}

total_management <- function(input,phase){sum(c(
  sales_training(input,phase),
  field_work(input,phase),
  as.numeric(input[[paste("p",phase,"_flm_team_meeting",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_flm_kpi_analysis",sep = "")]]),
  as.numeric(input[[paste("p",phase,"_flm_admin_work",sep = "")]])),
  na.rm = T
)}

calculation <- function(pp_data1,
                        pp_data2,
                        cp_data1,
                        cp_data2){
  #
  #
  tmp1 <- left_join(cp_data1,pp_data1,by=c("hospital","product"))
  tmp2 <- left_join(cp_data2,pp_data2,by=c("sales_rep"))
  
  tmp <- left_join(tmp1,tmp2,by=c("phase","sales_rep")) %>%
    dplyr::mutate(product_price = sapply(product,function(x) product_info[which(product_info$类别==x),]$`单价（公司考核价）`),
           target_revenue= sales_target,
           target_volume = round(target_revenue/product_price)) %>%
    group_by(phase,sales_rep) %>%
    dplyr::mutate(other_time=work_time-(
      # sales_training+
             product_training+
             meetings_with_team),
           sr_time=sr_time_proportion*other_time,
           no.hospitals = n_distinct(hospital),
           sr_time_total=sum(sr_time,na.rm=T)) %>%
    ungroup %>%
    group_by(phase,hospital) %>%
    dplyr::mutate(sr_time_by_hosp=sum(sr_time,na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(product_time_proportion=round(sr_time/ifelse(sr_time_by_hosp==0,0.0001,sr_time_by_hosp),2),
                  promotional_budget = round(promotional_budget*product_time_proportion),
                  sr_acc_field_work = pp_sr_acc_field_work+field_work,
                  overhead_factor = sapply(pp_motivation_index,function(x) curve(curve12,x)),
                  overhead_time = round(overhead_factor*overhead,0),
                  real_sr_time = round(sr_time-overhead_time*sr_time_proportion,2),
                  pp_experience_index = sapply(pp_sr_acc_revenue,function(x) round(curve(curve11,x),2)),
                  field_work_peraccount = field_work/ifelse(no.hospitals==0,0.0001,no.hospitals),
                  product_knowledge_addition_current_period = sapply(product_training,function(x)curve(curve26,x)),
                  product_knowledge_transfer_value = sapply(pp_product_knowledge_index,function(x)curve(curve28,x)),
                  ss_accumulated_field_work_delta = sapply(sr_acc_field_work,function(x)curve(curve42,x)),
                  ss_accumulated_sales_training_delta = sapply(sales_training,function(x)curve(curve43,x)),
                  ss_experience_index_pp = sapply(pp_experience_index,function(x)curve(curve44,x)),
                  m_sales_training_delta = sapply(sales_training,function(x)curve(curve17,x)),
                  m_admin_work_delta = sapply(admin_work,function(x)curve(curve18,x)))%>%
    dplyr::mutate(sales_skills_index = round(
      (ss_accumulated_field_work_delta+pp_sales_skills_index)*((weightage$sales_skills)$field_work)+
        (ss_accumulated_sales_training_delta+pp_sales_skills_index)*((weightage$sales_skills)$sales_training)+
        (ss_experience_index_pp+pp_sales_skills_index)*((weightage$sales_skills)$experience)),
      product_knowledge_index = round(
        product_knowledge_addition_current_period+
          pp_product_knowledge_index)
          #product_knowledge_transfer_value),
    ) %>%
    dplyr::mutate(srsp_motivation_factor = sapply(pp_motivation_index,function(x)curve(curve32,x)),
           srsp_sales_skills_factor = sapply(sales_skills_index,function(x)curve(curve34,x)),
           srsp_product_knowledge_factor = sapply(product_knowledge_index,
                                                  function(x)curve(curve33,x)),
           srsp_time_with_account_factor = 
             mapply(function(x,y){if (x==as.character(product_info_list$product[1])){
               curve(curve35,y)} else if(
                 x==as.character(product_info_list$product[2])){
                 curve(curve36,y)} else if (
                   x==as.character(product_info_list$product[3])) {
                   curve(curve37,y)} else {
                     curve(curve38,y)}},
               product,real_sr_time)) %>%
    dplyr::mutate(sr_sales_performance = 
             srsp_motivation_factor*pp_sr_sales_performance*
             ((weightage$sr_sales_performance)$motivation)+
             srsp_sales_skills_factor*pp_sr_sales_performance*
             ((weightage$sr_sales_performance)$sales_skills)+
             srsp_product_knowledge_factor*pp_sr_sales_performance*
             ((weightage$sr_sales_performance)$product_knowledge)+
             srsp_time_with_account_factor*pp_sr_sales_performance*
             ((weightage$sr_sales_performance)$time_with_account))%>%
    dplyr::mutate(dq_admin_work_delta = sapply(admin_work,function(x)curve(curve5,x)),
           dq_meetings_with_team_delta =sapply(meetings_with_team,function(x)curve(curve7,x)),
           dq_kpi_analysis_factor = sapply(kpi_analysis,function(x)curve(curve8,x)))%>%
    dplyr::mutate(deployment_quality_index = round(
      (pp_deployment_quality_index+dq_admin_work_delta)*
        ((weightage$deployment_quality)$admin_work)+
        (pp_deployment_quality_index+dq_meetings_with_team_delta)*
        ((weightage$deployment_quality)$meetings_with_team)+
        pp_deployment_quality_index*dq_kpi_analysis_factor*
        ((weightage$deployment_quality)$kpi_report_analysis)))%>%
    dplyr::mutate(ps_promotional_budget_factor = sapply(promotional_budget,function(x)curve(curve30,x))) %>%
    dplyr::mutate(promotional_support_index = 
             pp_promotional_support_index*ps_promotional_budget_factor) %>%
    dplyr::mutate(sp_field_work_delta = sapply(field_work_peraccount,function(x)curve(curve40,x)),
           sp_deployment_quality_factor = sapply(deployment_quality_index,function(x)curve(curve41,x))) %>%
    dplyr::mutate(sales_performance = 
             sr_sales_performance*((weightage$sales_performance)$sr_sales_performance)+
             (pp_sales_performance+sp_field_work_delta)*
             ((weightage$sales_performance)$field_work)+
             (pp_sales_performance*sp_deployment_quality_factor)*
             ((weightage$sales_performance)$deployment_quality))%>%
    dplyr::mutate(cr_product_knowledge_delta = 
             sapply(product_knowledge_index,function(x)curve(curve2,x)),
           cr_promotional_support_delta = 
             sapply(ps_promotional_budget_factor,function(x)curve(curve3,x)),
           cr_pp_customer_relationship_index = 
             sapply(pp_customer_relationship_index,function(x)curve(curve4,x)))%>%
    dplyr::mutate(customer_relationship_index = 
      (cr_pp_customer_relationship_index+cr_product_knowledge_delta)*
        (weightage$customer_relaitonship)$product_knowledge+
        (cr_pp_customer_relationship_index+cr_promotional_support_delta)*
        (weightage$customer_relaitonship)$promotional_support) %>%
    dplyr::mutate(customer_relationship_index=sapply(customer_relationship_index,
                                                     function(x) ifelse(x<0,0,x))) %>%
      # +cr_pp_customer_relationship_index*
      #   (weightage$customer_relaitonship)$past_relationship) %>%
    dplyr::mutate(oa_customer_relationship_factor = 
             mapply(function(x,y){if (x==as.character(product_info_list$product[1])){
               curve(curve19,y)} else if(
                 x==as.character(product_info_list$product[2])){
                 curve(curve20,y)} else if (
                   x==as.character(product_info_list$product[3])) {
                   curve(curve21,y)} else {
                     curve(curve22,y)}},
               product,customer_relationship_index),
           oa_sales_performance_factor = sapply(sales_performance,function(x)curve(curve25,x))) %>%
    dplyr::mutate(cp_offer_attractiveness = 
             pp_offer_attractiveness*oa_customer_relationship_factor*
             (weightage$cp_offer_attractiveness)$customer_relationship+
             pp_offer_attractiveness*oa_sales_performance_factor*
             (weightage$cp_offer_attractiveness)$sales_performance) %>%
    dplyr::mutate(offer_attractiveness = 
                    cp_offer_attractiveness*(weightage$total_attractiveness)$cp_offer_attractiveness+
                            pp_offer_attractiveness*(weightage$total_attractiveness)$pp_offer_attractiveness,
           acc_offer_attractiveness = round(pp_acc_offer_attractiveness+offer_attractiveness),
           market_share =  mapply(function(x,y){if (x==as.character(product_info_list$product[1])){
             curve(curve51_1,y)} else if(
               x==as.character(product_info_list$product[2])){
               curve(curve51_2,y)} else if (
                 x==as.character(product_info_list$product[3])) {
                 curve(curve51_2,y)} else {
                   curve(curve51_3,y)}},
             product,offer_attractiveness),
           real_revenue = round(market_share/100*potential_revenue),
           real_volume = round(real_revenue/product_price)) %>%
    ungroup() %>%
    dplyr::group_by(phase,sales_rep) %>%
    dplyr::mutate(target_revenue_by_sr = sum(target_revenue,na.rm=T),
           real_revenue_by_sr = sum(real_revenue,na.rm=T),
           target_revenue_realization_by_sr = round(real_revenue_by_sr/target_revenue_by_sr*100,2),
           target_volume_by_sr = sum(target_volume,na.rm=T),
           real_volume_by_sr = sum(real_volume,na.rm=T),
           target_volume_realization_by_sr = round(real_volume_by_sr/target_volume_by_sr*100,2),
           #incentive_factor = sapply(target_revenue_realization_by_sr, function(x) curve(curve10,x)),
           bonus = #round(0.03*ifelse(target_revenue_realization_by_sr>=90 & target_revenue_realization_by_sr<=120,
                                     #target_revenue_realization_by_sr,
                                     #0)/100*real_revenue_by_sr),
             mapply(function(x,y) {if (x >= 90 & x <= 120){
               round(x/100*y*0.03)} else if(x >120) {
                 round(1.2*y*0.03)} else {0}},
               target_revenue_realization_by_sr,real_revenue_by_sr),
           sr_acc_revenue = real_revenue_by_sr+pp_sr_acc_revenue,
           experience_index = sapply(sr_acc_revenue, function(x) round(curve(curve11,x),2)),
           m_meeting_with_team_delta =  mapply(function(x,y){
             if (x == "junior") {
               curve(curve13,y)
             } else if(x=="middle"){
               curve(curve14,y)
             } else {curve(curve15,y)}
           },sales_level,
           meetings_with_team,SIMPLIFY=T),
           m_sales_target_realization_delta = sapply(target_revenue_realization_by_sr,function(x)curve(curve16,x)),
           motivation_index = round(
             (pp_motivation_index+m_admin_work_delta)*
               ((weightage$motivation)$admin_work)+
               (pp_motivation_index+m_sales_target_realization_delta)*
               ((weightage$motivation)$sales_target_realization)+
               (pp_motivation_index+m_meeting_with_team_delta)*
               ((weightage$motivation)$meetings_with_team)+
               (pp_motivation_index+m_sales_training_delta)*
               ((weightage$motivation)$sales_training))) %>%
    ungroup()
             
  
  tmp
}


get.data1 <- function(input,phase){
  data_decision <- data.frame(
    phase = NULL, 
    hospital = NULL,
    sales_rep = NULL,
    product = NULL,
    sales_target = NULL,
    potential_revenue = NULL,
    promotional_budget = NULL,
    sr_time_proportion = NULL,
    stringsAsFactors = F)
  
  for (j in 1:10) {
    for (q in 1:4){
      name.phase = as.character(paste("周期",phase,sep=""))
      name.hospital = as.character(unique(hospital_info$名称)[j])
      name.product = as.character(product_info$类别[q])
      name.sales_rep <- as.character(input[[paste("p",phase,"_sr_hosp",j,sep="")]])
      value.sales_target <- as.numeric(input[[paste("p",phase,"_hosp",j,"_sales_target_",q,sep="")]])
      value.promotional_budget <- as.numeric(input[[paste("p",phase,"_promotional_budget_hosp",j,sep="")]])/100*
        total_promotional_budget[which(total_promotional_budget$phase==paste("周期",phase,sep="")),]$budget
      value.sr_time_proportion <- as.numeric(input[[paste("p",phase,"_hosp",j,"_worktime_",q,sep="")]])/100
      
      
      data_decision <- plyr::rbind.fill(data_decision,data.frame(
        phase = name.phase,
        hospital = name.hospital,
        sales_rep = name.sales_rep, 
        product = name.product,
        sales_target = ifelse(is.na(value.sales_target),0,value.sales_target),
        potential_revenue = hospital_info[which(hospital_info$phase==name.phase&
                                                 hospital_info$名称==name.hospital&
                                                 hospital_info$产品==name.product),]$`潜力(元)`,
        promotional_budget = ifelse(is.na(value.promotional_budget),0,value.promotional_budget),
        sr_time_proportion = ifelse(is.na(value.sr_time_proportion),0,value.sr_time_proportion)
      ))
    }}
  data_decision
}


get.data2 <- function(input,phase){
  data_decision2 <- data.frame(
    phase = NULL,
    sales_rep = NULL,
    sales_training = NULL,
    product_training = NULL,
    field_work = NULL,
    meetings_with_team = NULL,
    kpi_analysis = NULL,
    admin_work = NULL,
    work_time = NULL,
    stringsAsFactors = F
  )
  
  for (j in 1:5) {
    name.sales_rep <- as.character(sr_info_list$sales_rep[j])
    value.sales_training <- as.numeric(
      input[[paste("p",phase,"_sr",j,"_sales_training",sep="")]])
    value.product_training <- as.numeric(
      input[[paste("p",phase,"_sr",j,"_product_training",sep="")]])
    value.field_work <- as.numeric(
      input[[paste("p",phase,"_sr",j,"_field_work",sep="")]])
    value.meetings_with_team <- as.numeric(
      input[[paste("p",phase,"_flm_team_meeting",sep="")]])
    value.kpi_analysis <- as.numeric(
      input[[paste("p",phase,"_flm_kpi_analysis",sep="")]])
    value.admin_work <- as.numeric(
      input[[paste("p",phase,"_flm_admin_work",sep="")]])
    
    data_decision2 <- plyr::rbind.fill(data_decision2,data.frame(
      phase = as.character(paste("周期",phase,sep="")),
      sales_rep = name.sales_rep,
      sales_training = ifelse(is.na(value.sales_training),0,value.sales_training),
      product_training = ifelse(is.na(value.product_training),0,value.product_training),
      field_work = ifelse(is.na(value.field_work),0,value.field_work),
      meetings_with_team = ifelse(is.na(value.meetings_with_team),0,value.meetings_with_team),
      kpi_analysis = ifelse(is.na(value.kpi_analysis),0,value.kpi_analysis),
      admin_work = ifelse(is.na(value.admin_work),0,value.admin_work),
      work_time = worktime
    ))
  }
  data_decision2
}

get.data3 <- function(input,phase){
  flm_decision <- data.frame(
    flm_sales_training = sales_training(input,phase),
    flm_field_work = field_work(input,phase),
    flm_meetings_with_team = ifelse(is.na(as.numeric(input[[paste("p",phase,"_flm_team_meeting",sep = "")]])),
                                    0,
                                    as.numeric(input[[paste("p",phase,"_flm_team_meeting",sep = "")]])),
    flm_kpi_analysis = ifelse(is.na(as.numeric(input[[paste("p",phase,"_flm_kpi_analysis",sep = "")]])),
                              0,
                              as.numeric(input[[paste("p",phase,"_flm_kpi_analysis",sep = "")]])),
    flm_admin_work = ifelse(is.na(as.numeric(input[[paste("p",phase,"_flm_admin_work",sep = "")]])),
                            0,
                            as.numeric(input[[paste("p",phase,"_flm_admin_work",sep = "")]])),
    stringsAsFactors = F)
  flm_decision
  
}

## staff report 1

report_data <- function(tmp,flm_data,null_report) {
  
  tmp <- tmp %>% 
    dplyr::mutate(sales_rep=factor(sales_rep,levels = c("小宋",
                                                        "小兰",
                                                        "小白",
                                                        "小木",
                                                        "小青")),
                  product = factor(product,levels=c("口服抗生素",
                                                    "一代降糖药",
                                                    "三代降糖药",
                                                    "皮肤药"))) 
  report1_mod1 <- tmp %>%
    group_by(sales_rep) %>%
    dplyr::mutate(visit_time=round(sum(real_sr_time,na.rm=T)),
                  total_sr_time=round(overhead_time+
                                        product_training+
                                        meetings_with_team+
                                        visit_time)) %>%
    ungroup() %>%
    select(overhead_time,
           product_training,
           meetings_with_team,
           visit_time,
           total_sr_time,
           sales_rep) %>%
    distinct()
  
  colnames(report1_mod1) <- c("日常事物(天)",
                              "产品培训(天)",
                              # "销售培训(天)",
                              "团队会议(天)",
                              "医院拜访(天)",
                              "总工作时间(天)",
                              "销售代表")
  
  report1_mod1 <- report1_mod1 %>%
    gather(variable,`值`,-`销售代表`) %>%
    spread(`销售代表`,`值`) 
  
  report1_rank1 <- data.frame(
    variable=c("日常事物(天)",
               "产品培训(天)",
               # "销售培训(天)",
               "团队会议(天)",
               "医院拜访(天)",
               "总工作时间(天)"),
    rank=1:5,
    stringsAsFactors = F
  )
  
  report1_mod1 <- report1_mod1 %>%
    left_join(report1_rank1,by="variable") %>%
    arrange(rank) 
  
  rownames(report1_mod1) <- report1_mod1$variable
  
  report1_mod1 <- report1_mod1 %>% select(-variable,-rank)
  
  
  
  
  report1_mod2 <- tmp %>%
    select(sales_rep,
           pp_product_knowledge_index,
           product_knowledge_index) %>%
    distinct()
  
  colnames(report1_mod2) <- c("销售代表",
                              "前期产品知识(指数)",
                              "当期产品知识(指数)")
  
  report1_mod2 <- report1_mod2 %>%
    gather(variable,`值`,-`销售代表`) %>%
    spread(`销售代表`,`值`)
  
  rownames(report1_mod2) <- report1_mod2$variable
  report1_mod2 <- report1_mod2 %>% select(-variable)
  
  report1_mod3 <- tmp %>%
    select(pp_experience_index,
           #real_revenue_by_sr,
           #sr_acc_revenue,
           experience_index,
           sales_rep) %>%
    distinct()
  colnames(report1_mod3) <- c("前期经验",
                              # "当期销售(元)",
                              # "累计总销售(元)",
                              "当期经验",
                              "销售代表")
  report1_mod3 <- report1_mod3 %>%
    gather(variable,`值`,-`销售代表`) %>%
    spread(`销售代表`,`值`)
  
  rownames(report1_mod3) <- report1_mod3$variable
  report1_mod3 <- report1_mod3 %>% select(-variable)
  
  
  
  report1_mod4 <- tmp%>%
    select(sales_rep,
           pp_sales_skills_index,
           sales_skills_index) %>%
    distinct()
  colnames(report1_mod4) <- c("销售代表",
                              "前期销售技巧(指数)",
                              "当期销售技巧(指数)")
  report1_mod4 <- report1_mod4 %>%
    gather(variable,`值`,-`销售代表`) %>%
    spread(`销售代表`,`值`)
  
  rownames(report1_mod4) <- report1_mod4$variable
  report1_mod4 <- report1_mod4 %>% select(-variable)
  
  report1_mod5 <- tmp %>%
    select(sales_rep,
           pp_motivation_index,
           motivation_index) %>%
    distinct()  
  colnames(report1_mod5) <- c("销售代表",
                              "前期动力值(指数)",
                              "当期动力值(指数)")
  report1_mod5 <- report1_mod5 %>%
    gather(variable,`值`,-`销售代表`) %>%
    spread(`销售代表`,`值`)
  
  rownames(report1_mod5) <- report1_mod5$variable
  report1_mod5 <- report1_mod5 %>% select(-variable)
  
  
  ## flm report
  flm_report <- tmp %>%
    select(sales_rep,
           bonus) %>%
    distinct() %>%
    dplyr::mutate(all_sr_bonus=sum(bonus,na.rm=T)) %>%
    select(all_sr_bonus) %>%
    distinct()
  
  flm_report <- flm_data %>%
    dplyr::mutate(all_sr_bonus = flm_report$all_sr_bonus,
                  work_time=flm_sales_training+
                    flm_field_work+
                    flm_meetings_with_team+
                    flm_kpi_analysis+
                    flm_admin_work)
  
  report2_mod1 <- flm_report%>%
    select(all_sr_bonus) %>%
    dplyr::mutate(variable="绩效奖金(元)")
  rownames(report2_mod1) <- report2_mod1$variable
  colnames(report2_mod1)[1] <- "值" 
  report2_mod1 <- report2_mod1 %>%
    select(-variable)
  
  report2_mod2 <- flm_report %>%
    select(-all_sr_bonus) 
  
  colnames(report2_mod2) <- c("能力辅导(天)",
                              "经理随访(天)",
                              "团队会议(天)",
                              "KPI分析(天)",
                              "行政工作(天)",
                              "总工作时间(天)")
  report2_mod2 <- report2_mod2 %>%
    gather(variable,`值`)
  
  rownames(report2_mod2) <- report2_mod2$variable
  report2_mod2 <- report2_mod2 %>% select(-variable)
  
  
  ## brief time allocation of hospital report
  report3_rank1 <- data.frame(
    "因素"=c("销售代表",
           "计划时间分配(天)",
           "实际时间分配(天)"),
    rank=1:3,
    stringsAsFactors =F
  )
  
  report3_mod1 <- tmp %>%
    select(hospital,
           product,
           sales_rep,
           sr_time,
           real_sr_time,
           hosp_code) %>%
    distinct() 
  
  
  colnames(report3_mod1) <- c("医院",
                              "产品",
                              "销售代表",
                              "计划时间分配(天)",
                              "实际时间分配(天)",
                              "hosp_code") 
  
  report3_mod1 <- report3_mod1 %>%
    gather(`因素`,value,`销售代表`,`计划时间分配(天)`,`实际时间分配(天)`) %>%
    spread(`产品`,value) %>%
    left_join(report3_rank1,by="因素") %>%
    arrange(hosp_code,rank) %>%
    select(-rank,-hosp_code)
  
  
  ## report d
  report4_mod1 <- tmp %>%
    select(hospital,
           product,
           real_revenue,
           real_volume) %>%
    group_by(product) %>%
    dplyr::mutate(real_revenue_by_product=round(sum(real_revenue,na.rm=T)),
                  real_volume_by_product=round(sum(real_volume,na.rm=T)),
                  production_cost = sapply(product,function(x) product_info[which(product_info$类别==x),]$单位成本),
                  production_fee = real_volume_by_product*production_cost,
                  profit = real_revenue_by_product - production_fee,
                  production_fee_percent = round(production_fee/real_revenue_by_product*100,2),
                  profit_percent = round(profit/real_revenue_by_product*100,2)) %>%
    ungroup() %>%
    select(-hospital,
           -production_cost,
           -real_revenue,
           -real_volume,
           -real_volume_by_product) %>%
    distinct() 
  
  
  colnames(report4_mod1) <- c("产品",
                              "销售金额(元)",
                              "生产成本(元)",
                              "利润贡献(元)",
                              "生产成本(%)",
                              "利润贡献(%)")
  report4_mod1 <- report4_mod1 %>%
    gather(`因素`,value,-`产品`) %>%
    spread(`产品`,value)
  
  report4_rank1 <- data.frame(
    "因素"= c("销售金额(元)",
            "生产成本(元)",
            "生产成本(%)",
            "利润贡献(元)",
            "利润贡献(%)"),
    rank=1:length(report4_mod1$因素),
    stringsAsFactors = F
  )
  
  report4_mod1 <- report4_mod1 %>%
    left_join(report4_rank1,by="因素") %>%
    distinct() %>%
    arrange(rank) %>%
    select(-rank)
  
  rownames(report4_mod1) <- report4_mod1$因素
  
  report4_mod1 <- report4_mod1 %>% select(-`因素`)  
  
  rownames(report4_mod1) <- rownames(report4_mod1)
  
  
  report4_mod2 <- tmp %>%
    select(real_revenue,
           real_volume,
           promotional_budget,
           product) %>%
    dplyr::mutate(production_cost = sapply(product,function(x)product_info[which(product_info$类别==x),]$单位成本),
                  production_fee = round(production_cost*real_volume),
                  total_revenue =round(sum(real_revenue,na.rm=T)),
                  total_production_fee =round(sum(production_fee,na.rm=T)),
                  total_promotional_budget = round(sum(promotional_budget,na.rm=T)),
                  total_salary=round(report2_mod1$值))  %>%
    select(total_revenue,
           total_production_fee,
           total_promotional_budget,
           total_salary) %>%
    distinct() %>%
    dplyr::mutate(profit=total_revenue-
                    total_production_fee-
                    total_promotional_budget-
                    total_salary)
  
  report4_rank2 <- data.frame(
    variable=c("销售额",
               "生产成本",
               "推广费用",
               "员工奖金",
               "利润贡献"),
    rank = 1:5,
    stringsAsFactors = F
  )
  
  
  
  report4_mod2_1 <- report4_mod2 
  colnames(report4_mod2_1) <- c("销售额",
                                "生产成本",
                                "推广费用",
                                "员工奖金",
                                "利润贡献")
  report4_mod2_1 <- report4_mod2_1 %>%
    gather(variable,"金额(元)") 
  
  
  
  
  report4_mod2_2 <- report4_mod2 %>%
    dplyr::mutate(total_revenue_percent = round(total_revenue/total_revenue*100,2),
                  total_production_fee_percent = round(total_production_fee/total_revenue*100,2),
                  total_promotional_budget_percent = round(total_promotional_budget/total_revenue*100,2),
                  total_salary_percent = round(total_salary/total_revenue*100,2),
                  profit_percent = round(profit/total_revenue*100,2)) %>%
    select(total_revenue_percent,
           total_production_fee_percent,
           total_promotional_budget_percent,
           total_salary_percent,
           profit_percent) 
  
  colnames(report4_mod2_2) <- c("销售额",
                                "生产成本",
                                "推广费用",
                                "员工奖金",
                                "利润贡献")
  
  report4_mod2 <- report4_mod2_2 %>%
    gather(variable,"占比(%)") %>%
    left_join(report4_mod2_1,by="variable") %>%
    left_join(report4_rank2,by="variable") %>%
    arrange(rank) %>%
    select(-rank)
  
  rownames(report4_mod2) <- report4_mod2$variable
  report4_mod2 <- report4_mod2 %>% 
    select(-variable) %>%
    select(`金额(元)`,`占比(%)`)
  
  ## report c
  report5_rank <- data.frame(
    "指标"=c("销售额(元)",
           "生产成本(元)",
           "生产成本(%)",
           "推广费用预算(元)",
           "推广费用预算(%)",
           "利润贡献(元)",
           '利润贡献(%)'),
    rank=1:7,
    stringsAsFactors = F)
  
  product_report_peraccount <- tmp %>%
    select(hospital,
           hosp_code,
           product,
           real_revenue,
           real_volume,
           promotional_budget) %>%
    group_by(hospital,product) %>%
    dplyr::mutate(no.product=n_distinct(product),
                  production_cost = sapply(product,function(x)product_info[which(product_info$类别==x),]$单位成本),
                  production_fee = round(production_cost*real_volume),
                  promotion_fee = round(promotional_budget/no.product),
                  profit = round(real_revenue-production_fee-promotion_fee)) %>%
    ungroup() %>%
    select(hospital,
           hosp_code,
           product,
           real_revenue,
           production_fee,
           promotion_fee,
           profit) %>%
    group_by(hospital) %>%
    do(plyr::rbind.fill(.,data.frame(hospital=first(.$hospital),
                                     hosp_code=first(.$hosp_code),
                                     product="总体",
                                     real_revenue = sum(.$real_revenue,na.rm=T),
                                     production_fee = sum(.$production_fee,na.rm=T),
                                     promotion_fee = sum(.$promotion_fee,na.rm=T),
                                     profit = sum(.$profit,na.rm=T)))) %>%
    ungroup() %>%
    dplyr::mutate(production_fee_percent = round(production_fee/real_revenue*100,2),
                  promotion_fee_percent = round(promotion_fee/real_revenue*100,2),
                  profit_percent = round(profit/real_revenue*100,2),
                  profit_percent = ifelse(is.nan(profit_percent),0,profit_percent))
  
  colnames(product_report_peraccount) <- c("医院",
                                           "hosp_code",
                                           "产品",
                                           "销售额(元)",
                                           "生产成本(元)",
                                           "推广费用预算(元)",
                                           "利润贡献(元)",
                                           "生产成本(%)",
                                           "推广费用预算(%)",
                                           '利润贡献(%)')
  
  report5_mod1 <- product_report_peraccount %>%
    gather(`指标`,value,-`医院`,-`产品`,-hosp_code) %>%
    spread(`产品`,value) %>%
    left_join(report5_rank,by="指标") %>%
    arrange(hosp_code,rank) %>%
    select(-rank,-hosp_code)
  
  
  ## report b
  report6_mod1 <- tmp %>%
    select(hospital,
           hosp_code,
           real_revenue,
           pp_real_revenue,
           target_revenue)%>%
    group_by(hospital) %>%
    dplyr::summarise(hosp_code = first(hosp_code),
                     real_revenue_by_hosp = round(sum(real_revenue,na.rm=T),2),
                     pp_real_revenue_by_hosp = round(sum(pp_real_revenue,na.rm=T),2),
                     real_revenue_increase = round(real_revenue_by_hosp - pp_real_revenue_by_hosp,2),
                     target_revenue_by_hosp = sum(target_revenue,na.rm=T)) %>%
    do(plyr::rbind.fill(.,data.frame(hospital="总体",
                                     hosp_code=11,
                                     real_revenue_by_hosp=sum(.$real_revenue_by_hosp,na.rm=T),
                                     pp_real_revenue_by_hosp=sum(.$pp_real_revenue_by_hosp,na.rm=T),
                                     real_revenue_increase=sum(.$real_revenue_increase,na.rm=T),
                                     target_revenue_by_hosp = sum(.$target_revenue_by_hosp,na.rm=T)))) %>%
    dplyr::mutate(real_revenue_increase_ratio = paste(round(real_revenue_increase/pp_real_revenue_by_hosp*100,2),"%",sep = ""),
                  target_revenue_realization_by_hosp = paste(round(real_revenue_by_hosp/target_revenue_by_hosp*100,2),"%",sep = "")) %>%
    arrange(hosp_code) %>%
    select(hospital,           
           target_revenue_by_hosp,
           pp_real_revenue_by_hosp,
           real_revenue_by_hosp,
           real_revenue_increase,
           real_revenue_increase_ratio,
           target_revenue_realization_by_hosp)
  
  
  
  colnames(report6_mod1) <- c("医院",
                              "当期销售指标",
                              "上期销售额",
                              "当期销售额",
                              "销售额增长",
                              "销售额增长率",
                              "销售额达成率")
  
  
  rownames(report6_mod1) <- report6_mod1$医院
  report6_mod1 <- report6_mod1 %>%
    select(-`医院`)
  
  report6_mod2_rank <- data.frame(
    sales_rep=c(sr_info_list$sales_rep,"总体"),
    rep_code=1:6
  )
  
  report6_mod2 <- tmp %>%
    select(sales_rep,
           real_revenue_by_sr,
           pp_real_revenue_by_sr,
           target_revenue_by_sr) %>%
    distinct() %>%
    do(plyr::rbind.fill(.,data.frame(sales_rep="总体",
                                     real_revenue_by_sr=sum(.$real_revenue_by_sr,na.rm=T),
                                     pp_real_revenue_by_sr =sum(.$pp_real_revenue_by_sr,na.rm=T),
                                     target_revenue_by_sr=sum(.$target_revenue_by_sr,na.rm=T)))) %>%
    dplyr::mutate(sr_target_revenue_realization = paste(round(real_revenue_by_sr/target_revenue_by_sr*100,2),"%",sep = "")) %>%
    select(sales_rep,
           target_revenue_by_sr,
           pp_real_revenue_by_sr,
           real_revenue_by_sr,
           sr_target_revenue_realization) %>%
    arrange(sales_rep)
  
  
  colnames(report6_mod2) <- c("销售代表",
                              "当期销售指标",
                              "上期销售额",
                              "当期销售额",
                              "销售额达成率")
  
  rownames(report6_mod2) <- report6_mod2$销售代表
  report6_mod2 <- report6_mod2 %>%
    select(-`销售代表`)
  
  
  report6_mod3 <- tmp %>%
    select(product,
           real_revenue,
           pp_real_revenue,
           target_revenue) %>%
    group_by(product) %>%
    dplyr::summarise(real_revenue_by_product = round(sum(real_revenue,na.rm=T)),
                     pp_real_revenue_by_product = round(sum(pp_real_revenue,na.rm=T)),
                     real_revenue_increase = round(real_revenue_by_product - pp_real_revenue_by_product),
                     target_revenue_by_product = round(sum(target_revenue,na.rm=T))) %>%
    do(plyr::rbind.fill(.,data.frame(product="总体",
                                     real_revenue_by_product=round(sum(.$real_revenue_by_product,na.rm=T)),
                                     pp_real_revenue_by_product=round(sum(.$pp_real_revenue_by_product,na.rm=T)),
                                     real_revenue_increase=sum(.$real_revenue_increase,na.rm=T),
                                     target_revenue_by_product=round(sum(.$target_revenue_by_product,na.rm=T))))) %>%
    dplyr::mutate(real_revenue_increase_ratio = ifelse(is.nan(round(real_revenue_increase/pp_real_revenue_by_product*100,2)),0,
                                                       paste(round(real_revenue_increase/pp_real_revenue_by_product*100,2),"%",sep = "")),
                  target_revenue_realization_by_product = ifelse(is.nan(round(real_revenue_by_product/target_revenue_by_product*100,2)),0,
                                                                 paste(round(real_revenue_by_product/target_revenue_by_product*100,2),"%",sep = ""))) %>%
    select(product,
           target_revenue_by_product,
           pp_real_revenue_by_product,
           real_revenue_by_product,
           real_revenue_increase,
           real_revenue_increase_ratio,
           target_revenue_realization_by_product)
  
  
  report6_mod3 <- report6_mod3 %>%
    left_join(product_info_list,by="product") %>%
    arrange(prod_code) %>%
    select(-prod_code)
  
  colnames(report6_mod3) <- c("产品",
                              "当期销售指标",
                              "上期销售额",
                              "当期销售额",
                              "销售额增长",
                              "销售额增长率",
                              "销售额达成率")
  
  rownames(report6_mod3) <- report6_mod3$产品
  report6_mod3 <- report6_mod3 %>%
    select(-`产品`)
  
  
  
  ## report a
  report7_mod1 <- tmp %>%
    select(phase,
           sales_rep,
           hospital,
           product_knowledge_index,
           sales_skills_index,
           customer_relationship_index,
           motivation_index,
           real_revenue,
           pp_acc_success_value) %>%
    distinct() %>%
    dplyr::mutate(total_revenue = round(sum(real_revenue,na.rm=T),2),
                  average_customer_relationship_index = round(mean(customer_relationship_index,na.rm=T),2),
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
    dplyr::mutate(profit=as.numeric(report4_mod2[5,1]),
                  inter1=(weightage$success_value)$total_sales*curve(curve50,total_revenue),
                  inter2=(weightage$success_value)$team_capability*curve(curve46,team_capability),
                  success_value = round(inter1+inter2),
                  acc_success_value = success_value + pp_acc_success_value) %>%
    dplyr::mutate(success_value = ifelse(phase=="周期0","",success_value),
                  acc_success_value = ifelse(phase=="周期0","",acc_success_value)) %>%
    select(phase,
           total_revenue,
           profit,
           team_capability,
           success_value,
           acc_success_value) %>%
    distinct()
  
  acc_success_value <- ifelse(is.na(as.numeric(report7_mod1$acc_success_value)),0,
                              as.numeric(report7_mod1$acc_success_value))
  
  colnames(report7_mod1) <- c("phase",
                              "总销售(元)",
                              "总利润(元)",
                              "团队能力(指数)",
                              "得分",
                              "累计得分") 
  report7_mod1_tmp <- null_report
  
  report7_mod1_tmp[which(report7_mod1_tmp$phase==report7_mod1$phase),2:6] <- report7_mod1[1,2:6]
  
  report7_mod1 <- report7_mod1_tmp
  
  
  report7_mod2 <- tmp %>%
    select(hospital,
           hosp_code,
           product,
           real_revenue,
           pp_real_revenue) %>%
    group_by(hospital) %>%
    dplyr::mutate(hospital_revenue = round(sum(real_revenue,na.rm=T)),
                  pp_hospital_revenue = round(sum(pp_real_revenue,na.rm=T))) %>%
    ungroup() %>%
    select(hospital,
           hosp_code,
           pp_hospital_revenue,
           hospital_revenue) %>%
    distinct() %>%
    arrange(hosp_code) %>%
    select(-hosp_code)
  
  colnames(report7_mod2) <- c("医院",
                              "上期总销售(元)",
                              "当期总销售(元)")
  rownames(report7_mod2) <- report7_mod2$医院
  report7_mod2 <- report7_mod2 %>% select(-`医院`)
  
  new_report1_mod1 <- report1_mod1
  new_report1_mod1[] <- lapply(new_report1_mod1,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report1_mod2 <- report1_mod2
  new_report1_mod2[] <- lapply(new_report1_mod2,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report1_mod3 <- report1_mod3
  new_report1_mod3[] <- lapply(new_report1_mod3,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report1_mod4 <- report1_mod4
  new_report1_mod4[] <- lapply(new_report1_mod4,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report1_mod5 <- report1_mod5
  new_report1_mod5[] <- lapply(new_report1_mod5,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report2_mod1 <- report2_mod1
  new_report2_mod1[] <- lapply(new_report2_mod1,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report2_mod2 <- report2_mod2
  new_report2_mod2[] <- lapply(new_report2_mod2,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report3_mod1 <- report3_mod1
  new_report3_mod1[] <- lapply(new_report3_mod1,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report4_mod1 <- report4_mod1
  new_report4_mod1[] <- lapply(new_report4_mod1,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report4_mod2 <- report4_mod2
  new_report4_mod2[] <- lapply(new_report4_mod2,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report5_mod1 <- report5_mod1
  new_report5_mod1[] <- lapply(new_report5_mod1,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report6_mod1 <- report6_mod1
  new_report6_mod1[] <- lapply(new_report6_mod1,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report6_mod2 <- report6_mod2
  new_report6_mod2[] <- lapply(new_report6_mod2,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report6_mod3 <- report6_mod3
  new_report6_mod3[] <- lapply(new_report6_mod3,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  new_report7_mod1 <- report7_mod1
  new_report7_mod1[] <- lapply(new_report7_mod1,function(x) 
    ifelse(is.nan(x)|x=="","",prettyNum(x,big.mark = ",")))
  
  new_report7_mod2 <- report7_mod2
  new_report7_mod2[] <- lapply(new_report7_mod2,function(x) 
    ifelse(is.nan(x),0,prettyNum(x,big.mark = ",")))
  
  
  
  
  
  
  
  out<-list("report1_mod1"=new_report1_mod1,
            "report1_mod2"=new_report1_mod2,
            "report1_mod3"=new_report1_mod3,
            "report1_mod4"=new_report1_mod4,
            "report1_mod5"=new_report1_mod5,
            "report2_mod1"=new_report2_mod1,
            "report2_mod2"=new_report2_mod2,
            "report3_mod1"=new_report3_mod1,
            "report4_mod1"=new_report4_mod1,
            "report4_mod2"=new_report4_mod2,
            "report5_mod1"=new_report5_mod1,
            "report6_mod1"=new_report6_mod1,
            "report6_mod2"=new_report6_mod2,
            "report6_mod3"=new_report6_mod3,
            "report7_mod1"=new_report7_mod1,
            "report7_mod2"=new_report7_mod2,
            "acc_success_value"=acc_success_value
            
  )
  
  out
  
}


# 报告入表
writeDown <- function(phase,report,report8){
  
  wb <- createWorkbook()
  
  ## 1
  addWorksheet(wb, rsd_sheet_names[1])
  writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, tibble(商业价值 = " "),
                 startRow = 1,rowNames = F,colNames = T)
  report7_1 <- cbind(` `= rownames(report8),report8)
  writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, report7_1,
                 startCol = 2,rowNames = F,colNames = T)
  writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, tibble(销售业绩 = " "),
                 startCol = 1,startRow = 8,rowNames = F,colNames = T)
  report7_2 <- cbind(` `= rownames(report$report7_mod2),report$report7_mod2)
  writeDataTable(wb, sheet = rsd_sheet_names[1],withFilter = F, report7_2,
                 startCol = 2,startRow = 8,rowNames = F,colNames = T)
  
  ## 2
  addWorksheet(wb, rsd_sheet_names[2])
  writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, tibble(时间分配 = " "),
                 startRow = 1,rowNames = F,colNames = T)
  report1_1 <- cbind(` `= rownames(report$report1_mod1),report$report1_mod1)
  writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, report1_1,
                 startCol = 2,rowNames = F,colNames = T)
  writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, tibble(各项指标 = " "),
                 startCol = 1,startRow = 8,rowNames = F,colNames = T)
  report1_2 <- rbind(report$report1_mod2,
                     report$report1_mod3,
                     report$report1_mod4,
                     report$report1_mod5)
  report1_2 <- cbind(` `= rownames(report1_2),report1_2)
  writeDataTable(wb, sheet = rsd_sheet_names[2],withFilter = F, report1_2,
                 startCol = 2,startRow = 8,rowNames = F,colNames = T)
  
  ## 3
  addWorksheet(wb, rsd_sheet_names[3])
  writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, tibble(职员成本 = " "),
                 startRow = 1,rowNames = F,colNames = T)
  report2_1 <- cbind(` `= rownames(report$report2_mod1),report$report2_mod1)
  writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, report2_1,
                 startCol = 2,rowNames = F,colNames = T)
  writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, tibble(时间分配 = " "),
                 startCol = 1,startRow = 4,rowNames = F,colNames = T)
  report2_2 <- cbind(` `= rownames(report$report2_mod2),report$report2_mod2)
  writeDataTable(wb, sheet = rsd_sheet_names[3],withFilter = F, report2_2,
                 startCol = 2,startRow = 4,rowNames = F,colNames = T)
  
  ## 4
  addWorksheet(wb, rsd_sheet_names[4])
  writeDataTable(wb, sheet = rsd_sheet_names[4],withFilter = F, tibble(时间分配 = " "),
                 startRow = 1,rowNames = F,colNames = T)
  report3_1 <- report$report3_mod1
  writeDataTable(wb, sheet = rsd_sheet_names[4],withFilter = F, report3_1,
                 startCol = 2,rowNames = F,colNames = T)
  
  ## 5
  addWorksheet(wb, rsd_sheet_names[5])
  writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, tibble("利润贡献 每产品(总)" = " "),
                 startRow = 1,rowNames = F,colNames = T)
  report4_1 <- cbind(` `= rownames(report$report4_mod1),report$report4_mod1)
  writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report4_1,
                 startCol = 2,rowNames = F,colNames = T)
  writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, tibble("利润贡献 (总体)" = " "),
                 startCol = 1,startRow = 8,rowNames = F,colNames = T)
  report4_2 <- cbind(` `= rownames(report$report4_mod2),report$report4_mod2)
  writeDataTable(wb, sheet = rsd_sheet_names[5],withFilter = F, report4_2,
                 startCol = 2,startRow = 8,rowNames = F,colNames = T)
  
  ## 6
  addWorksheet(wb, rsd_sheet_names[6])
  writeDataTable(wb, sheet = rsd_sheet_names[6],withFilter = F, tibble("利润贡献 每客户 " = " "),
                 startRow = 1,rowNames = F,colNames = T)
  report5_1 <- report$report5_mod1
  writeDataTable(wb, sheet = rsd_sheet_names[6],withFilter = F, report5_1,
                 startCol = 2,rowNames = F,colNames = T)
  
  ## 7
  addWorksheet(wb, rsd_sheet_names[7])
  writeDataTable(wb, sheet = rsd_sheet_names[7],withFilter = F, tibble("销售额和数量/客户" = " "),
                 startRow = 1,rowNames = F,colNames = T)
  report6_1 <- cbind(` `= rownames(report$report6_mod1),report$report6_mod1)
  writeDataTable(wb, sheet = rsd_sheet_names[7],withFilter = F, report6_1,
                 startCol = 2,rowNames = F,colNames = T)
  writeDataTable(wb, sheet = rsd_sheet_names[7],withFilter = F, tibble("销售额和数量/代表" = " "),
                 startCol = 1,startRow = 14,rowNames = F,colNames = T)
  report6_2 <- cbind(` `= rownames(report$report6_mod2),report$report6_mod2)
  writeDataTable(wb, sheet = rsd_sheet_names[7],withFilter = F, report6_2,
                 startCol = 2,startRow = 14,rowNames = F,colNames = T)
  writeDataTable(wb, sheet = rsd_sheet_names[7],withFilter = F, tibble("销售额和数量/产品" = " "),
                 startCol = 1,startRow = 22,rowNames = F,colNames = T)
  report6_3 <- cbind(` `= rownames(report$report6_mod3),report$report6_mod3)
  writeDataTable(wb, sheet = rsd_sheet_names[7],withFilter = F, report6_3,
                 startCol = 2,startRow = 22,rowNames = F,colNames = T)
  return(wb)}



