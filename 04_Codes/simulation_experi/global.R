## ------------------------functions-------------------
## -- get data functions
get.data1 <- function(input) {
  tmp1 <- input %>%
    left_join(hospital_info, by= c("phase",
                                   "hosp_code",
                                   "prod_code")) %>%  
    left_join(promotional_budget_list, by="phase") %>%
    dplyr::mutate(budget = budget/100*total_budget,
                  prod_hours = prod_hours)
  return(tmp1)
  
}

get.data2 <- function(input) {
  tmp2 <- data.frame(input,
                     work_time = worktime)
  tmp2 <- rbind(tmp2,
                rep(0,ncol(tmp2)))
  tmp2$phase <- input$phase[1]
  return(tmp2)
}

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

## -- find data function
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

## -- curve function
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


## -- calculation functions
calculation <- function(pp_data1,
                        pp_data2,
                        cp_data1,
                        cp_data2){
  #
  #
  tmp1 <- left_join(dplyr::select(cp_data1,-prod_name,-hosp_name),pp_data1,by=c("hosp_code","prod_code"))
  tmp2 <- left_join(cp_data2,pp_data2,by=c("salesmen"))
  
  tmp <- left_join(tmp1,tmp2,by=c("phase","salesmen")) %>%
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
                  prod_value = ifelse(prod_code == 4,target_factor*potential,pp_real_revenue*target_factor),
                  target_revenue= prod_value,
                  target_volume = round(target_revenue/product_price)) %>%
    group_by(phase,salesmen) %>%
    dplyr::mutate(sr_time=prod_hours,
                  no_hospitals = n_distinct(hosp_name),
                  sr_time_total=sum(sr_time,na.rm=T),
                  last_revenue_by_sr = sum(pp_real_revenue,na.rm=T),
                  overhead_proportion = sr_time/sr_time_total,
                  overhead_proportion = ifelse(is.nan(overhead_proportion),0,overhead_proportion)) %>%
    ungroup() %>%
    group_by(phase,hosp_name) %>%
    dplyr::mutate(sr_time_by_hosp=sum(sr_time,na.rm=T)) %>%
    ungroup() %>%
    dplyr::mutate(product_time_proportion=sr_time/ifelse(sr_time_by_hosp==0,0.0001,sr_time_by_hosp),
                  budget = round(budget*product_time_proportion),
                  promotional_factor = ifelse(target_revenue==0,0,round(budget/target_revenue*100,2)),
                  sr_acc_field_work = pp_sr_acc_field_work+field_work,
                  overhead_factor = sapply(pp_motivation_index,
                                           function(x) curve(find_sta("curve12",curves,"curves"),x)),
                  overhead_time = round(overhead_factor*overhead,0),
                  real_sr_time = round(sr_time-overhead_time*overhead_proportion,2),
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
                    prod_name,real_sr_time,hosp_name)) %>%
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
## -- results functions
run_for_results <- function(tmp,flm_data) {
    
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
    
    out <- data.frame(total_revenue = report1_mod1$total_revenue,
                      total_profit = report1_mod1$profit,
                      team_abty = report1_mod1$team_capability,
                      score = report1_mod1$success_value)
    out
}
