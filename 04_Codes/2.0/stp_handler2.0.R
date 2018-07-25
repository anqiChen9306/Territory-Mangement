# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  T***T 2.0
# Purpose:      Calculation
# programmer:   Anqi Chen
# Date:         17-07-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #



library(DT)
library(dplyr)
library(plyr)
library(tidyr)
library(digest)
library(openxlsx)
library(mongolite)
library(jsonlite)
library(utf8)
library(modules)

options(scipen=200,
        mongodb = list("host" = "xxx"))


##-- load parameters from ui
argss <- commandArgs(TRUE)
R_Json_Path <- argss[1]
# R_Json_Path <- "65ccdece-cf90-4186-aeea-b14fee19a291"

# import("calculation", attach = T)
curve_cal <- function(input_curves_data,
                      input_curve_name,
                      input_x){
  # input_curves_data <- curves
  # input_curve_name <- "curve17"
  # input_x <- 6
  
  position <- which(input_curves_data$curve_name == input_curve_name)
  data <-input_curves_data$curve_data[[position]]
  
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

weight_cal <- function(input_weightage_data,
                       input_object,
                       origin_object) {
  
  # input_object <- "sales_skills"
  # origin_object <- "field_work"
  
  position <- which(input_weightage_data$objective_index == input_object)
  out <- input_weightage_data$weight[[position]][[origin_object]]
  out
}

update_rep_except_moti <- function(rep_index_info,
                                   training_info = NULL,
                                   curves_data,
                                   weightages_data) {
  
  ## rep_index_info denotes indexs from last phase
  ## training denotes training arranges in current phase
  # rep_index_info <- last_rep_part
  
  colnames(rep_index_info)[!colnames(rep_index_info)%in%c("phase", "rep_id", "rep_level")] <-
    paste0("pp_", colnames(rep_index_info)[!colnames(rep_index_info)%in%c("phase", "rep_id", "rep_level")])
  
  if (is.null(training_info)) {
    new_rep_index_info <- data.frame(rep_index_info,
                                     sales_train = 0,
                                     field_work = 0,
                                     prod_train = 0,
                                     team_meet = 0,
                                     admin_work = 0)
  } else {
    new_rep_index_info <- rep_index_info %>%
      left_join(training_info, by = "rep_id")
  }
  
  new_rep_index_info_m <- new_rep_index_info %>%
    mutate(acc_field_work = pp_acc_field_work + field_work,
           acc_sales_train = pp_acc_sales_train + sales_train,
           acc_prod_train = pp_acc_prod_train + prod_train,
           pp_exper = sapply(pp_acc_sales, function(x) {round(curve_cal(curves_data,
                                                                        "curve11",
                                                                        x))}),
           pp_prod_train_delta = sapply(prod_train, function(x) {round(curve_cal(curves_data,
                                                                                 "curve26",
                                                                                 x))}),
           prod_knowledge_val =  pp_prod_knowledge_val + pp_prod_train_delta,
           ss_acc_field_work_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                            "curve42",
                                                                            x)+y) *weight_cal(weightages_data,
                                                                                              "sales_skills",
                                                                                              "field_work"))},
                                           acc_field_work, 
                                           pp_sales_skills_val),
           ss_sales_train_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                         "curve43",
                                                                         x)+y) *weight_cal(weightages_data,
                                                                                           "sales_skills",
                                                                                           "sales_training"))},
                                        acc_sales_train, 
                                        pp_sales_skills_val),
           ss_experience_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                        "curve44",
                                                                        x)+y) *weight_cal(weightages_data,
                                                                                          "sales_skills",
                                                                                          "experience"))},
                                       pp_exper, 
                                       pp_sales_skills_val),
           sales_skills_val = ss_acc_field_work_part+
             ss_sales_train_part+
             ss_experience_part,
           mo_team_meet_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                       "curve17",
                                                                       x)+y) *weight_cal(weightages_data,
                                                                                         "motivation",
                                                                                         "meetings_with_team"))},
                                      team_meet, 
                                      pp_motivation_val),
           mo_admin_work_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                        "curve18",
                                                                        x)+y) *weight_cal(weightages_data,
                                                                                          "motivation",
                                                                                          "admin_work"))},
                                       admin_work, 
                                       pp_motivation_val),  
           mo_sales_train_part = mapply(function(x, y, z) {round((curve_cal(curves_data,
                                                                            switch(z,
                                                                                   "freshman" = "curve13",
                                                                                   "junior" = "curve14",
                                                                                   "senior" = "curve15"),
                                                                            x)+y) *weight_cal(weightages_data,
                                                                                              "motivation",
                                                                                              "sales_training"))},
                                        sales_train, 
                                        pp_motivation_val,
                                        rep_level)) %>% 
    select(rep_id,
           rep_level,
           prod_knowledge_val,
           sales_skills_val,
           pp_motivation_val,
           pp_acc_sales,
           acc_sales_train,
           acc_field_work,
           acc_prod_train,
           mo_team_meet_part,
           mo_admin_work_part,
           mo_sales_train_part)
  
  return(new_rep_index_info_m) }

update_rep_sales_perfor <- function(input_arrange,
                                    input_rep_info,
                                    input_dest_info,
                                    curves_data,
                                    weightages_data) {
  
  # input_arrange <- input_business_deci_m1
  # input_rep_info <- new_rep_status_except_moti
  # input_dest_info <- last_dest_part
  
  colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")])
  
  out <- input_arrange %>%
    left_join(input_rep_info, by = "rep_id") %>%
    left_join(select(input_dest_info,
                     -pp_motivation_val), by = c("dest_id", "goods_id")) %>%
    mutate(rsp_time_part = mapply(function(x, y, z) {round((curve_cal(curves_data,
                                                                      switch(z,
                                                                             "first_class" = "curve38",
                                                                             "second_class" = "curve39",
                                                                             "third_class" = "curve37"),
                                                                      x)+y) *weight_cal(weightages_data,
                                                                                        "sr_sales_performance",
                                                                                        "time_with_account"))},
                                  user_input_day, 
                                  pp_rep_sales_perfor_val,
                                  target_level),
           rsp_motivation_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                         "curve32",
                                                                         x)+y) *weight_cal(weightages_data,
                                                                                           "sr_sales_performance",
                                                                                           "motivation"))},
                                        pp_motivation_val, 
                                        pp_rep_sales_perfor_val),
           rsp_sales_skills_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                           "curve34",
                                                                           x)+y) *weight_cal(weightages_data,
                                                                                             "sr_sales_performance",
                                                                                             "sales_skills"))},
                                          sales_skills_val, 
                                          pp_rep_sales_perfor_val),
           rsp_prod_knowledge_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                             "curve33",
                                                                             x)+y) *weight_cal(weightages_data,
                                                                                               "sr_sales_performance",
                                                                                               "product_knowledge"))},
                                            prod_knowledge_val, 
                                            pp_rep_sales_perfor_val),
           rep_sales_perfor_val = rsp_time_part +
             rsp_motivation_part +
             rsp_sales_skills_part +
             rsp_prod_knowledge_part) %>%
    select(dest_id,
           goods_id,
           rep_id,
           rep_sales_perfor_val,
           sales_skills_val,
           prod_knowledge_val)
  
  return(out)
}

update_deploy_quality <- function(input_arrange,
                                  input_dest_info,
                                  curves_data,
                                  weightages_data) {
  
  # input_arrange <- input_business_deci_m1
  # input_dest_info <- last_dest_part
  
  colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")])
  
  out <- input_arrange %>%
    left_join(input_dest_info, by = c("dest_id", "goods_id")) %>%
    mutate(dq_admin_work_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                        "curve5",
                                                                        x)+y) *weight_cal(weightages_data,
                                                                                          "deployment_quality",
                                                                                          "admin_work"))},
                                       admin_work, 
                                       pp_deploy_quality_val),
           dq_team_meet_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                       "curve7",
                                                                       x)+y) *weight_cal(weightages_data,
                                                                                         "deployment_quality",
                                                                                         "meetings_with_team"))},
                                      team_meet, 
                                      pp_deploy_quality_val),
           dq_kpi_analysis_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                          "curve8",
                                                                          x)+y) *weight_cal(weightages_data,
                                                                                            "deployment_quality",
                                                                                            "kpi_report_analysis"))},
                                         kpi_analysis, 
                                         pp_deploy_quality_val),
           deploy_quality_val = dq_admin_work_part +
             dq_team_meet_part +
             dq_kpi_analysis_part) %>%
    select(dest_id,
           goods_id,
           rep_id,
           deploy_quality_val,
           field_work,
           kpi_analysis,
           team_meet,
           admin_work)
  
  return(out)
}

update_sales_perfor <- function(input_data, 
                                input_dest_info,
                                curves_data,
                                weightages_data) {
  
  # input_data <- prepared_sales_perfor_data
  # input_dest_info <- last_dest_part
  
  colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")])
  
  out <- input_dest_info %>%
    select(dest_id,
           goods_id,
           pp_sales_perfor_val) %>%
    left_join(input_data, by = c("dest_id",
                                 "goods_id")) %>%
    mutate(sp_field_work_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                        "curve40",
                                                                        x)+y) *weight_cal(weightages_data,
                                                                                          "sales_performance",
                                                                                          "field_work"))},
                                       field_work, 
                                       pp_sales_perfor_val),
           sp_rep_sales_perfor_part = mapply(function(x, y) {round((x+y) *weight_cal(weightages_data,
                                                                                     "sales_performance",
                                                                                     "sr_sales_performance"))},
                                             
                                             rep_sales_perfor_val, 
                                             pp_sales_perfor_val),
           sp_deploy_quality_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                            "curve41",
                                                                            x)+y) *weight_cal(weightages_data,
                                                                                              "sales_performance",
                                                                                              "deployment_quality"))},
                                           deploy_quality_val, 
                                           pp_sales_perfor_val),
           sales_perfor_val = sp_field_work_part +
             sp_rep_sales_perfor_part +
             sp_deploy_quality_part) %>%
    select(dest_id,
           goods_id,
           rep_id,
           sales_perfor_val,
           rep_sales_perfor_val,
           deploy_quality_val) 
  
  return(out)
}

update_custom_relation <- function(input_data, 
                                   input_dest_info,
                                   curves_data,
                                   weightages_data) {
  
  # input_data <- prepared_custom_relation_data
  # input_dest_info <- last_dest_part
  
  colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")])
  
  out <- input_dest_info %>%
    select(dest_id,
           goods_id,
           pp_promo_support_val,
           pp_custom_relation_val,
           pp_sales) %>%
    left_join(input_data, by = c("dest_id",
                                 "goods_id")) %>%
    mutate(budget_factor = ifelse(target==0,
                                  0,
                                  round(budget/pp_sales*100)),
           promo_support_val = mapply(function(x, y) {round(curve_cal(curves_data,
                                                                      "curve30",
                                                                      x)*y)},
                                      budget_factor, 
                                      pp_promo_support_val),
           pp_custom_relation_transfer = sapply(pp_custom_relation_val, function(x) {round(curve_cal(curves_data,
                                                                                                     "curve4",
                                                                                                     x))}),
           cr_promo_support_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                           "curve3",
                                                                           x)+y) *weight_cal(weightages_data,
                                                                                             "customer_relaitonship",
                                                                                             "promotional_support"))},
                                          promo_support_val, 
                                          pp_custom_relation_transfer),
           cr_prod_knowledge_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                            "curve2",
                                                                            x)+y) *weight_cal(weightages_data,
                                                                                              "customer_relaitonship",
                                                                                              "product_knowledge"))},
                                           prod_knowledge_val, 
                                           pp_custom_relation_transfer),
           custom_relation_val = cr_promo_support_part +
             cr_prod_knowledge_part) %>%
    select(dest_id,
           goods_id,
           rep_id,
           promo_support_val,
           custom_relation_val)
  
  return(out)
}

update_offer_attrac <- function(input_data,
                                input_dest_info,
                                curves_data,
                                weightages_data) {
  
  # input_data <- prepared_offer_attrac_data
  # input_dest_info <- last_dest_part
  
  colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(input_dest_info)[!colnames(input_dest_info)%in%c("phase", "dest_id", "goods_id")])
  
  out <- input_dest_info %>%
    select(dest_id,
           goods_id,
           pp_offer_attrac_val) %>%
    left_join(input_data, by = c("dest_id",
                                 "goods_id")) %>%
    mutate(oa_sales_perfor_part = sapply(sales_perfor_val, function(x) {round(curve_cal(curves_data,
                                                                                        "curve2",
                                                                                        x)*100*weight_cal(weightages_data,
                                                                                                          "cp_offer_attractiveness",
                                                                                                          "sales_performance"))}),
           
           oa_custom_relation_part = sapply(custom_relation_val, function(x) {round(curve_cal(curves_data,
                                                                                              "curve19",
                                                                                              x)*100*weight_cal(weightages_data,
                                                                                                                "cp_offer_attractiveness",
                                                                                                                "customer_relationship"))}),
           cp_offer_attrac_val = oa_sales_perfor_part +
             oa_custom_relation_part,
           offer_attrac_val = mapply(function(x, y) {round(x*weight_cal(weightages_data,
                                                                        "total_attractiveness",
                                                                        "pp_offer_attractiveness")+
                                                             y*weight_cal(weightages_data,
                                                                          "total_attractiveness",
                                                                          "cp_offer_attractiveness"))},
                                     pp_offer_attrac_val,
                                     cp_offer_attrac_val),
           share = mapply(function(x, y) {curve_cal(curves_data,
                                                    switch(y,
                                                           "launch" = "curve54",
                                                           "growth" = "curve53",
                                                           "maturity" = "curve51",
                                                           "decline" = "curve"),
                                                    x)},
                          offer_attrac_val,
                          life_cycle),
           sales = round(potential*share/100)) %>%
    select(dest_id,
           goods_id,
           rep_id,
           offer_attrac_val,
           sales_perfor_val,
           custom_relation_val,
           rep_sales_perfor_val,
           deploy_quality_val,
           promo_support_val,
           sales,
           potential,
           target)
  
  return(out)
}

update_rep_info <- function(rep_index_info,
                            sales_gain,
                            curves_data,
                            weightages_data) {
  
  # rep_index_info <- new_rep_status_except_moti
  # sales_gain <- new_offer_attract
  # curves_data <- curves
  # weightages_data <- weightages
  
  sales_gain_m <- sales_gain %>%
    filter(!is.na(rep_id)) %>%
    group_by(rep_id) %>%
    dplyr::summarise(target = sum(target, na.rm = T),
                     sales = sum(sales, na.rm = T)) %>%
    mutate(achieve_rate = sales/target,
           bonus = mapply(function(x, y) {ifelse(x>=0.9&x<=1.2,x*y*0.03,0)},
                          achieve_rate,
                          sales))
  
  out <- rep_index_info %>%
    left_join(sales_gain_m, by = c("rep_id")) %>%
    mutate(mo_achieve_rate_part = mapply(function(x, y) {round((curve_cal(curves_data,
                                                                          "curve16",
                                                                          x*100)+y) *weight_cal(weightages_data,
                                                                                                "motivation",
                                                                                                "sales_target_realization"))},
                                         achieve_rate, 
                                         pp_motivation_val),
           motivation_val = mo_achieve_rate_part +
             mo_admin_work_part +
             mo_sales_train_part +
             mo_team_meet_part,
           acc_sales = pp_acc_sales +sales) %>%
    select(rep_id,
           rep_level,
           prod_knowledge_val,
           sales_skills_val,
           motivation_val,
           target,
           sales,
           bonus,
           acc_sales,
           acc_sales_train,
           acc_field_work,
           acc_prod_train)
  
  return(out)
}

produce_summary_report <- function(current_dest_gain,
                                   last_dest_gain,
                                   goods_info,
                                   names_box) {
  
  # current_dest_gain <- new_dest_part
  # last_dest_gain <- last_dest_part
  # goods_info <- goods_info
  # names_box <- names_box
  
  colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")])
  
  report <- current_dest_gain %>%
    left_join(last_dest_gain, by = c("dest_id",
                                     "goods_id")) %>%
    left_join(goods_info, by = c("goods_id" = "_id")) %>%
    group_by(goods_id, prod_name) %>%
    dplyr::summarise(sales = sum(sales, na.rm = T),
                     pp_sales = sum(pp_sales, na.rm = T),
                     potential = sum(potential, na.rm = T),
                     pp_potential = sum(pp_potential, na.rm = T),
                     target = sum(target, na.rm = T)) %>%
    mutate(share = sales/potential,
           pp_share = pp_sales/pp_potential,
           share_change = share - pp_share,
           market_growth = potential/pp_potential,
           sales_growth = ifelse(pp_sales==0,
                                 0,
                                 sales/pp_sales),
           ev_value = sales_growth/market_growth,
           achieve_rate = sales/target,
           contri_rate = sales/sum(sales, na.rm = T),
           sales_growth = sales_growth-1,
           market_growth = market_growth-1)
  
  
  report_m <- report %>%
    select(goods_id,
           prod_name,
           potential,
           market_growth,
           sales,
           sales_growth,
           ev_value,
           share,
           share_change,
           target,
           achieve_rate,
           contri_rate)
  
  total_sales = sum(report$sales, na.rm = T)
  total_sales_uplift_ratio = sum(report$sales, na.rm = T)/sum(report$pp_sales, na.rm = T)-1
  average_achieve_rate = mean(report$achieve_rate)
  
  overview <- list(list(index = as.integer(0),
                        eng_title = "total_sales"),
                   list(index = as.integer(1),
                        eng_title = "total_sales_uplift_ratio",
                        ext = list(change = ifelse(total_sales_uplift_ratio>=0,
                                                   "up",
                                                   "down"))),
                   list(index = as.integer(2),
                        eng_title = "average_achieve_rate",
                        ext = list(change = ("none"))))
  
  overview_m <- lapply(overview, function(x) {
    x[["title"]] = subset(names_box, eng_title == x[["eng_title"]])$title
    x[["type"]] = subset(names_box, eng_title == x[["eng_title"]])$type
    x[["value"]] = get(x[["eng_title"]])
    x[["eng_title"]] <- NULL
    x
  })
  
  list(overview = overview_m,
       value = report_m) }

produce_dests_goods_report <- function(current_dest_gain,
                                       last_dest_gain,
                                       dests_info,
                                       goods_info,
                                       names_box){
  
  # current_dest_gain <- new_dest_part
  # last_dest_gain <- last_dest_part
  # dests_info <- dests_info
  # goods_info <- goods_info
  # names_box <- names_box
  
  colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")])
  
  report <- current_dest_gain %>%
    left_join(last_dest_gain, by = c("dest_id",
                                     "goods_id")) %>%
    left_join(dests_info, by =c("dest_id" = "_id")) %>%
    left_join(goods_info, by = c("goods_id" = "_id")) %>%
    mutate(market_growth = potential/pp_potential,
           sales_growth = ifelse(pp_sales==0,
                                 0,sales/pp_sales),
           ev_value = sales_growth/market_growth,
           share = sales/potential,
           pp_share = pp_sales/pp_potential,
           share_change = share -pp_share,
           achieve_rate = sales/target,
           contri_rate = sales/sum(sales, na.rm = T),
           sales_growth = sales_growth-1,
           market_growth = market_growth-1)
  
  report_m <- report %>%
    select(dest_id,
           hosp_name,
           goods_id,
           prod_name,
           potential,
           market_growth,
           sales,
           sales_growth,
           ev_value,
           share,
           share_change,
           target,
           achieve_rate,
           contri_rate)
  
  best_perform_object <- report[which.max(report$share),]
  best_perform_value <- paste(best_perform_object$hosp_name,
                              best_perform_object$prod_name,
                              sep = "-")
  best_perform_sub_value <- best_perform_object$share
  best_perform_sub_sub_value <- best_perform_object$share_change
  
  worst_perform_object <- report[which.min(report$share),]
  worst_perform_value <- paste(worst_perform_object$hosp_name,
                               worst_perform_object$prod_name,
                               sep = "-")
  worst_perform_sub_value <- worst_perform_object$share
  worst_perform_sub_sub_value <- worst_perform_object$share_change
  
  best_climb_object <- report[which.max(report$sales_growth),]
  best_climb_value <- paste(best_climb_object$hosp_name, 
                            best_climb_object$prod_name,
                            sep = "-")
  best_climb_sub_value <- best_climb_object$sales
  best_climb_sub_sub_value <- best_climb_object$sales_growth
  
  best_contri_object <- report[which.max(report$contri_rate),]
  best_contri_value <- paste(best_contri_object$hosp_name,
                             best_contri_object$prod_name,
                             sep = "-")
  best_contri_sub_value = best_contri_object$sales
  best_contri_sub_sub_value = best_contri_object$contri_rate
  
  average_achieve_rate <- mean(report$achieve_rate)
  
  overview <- list(list(index = as.integer(0),
                        eng_title = "best_perform",
                        value = best_perform_value,
                        ext = list(sub = list(value = best_perform_sub_value,
                                              eng_title = "share",
                                              ext = list(type = "percent",
                                                         change = ifelse(best_perform_sub_sub_value >= 0,
                                                                         "up",
                                                                         "down"),
                                                         value = best_perform_sub_sub_value)))),
                   list(index = as.integer(1),
                        eng_title = "worst_perform",
                        value = worst_perform_value,
                        ext = list(sub = list(value = worst_perform_sub_value,
                                              eng_title = "share",
                                              ext = list(type = "percent",
                                                         change = ifelse(worst_perform_sub_sub_value >= 0,
                                                                         "up",
                                                                         "down"),
                                                         value = worst_perform_sub_sub_value)))),
                   list(index = as.integer(2),
                        eng_title = "best_climb",
                        value = best_climb_value,
                        ext =list(sub = list(value = best_climb_sub_value,
                                             type = "title",
                                             ext = list(type = "percent",
                                                        change = ifelse(best_climb_sub_sub_value >= 0,
                                                                        "up",
                                                                        "down"),
                                                        value = best_climb_sub_sub_value)))),
                   list(index = as.integer(3),
                        eng_title = "best_contri",
                        value = best_contri_value,
                        ext =list(sub = list(value = best_contri_sub_value,
                                             type = "title",
                                             ext = list(type = "percent",
                                                        change = "none",
                                                        value = best_contri_sub_sub_value)))),
                   list(index = as.integer(4),
                        eng_title = "average_achieve_rate",
                        value = average_achieve_rate,
                        ext =list(change = "none")))
  
  
  
  overview_m <- lapply(overview, function(x) {
    x[["title"]] = subset(names_box, eng_title == x[["eng_title"]])$title
    x[["type"]] = subset(names_box, eng_title == x[["eng_title"]])$type
    if (!is.null(x[["ext"]][["sub"]])) {
      if (!is.null(x[["ext"]][["sub"]][["eng_title"]])) {
        x[["ext"]][["sub"]][["title"]] = subset(names_box, eng_title == x[["ext"]][["sub"]][["eng_title"]])$title
        x[["ext"]][["sub"]][["eng_title"]] <- NULL
      }
    }
    x[["eng_title"]] <- NULL
    x
  })
  
  list(overview = overview_m,
       value = report_m)
}

produce_rep_goods_report <- function(current_dest_gain,
                                     last_dest_gain,
                                     rep_info,
                                     goods_info,
                                     names_box){
  
  # current_dest_gain <- new_dest_part
  # last_dest_gain <- last_dest_part
  # rep_info <- rep_info
  # goods_info <- goods_info
  # names_box <- names_box
  
  colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")])
  
  report <- current_dest_gain %>%
    left_join(last_dest_gain, by = c("dest_id",
                                     "goods_id")) %>%
    left_join(rep_info, by =c("rep_id" = "_id")) %>%
    left_join(goods_info, by = c("goods_id" = "_id")) %>%
    group_by(rep_id, 
             rep_name,
             goods_id,
             prod_name) %>%
    dplyr::summarise(sales = sum(sales, na.rm = T),
                     pp_sales = sum(pp_sales, na.rm = T),
                     target = sum(target, na.rm = T)) %>%
    mutate(sales_growth = ifelse(pp_sales == 0,
                                 0,
                                 sales/pp_sales),
           achieve_rate = sales/target,
           contri_rate = sales/sum(sales, na.rm = T)) %>%
    select(-pp_sales)
  
  best_climb_object <- report[which.max(report$sales_growth),]
  best_climb_value <- paste(best_climb_object$rep_name,
                            best_climb_object$prod_name,
                            sep = "-")
  best_climb_sub_value <- best_climb_object$sales
  best_climb_sub_sub_value <- best_climb_object$sales_growth
  
  best_contri_object <- report[which.max(report$contri_rate),]
  best_contri_value <- paste(best_contri_object$rep_name,
                             best_contri_object$prod_name,
                             sep = "-")
  best_contri_sub_value <- best_contri_object$sales
  best_contri_sub_sub_value <- best_contri_object$contri_rate
  
  rep_average_achieve_rate <- mean(report$achieve_rate)
  
  overview <- list(list(index = as.integer(0),
                        eng_title = "best_climb",
                        value = best_climb_value,
                        ext = list(sub = list(type = "title",
                                              value = best_climb_sub_value,
                                              ext = list(type = "percent",
                                                         change = ifelse(best_climb_sub_sub_value >= 0,
                                                                         "up",
                                                                         "down"),
                                                         value = best_climb_sub_sub_value)))),
                   list(index = as.integer(1),
                        eng_title = "best_contri",
                        value = best_contri_value,
                        ext = list(sub = list(type = "title",
                                              value = best_contri_sub_value,
                                              ext = list(type = "percent",
                                                         change = "none",
                                                         value = best_contri_sub_sub_value)))),
                   list(index = as.integer(2),
                        eng_title = "average_rep_achieve_rate",
                        value = rep_average_achieve_rate,
                        ext = list(change = "none")))
  
  overview_m <- lapply(overview, function(x) {
    
    x[["title"]] = subset(names_box, eng_title == x[["eng_title"]])$title
    x[["type"]] = subset(names_box, eng_title == x[["eng_title"]])$type
    x[["eng_title"]] <- NULL
    x
  })
  
  list(overview = overview_m,
       value = report)
}


produce_reso_allocation_report <- function(current_dest_gain,
                                           last_dest_gain,
                                           input_data,
                                           dests_info,
                                           goods_info,
                                           rep_info,
                                           names_box){
  
  # current_dest_gain <- new_dest_part
  # last_dest_gain <- last_dest_part
  # input_data <- input_business_deci_m1
  # dests_info <- dests_info
  # goods_info <- goods_info
  # rep_info <- rep_info
  # names_box <- names_box
  
  colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")] <-
    paste0("pp_", colnames(last_dest_gain)[!colnames(last_dest_gain)%in%c("phase", "dest_id", "goods_id")])
  
  report <- current_dest_gain %>%
    left_join(last_dest_gain, by = c("dest_id",
                                     "goods_id")) %>%
    left_join(select(input_data,
                     -target,
                     -potential), by = c("dest_id",
                                         "goods_id",
                                         "rep_id")) %>%
    left_join(dests_info, by =c("dest_id" = "_id")) %>%
    left_join(goods_info, by = c("goods_id" = "_id")) %>%
    left_join(rep_info, by = c("rep_id" = "_id")) %>%
    mutate(time = user_input_day/workdays,
           market_growth = potential/pp_potential,
           sales_growth = ifelse(pp_sales == 0,
                                 0,
                                 sales/pp_sales),
           share = sales/potential,
           pp_share = pp_sales/pp_potential,
           share_change = share - pp_share,
           contri_rate = sales/sum(sales, na.rm = T))
  
  report_m <- report %>%
    select(dest_id,
           hosp_name,
           goods_id,
           prod_name,
           rep_id,
           rep_name,
           time,
           target,
           potential,
           market_growth,
           sales,
           sales_growth,
           share,
           share_change,
           contri_rate)
  
  most_budget_input_object <- report[which.max(report$budget),]
  most_budget_input_value <- paste(most_budget_input_object$hosp_name,
                                   most_budget_input_object$prod_name,
                                   sep = "-")
  most_budget_input_sub_sub_value <- most_budget_input_object$budget_proportion
  
  most_time_input_object <- report[which.max(report$time),] 
  most_time_input_value <- paste(most_time_input_object$hosp_name,
                                 most_time_input_object$prod_name,
                                 sep = "-")
  most_time_input_sub_value <- most_time_input_object$rep_name
  most_time_input_sub_sub_value <- most_time_input_object$time
  
  best_perform_object <- report[which.max(report$share),]
  best_perform_value <- paste(best_perform_object$hosp_name,
                              best_perform_object$prod_name,
                              sep = "-")
  best_perform_sub_value <- best_perform_object$share
  best_perform_sub_sub_value <- best_perform_object$share_change
  
  best_contri_object <- report[which.max(report$contri_rate),]
  best_contri_value <- paste(best_contri_object$hosp_name,
                             best_contri_object$prod_name,
                             sep = "-")
  best_contri_sub_value = best_contri_object$sales
  best_contri_sub_sub_value = best_contri_object$contri_rate
  
  overview <- list(list(index = as.integer(0),
                        eng_title = "most_budget_input",
                        value = most_budget_input_value,
                        ext = list(sub = list(type = "title",
                                              value = subset(names_box, eng_title == "budget_proportion")$title,
                                              ext = list(type = "percent",
                                                         change = "none",
                                                         value = most_budget_input_sub_sub_value)))),
                   
                   list(index = as.integer(1),
                        eng_title = "most_time_input",
                        value = most_time_input_value,
                        ext = list(sub = list(type = "tile",
                                              value = most_time_input_sub_value,
                                              ext = list(type = "percent",
                                                         change = "none",
                                                         value = most_time_input_sub_sub_value)))),
                   list(index = as.integer(2),
                        eng_title = "best_perfom",
                        value = best_perform_value,
                        ext = list(sub = list(eng_title = "share",
                                              value = best_perform_sub_value,
                                              ext = list(type = "percent",
                                                         change = ifelse(best_perform_sub_sub_value >= 0,
                                                                         "up",
                                                                         "down"),
                                                         value = best_perform_sub_sub_value)))),
                   list(index = as.integer(3),
                        eng_title = "best_contri",
                        value = best_contri_value,
                        ext = list(sub = list(type = "title",
                                              value = best_contri_sub_value,
                                              ext = list(type = "percent",
                                                         change = "none",
                                                         value = best_contri_sub_sub_value)))))
  
  overview_m <- lapply(overview, function(x) {
    x[["title"]] = subset(names_box, eng_title == x[["eng_title"]])$title
    x[["type"]] = subset(names_box, eng_title == x[["eng_title"]])$type
    if (!is.null(x[["ext"]][["sub"]][["eng_title"]])) {
      x[["ext"]][["sub"]][["title"]] = subset(names_box, eng_title == x[["ext"]][["sub"]][["eng_title"]])$title
      x[["ext"]][["sub"]][["eng_title"]] <- NULL
    }
    x[["eng_title"]] <- NULL
    x
  })
  
  list(overview = overview_m,
       value = report_m)
  
}

produce_rep_ind_resos_report <- function(current_dest_gain,
                                         new_rep_part,
                                         input_data,
                                         rep_info,
                                         names_box){
  # new_rep_part <- new_rep_part
  # current_dest_gain <- new_dest_part
  # input_data <- input_business_deci_m1
  # rep_info <- rep_info
  # names_box <- names_box
  
  report <- current_dest_gain %>%
    left_join(select(input_data,
                     -target,
                     -potential), by = c("dest_id",
                                         "goods_id",
                                         "rep_id")) %>%
    left_join(rep_info, by = c("rep_id" = "_id")) %>%
    group_by(rep_id, rep_name) %>%
    dplyr::summarise(target = sum(target, na.rm = T),
                     sales = sum(sales, na.rm = T),
                     budget_proportion = sum(budget_proportion, na.rm = T),
                     user_input_day = sum(user_input_day, na.rm = T)) %>%
    left_join(select(new_rep_part,
                     rep_id,
                     bonus), by = "rep_id") %>%
    mutate(achieve_rate = sales/target,
           contri_rate = sales/sum(sales, na.rm = T)) 
  
  most_target_on_object <- report[which.max(report$target),]
  most_target_on_value <- most_target_on_object$rep_name
  most_target_on_sub_value <- most_target_on_object$target
  
  most_reso_on_object <- report[which.max(report$budget_proportion),]
  most_reso_on_value <- most_reso_on_object$rep_name
  most_reso_on_sub_value <- most_reso_on_object$budget_proportion
  
  highest_achieve_rate_object <- report[which.max(report$achieve_rate),]
  highest_achieve_rate_value <- highest_achieve_rate_object$rep_name
  highest_achieve_rate_sub_value <- highest_achieve_rate_object$achieve_rate
  
  highest_contri_rate_object <- report[which.max(report$contri_rate),]
  highest_contri_rate_value <- highest_contri_rate_object$rep_name
  highest_contri_rate_sub_value <- highest_contri_rate_object$contri_rate
  
  overview <- list(list(index = as.integer(0),
                        eng_title = "most_target_on",
                        value = most_target_on_value,
                        ext = list(sub = list(type = "title",
                                              eng_title = "target",
                                              ext = list(type = "sales",
                                                         value = most_target_on_sub_value)))),
                   list(index = as.integer(1),
                        eng_title = "most_reso_on",
                        value = most_reso_on_value,
                        ext = list(sub = list(type = "title",
                                              eng_title = "target",
                                              ext = list(type = "percent",
                                                         change = "none",
                                                         value = most_reso_on_sub_value)))),
                   list(index = as.integer(2),
                        eng_title = "highest_achieve_rate",
                        value = highest_achieve_rate_value,
                        ext = list(sub = list(type = "title",
                                              eng_title = "target",
                                              ext = list(type = "percent",
                                                         change = "none",
                                                         value = highest_achieve_rate_sub_value)))),
                   list(index = as.integer(3),
                        eng_title = "highest_contri_rate",
                        value = highest_contri_rate_value,
                        ext = list(sub = list(type = "title",
                                              eng_title = "target",
                                              ext = list(type = "percent",
                                                         change = "none",
                                                         value = highest_contri_rate_sub_value)))))
  
  overview_m <- lapply(overview, function(x) {
    x[["title"]] = subset(names_box, eng_title == x[["eng_title"]])$title
    x[["type"]] = subset(names_box, eng_title == x[["eng_title"]])$type
    if (!is.null(x[["ext"]][["sub"]][["eng_title"]])) {
      x[["ext"]][["sub"]][["value"]] = subset(names_box, eng_title == x[["ext"]][["sub"]][["eng_title"]])$title
      x[["ext"]][["sub"]][["eng_title"]] <- NULL
    }
    x[["eng_title"]] <- NULL
    x
  })
  
  list(overview = overview_m,
       value = report)
}


update_compete_report <- function(last_compete_part,
                                  new_dest_part){
  
  last_compete_part <- last_compete_part
  new_dest_part <- new_dest_part
  
  colnames(last_compete_part)[!colnames(last_compete_part)%in%c("phase", "dest_id", "goods_id", "goods_id1")] <-
    paste0("pp_", colnames(last_compete_part)[!colnames(last_compete_part)%in%c("phase", "dest_id", "goods_id", "goods_id1")])
  
  new_dest_part$need_sales <- new_dest_part$sales
  
  report <- last_compete_part %>%
    group_by(dest_id,
             goods_id) %>%
    mutate(proportion = pp_sales/sum(pp_sales, na.rm = T)) %>%
    left_join(select(new_dest_part,
                     dest_id,
                     goods_id,
                     need_sales,
                     potential), by = c("dest_id",
                                        "goods_id")) %>%
    mutate(left_sales = potential - need_sales,
           sales = left_sales*proportion,
           share = sales/potential,
           pp_share = pp_sales/pp_potential,
           sales_growth = sales/pp_sales,
           share_change = share-pp_share)  %>%
    ungroup()
  
  new_compete_part <- report %>%
    select(dest_id,
           goods_id,
           goods_id1,
           sales,
           potential)
  
  compete_report <- report %>%
    mutate(outside_goods_id = goods_id,
           goods_id = goods_id1) %>%
    select(dest_id,
           outside_goods_id,
           goods_id,
           sales,
           sales_growth,
           share,
           share_change) %>%
    nest(-dest_id,
         -outside_goods_id, .key = "compete_info") %>%
    mutate(goods_id = outside_goods_id) %>%
    select(-outside_goods_id)
  
  list(new_compete_part,
       compete_report)
  
}


##-- load scenarios
db_scenarios <- mongo(collection = "scenarios", db = "pharbers-tm-client",
                      url = options()$mongodb$host,
                      verbose = F, options = ssl_options())

scenarios_data_doc <- db_scenarios$find(query = paste('{"uuid" : "',R_Json_Path,'"}',sep = ""),
                                    field = '{}')

proposal_id <- scenarios_data_doc$proposal_id

scenarios_data <- scenarios_data_doc$current
current_phase <- scenarios_data$phase

if (is.null(scenarios_data_doc$past[[1]])) {
  last_report_id <- "all_starting_value"
} else {
  last_report_id <- subset(scenarios_data_doc$past[[1]], phase == current_phase-1)$report_id
}

dest_in_scenarios <- scenarios_data$connect_dest[[1]]

goods_in_scenarios <- scenarios_data$connect_goods[[1]]

reso_in_scenarios <- bind_cols(data.frame(type = scenarios_data$connect_reso[[1]]$type,
                                          id = scenarios_data$connect_reso[[1]]$id,
                                          stringsAsFactors =  F),
                                          scenarios_data$connect_reso[[1]]$relationship)

workdays <- subset(reso_in_scenarios, type == "day")$value

available_budget <- subset(reso_in_scenarios, type == "money")$value

good_in_dest_info <- scenarios_data$dest_goods[[1]]
good_in_dest_info_m <- bind_cols(data.frame(dest_id = good_in_dest_info$dest_id,
                                                   goods_id = good_in_dest_info$goods_id,
                                                   stringsAsFactors =  F),
                                        good_in_dest_info$relationship) 
  
good_in_dest_info_m1 <- good_in_dest_info_m %>%
  select(-compete_goods)

compete_info <- good_in_dest_info_m %>%
  select(dest_id,
         goods_id,
         potential,
         compete_goods) %>%
  unnest()

input_business_deci <- scenarios_data$dest_goods_rep[[1]]
input_business_deci_m <- bind_cols(data.frame(dest_id = input_business_deci$dest_id,
                                              goods_id = input_business_deci$goods_id,
                                              rep_id = input_business_deci$rep_id,
                                              stringsAsFactors = F),
                                   input_business_deci$relationship) %>%
  select(-target_growth,
         -achieve_rate)


##-- load dest names/goods name/rep name
db_dests <- mongo(collection = "dests", db = "pharbers-tm-client",
                      url = options()$mongodb$host,
                      verbose = F, options = ssl_options())
dests_info <- db_dests$find(field = '{}') %>%
  filter(`_id`%in%dest_in_scenarios$id) %>%
  select(`_id`, hosp_name)

db_goods <- mongo(collection = "goods", db = "pharbers-tm-client",
                 url = options()$mongodb$host,
                 verbose = F, options = ssl_options())
goods_info <- db_goods$find(field = '{}') %>%
  filter(`_id`%in%goods_in_scenarios$id) %>%
  select(`_id`, prod_name)


db_rep <- mongo(collection = "representatives", db = "pharbers-tm-client",
                 url = options()$mongodb$host,
                 verbose = F, options = ssl_options())

rep_info <- db_rep$find(field = '{}') %>%
  select(`_id`, rep_name)

##-- bind info togother 
input_business_deci_m1 <- input_business_deci_m %>%
  left_join(good_in_dest_info_m1, by = c("dest_id", "goods_id")) %>%
  mutate(budget = user_input_money,
         target = user_input_target,
         workdays = workdays) %>%
  # left_join(dest_info, by = c("dest_id" = "_id")) %>%
  # left_join(goods_info, by = c("goods_id" = "_id")) %>%
  # left_join(rep_info, by = c("rep_id" = "_id")) %>%
  select(-contri_rate,
         -share,
         -share_change,
         -sales,
         -sales_growth,
         -user_input_money,
         -user_input_target)  %>%
  mutate(kpi_analysis = 0,
         admin_work = 0,
         team_meet = 0,
         field_work = 0)

##-- load management decisions
# input_manage_deci <- data.frame(rep_)

##-- load last phase data 
db_inter <- mongo(collection = "intermedia", db = "pharbers-tm-report",
                  url = options()$mongodb$host,
                  verbose = F, options = ssl_options())

last_phase_doc <- db_inter$find(query = paste0('{"uuid" : "the_start","proposal_id":"',proposal_id,'"}'),
                                field = '{}')
last_phase_data <- last_phase_doc$data
last_dest_part <- last_phase_data$inter_data$dest_part[[1]]
last_rep_part <- last_phase_data$inter_data$rep_part[[1]]
last_compete_part <- last_phase_data$inter_data$compete_part[[1]]

##-- load models
db_models <- mongo(collection = "models", db = "pharbers-tm-report",
                   url = options()$mongodb$host,
                   verbose = F, options = ssl_options())
curves <- db_models$find(query = paste0('{"type" : "curves","proposal_id":"',proposal_id,'"}'))[["value"]][[1]]
weightages <- db_models$find(query = paste0('{"type" : "weightage","proposal_id":"',proposal_id,'"}'))[["value"]][[1]]
names_box <- db_models$find(query = paste0('{"type" : "names_box","proposal_id":"',proposal_id,'"}'))[["value"]][[1]]


##-- start calculation
new_rep_status_except_moti <- update_rep_except_moti(rep_index_info = last_rep_part,
                                                     curves_data = curves,
                                                     weightages_data = weightages) 

new_rep_sales_perfor <- update_rep_sales_perfor(input_business_deci_m1,
                                                new_rep_status_except_moti,
                                                last_dest_part,
                                                curves,
                                                weightages)

new_deply_quality <- update_deploy_quality(input_business_deci_m1,
                                           last_dest_part,
                                           curves,
                                           weightages)

prepared_sales_perfor_data <- new_rep_sales_perfor %>%
  left_join(new_deply_quality, by = c("dest_id",
                                      "goods_id",
                                      "rep_id"))

new_sales_perfor <- update_sales_perfor(prepared_sales_perfor_data,
                                        last_dest_part,
                                        curves,
                                        weightages)

prepared_custom_relation_data <- input_business_deci_m1 %>%
  left_join(new_rep_status_except_moti, by = "rep_id")

new_custom_relation <- update_custom_relation(prepared_custom_relation_data,
                                              last_dest_part,
                                              curves,
                                              weightages)

prepared_offer_attrac_data <- new_sales_perfor %>%
  left_join(new_custom_relation, by = c("dest_id",
                                        "goods_id",
                                        "rep_id")) %>%
  left_join(input_business_deci_m1, by = c("dest_id",
                                          "goods_id",
                                          "rep_id"))

new_offer_attract <- update_offer_attrac(prepared_offer_attrac_data,
                                         last_dest_part,
                                         curves,
                                         weightages)

new_rep_status <- update_rep_info(new_rep_status_except_moti,
                                  new_offer_attract,
                                  curves,
                                  weightages)

new_dest_part <- new_offer_attract %>%
  left_join(select(new_rep_status,
                   rep_id,
                   prod_knowledge_val,
                   sales_skills_val,
                   motivation_val), by = "rep_id") %>%
  select(one_of(colnames(last_dest_part)))

new_rep_part <- new_rep_status %>%
  select(one_of(colnames(last_rep_part)))

## produce report
summary_report <- produce_summary_report(new_dest_part,
                                         last_dest_part,
                                         goods_info,
                                         names_box)

dests_goods_report <- produce_dests_goods_report(new_dest_part,
                                                 last_dest_part,
                                                 dests_info,
                                                 goods_info,
                                                 names_box)

rep_goods_report <- produce_rep_goods_report(new_dest_part,
                                             last_dest_part,
                                             rep_info,
                                             goods_info,
                                             names_box)

reso_allocation_report <- produce_reso_allocation_report(new_dest_part,
                                                         last_dest_part,
                                                         input_business_deci_m1,
                                                         dests_info,
                                                         goods_info,
                                                         rep_info,
                                                         names_box)

rep_ind_resos <- produce_rep_ind_resos_report(new_dest_part,
                                              new_rep_part,
                                              input_business_deci_m1,
                                              rep_info,
                                              names_box)

compete_report <- update_compete_report(last_compete_part,
                                        new_dest_part)[[2]]

new_compete_part <- update_compete_report(last_compete_part,
                                          new_dest_part)[[1]]


##-- insert report
db_report <- mongo(collection = "reports", db = "pharbers-tm-report",
                   url = options()$mongodb$host,
                   verbose = F, options = ssl_options())

db_report$insert(list(uuid = R_Json_Path,
                      timestamp = as.integer(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),
                      summary_report = summary_report,
                      dests_goods_report = dests_goods_report,
                      rep_goods_report = rep_goods_report,
                      reso_allocation_report = reso_allocation_report,
                      rep_ind_resos = rep_ind_resos,
                      compete_report = compete_report),
                 na="string",
                 auto_unbox = T)

db_report <- mongo(collection = "reports", db = "pharbers-tm-report",
                   url = options()$mongodb$host,
                   verbose = F, options = ssl_options())

report_info <- db_report$find(query = paste('{"uuid" : "',R_Json_Path,'"}',sep = ""),
                              field = '{}',
                              sort = '{"timestamp":-1}',
                              limit = 1)

report_id <- report_info$`_id`

##-- update report id within current scenarios
db_scenarios <- mongo(collection = "scenarios", db = "pharbers-tm-client",
                      url = options()$mongodb$host,
                      verbose = F, options = ssl_options())

scenarios_data_doc <- db_scenarios$update(query = paste('{"uuid" : "',R_Json_Path,'"}',sep = ""),
                                          update = paste0('{"$set":{"current.report_id":"',report_id,'"}}'))

##-- insert intermedia data
db_inter <- mongo(collection = "intermedia", db = "pharbers-tm-client",
                  url = options()$mongodb$host,
                  verbose = FALSE, options = ssl_options())

db_inter$insert(list("proposal" = proposal_id,
                     "phase" = as.integer(current_phase),
                     "uuid" = R_Json_Path,
                     "timestamp" = as.integer(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),
                     "data" = list(phase = current_phase,
                                   inter_data = list(dest_part = new_dest_part,
                                                     rep_part = new_rep_part,
                                                     compete_part = new_compete_part))),
                
                na="string",
                auto_unbox = T)