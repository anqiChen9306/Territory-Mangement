library(DT)
library(plyr)
library(dplyr)
library(tidyr)
library(digest)
library(openxlsx)
library(mongolite)
library(jsonlite)
library(utf8)

## -- read in database
load("backupofdata_0.RData")
source("global.R")
# rm(list=setdiff(ls(), c("pp_data1", "pp_data2","product_info","curves","overhead",
#                         "weightages","big_hosp_list","salesmen_list","hospital_info",
#                         "promotional_budget_list","worktime")))



## -- prepare simulation inputs
tmpt <- list(cp_data1,
             cp_data2,
             flm_data)

decision_input <- data.frame(phase = ,
                             hosp_code = ,
                             prod_code = ,
                             salesmen = ,
                             budget = ,
                             prod_value = ,
                             prod_hours = )

basket_business_par1 <- expand.grid(hosp_code = 1:10,
                      prod_code = 1:4,
                      salesmen = c(salesmen_list$salesmen,"0"),
                      budget = seq(0, 40, 2.5),
                      prod_hours = seq(0, 51, 3))

restrict <- data.frame(serial_num = 1:length(seq(0, 40, 2.5)),
                       budget = seq(0, 40, 2.5))

basket_business_par1m <- basket_business_par1 %>%
  left_join(restrict, by = "budget")

# - set reasonable product value 
pp_revenue_list <- pp_data1 %>%
  select(hosp_code,
         prod_code,
         pp_real_revenue)

data.frame(apply(pp_revenue_list,1,function(x)as.list(bind_rows(x,x))))

updown_ratio = c(0,seq(0.6,2,0.1))

basket_business_par2 <- expand.grid(hosp_code = 1:10,
                                    prod_code = 1:4,
                                    updown_ratio = c(0,seq(0.6,2,0.1))) %>%
  left_join(pp_revenue_list, by = c("hosp_code", "prod_code")) %>%
  mutate(prod_value = pp_real_revenue*updown_ratio) %>%
  select(hosp_code,
         prod_code,
         prod_value)

basket_business <- basket_business_par1m %>%
  left_join(basket_business_par2, by = c("hosp_code", "prod_code"))


management_input <- data.frame(phase = ,
                               salesmen = ,
                               admin_work = , 
                               field_work = ,
                               kpi_analysis = ,
                               meetings_with_team = ,
                               product_training = ,
                               sales_training = )

basket_management_par1 <- expand.grid(salesmen = salesmen_list$salesmen,
                           field_work = seq(0, 20, 2),
                           product_training = seq(0, 20, 2),
                           sales_training = seq(0, 20, 2))

basket_management_par2 <- expand.grid(admin_work = seq(0, 20, 2), 
                           kpi_analysis = seq(0, 20, 2),
                           meetings_with_team = seq(0, 20, 2))
basket_management_par2 <- basket_management_par2 %>%
  mutate(serial_num = n())

basket_management <-  lapply(1:nrow(basket_management_par2), function(x) {
  cbind(basket_management_par1,basket_management_par2[x,])
})

basket_management <- bind_rows(basket_management) 

# basket_management_m <- basket_management %>%
#   distinct() %>%
#   group_by(serial_num) %>%
#   mutate(field_work_m = sum(field_work),
#          sales_training_m = sum(sales_training),
#          admin_work_m = first(admin_work),
#          kpi_analysis_m = first(kpi_analysis),
#          meetings_with_team_m = first(meetings_with_team),
#          flm_time = field_work_m+ sales_training_m+ admin_work_m+ 
#            kpi_analysis_m+ meetings_with_team_m) %>%
#   filter(flm_time <= 100) %>%
#   ungroup() %>%
#   select(one_of(c("serial_num", "salesmen", "field_work", "sales_training",
#                   "admin_work", "kpi_analysis", "meetings_with_team","product_training"))) %>%
#   distinct()



## -- phase 1

basket_business_p1 <- basket_business %>%
  mutate(prod_hours = ifelse(prod_code == 4, 0, prod_hours)) %>%
  distinct()

basket_business_p1$phase <- 1

basket_business_p1_rown <- basket_business_p1 %>%
  group_by(hosp_code, prod_code) %>%
  summarise(rown = n()) %>%
  ungroup() %>%
  arrange(hosp_code, prod_code)

basket_management_p1 <- basket_management

basket_management_p1_rown <- nrow(basket_management_p1)/5

basket_management_p1$phase <- 1

## -- phase 2

basket_business_p2 <- basket_business %>%
  mutate(prod_hours = ifelse(hosp_code %in% c(4,5,6,7,9,10)&prod_code == 4, 0, prod_hours)) %>%
  distinct()

basket_business_p2$phase <- 2

basket_business_p2_rown <- basket_business_p2 %>%
  group_by(hosp_code, prod_code) %>%
  summarise(rown = n()) %>%
  ungroup() %>%
  arrange(hosp_code, prod_code)

basket_management_p2 <- basket_management

basket_management_p2_rown <- nrow(basket_management_p2)/5

basket_management_p2$phase <- 2

## -- travel around possible solutions
iterator <- 10
result <- vector("list", iterator)

## -- phase 1 
for (round in 1:iterator) {
  for (i in 1:40) {
    tmp_rown <- sample(1:basket_business_p1_rown$rown[i],1)
    
  }
  serial_num_of_busin
  business_decision <- lapply(1:40, function(x) {
    tmp_rown <- sample(1:basket_business_p1_rown$rown[x],1)
    out <- basket_business_p1 %>%
      filter(hosp_code == ceiling(x/4), prod_code == ifelse(x%%10==0,10,x%%10))
    out[tmp_rown,] 
  })
  business_decision <- bind_rows(business_decision)
   
  
}


tmp1 <- calculation(pp_data1,
                    pp_data2,
                    cp_data1,
                    cp_data2)

run_for_results(tmp1,
                flm_data)
