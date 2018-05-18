# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TMIST
# Purpose:      Competence assessment
# programmer:   Anqi Chen
# Date:         20-11-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

library(plyr)
library(stringr)

## grasp phase 0 data 
db_inter <- mongo(collection = "intermedia",
                  url = sprintf(
                    "mongodb://%s/%s",
                    # options()$mongodb$username,
                    # options()$mongodb$password,
                    options()$mongodb$host,
                    "TMIST"))

db_inter_0 <- db_inter$find(paste('{"uuid" : "all"}',sep = ""))
inter_data_0 <- db_inter_0$inter$data[[1]]
summary_0 <- db_inter_0$inter$report[[1]]

phase_0 <- inter_data_0 %>% select(phase,
                                 hosp_name,
                                 hosp_code,
                                 prod_name,
                                 prod_code,
                                 real_revenue,
                                 potential_revenue) %>%
  distinct() %>%
  mutate(potential = potential_revenue,
         phase = 0) %>%
  select(-potential_revenue)


## grasp phase 1 &2 data
db_inter_12 <- db_inter$find(paste('{"uuid" : "',R_Json_Path,'"}',sep = ""))
final_report_add <- info_pre$colnames$final_report_add[[1]]
user_name <- db_inter_12$user_id
inter_data_12 <- db_inter_12$inter[[1]]
inter_data_1 <- subset(inter_data_12, phase ==1)$data[[1]]
inter_data_2 <- subset(inter_data_12, phase ==2)$data[[1]]
summary_1 <- subset(inter_data_12, phase == 1)$report[[1]]
summary_2 <- subset(inter_data_12, phase == 2)$report[[1]]

phase_1 <- inter_data_1 %>% select(phase,
                                   hosp_name,
                                   hosp_code,
                                   prod_name,
                                   prod_code,
                                   real_revenue,
                                   potential) %>%
  distinct()


phase_2 <- inter_data_2 %>% select(phase,
                                   hosp_name,
                                   hosp_code,
                                   prod_name,
                                   prod_code,
                                   real_revenue,
                                   potential) %>%
  distinct()


## bind togother
all_data <- bind_rows(phase_0,
                      phase_1,
                      phase_2) %>%
  mutate(hosp_code = as.character(hosp_code),
         prod_code = as.character(prod_code)) %>%
  group_by(phase, hosp_code) %>%
  do(plyr::rbind.fill(.,data.frame(hosp_name = first(.$hosp_name),
                                  hosp_code = first(.$hosp_code),
                                  prod_name = "All",
                                  prod_code = "All",
                                  real_revenue = sum(.$real_revenue, na.rm = T),
                                  potential = sum(.$potential, na.rm = T),
                                  phase = first(.$phase)))) %>%
  ungroup() %>%
  group_by(phase, prod_code) %>%
  do(plyr::rbind.fill(.,data.frame(hosp_name = "All",
                                   hosp_code = "All",
                                   prod_name = first(.$prod_name),
                                   prod_code = first(.$prod_code),
                                   real_revenue = sum(.$real_revenue, na.rm = T),
                                   potential = sum(.$potential, na.rm = T),
                                   phase = first(.$phase)))) %>%
  ungroup() %>%
  group_by(phase, hosp_code) %>%
  mutate(total_revenue_by_hosp = sum(real_revenue, na.rm =T)) %>%
  ungroup() %>%
  group_by(phase, prod_code) %>%
  mutate(total_revenue_by_prod = sum(real_revenue, na.rm =T)) %>%
  ungroup() %>%
  arrange(hosp_code,prod_code,phase) %>%
  group_by(hosp_code, prod_code) %>%
  mutate(pp_real_reveue = ifelse(row_number()>1, real_revenue[row_number()-1], NA),
         pp_potential = ifelse(row_number()>1, potential[row_number()-1],NA),
         pp_total_revenue_by_hosp = ifelse(row_number()>1, total_revenue_by_hosp[row_number()-1],NA),
         pp_total_revenue_by_prod = ifelse(row_number()>1, total_revenue_by_prod[row_number()-1],NA)) %>%
  ungroup() %>%
  filter(phase != 0) %>%
  mutate(total_revenue_by_hosp_growth = round(total_revenue_by_hosp/pp_total_revenue_by_hosp*100-100,2),
         total_revenue_by_prod_growth = round(total_revenue_by_prod/pp_total_revenue_by_prod*100-100,2),
         market_share = round(real_revenue/potential*100),
         pp_market_share = round(pp_real_reveue/pp_potential*100),
         market_share_growth = market_share- pp_market_share)

team_summary <- summary_2
colnames(team_summary)[4] <- "team_ability"
team_summary$phase <- as.numeric(str_extract_all(team_summary$phase,"[0-9]"))

all_data_m <-  bind_rows(phase_0,
                         phase_1,
                         phase_2) %>%
  group_by(phase) %>%
  summarise(total_revenue = sum(real_revenue, na.rm = T),
            total_potential = sum(potential, na.rm =T)) %>%
  left_join(team_summary, by = "phase") %>%
  mutate(market_share = round(total_revenue/total_potential*100))  %>%
  arrange(phase) %>%
  mutate(pp_total_revenue = total_revenue[1],
         pp_market_share = market_share[1],
         pp_team_ability = team_ability[1]) %>%
  filter(phase == 2) %>%
  mutate(market_share_growth = market_share - pp_market_share,
         team_ability_growth = team_ability - pp_team_ability) %>%
  select(phase, total_revenue, market_share, market_share_growth,
         team_ability, team_ability_growth)



## hospital angle of view
hosp_view <- subset(all_data, prod_code != "All")
hosp_view_1 <- subset(hosp_view, phase == 1)
hosp_view_2 <- subset(hosp_view, phase == 2)

## product angle of view
prod_view <- subset(all_data, hosp_code != "All")
prod_view_1 <- subset(prod_view, phase == 1)
prod_view_2 <- subset(prod_view, phase == 2)
  

# combine phase 1 &2  
all_data_sum_for_phase <- all_data %>%
  filter(phase %in% 1:2) %>%
  group_by(hosp_name, hosp_code, prod_name, prod_code) %>%
  summarise(real_revenue = sum(real_revenue, na.rm =T),
            potential = sum(potential, na.rm = T)) %>%
  ungroup() %>%
  mutate(phase = "All") %>%
  group_by(hosp_code) %>%
  mutate(total_revenue_by_hosp = sum(real_revenue, na.rm =T)) %>%
  ungroup() %>%
  group_by(prod_code) %>%
  mutate(total_revenue_by_prod = sum(real_revenue, na.rm =T))


  
competence_assess_result <- list(list(type = "overall",
                                      data = all_data_m),
                                 list(type = "hospital",
                                      data = list(list(phase = 1,
                                                       table = hosp_view_1),
                                                  list(phase = 2,
                                                       table = hosp_view_2))),
                                 list(type = "product",
                                      data = list(list(phase = 1,
                                                       table = prod_view_1),
                                                  list(phase = 2,
                                                       table = prod_view_2))))

## insert into mongoDB
db_assess <- mongo(collection = "assessment",
                  url = sprintf(
                    "mongodb://%s/%s",
                    # options()$mongodb$username,
                    # options()$mongodb$password,
                    options()$mongodb$host,
                    "TMIST"))

if (R_Json_Path %in% db_assess$uuid) {
  
  mongo_tmp <- paste('{"uuid" : ', '"', R_Json_Path, '"}',sep = "")
  mongo_tmp_m <- paste('{"$set":{"time":',as.numeric(as.POSIXct(Sys.Date(), format="%Y-%m-%d")),',"assessment":',toJSON(competence_assess_result,auto_unbox = T),'}}', sep = "")
  db_assess$update(mongo_tmp, mongo_tmp_m)
  
  
} else {
  
  db_assess$insert(list(uuid = R_Json_Path,
                          user_id = user_name,
                          assessment = competence_assess_result), auto_unbox = T, na = "string")
  
}


