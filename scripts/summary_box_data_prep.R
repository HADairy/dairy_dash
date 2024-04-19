# summmary box calc
latest_sum_prod <-
  prod_data %>%
  summarise(max_date = max(date)) %>%
  mutate(
    year = year(max_date),
    month = month(max_date),
    last_comp_year = case_when(month < 12 ~ (year - 1),
                               TRUE ~ year)
  )

latest_comp_year_prod <- latest_sum_prod$last_comp_year

latest_comp_year_sum_prod <-
  prod_data %>%
  mutate(year = year(date)) %>%
  filter(year == latest_comp_year_prod) %>%
  summarise(
    mean_yield = format(round(mean(
      milk_per_cow_prod, na.rm = TRUE
    ), 1), nsmall = 1),
    mean_protein = format(round(mean(
      Protein_mqual, na.rm = TRUE
    ), 1), nsmall = 1),
    mean_fat = format(round(mean(Fat_mqual, na.rm = TRUE), 1), nsmall = 1)
  )



latest_yield <- latest_comp_year_sum_prod$mean_yield
latest_fat <- latest_comp_year_sum_prod$mean_fat
latest_protein <- latest_comp_year_sum_prod$mean_protein

###
# fertility and health

latest_sum_fert <-
  fert_health_data %>% 
  summarise(max_date = max(date)) %>%
  mutate(year = year(max_date),
         month = month(max_date),
         last_comp_year = case_when(month < 12 ~ (year -1),
                                    TRUE ~ year))

latest_comp_year_fert <- latest_sum_fert$last_comp_year


latest_comp_year_sum_fert <-
  fert_health_data %>%
  mutate(year = year(date)) %>%
  filter(year == latest_comp_year_fert) %>% 
    rowwise() %>% 
  summarise(year = year[1],
    tot_week_calves = sum(c(cows_calved_fert, heifers_calved_fert), na.rm = TRUE), 
         tot_week_health = sum(c(mastitis_health, milk_fever_health, da_health,
                                 cystic_health, lamenesss_health, rfm_health), na.rm = TRUE), 
         tot_week_exit = sum(c(culls_exit, deaths_exit), na.rm = TRUE)) %>% 
  summarise(
    total_calving = round(sum(
      tot_week_calves, na.rm = TRUE
    ), 1),
    total_health = round(sum(
      tot_week_health, na.rm = TRUE
    ), 1),
    total_exits = round(sum(tot_week_exit, na.rm = TRUE), 1)
  )



tot_calves <- latest_comp_year_sum_fert$total_calving
tot_health <- latest_comp_year_sum_fert$total_health
tot_exit <- latest_comp_year_sum_fert$total_exits 



# stock take 

milk_stat_2 <-
  milk_stat_st %>%
  left_join(
    milk_stat_prod,
    by = c("year", "week"),
    suffix = c("_s", "_p"),
    relationship = "many-to-many"
  ) %>%
  filter(date_s == date_p) %>%
  select(date_p,
         year,
         week,
         cows_in_milk_prod,
         cows_in_tank_prod,
         total_dry) %>%
  mutate(date = as.Date(date_p),
    month = month(date_p), 
         year = year(date_p)) 

latest_sum_st <-
  milk_stat_2 %>% 
  summarise(max_date = max(date_p)) %>%
  mutate(year = year(max_date),
         month = month(max_date),
         last_comp_year = case_when(month < 12 ~ (year -1),
                                    TRUE ~ year), 
         last_comp_year = case_when(last_comp_year == 2021 ~ (last_comp_year -1), 
                                    TRUE ~ last_comp_year))

latest_comp_year_st <- latest_sum_st$last_comp_year

latest_comp_year_sum_st <-
  milk_stat_2 %>%
  mutate(year = year(date_p)) %>% 
  filter(year == latest_comp_year_st) %>% 
  summarise(avg_milk = round(mean(cows_in_milk_prod, na.rm = TRUE),0),
            avg_tank = round(mean(cows_in_tank_prod, na.rm = TRUE),0),
            avg_dry = round(mean(total_dry, na.rm = TRUE),0))


tot_in_milk_latest <- latest_comp_year_sum_st$avg_milk
tot_in_tank_latest <- latest_comp_year_sum_st$avg_tank
tot_dry_lastest <- latest_comp_year_sum_st$avg_dry
