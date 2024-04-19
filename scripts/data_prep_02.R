# v2 data prep

## stock take data
# 
# glimpse(prod_data) # daily
# glimpse(stock_take_data) # weekly


milk_stat_st <- 
  stock_take_data %>% 
  select(date, total_inmilk, total_dry) %>% 
  mutate(year = year(date), 
         month = month(date), 
         week = week(date), 
         day = day(date), 
         wk_day = wday(date)) 

milk_stat_prod <- 
  prod_data %>% 
  select(date, cows_in_milk_prod, cows_in_tank_prod) %>%
  mutate(
    year = year(date), 
    month = month(date), 
    week = week(date), 
    day = day(date),
    wk_day = wday(date)
     ) 



milk_stat <- 
  milk_stat_st %>% 
  left_join(milk_stat_prod, by = c("year", "week"), suffix = c("_s", "_p"), relationship = "many-to-many") %>% 
  filter( date_s == date_p) %>% 
   select(date_p, year, week, 
          cows_in_milk_prod, cows_in_tank_prod, 
          total_dry) %>%  
    mutate(total_head = cows_in_milk_prod+total_dry) %>%
    mutate(cows_not_in_tank = cows_in_milk_prod-cows_in_tank_prod)  %>% 
    select(date_p, year, week, cows_in_milk_prod, cows_in_tank_prod, total_dry, total_head) %>%
    pivot_longer(cols = c(cows_in_milk_prod, cows_in_tank_prod, total_dry), names_to = "status", values_to = "count") %>% view()


## yield data

# total yield and  yield per cow summed weekly?
yield_data <- 
  prod_data %>%
  select(date,
         milk_prod,
         milk_per_cow_prod,
         cows_in_milk_prod,
         cows_in_tank_prod) %>%
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    day = day(date),
    wk_day = wday(date),
    .after = date
  ) %>% 
  group_by(year, week) %>%
  mutate(
    weekly_yield = sum(milk_prod, na.rm = TRUE),
    weekly_yield_per_cow = sum(milk_per_cow_prod, na.rm = TRUE),
    .after = day
  ) %>% 
  filter(wk_day == 1) 





## quality data # create weekly avaerage
mqual_data <- prod_data %>%
  select(date, SCC_mqual, BS_mqual, Fat_mqual, Protein_mqual) %>% 
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    day = day(date),
    wk_day = wday(date),
    .after = date
  ) %>% 
  group_by(year, week) %>%
  mutate(
    scc = mean(SCC_mqual, na.rm = TRUE),
         bs = mean(BS_mqual, na.rm = TRUE),
         fat = mean(Fat_mqual, na.rm = TRUE),
         protein = mean(Protein_mqual, na.rm = TRUE)) %>% 
    filter(wk_day == 1) %>% 
  select(date, year, week, scc, bs, fat, protein)



## fertility and health data

#glimpse(fert_health_data)
