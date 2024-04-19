# v2 data prep

## stock take data


milk_stat_st <-
  stock_take_data %>%
  select(date, total_inmilk, total_dry) %>%
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    day = day(date),
    wk_day = wday(date)
  )

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
  mutate(total_head = cows_in_milk_prod + total_dry) %>%
  mutate(cows_not_in_tank = cows_in_milk_prod - cows_in_tank_prod)  %>%
  select(date_p,
         year,
         week,
         cows_in_milk_prod,
         cows_in_tank_prod,
         total_dry,
         total_head) %>%
  pivot_longer(
    cols = c(cows_in_milk_prod, cows_in_tank_prod, total_dry),
    names_to = "status",
    values_to = "count"
  )


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
  )



weekly_yield_data <-
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
mqual_data_weekly <- prod_data %>%
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
    protein = mean(Protein_mqual, na.rm = TRUE)
  ) %>%
  filter(wk_day == 1) %>%
  select(date, year, week, scc, bs, fat, protein)



mqual_data_daily <- prod_data %>%
  select(date, SCC_mqual, BS_mqual, Fat_mqual, Protein_mqual) %>%
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    day = day(date),
    wk_day = wday(date),
    .after = date
  )

# benchmark data?

mq_bench <-
  read_excel("milkprices_dataset_280324.xlsx",
             sheet = "Prices_Monthly",
             skip = 1) %>%
  mutate(across(c(Month), ~ as.numeric(.x))) %>%
  mutate(date = as.Date(Month, origin = "1899-12-30")) %>%
  filter(date >= "2018-01-01")


## fertility and health data
fh_data <- 
fert_health_data %>% 
  rowwise() %>%
  mutate(calves = sum(c(cows_calved_fert, heifers_calved_fert), na.rm = TRUE),
            pregs = sum(c(cows_pd_fert+heifer_pd_fert), na.rm = TRUE), 
         year = year(date), 
         month = month(date)
         ) %>% 
  select(date, year, month,calves, pregs, services_fert) %>% 
  pivot_longer(cols = c(calves, pregs, services_fert), names_to = "fert_measure", values_to = "Count")




#glimpse(fert_health_data)
