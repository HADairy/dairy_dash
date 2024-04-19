# data_prep

my_comma <-
  scales::label_comma(accuracy = .1,
                      big.mark = ",",
                      decimal.mark = ".")

prod_data_og <- prod_data

# production data
# prod_data
# glimpse(prod_data)
# create new variables for month and year
prod_data <-  prod_data_og %>%
  mutate(month = month(ymd(date), label = TRUE), .after = date) %>%
  mutate(year = year(ymd(date)), .after = date) %>%
  mutate(day = day(ymd(date)), .after = date)


# key variables for production data
### number of cows in milk; number of cows in tank; total milk produced; milk produced per cow;
### HSCC; SCC; BS; Fat; Protein

# variable summaries for summary boxes

## average number of cows in milk per month? per year?
## average amount of milk produced per month? Per year?
## average amount of milk produced per cow per month? per year?
## average HSCC per month? per year?
## average SCC per month? per year?
## average BS per month? per year?
## average fat per month? per year?
## Average protein per month? per year?


latest_month_summary <-
  prod_data %>%
  group_by(year, month) %>%
  summarise(
    day = max(day),
    avg_in_milk_month = round(mean(cows_in_milk_prod, na.rm = TRUE), 0),
    avg_in_tank_month = round(mean(cows_in_tank_prod, na.rm = TRUE), 0),
    avg_milk_prod_month = mean(milk_prod, na.rm = TRUE),
    avg_milk_prod_cow_month = mean(milk_per_cow_prod, na.rm = TRUE),
    avg_hscc_month = mean(HSCC_mqual, na.rm = TRUE),
    avg_scc_month = mean(SCC_mqual, na.rm = TRUE),
    avg_bs_month = mean(BS_mqual, na.rm = TRUE),
    avg_fat_month = mean(Fat_mqual, na.rm = TRUE),
    avg_protein_month = mean(Protein_mqual, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(avg_in_milk_month)) %>%
  filter(year == max(year)) %>%
  filter(month == max(month))

latest_year <- latest_month_summary$year
latest_month <- latest_month_summary$month
latest_day <- latest_month_summary$day
latest_inmilk_av <- latest_month_summary$avg_in_milk_month
latest_intank_av <- latest_month_summary$avg_in_tank_month
latest_milk_av <-
  round(latest_month_summary$avg_milk_prod_month, 2)
latest_milk_cow_av <-
  round(latest_month_summary$avg_milk_prod_cow_month, 2)
latest_hscc_av <- round(latest_month_summary$avg_hscc_month, 2)
latest_scc_av <- round(latest_month_summary$avg_scc_month, 2)
latest_bs_av <- round(latest_month_summary$avg_bs_month, 2)
latest_fat_av <- round(latest_month_summary$avg_fat_month, 2)
latest_protien_av <-
  round(latest_month_summary$avg_protein_month, 2)



### fertility data ----
fert_data <-
  fert_health_data %>%
  select(c(date:dry_tubes_fert)) %>%
  mutate(month = month(ymd(date), label = TRUE), .after = date) %>%
  mutate(year = year(ymd(date)), .after = date) %>%
  mutate(day = day(ymd(date)), .after = date)



# latest month summary boxes
fert_summary_month <-
  fert_data %>%
  group_by(year, month) %>%
  summarise(
    day = max(day),
    cows_calved_mean_m = mean(cows_calved_fert, na.rm = TRUE),
    # mean number of cows with calves in that month for that year
    heifers_calved_mean_m = mean(heifers_calved_fert, na.rm = TRUE),
    services_mean_m = mean(services_fert, na.rm = TRUE),
    ovsynch_mean_m = mean(ovsynch_fert, na.rm = TRUE),
    cows_pd_mean_m = mean(cows_pd_fert, na.rm = TRUE),
    heifers_pd_mean_m = mean(heifer_pd_fert, na.rm = TRUE),
    dry_off_mean_m = mean(dry_off_fert, na.rm = TRUE),
    dry_tubes_mean_m = mean(dry_tubes_fert, na.rm = TRUE),
    cows_calved_sum_m = sum(cows_calved_fert, na.rm = TRUE),
    heifers_calved_sum_m = sum(heifers_calved_fert, na.rm = TRUE),
    services_sum_m = sum(services_fert, na.rm = TRUE),
    ovsynch_sum_m = sum(ovsynch_fert, na.rm = TRUE),
    cows_pd_sum_m = sum(cows_pd_fert, na.rm = TRUE),
    heifers_pd_sum_m = sum(heifer_pd_fert, na.rm = TRUE),
    dry_off_sum_m = sum(dry_off_fert, na.rm = TRUE),
    dry_tubes_sum_m = sum(dry_tubes_fert, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(cows_calved_mean_m)) %>%
  filter(year == max(year)) %>%
  filter(month == max(month))

fert_latest_year <- fert_summary_month$year
fert_latest_month <- fert_summary_month$month
fert_latest_day <- fert_summary_month$day
fert_latest_avg_cows_calved <- fert_summary_month$cows_calved_mean_m
fert_latest_avg_heif_calved <-
  fert_summary_month$heifers_calved_mean_m
fert_latest_avg_services <- fert_summary_month$services_mean_m
fert_latest_avg_ovsynch <- fert_summary_month$ovsynch_mean_m
fert_latest_avg_cows_pd <- fert_summary_month$cows_pd_mean_m
fert_latest_avg_heif_pd <- fert_summary_month$heifers_pd_mean_m
fert_latest_avg_dry_off <- fert_summary_month$dry_off_mean_m
fert_latest_avg_dry_tubes <- fert_summary_month$dry_tubes_mean_m

fert_latest_tot_cows_calved <- fert_summary_month$cows_calved_sum_m
fert_latest_tot_heif_calved <-
  fert_summary_month$heifers_calved_sum_m
fert_latest_tot_services <- fert_summary_month$services_sum_m
fert_latest_tot_ovsynch <- fert_summary_month$ovsynch_sum_m
fert_latest_tot_cows_pd <- fert_summary_month$cows_pd_sum_m
fert_latest_tot_heif_pd <- fert_summary_month$heifers_pd_sum_m
fert_latest_tot_dry_off <- fert_summary_month$dry_off_sum_m
fert_latest_tot_dry_tubes <- fert_summary_month$dry_tubes_sum_m

### health data ----

# latest month summary boxes
health_data <-
  fert_health_data %>%
  select(c(date, culls_exit:rfm_health)) %>%
  mutate(month = month(ymd(date), label = TRUE), .after = date) %>%
  mutate(year = year(ymd(date)), .after = date) %>%
  mutate(day = day(ymd(date)), .after = date)


health_summary <-
  health_data %>%
  group_by(year, month, day) %>%
  arrange(year, month, day) %>%
  group_by(year, month) %>%
  mutate(total_exit = culls_exit + deaths_exit) %>%
  summarise(
    day = max(day),
    total_exit = total_exit[1],
    avg_culls = mean(culls_exit, na.rm = TRUE),
    avg_deaths = mean(deaths_exit, na.rm = TRUE),
    avg_mastitis = mean(mastitis_health, na.rm = TRUE),
    avg_milk_fever = mean(milk_fever_health, na.rm = TRUE),
    avg_da = mean(da_health, na.rm = TRUE),
    avg_cystic = mean(cystic_health, na.rm = TRUE),
    avg_treatments = mean(treatments_health, na.rm = TRUE),
    avg_dirty_pnc = mean(dirty_pnc_health, na.rm = TRUE),
    avg_lameness = mean(lamenesss_health, na.rm = TRUE),
    avg_rfm = mean(rfm_health, na.rm = TRUE),
    tot_culls = sum(culls_exit, na.rm = TRUE),
    tot_deaths = sum(deaths_exit, na.rm = TRUE),
    tot_mastitis = sum(mastitis_health, na.rm = TRUE),
    tot_milk_fever = sum(milk_fever_health, na.rm = TRUE),
    tot_da = sum(da_health, na.rm = TRUE),
    tot_cystic = sum(cystic_health, na.rm = TRUE),
    tot_treatments = sum(treatments_health, na.rm = TRUE),
    tot_dirty_pnc = sum(dirty_pnc_health, na.rm = TRUE),
    tot_lameness = sum(lamenesss_health, na.rm = TRUE),
    tot_rfm = sum(rfm_health, na.rm = TRUE)
  ) %>%
  ungroup() 


health_summary_month <- health_summary %>%
  filter(!is.na(total_exit)) %>%
  filter(year == max(year)) %>%
  filter(month == max(month)) 

health_latest_year <- health_summary_month$year
health_latest_month <- health_summary_month$month
health_latest_day <- health_summary_month$day

health_latest_exit <- health_summary_month$total_exit
health_latest_culls <- health_summary_month$tot_culls
health_latest_deaths <- health_summary_month$tot_deaths
health_latest_mastisis <- health_summary_month$tot_mastitis
health_latest_milk_fever <- health_summary_month$tot_milk_fever
health_latest_da <- health_summary_month$tot_da
health_latest_cystic <- health_summary_month$tot_cystic
health_latest_treatments <- health_summary_month$tot_treatments
health_latest_dirty_pnc <- health_summary_month$tot_dirty_pnc
health_latest_rfm <- health_summary_month$tot_rfm
health_latest_lameness <- health_summary_month$tot_lameness



