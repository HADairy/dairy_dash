# data_prep

my_comma <-
  scales::label_comma(accuracy = .1,
                      big.mark = ",",
                      decimal.mark = ".")



# production data 
# prod_data
# glimpse(prod_data)
# create new variables for month and year
prod_data <-  prod_data %>% 
   mutate(month = month(ymd(date), label = TRUE), .after = date) %>% 
   mutate(year = year(ymd(date)), .after = date) 

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
   summarise(avg_in_milk_month = round(mean(cows_in_milk_prod, na.rm = TRUE), 0),
             avg_in_tank_month = round(mean(cows_in_tank_prod, na.rm = TRUE),0),
             avg_milk_prod_month = mean(milk_prod, na.rm = TRUE),
             avg_milk_prod_cow_month = mean(milk_per_cow_prod, na.rm = TRUE),
             avg_hscc_month = mean(HSCC_mqual, na.rm = TRUE), 
             avg_scc_month = mean(SCC_mqual, na.rm = TRUE), 
             avg_bs_month = mean(BS_mqual, na.rm = TRUE),
             avg_fat_month = mean(Fat_mqual, na.rm = TRUE),
             avg_protein_month = mean(Protein_mqual, na.rm = TRUE)) %>%
   ungroup() %>% 
   filter(!is.na(avg_in_milk_month)) %>%
   filter(year == max(year)) %>% 
   filter(month == max(month)) 
 
 latest_year <- latest_month_summary$year
 latest_month <- latest_month_summary$month
 latest_inmilk_av <- latest_month_summary$avg_in_milk_month
 latest_intank_av <- latest_month_summary$avg_in_tank_month
 latest_milk_av <- round(latest_month_summary$avg_milk_prod_month, 2)
 latest_milk_cow_av <- round(latest_month_summary$avg_milk_prod_cow_month, 2)
 latest_hscc_av <- round(latest_month_summary$avg_hscc_month, 2)
 latest_scc_av <- round(latest_month_summary$avg_scc_month, 2)
 latest_bs_av <- round(latest_month_summary$avg_bs_month, 2)
 latest_fat_av <- round(latest_month_summary$avg_fat_month, 2)
 latest_protien_av <- round(latest_month_summary$avg_protein_month, 2)
 
 


   
 
 
 
 
 
 

 
 
 