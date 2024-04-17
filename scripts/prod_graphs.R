#### production graphs  

### summary graphs ----

##  cows in milk graphs

# year averages 
year_av_in_milk_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_in_milk = round(mean(cows_in_milk_prod, na.rm = TRUE),0),
            sd_in_milk = sd(cows_in_milk_prod, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_in_milk, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of cows in milk", 
       fill = "Year") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_in_milk_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_in_milk = round(mean(cows_in_milk_prod, na.rm = TRUE),0),
            sd_in_milk = sd(cows_in_milk_prod, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = mean_in_milk, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Number of cows in milk", 
       fill = "Month") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


## cows in tank graph 
# year averages 
year_av_in_tank_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_in_tank = round(mean(cows_in_tank_prod, na.rm = TRUE),0),
            sd_in_tank = sd(cows_in_tank_prod, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean_in_tank, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of cows in tank", 
       fill = "Year") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_in_tank_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_in_tank = round(mean(cows_in_tank_prod, na.rm = TRUE),0),
            sd_in_tank = sd(cows_in_tank_prod, na.rm = TRUE)) %>% 
  ggplot(aes(x = month, y = mean_in_tank, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Number of cows in tank", 
       fill = "Month") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

## total milk prod 
# year averages 
year_av_milk_prod_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_v = round(mean(milk_prod, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = mean_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Average milk production per day (Litres)", 
       fill = "Year") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_milk_prod_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_v = round(mean(milk_prod, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average milk production per day (Litres)", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

## milk_prod_day_cow
# year averages 
year_av_milk_prod_cow_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_v = round(mean(milk_per_cow_prod, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = mean_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Average milk production per cow \nper day (Litres)", 
       fill = "Year") +
 # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

# year total 
year_tot_milk_prod_cow_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(total_v = round(sum(milk_per_cow_prod, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = total_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Total milk production per cow \nper day (Litres)", 
       fill = "Year") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_milk_prod_cow_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_v = round(mean(milk_per_cow_prod, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average milk production per cow \nper day (Litres)", 
       fill = "Month") +
 # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

# month total 
month_tot_milk_prod_cow_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(total_v = round(sum(milk_per_cow_prod, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = total_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Total milk production per cow \nper day (Litres)", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

## HSCC
# year averages 
year_av_hscc_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_v = round(mean(HSCC_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = mean_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "High Somatic Cell Count (unit?)", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_hscc_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_v = round(mean(HSCC_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "High Somatic Cell Count (unit?)", 
       fill = "Month") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 




## SCC
# year averages 
year_av_scc_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_v = round(mean(SCC_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = mean_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Somatic Cell Count (unit?)", 
       fill = "Year") +
 # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_scc_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_v = round(mean(SCC_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Somatic Cell Count (unit?)", 
       fill = "Month") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


## BS
# year averages 
year_av_bs_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_v = round(mean(BS_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = mean_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Bactoscan (unit?)", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_bs_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_v = round(mean(BS_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "BactoScan (unit?)", 
       fill = "Month") +
 # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 



## Fat
# year averages 
year_av_fat_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_v = round(mean(Fat_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = mean_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Fat (unit?)", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_fat_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_v = round(mean(Fat_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Fat (unit?)", 
       fill = "Month") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


## Protein
# year averages 
year_av_protein_plot <- 
prod_data %>% 
  group_by(year) %>%
  summarise(mean_v = round(mean(Protein_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = mean_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Protein (unit?)", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# month averages 
month_av_protein_plot <- 
prod_data %>% 
  group_by(month) %>%
  summarise(mean_v = round(mean(Protein_mqual, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Protein (unit?)", 
       fill = "Month") +
 # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

## detail graphs ----

# cows in milk
in_milk_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = cows_in_milk_prod, colour = year)) +
  geom_line(aes(colour = year)) +
  labs(x = "Day of year", 
       y = "Number of cows in milk",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

#cows in tank
in_tank_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = cows_in_tank_prod, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of cows in tank",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

#total  daily milk prod
milk_prod_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = milk_prod/100, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Total milk production per day (Decalitres)",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

# daily milk per cow
milk_prod_cow_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = milk_per_cow_prod, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Total milk production per \n day per cow (Litres)",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

# hscc
hscc_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = HSCC_mqual, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "High Somatic Cell Count (units?)",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

# scc
scc_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = SCC_mqual, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Somatic Cell Count (units?)",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

# bs
bs_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = BS_mqual, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "BactoScan (units?)",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

# fat
fat_detail_plot <- 
prod_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = Fat_mqual, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Fat (units?)",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 

# protien
protein_detail_plot <- 
prod_data %>% 
  filter(Protein_mqual < 200) %>%
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date), .after = year) %>% 
  ggplot(aes(x = year_day, y = Protein_mqual, colour = as.factor(year))) +
  geom_line(aes(colour = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Protien (units?)",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
