# health graphs 

# Total heath issues graph

# avgerage health issues in months

### exits df

exits <- 
health_data %>% 
  select(date:deaths_exit) %>%
pivot_longer(cols = c(culls_exit, deaths_exit), names_to = "exit_type", values_to = "exit_count" )


# total exits in year
tot_exit_year_plot <- 
exits %>% 
  mutate(across(c(year), ~as.factor(.x))) %>% 
  group_by(year, exit_type) %>%
  summarise( exit_tot = sum(exit_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = exit_tot, fill = year)) +
  geom_bar(stat= "identity") +
  labs(x = "Year", 
       y = "Number of recorded exits", 
       fill = "Year") +
  scale_y_continuous(breaks = seq(0, 150, 25)) +
  scale_fill_manual(
                    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
theme_m.nl 


# exit reasons
exit_reasons_year_plot <- 
exits %>% 
  mutate(across(c(year), ~as.factor(.x))) %>% 
  group_by(year, exit_type) %>%
  summarise( exit_tot = sum(exit_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = exit_tot, fill = exit_type)) +
  geom_bar(stat= "identity", position = "fill") +
  labs(x = "Year", 
       y = "Exit reason", 
       fill = "Year") +
  #scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(labels = c("Culls", "Deaths"),
                    limits = c("culls_exit", "deaths_exit"),
    values = exits_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.rl

# average exits in months
avg_exit_month_plot <- 
  exits %>% 
    mutate(across(c(year, month), ~as.factor(.x))) %>% 
    group_by(month, exit_type) %>%
    summarise( exit_avg = mean(exit_count, na.rm = TRUE)) %>%
    ggplot(aes(x = month, y = exit_avg, fill = month)) +
    geom_bar(stat= "identity") +
    labs(x = "Month", 
         y = "Avergae number of exits in a month", 
         fill = "Month") +
scale_fill_manual(values = cb_pal_2) +
    theme_bw() +
  theme_m.nl 
  
av_exit_reason_month_plot <- 
  exits %>% 
    mutate(across(c(year, month), ~as.factor(.x))) %>% 
    group_by(month, exit_type) %>%
    summarise( exit_avg = mean(exit_count, na.rm = TRUE)) %>%
    ggplot(aes(x = month, y = exit_avg, fill = exit_type)) +
    geom_bar(stat= "identity", position = "fill") +
    labs(x = "Month", 
         y = "Exit reason", 
         fill = "Month") +
    scale_fill_manual(labels = c("Culls", "Deaths"),
                      limits = c("culls_exit", "deaths_exit"),
                      values = exits_pal) + 
    theme_bw() +
  theme_m.rl
 


exits_plot_detail <-
  exits %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = exit_count)) +
  geom_line(aes(colour = exit_type)) +
  labs(x = "Day of year", 
       y = "Number of exits",
       colour = "Exit reason") +
    scale_colour_manual(labels = c("Culls", "Deaths"),
                      limits = c("culls_exit", "deaths_exit"),
                      values = exits_pal) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 15), breaks = seq(0,15, 5)) +
    facet_grid(rows = vars(year)) +
  theme_bw() +
  theme_m.rl 


#### treatment graphs



### issues 

issues <- 
  health_data %>% 
  select(date, day, year, month, 
         mastitis_health, milk_fever_health, da_health, cystic_health, 
         lamenesss_health, rfm_health) %>%
  pivot_longer(
    cols = c(mastitis_health, milk_fever_health, da_health, cystic_health,
             lamenesss_health, rfm_health), 
    names_to = "issue_type", 
    values_to = "issue_count" )


# total issues in year
tot_issues_year_plot <- 
  issues %>% 
  mutate(across(c(year), ~as.factor(.x))) %>% 
  group_by(year, issue_type) %>%
  summarise( issue_tot = sum(issue_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = issue_tot, fill = year)) +
  geom_bar(stat= "identity") +
  labs(x = "Year", 
       y = "Number of recorded health issues", 
       fill = "Year") +
 # scale_y_continuous(breaks = seq(0, 150, 25)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 


# issue causes
issues_cause_year_plot <- 
  issues %>% 
  mutate(across(c(year, issue_type), ~as.factor(.x))) %>% 
  group_by(year, issue_type) %>%
  summarise( issue_tot = sum(issue_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = issue_tot, fill = issue_type)) +
  geom_bar(stat= "identity", position = "fill") +
  labs(x = "Year", 
       y = "Proportion of total health issues recorded", 
       fill = "Year") +
  #scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(labels = c("Cystic", "DA", "Lameness", "Mastisis", "Milk Fever", "RFM"),
                    limits = c("cystic_health", "da_health", "lamenesss_health", 
                               "mastitis_health", "milk_fever_health", "rfm_health"),
                    values = issues_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.rl




# average exits in months
avg_issues_month_plot <- 
  issues %>% 
  mutate(across(c(year, month), ~as.factor(.x))) %>% 
  group_by(month, issue_type) %>%
  summarise( issue_avg = mean(issue_count, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = issue_avg, fill = month)) +
  geom_bar(stat= "identity") +
  labs(x = "Month", 
       y = "Average number of health issues in a month", 
       fill = "Month") +
  scale_fill_manual(values = cb_pal_2) +
  theme_bw() +
  theme_m.nl 

av_issues_cause_month_plot <- 
  issues %>% 
  mutate(across(c(year, month), ~as.factor(.x))) %>% 
  group_by(month, issue_type) %>%
  summarise( issue_avg = mean(issue_count, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = issue_avg, fill = issue_type)) +
  geom_bar(stat= "identity", position = "fill") +
  labs(x = "Month", 
       y = "Proportion of  health issues recorded", 
       fill = "Health issue") +
  scale_fill_manual(labels = c("Cystic", "DA", "Lameness", "Mastisis", "Milk Fever", "RFM"),
                    limits = c("cystic_health", "da_health", "lamenesss_health", 
                               "mastitis_health", "milk_fever_health", "rfm_health"),
                    values = issues_pal_2) + 
  theme_bw() +
  theme_m.rl



issues_plot_detail <-
  issues %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = issue_count)) +
  geom_line(aes(colour = issue_type), linewidth = 1) +
  labs(x = "Day of year", 
       y = "Number of health issues recorded",
       colour = "Health issue") +
  scale_colour_manual(labels = c("Cystic", "DA", "Lameness", "Mastisis", "Milk Fever", "RFM"),
                    limits = c("cystic_health", "da_health", "lamenesss_health", 
                               "mastitis_health", "milk_fever_health", "rfm_health"),
                    values = issues_pal_2) + 
  scale_y_continuous(limits = c(0, 8), breaks = seq(0,8, 4)) +
  facet_grid(rows = vars(year)) +
  theme_bw() +
  theme_m.rl 
