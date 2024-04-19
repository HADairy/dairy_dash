# final graphs to use 
stock_take_graph <- 
  milk_stat %>% 
  mutate(date = as.Date(date_p)) %>%
  ggplot(aes(x = date, y = count))+
  # geom_area(aes(x = date, y = total_head), alpha = 0.2)+
  stat_smooth(geom = "area", aes(x = date, y = total_head), alpha = 0.2, method = "loess") +
  # geom_line(aes(colour = status)) +
  geom_smooth(aes(colour = status, fill = status, linetype = status), method = "loess", se = FALSE)+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  scale_colour_manual("Cow status",labels = c("In milk", "In tank", "Dry"),
                        limits = c("cows_in_milk_prod", "cows_in_tank_prod", "total_dry"),
                        values = status_pal
  ) +
  scale_fill_manual("Cow status",labels = c("In milk", "In tank", "Dry"),
                      limits = c("cows_in_milk_prod", "cows_in_tank_prod", "total_dry"),
                      values = status_pal
  ) +
  scale_linetype_discrete("Cow status", 
                          labels = c("In milk", "In tank", "Dry"),
                          limits = c("cows_in_milk_prod", "cows_in_tank_prod", "total_dry"))+
   labs(x = "\n Date", 
        y = "Number of cows",
) +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 


# simple yield graphs


# yield per cow
yield_per_cow_day_plot <- 
yield_data %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = milk_per_cow_prod)) +
 # geom_smooth(method = "loess", color = "#0388A6", fill = "#0388A6") +
  stat_smooth(geom = "smooth", method = "loess",
              se = TRUE, level = 0.95, 
              show.legend = TRUE,
              color = "#0388A6", fill = "#0388A6") +
  #geom_point(alpha = 0.2) +
  scale_y_continuous(limits = c(25,35)) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Milk yield per cow (Litres)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 


# simple milk quality graph

daily_fat_plot <-
  mqual_data_daily %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = Fat_mqual)) +
    stat_smooth(geom = "smooth", method = "loess",
                se = TRUE, level = 0.95, 
                show.legend = TRUE,
                color = "#F29E38", fill = "#F29E38", linetype = 3) +
  #geom_point() +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
    #geom_smooth(inherit.aes = FALSE, data = mq_bench, aes(x = date, y = `Butterfat\r\n(%)`)) +
    scale_y_continuous(limits = c(3,6)) +
  labs(x = "\n Date", 
       y = "Butterfat (%) ") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 
  
daily_protein_plot <- 
  mqual_data_daily %>% 
  mutate(date = as.Date(date)) %>%
    filter(Protein_mqual < 250) %>%
  ggplot(aes(x=date, y = Protein_mqual)) +
    stat_smooth(geom = "smooth", method = "loess",
                se = TRUE, level = 0.95, 
                show.legend = TRUE,
                color = "#F29E38", fill = "#F29E38", linetype = 2) +
    #geom_smooth(inherit.aes = FALSE, data = mq_bench, aes(x = date, y = `Protein\r\n(%)`)) +
   # geom_point()+
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  scale_y_continuous(limits = c(3.0,3.5)) +
  labs(x = "\n Date", 
       y = "Protein (%)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 

# simple fertility graph


## health plots 


exits <- 
  health_data %>% 
  select(date:deaths_exit) %>%
  pivot_longer(cols = c(culls_exit, deaths_exit), names_to = "exit_type", values_to = "exit_count" )

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



exit_reasons_prop_plot <- 
  exits %>% 
  mutate(across(c(year), ~as.factor(.x))) %>% 
  group_by(year, exit_type) %>%
  summarise( exit_tot = sum(exit_count, na.rm = TRUE)) %>% ungroup() %>% 
  ggplot(aes(x = year, y = exit_tot )) +
  geom_bar(stat= "identity", position = "fill", aes(fill = exit_type) )+
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
  theme_m.tl

exit_count_plot <- 
  exits %>% 
  mutate(across(c(year), ~as.factor(.x))) %>% 
  group_by(year, exit_type) %>%
  summarise( exit_tot = sum(exit_count, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = exit_tot )) +
  geom_bar(stat= "identity", position = "stack", aes(fill = exit_type))+
  labs(x = "Year", 
       y = "Number of exits", 
       fill = "Exit reason") +
  #scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(labels = c("Culls", "Deaths"),
                    limits = c("culls_exit", "deaths_exit"),
                    values = exits_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.tl

  
issues_cause_prop_plot <- 
  issues %>% 
  mutate(across(c(year, issue_type), ~as.factor(.x))) %>% 
  group_by(year, issue_type) %>%
  summarise( issue_tot = sum(issue_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = issue_tot, fill = issue_type)) +
  geom_bar(stat= "identity", position = "fill") +
  labs(x = "Year", 
       y = "Proportion of total health issues recorded", 
       fill = "Health issue") +
  #scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(labels = c("Cystic", "DA", "Lameness", "Mastisis", "Milk Fever", "RFM"),
                    limits = c("cystic_health", "da_health", "lamenesss_health", 
                               "mastitis_health", "milk_fever_health", "rfm_health"),
                    values = issues_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.tl


issues_cause_count_plot <- 
  issues %>% 
  mutate(across(c(year, issue_type), ~as.factor(.x))) %>% 
  group_by(year, issue_type) %>%
  summarise( issue_tot = sum(issue_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = issue_tot, fill = issue_type)) +
  geom_bar(stat= "identity", position = "stack") +
  labs(x = "Year", 
       y = "Number of health issues recorded", 
       fill = "Health issue") +
  #scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(labels = c("Cystic", "DA", "Lameness", "Mastisis", "Milk Fever", "RFM"),
                    limits = c("cystic_health", "da_health", "lamenesss_health", 
                               "mastitis_health", "milk_fever_health", "rfm_health"),
                    values = issues_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.tl


#### fertility
fert_events_plot <- 
fh_data %>% 
  mutate(date_date = as.Date(date), 
         year = as.factor(year)) %>%
  ggplot(aes(x = date_date, y = Count, colour = fert_measure, fill = fert_measure)) + 
  # geom_bar(stat="identity", position = "dodge")
  geom_smooth(method = "loess") +
 # geom_point() +
  #geom_vline(aes(xintercept = (date_date[1] + 281)), linetype=2, size = 1) +
  scale_fill_manual("Fertility Events",labels = c("Calvings", "+ Pregnancies", "Services"),
                    limits = c("calves", "pregs", "services_fert"),
                    values = fert_pal) +
  scale_colour_manual("Fertility Events", labels = c("Calvings", "+ Pregnancies", "Services"),
                      limits = c("calves", "pregs", "services_fert"),
                      values = fert_pal) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  # facet_grid(rows = vars(fert_measure), 
  #            scales = "free_y") +
  
  #scale_y_continuous(limits = c(3.0,3.5)) +
  labs(x = "\n Date", 
       y = "Count") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 


###### OTHER GRAPHS ------
  # yield per cow/weekly
  yield_per_cow_week_plot <-
    
  weekly_yield_data %>% 
    mutate(date = as.Date(date)) %>%
    ggplot(aes(x=date, y = weekly_yield_per_cow))+
    geom_smooth(method = "loess", color = "#0388A6", fill = "#0388A6") +
    # geom_point(alpha = 0.2) +
    scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
    labs(x = "\n Date", 
         y = "Weekly milk yield pr cow (Litres)") +
    theme_bw() +
    theme_m.tl +
    theme(
      axis.text.x = element_text(angle = 270)
    ) 
  # yield per cow/weekly
  yield_per_cow_week_plot <-
    weekly_yield_data %>% 
    mutate(date = as.Date(date)) %>%
    ggplot(aes(x=date, y = weekly_yield_per_cow))+
    geom_smooth(method = "loess", color = "#0388A6", fill = "#0388A6") +
    # geom_point(alpha = 0.2) +
    scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
    labs(x = "\n Date", 
         y = "Weekly milk yield pr cow (Litres)") +
    theme_bw() +
    theme_m.tl +
    theme(
      axis.text.x = element_text(angle = 270)
    ) 
  

# simple milk quality graph
week_av_scc_plot <-
  mqual_data_weekly %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = scc)) +
  geom_smooth(method = "loess", color = "#F29E38", fill = "#F29E38", linetype = 1) +
  geom_hline(yintercept = 200, colour = "darkred", linetype = 5) +
  geom_hline(yintercept = 100, colour = "darkgreen", linetype = 5) +
  annotate(geom = "text", label = "Abnormal SCC - High", y = 210, x = as.Date("2022-08-01"), color = "darkred") +
  annotate(geom = "text", label = "Status unknown", y = 180, x = as.Date("2018-08-01"), color = "darkgrey") +
  annotate(geom = "text", label = "Normal SCC", y = 90, x = as.Date("2018-08-01"), color = "darkgreen") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Mean weekly Somatic Cell Count L ") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 

week_av_bs_plot <-
  mqual_data_weekly %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = bs)) +
  geom_smooth(method = "loess", color = "#F29E38", fill = "#F29E38", linetype = 2) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Mean weekly Bactoscan score (unit?)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 
week_av_fat_plot <-
  mqual_data_weekly %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = fat)) +
  geom_smooth(method = "loess", color = "#F29E38", fill = "#F29E38", linetype = 3) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Mean weekly butterfat (unit?) ") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 
week_av_protein_plot <- 
  mqual_data_weekly %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = protein)) +
  geom_smooth(method = "loess", color = "#F29E38", fill = "#F29E38", linetype = 4) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Mean weekly protein (unit?)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 

daily_scc_plot <-
  mqual_data_daily %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = scc)) +
  geom_smooth(method = "loess", color = "#F29E38", fill = "#F29E38", linetype = 1) +
  geom_hline(yintercept = 200, colour = "darkred", linetype = 5) +
  geom_hline(yintercept = 100, colour = "darkgreen", linetype = 5) +
  annotate(geom = "text", label = "Abnormal SCC - High", y = 210, x = as.Date("2022-08-01"), color = "darkred") +
  annotate(geom = "text", label = "Status unknown", y = 180, x = as.Date("2018-08-01"), color = "darkgrey") +
  annotate(geom = "text", label = "Normal SCC", y = 90, x = as.Date("2018-08-01"), color = "darkgreen") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Mean weekly Somatic Cell Count L ") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 
daily_bs_plot <-
  mqual_data_daily %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = bs)) +
  geom_smooth(method = "loess", color = "#F29E38", fill = "#F29E38", linetype = 2) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Mean weekly Bactoscan score (unit?)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270))
