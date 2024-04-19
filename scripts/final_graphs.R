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
# total yield
tot_yield_day_plot <- 
prod_data %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = milk_prod/1000)) +
  # geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "#0388A6", fill = "#0388A6") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Milk yield (Kilolitres)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 

#total_yield/weekly
tot_yield_week_plot <- 
yield_data %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x=date, y = weekly_yield/1000))+
 # geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "#0388A6", fill = "#0388A6") +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Weekly milk yield (Kilolitres)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 

# yield per cow
yield_per_cow_day_plot <- 
yield_data %>% 
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = milk_per_cow_prod)) +
  geom_smooth(method = "loess", color = "#0388A6", fill = "#0388A6") +
  # geom_point(alpha = 0.2) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "4 months") +
  labs(x = "\n Date", 
       y = "Milk yield per cow (Litres)") +
  theme_bw() +
  theme_m.tl +
  theme(
    axis.text.x = element_text(angle = 270)
  ) 


# yield per cow/weekly
yield_per_cow_week_plot <-
yield_data %>% 
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
mqual_data %>% 
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
mqual_data %>% 
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
mqual_data %>% 
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
mqual_data %>% 
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

# simple fertility graph
