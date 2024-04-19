# # fert graphs 
# 
tot_cows_calved_plot_detail <- 
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = cows_calved_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of cows calved",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 



tot_cows_calved_plot_year <- 
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(cows_calved_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of cows calved in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
 
 avg_cows_calved_plot_month <- 
  fert_data %>% 
  group_by(year, month) %>%
  summarise(tot_v = sum(cows_calved_fert, na.rm = TRUE)) %>%
    ungroup() %>% 
    group_by(month) %>%
    summarise(
            mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of cows calved", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
# 
# 
tot_heif_calved_plot_detail <- 
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = heifers_calved_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of heifers calved",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
# 
tot_heif_calved_plot_year <-
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(heifers_calved_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of heifers calved in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
avg_heif_calved_plot_month <-
   fert_data %>% 
group_by(year, month) %>%
  summarise(tot_v = sum(heifers_calved_fert, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(month) %>%
  summarise(
    mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of heifers calved", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
tot_cows_pd_plot_detail <-
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = cows_pd_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of pregnant cows",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
# 
tot_cows_pd_plot_year <-
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(cows_pd_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of pregnant cows in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
avg_cows_pd_plot_month <-
fert_data %>% 
  group_by(year, month) %>%
  summarise(tot_v = sum(cows_pd_fert, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(month) %>%
  summarise(
    mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of pregnant cows", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

# 
tot_heif_pd_plot_detail <-
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = heifer_pd_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of pregnant heifers",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
# 
tot_heif_pd_plot_year <-
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(heifer_pd_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of pregnant heifers in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
avg_heif_pd_plot_month <-
fert_data %>% 
  group_by(year, month) %>%
  summarise(tot_v = sum(heifer_pd_fert, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(month) %>%
  summarise(
    mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of pregnant heifers", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
tot_services_plot_detail <-
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = services_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of services",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
# 
tot_services_plot_year <-
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(services_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of services in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
avg_services_plot_month <-
fert_data %>% 
  group_by(year, month) %>%
  summarise(tot_v = sum(services_fert, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(month) %>%
  summarise(
    mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of services", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
# 
tot_ovsynch_plot_detail <-
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = ovsynch_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of ovsynch",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
# 
tot_ovsynch_plot_year <-
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(ovsynch_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of ovsynch  in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
avg_ovsynch_plot_month <-
fert_data %>% 
  group_by(year, month) %>%
  summarise(tot_v = sum(ovsynch_fert, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(month) %>%
  summarise(
    mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of ovsynch", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
tot_dry_off_plot_detail <-
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = dry_off_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of dry off cows",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
# 
tot_dry_off_plot_year <-
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(dry_off_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of dry off cows in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
# 
avg_dry_off_plot_month <-
fert_data %>% 
  group_by(year, month) %>%
  summarise(tot_v = sum(dry_off_fert, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(month) %>%
  summarise(
    mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of dry off cows", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
 
# 
tot_dry_tubes_plot_detail <-
fert_data %>% 
  mutate(across(c(year), ~as.factor(.x))) %>%
  mutate(year_day = yday(date)) %>%
  ggplot(aes(x = year_day, y = dry_tubes_fert)) +
  geom_smooth(method = "loess", aes(colour = as.factor(year), fill = as.factor(year))) +
  labs(x = "Day of year", 
       y = "Number of dry tube cows",
       colour = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  theme_bw() +
  theme_m.rl 
# 
tot_dry_tubes_plot_year <-
fert_data %>% 
  group_by(year) %>%
  summarise(sum_v = round(sum(dry_tubes_fert, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = year, y = sum_v, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", 
       y = "Number of dry tube cows in year", 
       fill = "Year") +
  #scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_x_continuous(breaks = seq(2018, 2024, 1)) +
  scale_fill_manual(
    values = year_pal) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 

#
 avg_dry_tubes_plot_month <-
fert_data %>% 
  group_by(year, month) %>%
  summarise(tot_v = sum(dry_tubes_fert, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(month) %>%
  summarise(
    mean_v = round(mean(tot_v, na.rm = TRUE),0)) %>% 
  ggplot(aes(x = month, y = mean_v, fill = as.factor(month))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", 
       y = "Average number of dry tube cows", 
       fill = "Month") +
  # scale_y_continuous(expand = c(0,0), limits = c(0, 400), breaks = seq(0,375, 50)) +
  scale_fill_manual(
    values = cb_pal_2) + 
  # facet_wrap(vars(year)
  #            )+
  theme_bw() +
  theme_m.nl 
