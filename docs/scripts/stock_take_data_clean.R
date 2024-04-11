library(readxl)
library(lubridate)
library(tidyverse)

stock_take_data <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                              sheet = "Stock Take", # excel sheet name
                              col_names= TRUE) %>% # headings present
   mutate(across(c(ic_heifers_heifers,
                   bull_heifers,
                   bullinggroup_heifers,
                   m12m13_heifers,
                   m6m12_heifers,
                   m3m6_heifers,
                   heifers_calves,
                   beef_calves,
                   other,
                   total_head),
                 ~as.numeric(.x)))

 
# st_data_clean_tidy <- stock_take_data %>%
#   pivot_longer(-Date, names_to = c("type", "status"), names_sep = "_", values_to = "count") #%>% 
#   view()

