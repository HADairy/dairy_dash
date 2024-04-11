# Fertility and health data prep

library(readxl)
library(lubridate)
library(tidyverse)

## Edited in excel
## Cystic was an option on later data so added in a column for all years
## same for lameness and RFM
## made headings R friendly and incorporated master headings into var names
## removed repeated heading and summary rows 

fert_health_data <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                              sheet = "Fertility", # excel sheet name
                              col_names= TRUE) %>% # headings present
  mutate(across(c(cows_pd_fert,
                  heifer_pd_fert, 
                  dry_tubes_fert,
                  dirty_pnc_health),
                ~as.numeric(.x)))
