# Production data prep

library(readxl)
library(lubridate)
library(tidyverse)

#edited in excel
## Robot, heifer, trail, trial 1, trail 2, fresh options added across all years 
## There were three different trial headings, trial, trial 1 and trial 2, but trial and trial 1 weren't used together - unclear as to whether these were different trials or just names differently, so for now have treated them as separate and added a "trial" column
## added a target diff column to all years 
## removed duplicate headings
## removed empty space
## created r friendly headings to remove subheadings 
## removed comments and or merged cells 
## matched all column orders 

prod_18 <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                     sheet = "Production 2018", # excel sheet name
                     col_names= TRUE) %>% # headings present
mutate(across(c(target_diff_prod, 
                avg_milk_cow_day_trial, 
                avg_milk_cow_day_trial2, 
                avg_milk_cow_day_heifers,
                avg_milk_cow_day_fresh,
                HSCC_mqual), 
              ~ as.numeric(.x)))

prod_19 <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                     sheet = "Production 2019", # excel sheet name
                     col_names= TRUE) %>% # headings present
mutate(across(c(target_diff_prod, 
                avg_milk_cow_day_strawyard, 
                avg_milk_cow_day_trial,
                avg_milk_cow_day_robot,
                avg_milk_cow_day_heifers,
                avg_milk_cow_day_fresh), 
              ~ as.numeric(.x)))

prod_20 <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                     sheet = "Production 2020", # excel sheet name
                     col_names= TRUE) %>% # headings present
mutate(across(c(target_diff_prod, 
                avg_milk_cow_day_strawyard, 
                avg_milk_cow_day_trial1, 
                avg_milk_cow_day_robot, 
                avg_milk_cow_day_fresh,
                HSCC_mqual), 
              ~ as.numeric(.x)))

prod_21 <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                     sheet = "Production 2021", # excel sheet name
                     col_names= TRUE) %>% # headings present
mutate(across(c(target_diff_prod, 
                avg_milk_cow_day_trial1, 
                avg_milk_cow_day_trial2, 
                avg_milk_cow_day_robot, 
                avg_milk_cow_day_fresh,
                HSCC_mqual), 
              ~ as.numeric(.x)))

prod_22 <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                     sheet = "Production 2022", # excel sheet name
                     col_names= TRUE) %>% # headings present
mutate(across(c(target_diff_prod, 
                avg_milk_cow_day_trial1, 
                avg_milk_cow_day_trial2, 
                avg_milk_cow_day_robot, 
                HSCC_mqual), 
              ~ as.numeric(.x)))

prod_23 <- read_xlsx("dairy_edit.xlsx", # excel doc/workbook name
                     sheet = "Production 2023", # excel sheet name
                     col_names= TRUE) %>% # headings present
mutate(across(c(target_diff_prod, 
                avg_milk_cow_day_strawyard, 
                avg_milk_cow_day_trial1, 
                avg_milk_cow_day_trial2, 
                avg_milk_cow_day_low, 
                avg_milk_cow_day_robot, 
                HSCC_mqual), 
              ~ as.numeric(.x)))
       
       
prod_data <- bind_rows(prod_18, 
                       prod_19, 
                       prod_20, 
                       prod_21, 
                       prod_22, 
                       prod_23)
