# writing excel workbook for download
library(writexl)

write_xlsx(
  
)
 ?write_xlsx
write_xlsx(list(stock_take = as.data.frame(stock_take_data), 
                             production = as.data.frame(prod_data), 
                             fertility_and_health = as.data.frame(fert_health_data), 
                             data_dictionary = as.data.frame(NULL)), path = "C:/Users/00776360/OneDrive - Harper Adams University/github_repos/dairy_dash/HA_dairy_master.xlsx")
