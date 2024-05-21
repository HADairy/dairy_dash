# What tables have the information that we want
# NMR_CDL_Animal
## LINKAGES
## Herd
## animal
## dam_herd
## dam_animal
## sire

# NMR_CDL_Herd_Recording
## herd linking identifier


# NMR_CDL_Official_Milk_Test_Result
## herd
## animal

# NMR_CDL_Official_Vat_Result
## none

# NMR_CDL_Pregnancy_Check
## herd
## animal

# NMR_CDL_Insemination
## herd
## animal
## sire

# NMR_CDL_In_Heat
## none

# Creating useful table

# individual animal records:
## to include: 
### sex
### animal ID
### Breed
### arrived date
### left date

indiv_anim_records <- 
  NMR_CDL_Animal %>%
  select(Animal_Sex, Identification_Number, Animal_Breed, Date_Entered_Herd,Left_Herd_Date) %>%
  left_join(NMR_CDL_Breed %>%
              select(Animal_Breed, Breed_Name), by = "Animal_Breed") %>% 
  select(-Animal_Breed) %>% 
  relocate(Breed_Name, .after = "Identification_Number") %>% 
  glimpse()

# test results table
## to include:
### date samples
### date tested
### test type
### sample_type
### collected by 
### sample ref
### result
### classification
### test batch
### test facility
# unique test ID
milk_sampling_table <-
NMR_CDL_Official_Vat_Result %>%
  select(Milk_Sampling_Date, Milk_Test_Date, Urea_Percentage_Milk_Herd, 
         Percent_Protein_Herd, Percent_Lactose_Herd, Percent_Fat_Herd, Somatic_Cell_Count_Herd, 
         Vat_Number) %>%
  rename(Urea_percent = Urea_Percentage_Milk_Herd, 
         Protien_percent= Percent_Protein_Herd, 
         Lactose_percent= Percent_Lactose_Herd, 
         Fat_percent = Percent_Fat_Herd, 
         SC_count = Somatic_Cell_Count_Herd) %>%
  pivot_longer(cols= c(Urea_percent, 
                       Protien_percent, 
                       Lactose_percent, 
                       Fat_percent, 
                       SC_count),
               names_to = "Test_Type",
               values_to = "Result")


# production_table <- 
NMR_CDL_Herd_Recording %>%
  select(Date_Of_Milking_Session, Cows_In_Herd_Total, Cows_In_Milk_Total,
         # need to find cows and heifers calved, cows in milk bought in, cows sold as commercial,
         #total culled&died,
         Bulk_Fat_Percent, Bulk_Protein_Percent, Bulk_Lactose_Percent, 
         Bulk_Somatic_Cell_Count, Bulk_Milk_Weight
         # need to find milk urea, milk check after deductions,milk price for month
         # milk sold in month, milk retained on farm - est no of cows retained on farm 
         )




NMR_CDL_Animal_Event %>% 
  left_join(event_codes, by = c("Animal_Event_Code" = "Event_Code")) 
