
## Read in data ----
data <- as.data.frame( # convert to dataframe as read in
  x=readLines("CDL.Dat") # read in lines from CDL. dat file
  ) %>%
  rename( data_col = `readLines("CDL.Dat")`) # rename varible as read in 

# view(data) # view read in data frame


# reading in other datframes of importance
event_codes <- read_excel("data_dictionary.xlsx", sheet = "DD_NRM_event_codes") %>%
  rename(Event_Code = `Event code`) %>% 
  mutate(across(c(Event_Code), ~ as.factor(.x)))


# DD_Breed_code
# DD_CDL_Transaction_codes
# DDAnimal_reg_types
# DD_NMR_Breed_society_address_no
# DD_NMR_Country_branch_codes
## Create separate tables for each NRM table  ----


# 91001	NMR_CDL_Animal_Registration  ----
# 11 variables
NMR_CDL_Animal_Registration <-
  data %>% 
  filter(str_starts(data_col, "091001")) %>%
  separate(col = data_col, into= c("ADED_Number",
                                   "Identification_Number",
                                   "Registration_Number",
                                   "Pedigree_Status",
                                   "Registration_Authority",
                                   "Registration_Type",
                                   "Registration_Date",
                                   "Registration_Expired_Date",
                                   "Herd_Linkage_Identifier",
                                   "Animal_Linkage_Identifier",
                                   "Animal_Breed"
                                   ), 
           sep = ",") %>%
  mutate(across(c(Pedigree_Status, Registration_Type, Animal_Breed), ~as.factor(.x)),
         across(c(Herd_Linkage_Identifier, Animal_Linkage_Identifier), ~ as.integer(.x)),
         across(c(Registration_Date, Registration_Expired_Date), ~ as.Date(.x)))




# 91003	NMR_CDL_Official_Vat_Result ----
NMR_CDL_Official_Vat_Result <-
  data %>%
  filter(str_starts(data_col, "091003"))  %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Herd_Number",
                                 "Milk_Sampling_Date",
                                 "Vat_Number",
                                 "No_Test_Result_Code_Herd",
                                 "Milk_Test_Date",
                                 "Urea_Percentage_Milk_Herd",
                                 "Percent_Protein_Herd",
                                 "Somatic_Cell_Count_Herd",
                                 "Percent_Lactose_Herd",
                                 "Percent_Fat_Herd"
                                 ), 
         sep = ",") %>%
  mutate(across(c(Vat_Number), ~as.factor(.x)),
         across(c(Milk_Sampling_Date, 
                  Milk_Test_Date), ~ as.Date(.x, format = "%Y%m%d")),
         across(c(Urea_Percentage_Milk_Herd, 
                  Percent_Protein_Herd, 
                  Somatic_Cell_Count_Herd,
                  Percent_Lactose_Herd, 
                  Percent_Fat_Herd), ~ as.numeric(.x)))


# 91004	NMR_CDL_Version ----
NMR_CDL_Version <-
  data %>%
  filter(str_starts(data_col, "091004")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "CDL_Version_Number"
), sep = ",") 


# 91005	NMR_CDL_Animal_Event ----
NMR_CDL_Animal_Event <-
  data %>%
  filter(str_starts(data_col, "091005")) %>%
separate(col = data_col, into= c("ADED_Number",   
                                 "Animal_Number",
                                 "Animal_Event_Code",
                                 "Animal_Event_Date",
                                 "Herd_Number",
                                 "Identification_Number",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identitifier"
), sep = ",") %>%
mutate(across(c(Animal_Event_Code), ~as.factor(.x)),
       across(c(Animal_Number, Herd_Linkage_Identifier, Animal_Linkage_Identitifier), ~ as.integer(.x)),
       across(c(Animal_Event_Date), ~ as.Date(.x, format = "%y%m%d")))

# 91006	NMR_CDL_Herd ----
NMR_CDL_Herd <-
  data %>%
  filter(str_starts(data_col, "091006")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Herd_Number",
                                 "County_Branch_Code",
                                 "Recording_Sequence_Number",
                                 "National_Herd_Mark",
                                 "Predominant_Breed",
                                 "NMR_Field_Manager",
                                 "NMR_Superviser",
                                 "First_Milking_Time",
                                 "Second_Milking_Time",
                                 "Third_Milking_Time",
                                 "Map_Reference",
                                 "Farm_Contact_Name",
                                 "Contact_Telephone_Number",
                                 "Neighbouring_Herd_Indicator",
                                 "Divided_Herd_Indicator",
                                 "Genus_Consultant_Number",
                                 "Herd_Linkage_Identifier",
                                 "Recorder_Enrolement_Date",
                                 "Recorder_Office_Code"
), sep = ",") %>%
mutate(across(c(County_Branch_Code, Predominant_Breed, Recorder_Office_Code), ~as.factor(.x)),
       across(c(Recording_Sequence_Number, Herd_Linkage_Identifier), ~ as.integer(.x)),
       across(c(Recorder_Enrolement_Date), ~ as.Date(.x, format = "%Y%m%d")),
       #across(c(First_Milking_Time, Second_Milking_Time, Third_Milking_Time), ~ as.Date(.x, format = "%H%M%S")),
       across(c(NMR_Field_Manager, NMR_Superviser, Genus_Consultant_Number), ~ as.numeric(.x)))



# 91007	NMR_CDL_Herd_Address ----
NMR_CDL_Herd_Address <-
  data %>%
  filter(str_starts(data_col, "091007")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Herd_Number",
                                 "Herd_Address_Type",
                                 "Herd_Name_1",
                                 "Herd_Name_2",
                                 "Herd_Address_1",
                                 "Herd_Address_2",
                                 "Herd_Address_3",
                                 "Herd_Address_4",
                                 "Herd_Post_Code",
                                 "Herd_Telephone_Number",
                                 "Herd_Linkage_Identifier"
), sep = ",") %>%
mutate(across(c(Herd_Address_Type), ~as.factor(.x)),
       across(c(Herd_Linkage_Identifier), ~ as.integer(.x)))

# 91008	NMR_CDL_Herd_Recording ----
NMR_CDL_Herd_Recording <-
  data %>%
  filter(str_starts(data_col, "091008")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Herd_Number",
                                 "Date_Of_Milking_Session",
                                 "Milk_Weight_Herd",
                                 "Cows_In_Herd_Total",
                                 "Youngstock_In_Herd_Total",
                                 "Cows_In_Milk_Total",
                                 "Milk_Weight_Used_On_Farm",
                                 "Herd_Linkage_Identifier",
                                 "Bulk_Fat_Percent",
                                 "Bulk_Protein_Percent",
                                 "Bulk_Lactose_Percent",
                                 "Bulk_Somatic_Cell_Count",
                                 "Total_Cows_3x_Milking",
                                 "Total_Fat_Weight",
                                 "Total_Lactose_Weight",
                                 "Total_Protein_Weight",
                                 "Total_Somatic_Cell_Count_Milk_Weight",
                                 "Total_Somatic_Cell_Count",
                                 "Bulk_Milk_Weight",
                                 "Farmer_Own_Recording_Indicator"
), sep = ",") %>%
mutate(across(c(Farmer_Own_Recording_Indicator), ~as.factor(.x)),
       across(c(Cows_In_Herd_Total, Youngstock_In_Herd_Total, 
                Cows_In_Milk_Total, Herd_Linkage_Identifier,
                Total_Cows_3x_Milking), ~ as.integer(.x)),
       across(c(Date_Of_Milking_Session), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Milk_Weight_Herd, Milk_Weight_Used_On_Farm,
                Bulk_Fat_Percent, Bulk_Protein_Percent, Bulk_Lactose_Percent,
                Bulk_Somatic_Cell_Count, Total_Fat_Weight, Total_Lactose_Weight,
                Total_Protein_Weight, Total_Somatic_Cell_Count_Milk_Weight, Total_Somatic_Cell_Count,
                Bulk_Milk_Weight), ~ as.numeric(.x)))

# 91009	NMR_CDL_Milking_Session_Recorder ----
NMR_CDL_Milking_Session_Recorder <-
  data %>%
  filter(str_starts(data_col, "091009")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Herd_Number",
                                 "Date_Of_Milking_Session",
                                 "Time_Of_Milking_Session",
                                 "Milking_Session_Recorder_Type",
                                 "Milking_Session_Recorder_Number",
                                 "Herd_Linkage_Identifier"
), sep = ",") %>%
mutate(across(c(Herd_Linkage_Identifier, Milking_Session_Recorder_Number), ~ as.integer(.x)),
       across(c(Date_Of_Milking_Session), ~ as.Date(.x, format = "%Y%m%d")),
       #across(c(Time_Of_Milking_Session), ~ as.Date(.x, format = "%H%M%S")),
       )

# 91010	NMR_CDL_Sire_Identity ----
NMR_CDL_Sire_Identity <-
  data %>%
  filter(str_starts(data_col, "091010")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Primary_Sire_Breed",
                                 "Primary_Sire_Registration_Number",
                                 "Registration_Breed_Sire",
                                 "Registration_Number_Sire",
                                 "Sire_Identifier_Type_Code",
                                 "Short_Name_Sire",
                                 "Long_Name_Sire",
                                 "AI_Short_Name_Sire",
                                 "Sire_Linkage_Identifier",
                                 "Sire_Linkage_Occurence"
), sep = ",") %>%
mutate(across(c(Primary_Sire_Breed, Registration_Breed_Sire, 
                Sire_Identifier_Type_Code, Sire_Linkage_Occurence), ~as.factor(.x)),
       across(c(Sire_Linkage_Identifier), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))
 
# 91011	NMR_CDL_Herd_Recording_Vat_Total ----
NMR_CDL_Herd_Recording_Vat_Total <-
  data %>%
  filter(str_starts(data_col, "091011")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Herd_Number",
                                 "Date_Of_Milking_Session",
                                 "Vat_Number",
                                 "Vat_Milk_Volume"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(Vat_Number), ~ as.integer(.x)),
       across(c(Date_Of_Milking_Session), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Vat_Milk_Volume), ~ as.numeric(.x)))

# 91014	NMR_CDL_Animal_Registration_Request ----
NMR_CDL_Animal_Registration_Request <-
  data %>%
  filter(str_starts(data_col, "091014")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Identification_Number",
                                 "Registration_Authority",
                                 "Registration_Type"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91015	NMR_CDL_Sire ----
NMR_CDL_Sire <-
  data %>%
  filter(str_starts(data_col, "091015")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Primary_Sire_Breed",
                                 "Primary_Sire_Registration_Number",
                                 "Country_Of_First_Proof",
                                 "Status_Sire",
                                 "Sire_Linkage_Identifier",
                                 "Atrificial_Insemination_Code"
), sep = ",") %>%
mutate(across(c(Primary_Sire_Breed, Country_Of_First_Proof, Status_Sire), ~as.factor(.x)),
       across(c(Sire_Linkage_Identifier, Primary_Sire_Registration_Number), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91016	NMR_CDL_Offspring ----
NMR_CDL_Offspring <-
  data %>%
  filter(str_starts(data_col, "091016")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number_Of_Dam",
                                 "Parturition_Date",
                                 "Sibling_Count",
                                 "Offspring_Survival_Code",
                                 "Offspring_Number",
                                 "Weight_At_Birth",
                                 "Sex_Of_Offspring",
                                 "Identification_Number_Offspring",
                                 "Herd_Number",
                                 "Identification_Number_Of_Dam",
                                 "Lactation_Number",
                                 "Calf_Usage",
                                 "Calf_Breed",
                                 "Dam_Herd_Linkage_Identifier",
                                 "Dam_Animal_Linkage_Identifier"
), sep = ",") %>%
mutate(across(c(Calf_Usage, Calf_Breed, Sex_Of_Offspring), ~as.factor(.x)),
       across(c(Dam_Herd_Linkage_Identifier, Dam_Animal_Linkage_Identifier,Lactation_Number,
                Identification_Number_Of_Dam, Identification_Number_Offspring, Offspring_Number,
                Animal_Number_Of_Dam), ~ as.integer(.x)),
       across(c(Parturition_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Weight_At_Birth), ~ as.numeric(.x)))

# 91017	NMR_CDL_Breed ----
NMR_CDL_Breed <-
  data %>%
  filter(str_starts(data_col, "091017")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Breed",
                                 "Breed_Name",
                                 "Breed_Abbreviation",
                                 "Breed_Usage",
                                 "Foreign_Breed_Indicator",
                                 "Minimum_Calving_Age",
                                 "Gestation_Tolerance_Days",
                                 "Cow_Gestation_Duration",
                                 "Maximum_Lactation_Length",
                                 "Lactation_Period",
                                 "Lactation_Curve_Code",
                                 "Minimum_Possible_Animal_Milk_Weight",
                                 "Minimum_Acceptable_Animal_Milk_weight",
                                 "Maximum_Acceptable_Animal_Milk_Weight",
                                 "Maximum_Possible_Animal_Milk_Weight",
                                 "Minimum_Possible_Animal_Fat_Percentage",
                                 "Minimum_Acceptable_Animal_Fat_Percentage",
                                 "Maximum_Acceptable_Animal_Fat_Percentage",
                                 "Maximum_Possible_Animal_Fat_Percentage",
                                 "Minimum_Possible_Animal_Protein_Percentage",
                                 "Minimum_Acceptable_Animal_Protein_Percentage",
                                 "Maximum_Acceptable_Animal_Protein_Percentage",
                                 "Maximum_Possible_Animal_Protein_Percentage",
                                 "Minimum_Possible_Animal_Lactose_Percentage",
                                 "Minimum_Acceptable_Animal_Lactose_Percentage",
                                 "Maximum_Acceptable_Animal_Lactose_Percentage",
                                 "Maximum_Possible_Animal_Lactose_Percentage",
                                 "Maximum_Acceptable_Animal_Lactation_Period_Milk_Weight",
                                 "Maximum_Possible_Animal_Lactation_Period_Milk_Weight",
                                 "Maximum_Acceptable_Animal_Natural_Lactation_Milk_Weight",
                                 "Maximum_Possible_Animal_Natural_Lactation_Milk_Weight",
                                 "Maximum_Acceptable_Animal_Milk_Weight_Difference",
                                 "Maximum_Acceptable_Animal_Milk_Weight_Percent_Difference",
                                 "Equivelant_Major_Breed_Code",
                                 "Breed_Society_Address_Number",
                                 "Valid_Sire_Breed_Indicator",
                                 "Valid_Dam_Breed_Indicator",
                                 "Species_Code"
), sep = ",") %>%
mutate(across(c(Breed_Usage, Foreign_Breed_Indicator, Lactation_Curve_Code, Valid_Sire_Breed_Indicator,
                Valid_Dam_Breed_Indicator, Species_Code, Animal_Breed), ~as.factor(.x)),
       across(c(Minimum_Calving_Age, Gestation_Tolerance_Days,Cow_Gestation_Duration,
                Maximum_Lactation_Length,Lactation_Period, Equivelant_Major_Breed_Code, 
                Breed_Society_Address_Number), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(Minimum_Possible_Animal_Milk_Weight, Minimum_Acceptable_Animal_Milk_weight,Maximum_Acceptable_Animal_Milk_Weight,
                Maximum_Possible_Animal_Milk_Weight, Minimum_Possible_Animal_Fat_Percentage, Minimum_Acceptable_Animal_Fat_Percentage,
                Maximum_Acceptable_Animal_Fat_Percentage, Maximum_Possible_Animal_Fat_Percentage, Minimum_Possible_Animal_Protein_Percentage,
                Minimum_Acceptable_Animal_Protein_Percentage, Maximum_Possible_Animal_Protein_Percentage, Minimum_Possible_Animal_Lactose_Percentage,
                Minimum_Acceptable_Animal_Lactose_Percentage, Maximum_Acceptable_Animal_Lactose_Percentage, Maximum_Possible_Animal_Lactose_Percentage,
                Maximum_Acceptable_Animal_Lactation_Period_Milk_Weight, Maximum_Possible_Animal_Lactation_Period_Milk_Weight, 
                Maximum_Acceptable_Animal_Natural_Lactation_Milk_Weight,Maximum_Possible_Animal_Natural_Lactation_Milk_Weight, 
                Maximum_Acceptable_Animal_Milk_Weight_Difference), ~ as.numeric(.x)))

# 91018	NMR_CDL_Evaluation_Group ----
NMR_CDL_Evaluation_Group <-
  data %>%
  filter(str_starts(data_col, "091018")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Evaluation_Group_Identifier",
                                 "Evaluation_Group_Name"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(Evaluation_Group_Identifier), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))


# 91019	NMR_CDL_Pedigree_Status ----
NMR_CDL_Pedigree_Status <-
  data %>%
  filter(str_starts(data_col, "091019")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Breed",
                                 "Pedigree_Status_Code",
                                 "Pedigree_Status_Description",
                                 "Range_Check_Indicator" 
), sep = ",") %>%
mutate(across(c(Pedigree_Status_Code, Range_Check_Indicator, Animal_Breed), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91020	NMR_CDL_Scheme ----
NMR_CDL_Scheme <-
  data %>%
  filter(str_starts(data_col, "091020")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Scheme_Identifier",
                                 "Scheme_Name",
                                 "Cell_Count_Ok_Indicator",
                                 "Scheme_Short_Name",
                                 "Scheme_Type",
                                 "Scheme_Valid_Indicator"
), sep = ",") %>%
mutate(across(c(Cell_Count_Ok_Indicator, Scheme_Type, Scheme_Valid_Indicator), ~as.factor(.x)),
       across(c(Scheme_Identifier), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91021	NMR_CDL_Sire_Identity_Type ----
NMR_CDL_Sire_Identity_Type <-
  data %>%
  filter(str_starts(data_col, "091021")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Sire_Identifier_Type_Code",
                                 "Sire_Identifier_Type_Name",
                                 "Sire_Identifier_Type_Status",
                                 "AI_Code_Indicator"
), sep = ",") %>%
mutate(across(c(Sire_Identifier_Type_Code, Sire_Identifier_Type_Status, AI_Code_Indicator), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91022	NMR_CDL_Animal_Predicted_Transmission_Ability ----
NMR_CDL_Animal_Predicted_Transmission_Ability <-
  data %>%
  filter(str_starts(data_col, "091022")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identitifier",
                                 "Profit_Index_Number",
                                 "Index_Of_Total_Economic_Merit",
                                 "Predicted_Transmission_Ability_Type",
                                 "Predicted_Transmission_Ability_Milk_Weight",
                                 "Predicted_Transmission_Ability_Fat_Weight",
                                 "Predicted_Transmission_Ability_Protein_Weight",
                                 "Predicted_Transmission_Ability_Fat_Percent",
                                 "Predicted_Transmission_Ability_Protein_Percent",
                                 "Predicted_Transmission_Ability_Reliability_Percent",
                                 "Evaluation_Group_Identifier"
), sep = ",") %>%
mutate(across(c(Evaluation_Group_Identifier, Predicted_Transmission_Ability_Type), ~as.factor(.x)),
       across(c(Animal_Linkage_Identitifier, Herd_Linkage_Identifier, Profit_Index_Number), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(Predicted_Transmission_Ability_Milk_Weight,
                Predicted_Transmission_Ability_Fat_Weight,
                Predicted_Transmission_Ability_Protein_Weight,
                Predicted_Transmission_Ability_Fat_Percent,
                Predicted_Transmission_Ability_Protein_Percent, 
                Predicted_Transmission_Ability_Reliability_Percent), ~ as.numeric(.x)))

# 91023	NMR_CDL_Sire_ Predicted_Transmission_Ability ----
NMR_CDL_Sire_Predicted_Transmission_Ability <-
  data %>%
  filter(str_starts(data_col, "091023")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Primary_Sire_Breed",
                                 "Primary_Sire_Registration_Number",
                                 "Sire_Linkage_Identifier",
                                 "Profit_Index_Number",
                                 "Index_Of_Total_Economic_Merit",
                                 "Predicted_Transmission_Ability_Type",
                                 "Predicted_Transmission_Ability_Milk_Weight",
                                 "Predicted_Transmission_Ability_Fat_Weight",
                                 "Predicted_Transmission_Ability_Protein_Weight",
                                 "Predicted_Transmission_Ability_Fat_Percent",
                                 "Predicted_Transmission_Ability_Protein_Percent",
                                 "Predicted_Transmission_Ability_Reliability_Percent",
                                 "Evaluation_Group_Identifier"
), sep = ",") %>%
mutate(across(c(Primary_Sire_Breed, Predicted_Transmission_Ability_Type), ~as.factor(.x)),
       across(c(Sire_Linkage_Identifier, Primary_Sire_Registration_Number, Profit_Index_Number), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(Predicted_Transmission_Ability_Milk_Weight,
                Predicted_Transmission_Ability_Fat_Weight,
                Predicted_Transmission_Ability_Protein_Weight,
                Predicted_Transmission_Ability_Fat_Percent,
                Predicted_Transmission_Ability_Protein_Percent, 
                Predicted_Transmission_Ability_Reliability_Percent), ~ as.numeric(.x)))

# 91024	NMR_CDL_Animal_Ex_Herd ----
NMR_CDL_Animal_Ex_Herd <-
  data %>%
  filter(str_starts(data_col, "091024")) %>%
separate(col = data_col, into= c("ADED_Number",                                                                                    
                                 "Earmark",
                                 "Animal_Type"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91096	NMR_CDL_Label {not documented currently} ----
# NMR_CDL_Label <-
#   data %>%
#   filter(str_starts(data_col, "091096")) %>%
# separate(col = data_col, into= c(), sep = ",") %>%
# mutate(across(c(), ~as.factor(.x)),
#        across(c(), ~ as.integer(.x)),
#        across(c(), ~ as.Date(.x)),
#        across(c(), ~ as.numeric(.x)))
# # 

# 91097	NMR_CDL_Memo  ----
NMR_CDL_Memo <-
  data %>%
  filter(str_starts(data_col, "091097")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Memo",
                                 "Herd_Number"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91098	NMR_CDL_Control ----
NMR_CDL_Control <-
  data %>%
  filter(str_starts(data_col, "091098")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Source_Identifier",
                                 "Herd_Number",
                                 "Sequence",
                                 "Data_Space"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 91099	NMR_CDL_Action  ----
NMR_CDL_Action <-
  data %>%
  filter(str_starts(data_col, "091099")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Action",
                                 "Key_String",
                                 "New_Key_String",
                                 "Herd_Number",
                                 "Sequence"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 990001	NMR_CDL_Header  ----
NMR_CDL_Header <-
  data %>%
  filter(str_starts(data_col, "990001")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Data_Dictionary_Type",
                                 "ADED_ISO_Version",
                                 "File_Created_Or_Updated_Date",
                                 "File_Created_Or_Updated_Time",
                                 "System_Status",
                                 "Sender_Name",
                                 "Reciever_Name",
                                 "Sender_Version",
                                 "ADED_National_Version",
                                 "Checksum",
                                 "Process_Computer_Type",
                                 "ADED_Manufacturer_Version"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(File_Created_Or_Updated_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(File_Created_Or_Updated_Time), ~ hms::as_hms(lubridate::as_datetime(.x, format = "%H%M%S"))),
       across(c(), ~ as.numeric(.x)))

# 990002	NMR_CDL_Animal  ----
NMR_CDL_Animal <-
  data %>%
  filter(str_starts(data_col, "990002")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Group_Number",
                                 "Date_Entered_Herd",
                                 "Tot_Parity_Count",
                                 "Left_Herd_Date",
                                 "Animal_Name",
                                 "Animal_Sex",
                                 "Birth_Date",
                                 "Electronic_ISO_ID_Number",
                                 "Identification_Number",
                                 "Left_Herd_Reason",
                                 "Primary_Sire_Registration_Number",
                                 "Identification_Number_Dam",
                                 "Animal_Status",
                                 "Herd_Specific_Electronic_ID_Number",
                                 "Herd_Number",
                                 "Animal_Full_Name",
                                 "Registration_Breed_Sire",
                                 "Animal_Breed",
                                 "Animal_Usage",
                                 "Animal_Barren_Date",
                                 "Identification_Number_Surrogate_Dam",
                                 "Blood_Test_Reference_Number",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identifier",
                                 "Dam_Herd_Linkage_Identifier",
                                 "Dam_Animal_Linkage_Identitifier",
                                 "Animal_Live_Ind",
                                 "Sire_Linkage_Identifier",
                                 "Pedigree_Status",
                                 "Sibling_Count" ,
                                 "Offspring_Survival_Code"
), sep = ",") %>%
mutate(across(c(Animal_Sex, Left_Herd_Reason, Registration_Breed_Sire,
                Animal_Breed, Animal_Usage, Animal_Live_Ind, Pedigree_Status,
                Offspring_Survival_Code), ~as.factor(.x)),
       across(c(Animal_Number, Group_Number, Tot_Parity_Count,
                Primary_Sire_Registration_Number, Identification_Number_Dam,
                Identification_Number_Surrogate_Dam, Herd_Linkage_Identifier, 
                Animal_Linkage_Identifier, Dam_Herd_Linkage_Identifier, 
                Dam_Animal_Linkage_Identitifier, Sire_Linkage_Identifier,
                Sibling_Count), ~ as.integer(.x)),
       across(c(Date_Entered_Herd, Left_Herd_Date, Birth_Date, Animal_Barren_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Blood_Test_Reference_Number), ~ as.numeric(.x)))

# 990003	NMR_CDL_Individual_Milking  ----
NMR_CDL_Individual_Milking <-
  data %>%
  filter(str_starts(data_col, "990003")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Date_Of_Milking_Session",
                                 "Time_Of_Milking_Session",
                                 "Box_Number_For_Sample_Bottle",
                                 "Sample_Bottle_Number",
                                 "Method_of_performance_recording",
                                 "Individual_Milk_Weight",
                                 "Average_Flow_Rate_During_Individual_Milking",
                                 "Milking_Error_Flag",
                                 "Maximum_Flow_Rate_for_Individual_Milking",
                                 "Stall_or_unit_number_for_individual_milking",
                                 "Milking_Duration",
                                 "Herd_Number",
                                 "No_Yield_Reason",
                                 "Identification_Number",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identitifier"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(Date_Of_Milking_Session), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Time_Of_Milking_Session), ~ hms::as_hms(lubridate::as_datetime(.x, format = "%H%M%S"))),
       
       across(c(), ~ as.numeric(.x)))

# 990004	NMR_CDL_Lactation  ----
NMR_CDL_Lactation <-
  data %>%
  filter(str_starts(data_col, "990004")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Parturition_Date",
                                 "Projected_Date_To_Turn_Dry",
                                 "Lactation_Period_End_Date",
                                 "Lactation_Number",
                                 "Amount_Of_Protein_In_Natural_Lactation",
                                 "Amount_Of_Protein_In_Lactation_Period_days",
                                 "Amount_Of_Fat_In_Natural_Lactation",
                                 "Amount_Of_Fat_In_Lactation_Period_days",
                                 "Amount_of_Milk_In_Natural_Lactation",
                                 "Days_In_Natural_Lactation",
                                 "Amount_of_Milk_In_Lactation_Period_days",
                                 "Lactation_Calculation_Method",
                                 "Amount_Of_Lactose_In_Natural_Lactation",
                                 "Amount_Of_Lactose_In_Lactation_Period",
                                 "Natural_Lactation_End_Reason",
                                 "Natural_Lactation_End_Date",
                                 "Number_Of_Tests_In_Lactation_Period",
                                 "Average_Somatic_Cell_Count_In_Natural_Lactation",
                                 "Cell_Value_In_Natural_Lactation",
                                 "Number_Of_Cell_Tests_Over_Limit_In_Natural_Lactation",
                                 "Total_3X_Days_In_Natural_Lactation",
                                 "Total_3X_Days_In_Lactation_Period",
                                 "Herd_Number",
                                 "Identification_Number",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identifier",
                                 "Lactation_Period_End_Reason_Code",
                                 "Verified_Lactation_Number_Indicator",
                                 "Average_Cell_Count_In_Lactation_Period",
                                 "Cell_Value_In_Lactation_Period",
                                 "Total_Cell_Test_Over_Limit_In_Lactation_Period",
                                 "Number_Of_Tests_In_Natural_Lactation"
), sep = ",") %>%
mutate(across(c(Lactation_Calculation_Method, Natural_Lactation_End_Reason, Lactation_Period_End_Reason_Code,
                Verified_Lactation_Number_Indicator), ~as.factor(.x)),
       across(c(Lactation_Number, 
                Number_Of_Tests_In_Lactation_Period,
                Herd_Linkage_Identifier, Animal_Linkage_Identifier, 
                Total_Cell_Test_Over_Limit_In_Lactation_Period, Number_Of_Tests_In_Natural_Lactation), ~ as.integer(.x)),
       across(c(Parturition_Date, Projected_Date_To_Turn_Dry,Lactation_Period_End_Date,
                Natural_Lactation_End_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Amount_Of_Protein_In_Natural_Lactation, Amount_Of_Protein_In_Lactation_Period_days,
                Amount_Of_Fat_In_Natural_Lactation, Amount_Of_Fat_In_Lactation_Period_days,
                Amount_of_Milk_In_Natural_Lactation, Amount_of_Milk_In_Lactation_Period_days,
                Amount_Of_Lactose_In_Natural_Lactation, Amount_Of_Lactose_In_Lactation_Period,
                Average_Somatic_Cell_Count_In_Natural_Lactation, Total_3X_Days_In_Natural_Lactation, 
                Total_3X_Days_In_Lactation_Period, Average_Cell_Count_In_Lactation_Period), ~ as.numeric(.x)))

# 990005	NMR_CDL_Official_Milk_Test_Result  ----
NMR_CDL_Official_Milk_Test_Result <-
  data %>%
  filter(str_starts(data_col, "990005")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Milk_Test_Date",
                                 "Somatic_Cell_Count_Linear_Score",
                                 "Urea_Percentage_Milk",
                                 "Percent_Protein",
                                 "Somatic_Cell_Count",
                                 "Percent_Lactose",
                                 "Percent_Fat",
                                 "Identification_Number",
                                 "Milk_Sampling_Date",
                                 "Herd_Number",
                                 "Estimated_Test_Results_Indicator",
                                 "No_Test_Result_Code",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identitifier",
                                 "Official_Milk_Weight",
                                 "Official_Milk_Weight_First_Session",
                                 "Official_Milk_Weight_Second_Session",
                                 "Official_Milk_Weight_Third_Session"
), sep = ",") %>%
mutate(across(c(Estimated_Test_Results_Indicator, No_Test_Result_Code), ~as.factor(.x)),
       across(c(Animal_Number, Somatic_Cell_Count_Linear_Score, Animal_Linkage_Identitifier,
                Herd_Linkage_Identifier), ~ as.integer(.x)),
       across(c(Milk_Sampling_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Urea_Percentage_Milk, Percent_Protein, Somatic_Cell_Count, Percent_Lactose,
                Percent_Fat, Official_Milk_Weight, Official_Milk_Weight_First_Session,
                Official_Milk_Weight_Second_Session, Official_Milk_Weight_Third_Session), ~ as.numeric(.x)))

# 990006	NMR_CDL_Daily_Feeding  ----
NMR_CDL_Daily_Feeding <-
  data %>%
  filter(str_starts(data_col, "990006")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Feed_Type_Identifier",
                                 "Feed_Cycle_Start_Date",
                                 "Feed_Consumption_24_Hour",
                                 "Feed_Allocation_24_Hour",
                                 "Feed_Remainder_24_Hour",
                                 "Herd_Number",
                                 "Identification_Number"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# # 990007	NMR_CDL_Weighing  ----
NMR_CDL_Weighing <-
  data %>%
  filter(str_starts(data_col, "990007")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Date_Of_Body_Weight_Measurement",
                                 "Time_Of_Body_Weight_Measurement",
                                 "Body_Weight",
                                 "Herd_Number",
                                 "Identification_Number"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 990008	NMR_CDL_In_Heat  ----
NMR_CDL_In_Heat <-
  data %>%
  filter(str_starts(data_col, "990008")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Heat_detection_date",
                                 "Heat_detection_method",
                                 "Herd_Number",
                                 "Identification_Number"
), sep = ",") %>%
mutate(across(c(), ~as.factor(.x)),
       across(c(), ~ as.integer(.x)),
       across(c(), ~ as.Date(.x)),
       across(c(), ~ as.numeric(.x)))

# 990009	NMR_CDL_Insemination  ----
NMR_CDL_Insemination <-
  data %>%
  filter(str_starts(data_col, "990009")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Insemination_Date",
                                 "Breed_service_sire",
                                 "Registration_Number_Service_Sire",
                                 "Insemination_Status",
                                 "Performing_Insemination",
                                 "Herd_Number",
                                 "Identification_Number",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identifier",
                                 "Sire_Linkage_Identifier"
), sep = ",") %>%
mutate(across(c(Breed_service_sire, Insemination_Status), ~as.factor(.x)),
       across(c(Animal_Number,Registration_Number_Service_Sire, Herd_Linkage_Identifier,
                Animal_Linkage_Identifier,Sire_Linkage_Identifier ), ~ as.integer(.x)),
       across(c(Insemination_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(), ~ as.numeric(.x)))

# 990010	NMR_CDL_Pregnancy_Check  ----
NMR_CDL_Pregnancy_Check <-
  data %>%
  filter(str_starts(data_col, "990010")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Pregnancy_Check_Date" ,
                                 "Pregnancy_check_result",
                                 "Pregnancy_Checking_Person",
                                 "Herd_Number",
                                 "Identification_Number",
                                 "Insemination_Date",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identifier"
), sep = ",") %>%
mutate(across(c(Pregnancy_check_result), ~as.factor(.x)),
       across(c(Animal_Number, Herd_Linkage_Identifier, Animal_Linkage_Identifier), ~ as.integer(.x)),
       across(c(Pregnancy_Check_Date, Insemination_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(), ~ as.numeric(.x)))

# 990011	NMR_CDL_Parturition  ----
NMR_CDL_Parturition <-
  data %>%
  filter(str_starts(data_col, "990011")) %>%
separate(col = data_col, into= c("ADED_Number",
                                 "Animal_Number",
                                 "Parturition_Date",
                                 "Offspring_Survival_Code",
                                 "Offspring_Number",
                                 "Weight_At_Birth",
                                 "Sex_Of_Offspring",
                                 "Parturition_Ease",
                                 "Identification_Number_Offspring",
                                 "Number_Of_Offsprings",
                                 "Herd_Number",
                                 "Identification_Number",
                                 "Lactation_Number",
                                 "Herd_Linkage_Identifier",
                                 "Animal_Linkage_Identitifier"
), sep = ",") %>%
mutate(across(c(Offspring_Survival_Code, Sex_Of_Offspring), ~as.factor(.x)),
       across(c(Animal_Number, Offspring_Number, Number_Of_Offsprings, Lactation_Number,
                Herd_Linkage_Identifier, Animal_Linkage_Identitifier), ~ as.integer(.x)),
       across(c(Parturition_Date), ~ as.Date(.x, format = "%Y%m%d")),
       across(c(Weight_At_Birth), ~ as.numeric(.x)))


