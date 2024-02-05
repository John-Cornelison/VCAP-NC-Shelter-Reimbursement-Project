#Working Directory
#Desktop
setwd("C:/Users/john/Documents/GitHub_Data/VCAP-NC-Shelter-Reimbursement-Project-Data")

#Laptop
#setwd("C:/Users/johnc/Documents/GitHub_Data/VCAP-NC-Shelter-Reimbursement-Project-Data")


###Libraries
library(plotly)
library(patchwork)
library(tidyverse)
library(readxl)
library(googlesheets4)
library(stringr)
library(reactable)
library(scales)
library(DT)

#### READING IN RAW DATA
NC_shelter_raw <- read_xlsx("Shelter&ReimburseReformatted.xlsx", sheet = "Shelter Final")
NC_reimbursements_raw <- read_xlsx("Shelter&ReimburseReformatted.xlsx", sheet = "Reimbursement Final")

#### CLEANING STEPS

#### STEP 1: SEPERATE DATA & ADD UPDATED VALUES 2019, 2020, 2021

species_table <- NC_shelter_raw %>% 
  select(1:4, 7:874) %>% 
  pivot_longer(
    cols = 5:872,
    names_to = c("Species", "Category"),
    names_sep = "_",
    values_to = c("Count")) %>%
  pivot_wider(
    names_from = "Category",
    values_from = "Count") %>% 
  mutate(remove_row = case_when(Intake == 0 & Adopted == 0 & Euthanized == 0 & Returned == 0 & Euthanized == 0 ~ TRUE,
                                TRUE ~ FALSE)) %>% 
  filter(remove_row == FALSE) %>% 
  select(-remove_row) %>% 
  mutate(Intake = case_when(Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Dog" ~ 624,
                            Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Cat" ~ 1012,
                            Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Raccoon" ~ 64,
                            Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Opossum" ~ 55,
                            TRUE ~ Intake),
         Adopted = case_when(Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Dog" ~ 380,
                             Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Cat" ~ 98,
                             Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Raccoon" ~ 0,
                             Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Opossum" ~ 0,
                             TRUE ~ Adopted),
         Returned = case_when(Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Dog" ~ 58,
                              Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Cat" ~ 16,
                              Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Raccoon" ~ 0,
                              Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Opossum" ~ 55,
                              TRUE ~ Returned),
         Euthanized = case_when(Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Dog" ~ 186,
                                Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Cat" ~ 899,
                                Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Raccoon" ~ 64,
                                Facility_Name == "Montgomery Co. Animal Control Facility" & Year == 2019 & Species == "Opossum" ~ 0,
                                TRUE ~ Euthanized)) %>%
  #2020
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Species = "Dog", Intake = 416, Adopted = 149, Returned = 79, Euthanized = 18) %>%
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Species = "Cat", Intake = 387, Adopted = 213, Returned = 13, Euthanized = 77) %>%
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Species = "Raccoon", Intake = 10, Adopted = 0, Returned = 0, Euthanized = 3) %>%
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Species = "Opossum", Intake = 3, Adopted = 0, Returned = 0, Euthanized = 1) %>%
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Species = "Fox", Intake = 1, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Species = "Bat", Intake = 4, Adopted = 0, Returned = 0, Euthanized = 2) %>%
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Species = "Skunk", Intake = 1, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "71", Year = 2020, "County/City" = "Stanly", Facility_Name = "Stanly County Animal Protective Ser",
          Species = "Dog", Intake = 385, Adopted = 207, Returned = 106, Euthanized = 193) %>%
  add_row(LIC = "71", Year = 2020, "County/City" = "Stanly", Facility_Name = "Stanly County Animal Protective Ser",
          Species = "Cat", Intake = 628, Adopted = 324, Returned = 10, Euthanized = 362) %>% 
  add_row(LIC = "129", Year = 2020, "County/City" = "Tyrrell", Facility_Name = "Tyrrell County Animal Shelter",
          Species = "Dog", Intake = 51, Adopted = 34, Returned = 9, Euthanized = 7) %>%
  add_row(LIC = "129", Year = 2020, "County/City" = "Tyrrell", Facility_Name = "Tyrrell County Animal Shelter",
          Species = "Cat", Intake = 142, Adopted = 32, Returned = 1, Euthanized = 109) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Dog", Intake = 926, Adopted = 376, Returned = 359, Euthanized = 134) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Cat", Intake = 530, Adopted = 394, Returned = 33, Euthanized = 74) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Rabbit", Intake = 25, Adopted = 25, Returned = 0, Euthanized = 0) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Raccoon", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 10) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Opossum", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 3) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Bat", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 3) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Coyote", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 2) %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Chicken", Intake = 15, Adopted = 8, Returned = 2, Euthanized = 5) %>% 
  #2021
  add_row(LIC = "377", Year = 2021, "County/City" = "Chatham", Facility_Name = "Chatham County Sheriff'S Office Animal Resource Center",
          Species = "Dog", Intake = 451, Adopted = 60, Returned = 136, Euthanized = 59) %>%
  add_row(LIC = "377", Year = 2021, "County/City" = "Chatham", Facility_Name = "Chatham County Sheriff'S Office Animal Resource Center",
          Species = "Cat", Intake = 494, Adopted = 142, Returned = 8, Euthanized = 125) %>% 
  add_row(LIC = "377", Year = 2021, "County/City" = "Chatham", Facility_Name = "Chatham County Sheriff'S Office Animal Resource Center",
          Species = "Rabbit", Intake = 17, Adopted = 2, Returned = 2, Euthanized = 6) %>% 
  add_row(LIC = "377", Year = 2021, "County/City" = "Chatham", Facility_Name = "Chatham County Sheriff'S Office Animal Resource Center",
          Species = "Raccoon", Intake = 17, Adopted = 2, Returned = 2, Euthanized = 7) %>% 
  add_row(LIC = "377", Year = 2021, "County/City" = "Chatham", Facility_Name = "Chatham County Sheriff'S Office Animal Resource Center",
          Species = "Opossum", Intake = 16, Adopted = 2, Returned = 1, Euthanized = 7) %>%
  add_row(LIC = "147", Year = 2021, "County/City" = "Martin", Facility_Name = "Martin County Sheriff'S Office Animal Shelter",
          Species = "Dog", Intake = 208, Adopted = 90, Returned = 42, Euthanized = 78) %>% 
  add_row(LIC = "147", Year = 2021, "County/City" = "Martin", Facility_Name = "Martin County Sheriff'S Office Animal Shelter",
          Species = "Cat", Intake = 280, Adopted = 65, Returned = 2, Euthanized = 200) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Dog", Intake = 1000, Adopted = 521, Returned = 233, Euthanized = 139) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Cat", Intake = 564, Adopted = 457, Returned = 26, Euthanized = 62) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Rabbit", Intake = 12, Adopted = 12, Returned = 1, Euthanized = 2) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Raccoon", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 30) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Opossum", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 22) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Bat", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 4) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Chicken", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 7) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Fox", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 4) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Turtle", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 2) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Squirrel", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 5) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Deer", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 1) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Bear", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 1) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Gerbil", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 6) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Alligator", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 1) %>%
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Species = "Coyote", Intake = 0, Adopted = 0, Returned = 0, Euthanized = 1) %>%
  add_row(LIC = "115", Year = 2021, "County/City" = "Northampton", Facility_Name = "Northampton County Animal Shelter",
          Species = "Dog", Intake = 194, Adopted = 153, Returned = 30, Euthanized = 11) %>%
  add_row(LIC = "115", Year = 2021, "County/City" = "Northampton", Facility_Name = "Northampton County Animal Shelter",
          Species = "Cat", Intake = 188, Adopted = 167, Returned = 3, Euthanized = 18) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Dog", Intake = 437, Adopted = 42, Returned = 69, Euthanized = 67) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Cat", Intake = 632, Adopted = 86, Returned = 11, Euthanized = 431) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Rabbit", Intake = 3, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Raccoon", Intake = 1, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Cattle", Intake = 26, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Duck", Intake = 4, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Chicken", Intake = 47, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Turkey", Intake = 9, Adopted = 0, Returned = 0, Euthanized = 0) %>%
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Species = "Goat", Intake = 1, Adopted = 0, Returned = 0, Euthanized = 0) %>% 
  mutate(Outcome = Adopted + Euthanized + Returned) %>% 
  relocate(Outcome, .after = Intake)

expenses_table <- NC_shelter_raw %>% 
  select(1:6) %>% 
  add_row(LIC = "92", Year = 2020, "County/City" = "Jackson", Facility_Name = "Jackson County Animal Shelter",
          Operating_Expense = "316891", Cost_Per_Animal = "394.63") %>% 
  add_row(LIC = "72", Year = 2020, "County/City" = "Stanly", Facility_Name = "Stanly County Animal Protective Ser",
          Operating_Expense = "475000", Cost_Per_Animal = "468.90") %>%
  add_row(LIC = "129", Year = 2020, "County/City" = "Tyrrell", Facility_Name = "Tyrrell County Animal Shelter",
          Operating_Expense = "11214.67", Cost_Per_Animal = "57.81") %>% 
  add_row(LIC = "16", Year = 2020, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Operating_Expense = "1537722", Cost_Per_Animal = "870") %>% 
  add_row(LIC = "377", Year = 2021, "County/City" = "Chatham", Facility_Name = "Chatham County Sheriff'S Office Animal Resource Center",
          Operating_Expense = "1104472", Cost_Per_Animal = "1110.02") %>% 
  add_row(LIC = "147", Year = 2021, "County/City" = "Martin", Facility_Name = "Martin County Sheriff'S Office Animal Shelter",
          Operating_Expense = "119576.57", Cost_Per_Animal = "408.84") %>% 
  add_row(LIC = "16", Year = 2021, "County/City" = "New Hanover", Facility_Name = "New Hanover Co Sheriffs Ofc Animal Svcs Unit",
          Operating_Expense = "293568", Cost_Per_Animal = "890") %>% 
  add_row(LIC = "115", Year = 2021, "County/City" = "Northampton", Facility_Name = "Northampton County Animal Shelter",
          Operating_Expense = "48394.56", Cost_Per_Animal = "126.69") %>% 
  add_row(LIC = "63", Year = 2021, "County/City" = "Stokes", Facility_Name = "Stokes County Animal Shelter",
          Operating_Expense = "330320", Cost_Per_Animal = "290.70") %>% 
  group_by(Operating_Expense) %>% 
  mutate(Operating_Expense = as.numeric(Operating_Expense),
         Cost_Per_Animal = as.numeric(Cost_Per_Animal),
         LIC = as.numeric(LIC))

reimbursements_table <- NC_reimbursements_raw %>% 
  mutate(across(c(5:28), as.numeric))


#### STEP 2: WRITE TO CSV TO DO SOME CLEANING IN PYTHON

write_csv(species_table, "species_table.csv")
write_csv(expenses_table, "expenses_table.csv")
write_csv(reimbursements_table, "reimbursements_table.csv")

