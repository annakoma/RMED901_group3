library(tidyverse)
library(here)
# reading the tidy file from yesterday (12. september) and assigning to main_df
main_df <-  read_delim(here("Data", "IBD_tidy_2023-09-12.txt"), delim = "\t")


# removing the colomns "hct" and "rdw" and "wbc_copy"
main_df <- main_df %>%
  select(-"hct", -"rdw", -"wbc_copy")

#reading the additional dataset

join_df <- read_delim(here("Data", "exam_data_joindata.txt"), delim = "\t")

# joining the two datasets
complete_data <- 
  main_df %>%
  full_join(join_df, join_by("patient_id"))


#trying to visualise the complete dataset
glimpse(complete_data)
summary(complete_data)
skimr::skim(complete_data)

# converting the columns "active" and "remission" to factors.
complete_data <- complete_data %>%
  mutate(active = as.factor(active),
         remission = as.factor(remission))

#making a new column showing lymph cell count 

complete_data <- complete_data %>%
  mutate(lymph_cellcount = lymph_percent*wbc)
  

# making a new column showing sodium as a fraction of the sum of sodium,potassium and chloride
complete_data <- complete_data %>%
  mutate(sod_fraction = sod/(sod+pot+chlor))
 

#dividing the hgb column into quartiles
complete_data <- complete_data %>%
  mutate(hgb_percentiles = cut(hgb,c(4.50,11.60,12.70,13.80,18.60)))
  

summary(complete_data)

#creating a column checking whether Blood Urea Nitrogen is above 30

complete_data <- complete_data %>%
  mutate(un_above30 = if_else(un>30,"Yes","No"))
  

#Set the order of columns as: `patient_id, days_of_life, un, wbc` and other columns

complete_data <- complete_data %>%
  select(patient_id, days_of_life, un, wbc, everything())

#Arrange patient_id column of your dataset in order of increasing number or alphabetically. 

complete_data <- complete_data %>%
  arrange(sort(patient_id))


