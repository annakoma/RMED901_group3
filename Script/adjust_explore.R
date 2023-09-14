library(tidyverse)
library(here)

# reading the tidy file from yesterday (12. september) and assigning to main_df
main_df <-  read_delim(here("Data/blood_sample_2023-09-14.txt"), delim = "\t")


# removing the colomns "hct" and "rdw" and "wbc_copy"
main_df <- main_df %>%
  select(-"hct", -"rdw", -"wbc_copy")

#reading the additional dataset

join_df <- read_delim(here("Data", "exam_data_joindata.txt"), delim = "\t")

# joining the two datasets
complete_data <- 
  main_df %>%
  full_join(join_df, join_by("patient_id"))

glimpse(complete_data)

# converting the columns "active" and "remission" to factors.
complete_data <- complete_data %>%
  mutate(active = as.factor(active),
         remission = as.factor(remission),
         
         #making a new column showing lymph cell count 
         lymph_cellcount = lymf_perc*wbc,
         # making a new column showing sodium as a fraction of the sum of sodium,potassium and chloride
         sod_fraction = sod/(sod+pot+chlor),
         #dividing the hgb column into quartiles
         hgb_percentiles = cut(hgb,c(4.50,11.60,12.70,13.80,18.60)),
         #creating a column checking whether Blood Urea Nitrogen is above 30
         un_above30 = if_else(un>30,"Yes","No"))


#Set the order of columns as: `patient_id, days_of_life, un, wbc` and other columns and 
complete_data <- complete_data %>%
  select(patient_id, days_of_life, un, wbc, everything())

# arrange patient_id column of your dataset in order of increasing number or alphabetically.
complete_data <- complete_data %>%
  arrange(sort(patient_id))

glimpse(complete_data)
fileName <- paste0("tidy_joined_data.txt")
write_delim(complete_data,file = here("Data", fileName), delim = "\t" )


# glucose by gender
complete_data %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with hgb <= 10
complete_data %>%
  filter(hgb <= 10) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with remission
complete_data %>%
  filter(remission == T) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender for older than around 40 years
complete_data %>%
  filter(days_of_life > 40 * 365.25) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with more than 10% of monocytes in WBC
complete_data %>%
  filter(mono_percent > 10) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# table by gender and remission
complete_data %>%
  count(gender, remission)


