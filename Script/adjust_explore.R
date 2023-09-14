library(tidyverse)
library(here)

# Preparation----
# Read data
df_tidy = read_table(here("Data", "tidy_data.tsv"))
df_additional = read_table(here("Data", "exam_data_joindata.txt"))

# Check column name overlaps between the two dataset
colnames(df_tidy)
colnames(df_additional)
colnames(df_tidy)[colnames(df_tidy) %in% colnames(df_additional)]
# Only the patient_id is in common.

# There's an extra "gender" column in the additional data, but that's probably fine


# Tasks----
identical(unique(df_tidy$patient_id), unique(df_additional$patient_id))
# All patients IDs are present in both data; it doesn't matter how to join them

# Remove hct and rdw, then join with additional data by the patient ID
df_joined = df_tidy %>% 
  select(-c("hct", "rdw")) %>%
  left_join(df_additional, by = "patient_id")

# Change data types, create new columns, set column order, sort by patient ID
df_adjusted = df_joined %>%
  mutate(across(c(active, remission), as.logical)) %>% # `active` and `remession` as logical
  mutate(gender = as.factor(gender)) %>% # `gender` as factor
  mutate(lymph_count = wbc * lymph_percent, # New columns for lymph count,
         sod_frac = sod + pot + chlor, # fraction of sodium in the sum of Na, K and Cl,
         hgb_quartile = ntile(hgb, 4), # `hgb` as quartile,
         un_above30 = un > 30) %>% # and whether blood urea nitrogen is above 30 
  select(patient_id, days_of_life, un, wbc, everything()) %>% # Set column order
  arrange(patient_id) # Sort by patient ID

# Write for examination (maybe in Excel)
write_tsv(df_adjusted, here("Data", "joined_adjusted.tsv"))

# glucose by gender
df_adjusted %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with hgb <= 10
df_adjusted %>%
  
  filter(hgb <= 10) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with remission
df_adjusted %>%
  filter(remission == T) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender for older than around 40 years
df_adjusted %>%
  filter(days_of_life > 40 * 365.25) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with more than 10% of monocytes in WBC
df_adjusted %>%
  filter(mono_percent > 10) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# table by gender and remission
df_adjusted %>%
  count(gender, remission)