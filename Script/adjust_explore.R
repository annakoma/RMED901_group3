library(tidyverse)
library(here)
library(naniar)
library(visdat)

# Preparation----
df = read_table(here("tidy_data.tsv"))
df_to_join = read_table(here("Data", "exam_data_joindata.txt"))

colnames(df)
colnames(df_to_join)
colnames(df)[colnames(df) %in% colnames(df_to_join)]
colnames(df_to_join)[colnames(df_to_join) %in% colnames(df)]
# Only the patient_id is common.

# There's a weird column "wbc_copy" in df
df[df$wbc != df$wbc_copy, ]
# and they are identical. Removing the copy.
df = select(df, -wbc_copy)
# Now 23 columns as intended

# There's an extra "gender" column in the additional data, but that's probably fine


# Tasks----
identical(unique(df$patient_id), unique(df_to_join$patient_id))
# All patients IDs are present in both data; it doesn't matter how to join them

# Remove hct and rdw, then join by the patient ID
df_joined = df %>% 
  select(-c("hct", "rdw")) %>% left_join(df_to_join, by = "patient_id")

# Change data types, create new columns, set column order, sort by patient ID
df_joined %>%
  mutate(across(c(active, remission), as.logical)) %>%
  mutate(across(c(mean_RBC_characteristic, gender), as.factor)) %>%
  mutate(lymph_count = wbc * lymph_percent,
         sod_frac = sod + pot + chlor,
         hgb_quartile = ntile(hgb, 4),
         un_30 = un > 30) %>%
  select(patient_id, days_of_life, un, wbc, everything()) %>%
  arrange(patient_id)

# Write for examination (maybe in Excel)
write_tsv(df_joined, here("joined_adjusted.tsv"))

# glucose by gender
df_joined %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with hgb <= 10
df_joined %>%
  filter(hgb <= 10) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with remission
df_joined %>%
  filter(remission == T) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender for older than around 40 years
df_joined %>%
  filter(days_of_life > 40 * 365.25) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# glucose by gender with more than 10% of monocytes in WBC
df_joined %>%
  filter(mono_percent > 10) %>%
  group_by(gender) %>%
  summarize(
    min_gluc = min(gluc, na.rm = T),
    max_gluc = max(gluc, na.rm = T), 
    mean_gluc = mean(gluc, na.rm = T), 
    sd_gluc = sd(gluc, na.rm = T)
  )

# table by gender and remission
df_joined %>%
  count(gender, remission)
