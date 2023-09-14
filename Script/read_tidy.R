library(tidyverse)
library(here)

df_raw = read_tsv(here("Data", "exam_data.txt"))

colnames(df_raw)
# There's a weird column "wbc_copy" in df
df_raw[df_raw$wbc != df_raw$wbc_copy, ]
# and they are identical. 

df_tidy = df_raw %>%
  distinct %>% # Remove duplicate rows
  rename(days_of_life='days of life', # Rename columns
         neut_percent='%neut',
         lymph_percent='lymph%') %>%
  pivot_wider(names_from = mean_RBC_characteristic, 
              names_prefix = "mean_RBC_",
              values_from = mean_value) %>% # Mean of each RBC type in a column
  select(-wbc_copy) # Remove the duplicate column.
# Now 23 columns as intended

# Write data
write_tsv(df_tidy, here("Data", "tidy_data.tsv"))