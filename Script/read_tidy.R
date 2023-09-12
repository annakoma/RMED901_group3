library(tidyverse)
library(here)

df_raw = read_tsv(here("Data", "exam_data.txt"))

colnames(df_raw)
# There's a weird column "wbc_copy" in df
df_raw[df_raw$wbc != df_raw$wbc_copy, ]
# and they are identical. 

df_tidy = df_raw %>%
  distinct %>%
  setNames(colnames(df_raw) %>% 
             str_replace_all(" ", "_") %>%
             str_replace_all("%neut", "neut_percent") %>%
             str_replace_all("%$", "_percent")) %>%
  select(-wbc_copy) # Remove the copy.
# Now 23 columns as intended

write_tsv(df_tidy, here("tidy_data.tsv"))
