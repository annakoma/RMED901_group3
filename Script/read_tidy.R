library(tidyverse)
library(here)

df_raw = read_tsv(here("Data", "exam_data.txt"))

colnames(df_raw)

df_col_fixed = df_raw %>%
  distinct %>%
  setNames(colnames(df_raw) %>% 
             str_replace_all(" ", "_") %>%
             str_replace_all("%neut", "neut_percent") %>%
             str_replace_all("%$", "_percent")) %>%
  select(-wbc_copy)

write_tsv(df_col_fixed, here("tidy_data.tsv"))
