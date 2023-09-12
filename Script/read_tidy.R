library(tidyverse)
library(here)

df = read_delim(here("Data", "exam_data.txt"), delim = "\t")
colnames(df)
df = df %>%
  distinct %>%
  setNames(colnames(df) %>% 
             str_replace_all(" ", "_") %>%
             str_replace_all("^%", "percent_") %>%
             str_replace_all("%$", "_percent"))

write_delim(df, here("tidy_data.tsv"))
