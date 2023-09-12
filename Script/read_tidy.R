library(tidyverse)
library(here)

df = read_tsv(here("Data", "exam_data.txt"))

df = df %>%
  distinct %>%
  setNames(colnames(df) %>% 
             str_replace_all(" ", "_") %>%
             str_replace_all("^%", "percent_") %>%
             str_replace_all("%$", "_percent"))


write_tsv(df, here("tidy_data.tsv"))
