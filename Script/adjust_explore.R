library(tidyverse)
library(here)
# reading the tidy file from yesterday (12. september) and assigning to df
df <-  read_delim(here("Data", "IBD_tidy_2023-09-12.txt"))


# removing the colomns "hct" and "rdw"
df <- df %>%
  select(-"hct", -"rdw")

#reading the additional dataset into 

