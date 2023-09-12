library(tidyverse)
library(here)

df = read_delim(here("Data", "exam_data.txt"), delim = "\t")