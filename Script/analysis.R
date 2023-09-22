library(tidyverse)
library(here)

# Adjusted data
df_adjusted = read_delim(here("Data", "joined_adjusted.tsv"), delim = "\t")

glimpse(df_adjusted)

#extra plot
ggplot(df_adjusted) +
  aes(
    x = gender,
    y = remission
  ) +
  geom_count(colour="red")


