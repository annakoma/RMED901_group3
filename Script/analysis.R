library(tidyverse)
library(here)

myData<- read_table(here("Data", "joined_adjusted.tsv"))

glimpse(myData)

#extra plot
ggplot(myData) +
  aes(
    x = gender,
    y = remission
  ) +
  geom_count(colour="red")


