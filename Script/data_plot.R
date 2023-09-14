library(tidyverse)
library(here)
library(patchwork)

myData <- read_delim(here("Data/tidy_joined_data.txt"), delim = "\t")

# making a plot to find out if the sodium distribution depends on `gender`


#first grouping by gender and sod
glimpse(myData)
sod_gender <- myData %>%
  group_by(gender, sod) 

#plotting gender vs. sod as a violinplot 
violinplot <- ggplot(data = sod_gender) +
  aes(
    x = gender,
    y = sod
  ) +
  geom_violin()

#plotting gender vs. sod as a boxplot
boxplot <- ggplot(data = sod_gender) +
  aes(
    x = gender,
    y = sod
  ) +
  geom_boxplot()

violinplot + boxplot

ggsave(here("Plots", "plot_sod_gender.png"))

