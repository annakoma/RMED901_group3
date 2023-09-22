library(tidyverse)
library(here)
library(patchwork)

myData<-read_delim(here("Data","tidy_joined_data.txt"), delim="\t")

glimpse(myData)

#making a plot showing linear relationship between protein and albumin
plot_alb_prot<-
  ggplot(myData) +
  aes(
    x = prot,
    y = alb
  ) +
  geom_point() +
  geom_smooth(method = "lm")

#visualising
plot_alb_prot #it looks like there is a linear relationship between albumin and protein


#making a plot exploring relationship between white blood cells and potassium
plot_pot_wbc<-
  ggplot(myData) +
  aes(
    x = pot,
    y = wbc
  ) +
  geom_point() +
  geom_smooth(method = "lm")

plot_pot_wbc #it does not seem like white blood cells count distribution depend on potassium

#Adding colour red to the line in plot
plot_pot_wbc<-
  ggplot(myData) +
  aes(
    x = pot,
    y = wbc
  ) +
  geom_point() +
  geom_smooth(method = "lm", colour="red")


#looking at plots next to each other
plot_alb_prot + plot_pot_wbc


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


