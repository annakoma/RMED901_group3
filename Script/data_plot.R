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

#saving plot
plot_alb_prot
ggsave("plot_alb_prot.png", width=5, height=5)

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

#saving plot
plot_pot_wbc
ggsave("plot_pot_wbc.png", width=5, height=5)

#looking at plots next to each other
plot_alb_prot + plot_pot_wbc


