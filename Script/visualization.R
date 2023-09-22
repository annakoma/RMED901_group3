library(tidyverse)
library(here)
library(reshape2)
library(ggpubr)
library(patchwork)

# Data from Day 6
df_adjusted = read_table(here("Data", "joined_adjusted.tsv"))

theme_set(theme_bw() +
            theme(axis.ticks = element_blank()))

# Are there any correlated measurements?
cor(df_adjusted[, sapply(df_adjusted, is.numeric)], use = "complete.obs") %>%
  reshape2::melt() %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  labs(x = "", y = "", fill = "", title = "correlation heatmap") -> 
  plt_cor_heat
plt_cor_heat
# (answer: yes)
  
# Does the sodium distribution depend on `gender`?
# Normality check for t-test assumptions
sod_m = df_adjusted[df_adjusted$gender == "M", ]$sod
sod_f = df_adjusted[df_adjusted$gender == "F", ]$sod
shapiro.test(sod_m)
shapiro.test(sod_f) 
ggqqplot(sod_m)
ggqqplot(sod_f)
# ---Not normal. Should use non-parametric test.

df_adjusted %>%
  ggplot(aes(gender, sod, fill = gender)) +
  geom_violin(alpha = .5) +
  geom_boxplot(alpha = .75) +
  geom_signif(comparisons = list(c("M", "F")), test = "wilcox.test") +
  labs(y = "sodium", title = "Sodium does not depend on gender") +
  theme(legend.position = "none") ->
  plt_sod_gender
plt_sod_gender

# Does the white blood cell count distribution depend on `pot`?
df_adjusted %>%
  ggplot(aes(pot, wbc)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = .1) +
  stat_cor(label.x = Inf, label.y = Inf, hjust = 1, vjust = 1) +
  labs(x = "potassium", y = "white blood cell count", 
       title = "White blood cell does not depend on potassium") ->
  plt_wbc_pot
plt_wbc_pot

# Do albumin and protein have a linear relationship?
df_adjusted %>%
  ggplot(aes(prot, alb)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = .1) +
  stat_cor(label.x = Inf, label.y = Inf, hjust = 1, vjust = 1) +
  labs(x = "protein", y = "albumin", 
       title = "Albumin and protein have a linear relationship") ->
  plt_alb_prot
plt_alb_prot

# Does remission of inflammation after Thiopurines for > 12 weeks change with percent of monocytes in WBC count?
# p-value from logistic regression model
model = glm(remission ~ mono_percent, data = df_adjusted, family = binomial)
p_val = summary(model)$coefficients[2,4]

df_adjusted %>%
  ggplot(aes(mono_percent, as.integer(remission))) + 
  geom_smooth(method="glm", method.args=list(family="binomial")) +
  geom_point(alpha = .1) +
  annotate("text", x = Inf, y = Inf, 
           label = sprintf("p = %.3e", p_val), vjust = 1, hjust = 1) +
  labs(x = "monocyte percent in WBC", y = "", 
       title = "Remission does not depend on monocyte") +
  scale_y_continuous(breaks = c(0, 1), labels = c("no remission", "remission")) ->
  plt_rem_mono
plt_rem_mono

# Combined plot
plt_cor_heat /
(plt_sod_gender + plt_wbc_pot) /
(plt_alb_prot + plt_rem_mono)

# Save plot
ggsave(here("Plots", "Day_7_Combined.png"), width = 12, height = 15)

