library(tidyverse)
library(here)
library(reshape2)
library(ggpubr)
library(patchwork)

# Adjusted data
df_adjusted = read_table(here("Data", "joined_adjusted.tsv"))

# Does the remission depend on the gender?
# Chi-squared test p-value
contingency_table = table(df_adjusted$gender, df_adjusted$remission)
chi_test = chisq.test(contingency_table)
p_val = chi_test$p.value

df_adjusted %>%
  ggplot(aes(gender, remission, fill = remission)) +
  geom_col() +
  annotate("text", x = 1.5, y = Inf, 
           label = sprintf("p = %.3f", p_val), vjust = 1, hjust = .5) +
  theme(axis.text.y = element_blank()) +
  labs(title = "Remission does not depend on gender") ->
  plt_rem_gender
plt_rem_gender

# Does the remission depend on chloride?
# p-value from logistic regression model
model = glm(remission ~ chlor, data = df_adjusted, family = binomial)
p_val = summary(model)$coefficients[2,4]

# plot
df_adjusted %>%
  ggplot(aes(chlor, as.integer(remission))) + 
  geom_smooth(method="glm", method.args=list(family="binomial")) +
  geom_point(alpha = .1) +
  annotate("text", x = Inf, y = Inf, 
           label = sprintf("p = %.3e", p_val), vjust = 1, hjust = 1) +
  scale_y_continuous(breaks = c(0, 1), labels = c("no remission", "remission")) +
  labs(x = "chloride", y = "", 
       title = "Remission depends on chloride") ->
  plt_rem_cl
plt_rem_cl

# Is there an association between calcium and total bilirubin?
df_adjusted %>%
  ggplot(aes(tbil, cal)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = .1) +
  stat_cor(label.x = Inf, label.y = Inf, hjust = 1, vjust = 1) +
  labs(x = "total bilirubin", y = "calcium", 
       title = "Calcium and belirubin negatively correlate") ->
  plt_ca_bil
plt_ca_bil

# According to the data, was there a difference of alanine transaminase between gender categories?
# Normality check for t-test assumptions
alt_m = df_adjusted[df_adjusted$gender == "M", ]$alt
alt_f = df_adjusted[df_adjusted$gender == "F", ]$alt
shapiro.test(alt_m)
shapiro.test(alt_f)
ggqqplot(alt_m)
ggqqplot(alt_f)
# ---Not normal. Should use non-parametric test.

df_adjusted %>%
  ggplot(aes(gender, alt, fill = gender)) +
  geom_violin() +
  geom_signif(comparisons = list(c("M", "F")), test = "wilcox.test") +
  labs(y = "alanine transaminase", 
       title = "Alanine transaminase does not depend on gender") +
  theme(legend.position = "none") ->
  plt_ala_gender
plt_ala_gender

# Combined plot
(plt_rem_gender + plt_rem_cl) /
(plt_ca_bil + plt_ala_gender)

# Save plot
ggsave(here("Plots", "Day_8_Combined.png"), width = 12, height = 10)

