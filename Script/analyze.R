library(tidyverse)
library(here)
library(reshape2)
library(ggpubr)

# Adjusted data
df_adjusted = read_table(here("joined_adjusted.tsv"))

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
  labs(title = "Remission does not depend on gender.")

# Does the remission depend on chloride?
# p-value from logistic regression model
model = glm(remission ~ chlor, data = df_adjusted, family = binomial)
p_val = summary(model)$coefficients[2,4]

# plot
df_adjusted %>%
  ggplot(aes(chlor, as.integer(remission))) + 
  geom_point(alpha = .5) +
  geom_smooth(method="glm", method.args=list(family="binomial")) +
  annotate("text", x = Inf, y = Inf, 
           label = sprintf("p = %.3e", p_val), vjust = 1, hjust = 1) +
  scale_y_continuous(breaks = c(0, 1), labels = c("no remission", "remission")) +
  labs(x = "chloride", y = "", 
       title = "Remission depends on chloride")

# Is there an association between calcium and total bilirubin?
df_adjusted %>%
  ggplot(aes(tbil, cal)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  stat_cor(label.x = Inf, label.y = Inf, hjust = 1, vjust = 1) +
  labs(x = "total bilirubin", y = "calcium", 
       title = "Calcium and belirubin levels negatively correlate.")

# According to the data, was there a difference of alanine transaminase between gender categories?
df_adjusted %>%
  ggplot(aes(gender, alt, fill = gender)) +
  geom_violin() +
  geom_signif(comparisons = list(c("M", "F")), test = "wilcox.test") +
  labs(y = "alanine transaminase", title = "Alanine transaminase level does not depend on gender.") +
  theme(legend.position = "none")
