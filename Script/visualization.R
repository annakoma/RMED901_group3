library(tidyverse)
library(here)
library(reshape2)
library(ggpubr)

# Data from Day 6
df_adjusted = read_table(here("joined_adjusted.tsv"))

theme_set(theme_minimal())

# Are there any correlated measurements?
cor(df_adjusted[, sapply(df_adjusted, is.numeric)], use = "complete.obs") %>%
  reshape2::melt() %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "", fill = "", title = "correlation heatmap")
# (answer: yes)
  
# Does the sodium distribution depend on `gender`?
df_adjusted %>%
  ggplot(aes(gender, sod, fill = gender)) +
  geom_boxplot() +
  geom_signif(comparisons = list(c("M", "F")), test = "t.test") +
  labs(y = "sodium", title = "Sodium level does not depend on gender.") +
  theme(legend.position = "none")

# Does the white blood cell count distribution depend on `pot`?
df_adjusted %>%
  ggplot(aes(pot, wbc)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm") +
  stat_cor(label.x = Inf, label.y = Inf, hjust = 1, vjust = 1) +
  labs(x = "potassium", y = "white blood cell count", 
       title = "White blood cell count does not depend on the pottassium level.")

# Do albumin and protein have a linear relationship?
df_adjusted %>%
  ggplot(aes(prot, alb)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm") +
  stat_cor(label.x = Inf, label.y = Inf, hjust = 1, vjust = 1) +
  labs(x = "protein", y = "albumin", 
       title = "Albumin and protein have a linear relationship")

# Does remission of inflammation after Thiopurines for > 12 weeks change with percent of monocytes in WBC count?
# p-value from logistic regression model
model = glm(remission ~ wbc, data = df_adjusted, family = binomial)
p_val = summary(model)$coefficients[2,4]

df_adjusted %>%
  ggplot(aes(wbc, as.integer(remission))) + 
  geom_point(alpha = .5) +
  geom_smooth(method="glm", method.args=list(family="binomial")) +
  annotate("text", x = Inf, y = Inf, 
           label = sprintf("p = %.3e", p_val), vjust = 1, hjust = 1) +
  labs(x = "white blood cell count", y = "", 
       title = "Remission depends on white blood cell count.") +
  scale_y_continuous(breaks = c(0, 1), labels = c("no remission", "remission"))
# I'm not doing statistics for this one for now.
