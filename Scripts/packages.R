install.packages("medicaldata")
df <- medicaldata::laryngoscope
head(df)

mean(df$BMI[df$age > 30]) # mean BMI for persons over 30 years
?mean

# one value is missing, therefore returns NA

mean(df$BMI[df$age>30 & df$gender == 0], na.rm = T)

x <- 19
x %in% c(5,6)

x == c(5,6)

x %in% df$age

#nrow(df) er antall rader

?nrow



for (i in 1:nrow(df)){
  if (df$age[i] < 30){
    df$age_range[i] <- "under 30"
  } else if (df$age[i] >= 30 & df$age[i] <= 65) {
    df$age_range <- "30-65"
  } else if (df$age[i] > 65){
    df$age_range <- "over 65"
  }
}
df$age_range







