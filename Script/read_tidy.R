library(tidyverse)
library(here)

df = read_delim(here("Data", "exam_data.txt"), delim = "\t")

# trying to get an overview of the data
head(df)
summary(df)
glimpse(df)
skimr::skim(df)
tail(df)


#checking if there are duplicates
df<- df %>% 
  distinct()

df %>% 
  count(patient_id, sort=TRUE)
head(df)

glimpse(df)
#mean_RBC_characteristic and mean_value: could each mean_RBC_characteristic be a column?
# and the value of the column is the mean_value?


glimpse(df)
# some of the columns `%neut`and `lymph%`should maybe get a new name? neut_percent, lymph_percent
# also `days of life` should be days_of_life

#changing the column names
df <-
  df %>%
  rename(days_of_life = `days of life`,
         neut_percent = `%neut`,
         lymph_percent = `lymph%`)

glimpse(df)

#creating new columns from the mean_RBC_characteristic column
df <- df %>% 
  pivot_wider(names_from = mean_RBC_characteristic, 
              values_from = mean_value)
  
glimpse(df)

#combining all these steps

tidy_data <- read_delim(here("Data", "exam_data.txt"), delim = "\t") %>%
  distinct() %>%
  
  rename(days_of_life = `days of life`,
       neut_percent = `%neut`,
       lymph_percent = `lymph%`) %>%
  
  pivot_wider(names_from = mean_RBC_characteristic, 
              values_from = mean_value) 

glimpse(tidy_data)


fileName <- paste0("IBD_tidy_", Sys.Date(), ".txt")
write_delim(tidy_data,
            file = here("Data", fileName), delim="\t")


  