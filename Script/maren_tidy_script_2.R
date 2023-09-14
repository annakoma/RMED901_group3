library(tidyverse)
library(here)

#read data from yesterday
myData<-read_delim(here("Data", "blood_sample_2023-09-12.txt"), delim="\t")
myData

#remove colums test
myData %>% 
  select(-"hct", -"rdw") 

#assign moved colums to myData
myData<-
  myData %>%
  select(-"hct",-"rdw")%>%
  select(-"wbc_copy")

#visualise data with colums in y axis
glimpse(myData)

#read additional data file
myData2<-read_delim(here("Data", "exam_data_joindata.txt"), delim="\t")
myData2

#join the two data files
joinData<-
  myData %>%
  full_join(myData2, join_by("patient_id"))

#visualise data
joinData

glimpse(joinData)            

#explore data
summary(joinData)          
spec(joinData)
skimr::skim(joinData)

#change variable types of "active" and "remission" to factors
joinData<-
  joinData %>% 
  mutate(active = as.factor(active),
         remission = as.factor(remission))

#adding column with number of lymfocytes
joinData<-
  joinData %>% 
  mutate(lymph_cell_count = lymf_perc*wbc)

glimpse(joinData)

#adding column with sodium fraction
joinData<-
  joinData %>% 
  mutate(sod_fraction = sod/(sod+pot+chlor))%>%
  glimpse

#reading about cut function
help(cut)

joinData %>% 
  mutate(hbg_quart = cut(hgb, c(4.50,11.60,12.58,13.80,18.60),(a,b,c,d]))%>%
  glimpse

summary(joinData)

#Adding column showing if blood urea nitrogen is above 30
joinData<-
  joinData %>% 
  mutate(un_above_30 =  if_else(un >30, "YES", "NO"))
  
glimpse(joinData)
joinData
  
help(count)
count(joinData, un_above_30)

#sorting order of colums
joinData<-
  joinData %>% 
  select(patient_id, days_of_life, un, wbc, everything())

#arranging after patient id
joinData<-
  joinData%>%
  arrange(patient_id)

glimpse(joinData)
joinData

