library(tidyverse)
library(here)

#first notes are practising. Final code is further down.

df = read_delim(here("Data", "exam_data.txt"), delim = "\t")
df

#fjerner duplikasjoner
df %>%
  distinct()

#bytte om kolonner og rader
#identifiser kolonne-navn som inneholder mellomrom eller begynner med spesialtegn
df %>%
  glimpse


df %>%
  skimr::skim()


df %>%
  variable.names()

#see column information
df %>%
  spec()

df%>%
  summary

df<-df%>%
  distinct()
  
df<-df%>%
  rename(neut_percent='%neut',
         lymf_perc='lymph%')
glimpse(df)

df<-df%>%
  pivot_wider(names_from = mean_RBC_characteristic, 
              values_from = mean_value)
df
glimpse(df)

#final code, put into 1:
myData<-read_delim(here("Data", "exam_data.txt"), delim = "\t")%>%
  distinct()%>% #removing duplications
  rename(days_of_life='days of life', #renaming some colums
         neut_percent='%neut',
       lymf_perc='lymph%')%>%
  pivot_wider(names_from = mean_RBC_characteristic, 
              values_from = mean_value) #putting blood type in colums

#visualising myData:
glimpse(myData)

#saving myData:
fileName <- paste0("blood_sample_", Sys.Date(), ".txt")
write_delim(myData, 
            file = here("Data", fileName), delim="\t")
