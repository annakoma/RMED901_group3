library(tidyverse)
library(here)

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

..
