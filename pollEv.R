# a file for parsing polleverywhere composite imported csvs 
# (obtained from excels: get data->from file->from folder->combine and load)
# then sort by the user folder - delete any row not containing a named user 
# - then save as .csv in order to be imported by this script
# make sure all options clickable by a student begin with the numerical value
# of that option


library(readr)
library(tidyverse)
rba <- read_csv("pollev/results_by_activity.csv")
rba$Column1<-as.numeric(gsub("(^\\d+).*$", "\\1", rba$Column1))
rba<-select(rba, -Column2)
rba<-select(rba, -Column4)
rba<- rba %>% pivot_wider(names_from = Column3, values_from = Column1)
rba$Source.Name<-gsub("(^.*?\\d+).*$", "\\1", rba$Source.Name)
rba<- rba %>% remove_rownames %>% column_to_rownames(var="Source.Name")
rba<-t(rba)

                      
