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

                      
