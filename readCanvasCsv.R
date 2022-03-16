library(readr)
library(reshape)
library(data.table)
library(tidyr)
library(dplyr)

fileChosen<-file.choose(new = FALSE)
csvPath <- dirname(fileChosen)

df4 <- read_csv(fileChosen, col_types = cols(
       `565735: Mark the site above according to the three categories (each category has a drop down menu on the right)\n\n\n\nTechnical Accuracy\n[TechnicalAccuracy] out of 10\n\n\nVisual Design and Order\n[VisualDesignandOrder] out of 10\n\n\nSite Ambition/Vision\n[SiteAmbitionVision] out of 10\n\n\n` = col_character(), 
       `565739: Mark the site above according to the three categories (each category has a drop down menu on the right)\n\n\n\nTechnical Accuracy\n[TechnicalAccuracy] out of 10\n\n\nVisual Design and Order\n[VisualDesignandOrder] out of 10\n\n\nSite Ambition/Vision\n[SiteAmbitionVision] out of 10\n\n\n` = col_character(), 
       `565742: Mark the site above according to the three categories (each category has a drop down menu on the right)\n\n\n\nTechnical Accuracy\n[TechnicalAccuracy] out of 10\n\n\nVisual Design and Order\n[VisualDesignandOrder] out of 10\n\n\nSite Ambition/Vision\n[SiteAmbitionVision] out of 10\n\n\n` = col_character()))

names(df4)[9] <-  "Scores1"
names(df4)[13] <- "Scores2"
names(df4)[17] <- "Scores3"
names(df4)[11] <- "Fbk1"
names(df4)[15] <- "Fbk2"
names(df4)[19] <- "Fbk3"
names(df4)[21] <- "Site1"
names(df4)[23] <- "Site2"
names(df4)[25] <- "Site3"

df4$Site1<-gsub("x=>\\d+,","",df4$Site1)
df4$Site2<-gsub("x=>\\d+,","",df4$Site2)
df4$Site3<-gsub("x=>\\d+,","",df4$Site3)

df4<-df4 %>% select(-contains('1.0'))
df4<-df4 %>% select(-(section:attempt)) 
df4<-df4[,1:(ncol(df4)-3)]
df4<-df4[,c("name","id","sis_id","Site1","Scores1","Fbk1","Site2","Scores2","Fbk2","Site3","Scores3","Fbk3")]

mksAudit<-df4
mksAudit<-separate(mksAudit,col=Scores1,into=c("TechAccuracy1", "VizDesign1", "SiteAmbition1"),sep="\\,")
mksAudit<-separate(mksAudit,col=Scores2,into=c("TechAccuracy2", "VizDesign2", "SiteAmbition2"),sep="\\,")
mksAudit<-separate(mksAudit,col=Scores3,into=c("TechAccuracy3", "VizDesign3", "SiteAmbition3"),sep="\\,")

write.csv(mksAudit,file=paste0(csvPath,"/mksAudit.csv"))


dfz<-melt(setDT(df4), id = 1L, 
      measure = list(c(4,7,10), c(5,8,11), c(6,9,12)), 
      value.name = c("Site", "Scores", "Feedback"))
dfx<-separate(dfz,col=Scores,into=c("TechAccuracy", "VizDesign", "SiteAmbition"),sep="\\,")

write.csv(dfx,file=paste0(csvPath,"/reorg.csv"))

dfxfbk<-dfx[,c("name","Site","Feedback")]
groupdFbk <- dfxfbk %>% select(name, Site, Feedback) %>%  group_by(Site) %>%  mutate(all_feedback = paste(Feedback, collapse = "\n\n"))
groupdFbk<-groupdFbk[,c("Site","all_feedback")]
groupdFbk<-groupdFbk[!duplicated(groupdFbk),]
write.csv(groupdFbk,file="fbk4.csv")
