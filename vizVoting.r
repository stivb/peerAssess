library(readr)
library(ggplot2)
library(tidyverse)

give.n <- function(x){   
  return(c(y = 1, label=length(x)))
  }   


fileChosen<-file.choose(new = FALSE)

gapsAt<-as.numeric(4)
colCounts<-c()

data <- read_csv(fileChosen, col_names = FALSE)

for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("X", "", colnames(data)[col])
  #colCounts[col]<-length(c(data[,col])
  coldata=NULL
  coldata<-unlist(c(na.omit(data[,col])))
  print(coldata)
  print(length(coldata))
}

newtab<-gather(data,key=key,value=value)
#this puts the x factors into
newtab$group <- as.integer((as.numeric(newtab$key)-1)/gapsAt)
newtab$col <- ((as.numeric(newtab$key)-1)%% gapsAt)+1
newtab$key <- as.character(newtab$key)
newtab$key <- factor(newtab$key, levels=unique(newtab$key))




colors<-c("white","grey","silver")

ggplot(newtab,aes(x=key,y=value,fill = factor(col))) +  
  geom_violin(colour = "black", alpha=0.1)+ 
  geom_jitter(width = 0.2, height = 0.2, alpha=.2) + 
  stat_summary(fun=mean, geom="line",  size=1, 
               color="blue",  alpha=0.5,
               aes(group = 1))+
  stat_summary(fun=mean, geom="point",  size=14, 
               color="yellow",  fill="yellow", alpha=0.2,
               aes(group = 1)) +
  stat_summary(
    fun.data = give.n, 
    geom = "text", 
    color="blue",
    hjust = 0,
    vjust = 0,
    position=position_nudge(x = -0.1, y = -.5)
  ) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  facet_wrap(~group,scales="free_x")


  


