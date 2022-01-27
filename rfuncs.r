library(readxl)
library(readr)
library(irrCAC)
library(data.table)
library(ggplot2)
library(tidyverse)


give.n <- function(x){   
  return(c(y = 1, label=length(x)))
}   

graphVoting<-function(data,gapsAt,pngName)
{
print ("in function")
for ( col in 1:ncol(data)){
  colnames(data)[col] <-  sub("X", "", colnames(data)[col])
}
  
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = FALSE,na=c("","*","-")))
  if(!tibble) x <- lapply(x, as.data.frame)
}  

newtab<-gather(data,key=key,value=value)
#this puts the x factors into
newtab$group <- as.integer((as.numeric(newtab$key)-1)/gapsAt)
newtab$col <- ((as.numeric(newtab$key)-1)%% gapsAt)+1
newtab$key <- as.character(newtab$key)
newtab$key <- factor(newtab$key, levels=unique(newtab$key))



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

ggsave(pngName)
}






doCalc<-function(sheetDF,gap)
{
  
  percentages<-c()
  judgementsPerColumn<-c()
  totalJudgements=0
  totalAgreements=0
  for (i in names(sheetDF)) 
  {
    cc<-sheetDF[i]
    qq<-gsub("-|*",NA,cc[[1]],fixed=TRUE) #replaces dashes and starts with NAs
    bb<-as.numeric(na.omit(qq))#remove all NAs
    if (length(bb)<3) next
    zz<-combn(bb,2,simplify=FALSE)  #get the combinations
    judgementsPerColumn <-append(judgementsPerColumn,length(bb))
    totalJudgements=length(zz) + totalJudgements
    gp<-gap-1
    tbl<-table(unlist(lapply(zz,function(x) abs(x[1]-x[2])>gp)))
    aggsThisColumn=tbl["FALSE"] #all those judgements which are not greater than gp apart
    pc<-prop.table(tbl)["FALSE"]
    percentages <-append(percentages,pc)
    totalAgreements = aggsThisColumn + totalAgreements
  }
  
  percentage<-totalAgreements/totalJudgements
  aveJudgementsPerColumn<-mean(judgementsPerColumn)
  return(as.numeric(percentage))
}

doCalc2<-function(sheetDF,gap)
{
  print("doCalc2")
  allJudgements<-c()
  for (i in names(sheetDF)) 
  {
    cc<-sheetDF[i]
    qq<-gsub("-|*",NA,cc[[1]],fixed=TRUE) #replaces dashes and starts with NAs
    bb<-as.numeric(na.omit(qq))#remove all NAs
    if (length(bb)<3) next
    zz<-combn(bb,2,simplify=FALSE)  #get the combinations
    allJudgements<-append(allJudgements,zz)
  }
  gp<-gap-1
  totalJudgements=length(allJudgements)
  tbl<-table(unlist(lapply(allJudgements,function(x) abs(x[1]-x[2])>gp)))
  overallAggs=tbl["FALSE"] 
  return(as.numeric(overallAggs/totalJudgements))
}

countPercentileGaps<-function(sheetDF)
{
gapCount<-0
  for (i in names(sheetDF)) 
  {
  cc<-as.numeric(unlist(sheetDF[i]))
  pTiles<-quantile(cc, c(.25, .75),na.rm = TRUE) 
  gap<-abs(pTiles[[2]]-pTiles[[1]])
  if (!is.numeric(gap) | is.na(gap)) next
  print(gap)
  if (gap>1) gapCount<-gapCount+1
  }
return (gapCount/length(names(sheetDF)))
}


doGwet<-function(sheetDF,weighting)
{
  return (gwet.ac1.raw(sheetDF,weights=weighting)$est$coeff.val)
}

doPA<-function(sheetDF,weighting)
{
  return (pa.coeff.raw(sheetDF,weights=weighting)$est$coeff.val)
}

doKrippenAlphaRaw<-function(sheetDF,weighting)
{
  return (krippen.alpha.raw(sheetDF,weights=weighting)$est$coeff.val)
}

doFleissKappa<-function(sheetDF,weighting)
{
  return(fleiss.kappa.raw(sheetDF,weights=weighting)$est$coeff.val)
}

doCongerKappa<-function(sheetDF,weighting)
{
  return (conger.kappa.raw(sheetDF,weights=weighting)$est$coeff.val)
}

doBPRaw<-function(sheetDF,weighting)
{
  return (bp.coeff.raw(sheetDF,weights=weighting)$est$coeff.val)
}
