
library(readxl)
library(readr)
library(irrCAC)
library(data.table)
library(tcltk)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)



doHists<-function(sheetDF)
{
  names(sheetDF)[1:ncol(sheetDF)]<- paste('var', 1:ncol(sheetDF),sep="")
  oDf<-gather(sheetDF)
  oDf$value<-as.numeric(oDf$value)
  ggplot(oDf, aes(value)) + 
    geom_histogram(bins = 10,na.rm = TRUE) + 
    facet_wrap(~key, scales = 'free_x')
}


meanSd<-function(sheetDF)
{
  resultz<-c()
  for (i in names(sheetDF)) 
  {
    
    columnVals <-sheetDF[,i]
    columnVals<-na.omit(as.numeric(columnVals))#remove all NAs
    if (length(columnVals)>5)
    {
      #sprintf("column %s has %d items" , i, length(columnVals) )
      p<-sd(columnVals)
      resultz<-append(resultz,p)
    }
    
  } 
  denom<-max(resultz)-min(resultz)
  if (denom==0) return (0)
  return (mean(resultz)/denom)
}

propUniforms<-function(sheetDF)
{
  resultz<-c()
  
  for (i in names(sheetDF)) 
  {
    
    columnVals <-sheetDF[,i]
    columnVals<-na.omit(as.numeric(columnVals))#remove all NAs
    if (length(columnVals)>5)
    {
       p<-ks.test(columnVals, "punif", min(columnVals), max(columnVals))$p
      resultz<-append(resultz,round(p,3))
    }
    
  } 
  if (length(resultz)==0) return (0)
  else 
  return(length(resultz[resultz>0.05])/length(resultz))
}

doPercentAgg<-function(sheetDF)
{
  percentages<-c()
  judgementsPerColumn<-c()
  totalJudgements=0
  totalAgreements=0
  print(names(sheetDF))
  for (i in names(sheetDF)) 
  {
    cc<-sheetDF[i]
    qq<-gsub('-',NA,cc[[1]],fixed=TRUE) #replaces dashes with NAs
    
    bb<-as.numeric(na.omit(qq))#remove all NAs
    zz<-combn(bb,2,simplify=FALSE)  #get the combinations
    judgementsPerColumn <-append(judgementsPerColumn,length(bb))
    totalJudgements=length(zz) + totalJudgements
    tbl<-table(unlist(lapply(zz,function(x) abs(x[1]-x[2])>0)))
    disAggThisColumn=tbl["FALSE"]
    pc<-prop.table(tbl)["FALSE"]
    percentages <-append(percentages,pc)
    totalAgreements = disAggThisColumn + totalAgreements
  }
  
  if (totalJudgements==0) return (0)
  percentage<-totalAgreements*100/totalJudgements
  aveJudgementsPerColumn<-mean(judgementsPerColumn)
  return(percentage)
}


doGwet<-function(sheetDF)
{
  return (gwet.ac1.raw(sheetDF)$est$coeff.val)
}

doPA<-function(sheetDF)
{
  return (pa.coeff.raw(sheetDF)$est$coeff.val)
}

doICCNA<-function(sheetDF)
{
  tmpRes<-iccNA(sheetDF, rho0 = 0, conf = 0.95, detail = FALSE, oneG = TRUE, Cs = 10000)
  #print(tmpRes)
  return (tmpRes$ICCs[6,1])
}

doKrippenAlphaRaw<-function(sheetDF)
{
  return (krippen.alpha.raw(sheetDF)$est$coeff.val)
}

doFleissKappa<-function(sheetDF)
{
  return(fleiss.kappa.raw(sheetDF)$est$coeff.val)
}

doCongerKappa<-function(sheetDF)
{
  return (conger.kappa.raw(sheetDF)$est$coeff.val)
}

doBPRaw<-function(sheetDF)
{
  return (bp.coeff.raw(sheetDF)$est$coeff.val)
}


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = FALSE))
  if(!tibble) x <- lapply(x, as.data.frame)
}


#############BODY###########################

pdf("output.pdf")

fileChosen<-file.choose(new = FALSE)
mysheets <- read_excel_allsheets(fileChosen)
tab_names <- excel_sheets(fileChosen)

maxsheets = length(mysheets)



masterlist <- list() #create an empty list
looplist <- list()

# lapply(mysheets, function(df) {
#   cornerCell<-df[1,1]
#   if (str_detect(cornerCell,"[^A-Z0-9][IG]")!=TRUE)
#   {
#   stop()
#   }
#   firstRow<-na.omit(df[1,])
#   firstRow<-firstRow[firstRow!=""]
#   if (sum(duplicated(firstRow))!=0)
#   {
#     stop()
#   }
# 
# })
  

lapply(mysheets, function(df) {
  looplist <- list()
  print(sprintf("Reading file %s",tab_names[[length(masterlist)+1]]))
  looplist[["fn"]]<-tab_names[[length(masterlist)+1]]
  looplist[["pcAggreement"]] <-doCalc(df)
  tmpDf <- cbind(df)
  colnames(tmpDf) <- NULL
  irrDf <- t(tmpDf)
  nullVotesList<-checkUniforms(df)
  nullVotesPercent<-length(nullVotesList[nullVotesList>0.05])/length(nullVotesList)
  
  looplist[["pcUniform"]]<-propUniforms(df)
  looplist[["meanSd"]]<-meanSd(df)
  looplist[["gwet"]] <-doGwet(irrDf)
  looplist[["pa"]] <-doPA(irrDf)
  looplist[["pcAgg"]]<-doPercentAgg(df)
  looplist[["krippAR"]] <-doKrippenAlphaRaw(irrDf)
  looplist[["fleissKappa"]] <-doFleissKappa(irrDf)
  looplist[["bp"]] <-doBPRaw(irrDf)
  doHists(df)
  
  #looplist[["iccNA"]]<-doICCNA(irrDf)
  #print("iccNa")
  #print(looplist[["iccNA"]])

  looplist[["min"]]<-min(as.numeric(irrDf), na.rm=TRUE)
  looplist[["max"]]<-max(as.numeric(irrDf), na.rm=TRUE)
  looplist[["raters"]]<-nrow(df)
  looplist[["judgements"]]<-ncol(df)
  allVals<-table(unlist(df))
  
  mycols<-as.list(1:6)
  mycols<-append(mycols,"NA")
  #print("my cols is")
  #print(mycols)
  
  for(i in mycols)
  {
    
    if (as.character(i) %in% names(allVals))
    {
      looplist[[as.character(i)]]<-round(allVals[[as.character(i)]],1)
    }
    else
    {
      looplist[[as.character(i)]]<-0
    }
   }

 masterlist[[length(masterlist)+1]] <<- looplist
  
})

do.call("rbind", masterlist)
q<-data.table::rbindlist(masterlist)

View(q)

qq<-cbind(round(q))

qq$fn<-NULL
qq<-qq[,1:(length(qq)-11)]
res <- cor(qq,use="pairwise.complete.obs")
res<- round(res, 2)
print(res)
dev.off()
