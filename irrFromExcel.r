
library(readxl)
library(readr)
library(irrCAC)
library(data.table)

doCalc<-function(sheetDF)
{
  percentages<-c()
  judgementsPerColumn<-c()
  totalJudgements=0
  totalAgreements=0
  print(names(sheetDF))
  for (i in names(sheetDF)) 
  {
    print(i)
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


fileChosen<-file.choose(new = FALSE)
mysheets <- read_excel_allsheets(fileChosen)
tab_names <- excel_sheets(fileChosen)

maxsheets = length(mysheets)



masterlist <- list() #create an empty list
looplist <- list()

lapply(mysheets, function(df) {
  looplist <- list()
  print(sprintf("Reading file %s",tab_names[[length(masterlist)+1]]))
  looplist[["fn"]]<-tab_names[[length(masterlist)+1]]
  #looplist[["pcAggreement"]] <-doCalc(df)
  tmpDf <- cbind(df)
  colnames(tmpDf) <- NULL
  irrDf <- t(tmpDf)
  
  looplist[["gwet"]] <-doGwet(irrDf)
  looplist[["pa"]] <-doPA(irrDf)
  looplist[["krippAR"]] <-doKrippenAlphaRaw(irrDf)
  looplist[["fleissKappa"]] <-doFleissKappa(irrDf)
  looplist[["bp"]] <-doBPRaw(irrDf)
  masterlist[[length(masterlist)+1]] <<- looplist
  
})

do.call("rbind", masterlist)
q<-data.table::rbindlist(masterlist)

View(q)

qq<-cbind(q)

qq$fn<-NULL
res <- cor(qq)
res<- round(res, 2)
print(res)
