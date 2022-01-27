library(readr)
library(irrCAC)
library(data.table)

doCalc<-function(sheetDF)
{
  percentages<-c()
  judgementsPerColumn<-c()
  totalJudgements=0
  totalAgreements=0
  for (i in names(sheetDF)) 
  {
    cc<-sheetDF[i]
    qq<-Filter(is.numeric,na.omit(cc[[1]]))
    zz<-combn(qq,2,simplify=FALSE)
    judgementsPerColumn <-append(judgementsPerColumn,length(qq))
    totalJudgements=length(zz) + totalJudgements
    tbl<-table(unlist(lapply(zz,function(x) abs(x[1]-x[2])>1)))
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


files = list.files(pattern="*.csv")
masterlist <- list() #create an empty list
looplist <- list()

lapply(files, function(i) {
  looplist <- list()
  print(sprintf("Reading file %s",i))
  looplist[["fn"]]<-i
  df <- read_csv(i, col_names=FALSE, col_types = cols())
  looplist[["pcAggreement"]] <-doCalc(df)
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







