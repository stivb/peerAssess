
library(readxl)
library(readr)
library(irrCAC)
library(data.table)
library(tcltk)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(reshape2)
library(RColorBrewer)
library(DescTools)
library(dplyr)
library(tibble)
library(irr)
library(ltm)


doHists<-function(sheetDF,fn)
{
  if (!('doHists' %in% graphsToPlot))return(NULL);
  pngName<-paste(fn,".png",sep="")
  varz=unlist(strsplit("abcdefghijklmnopqrstuvwxyz", split = ""))
  names(sheetDF)[1:ncol(sheetDF)]<- varz[1:ncol(sheetDF)]
  oDf<-gather(sheetDF)
  oDf$value<-as.numeric(oDf$value)
  
  oDfMax<- max(na.omit(oDf$value))
  
  ggplot(oDf, aes(value)) + 
    geom_histogram(bins = oDfMax,na.rm = TRUE) + 
     ggtitle(fn) +
      scale_x_continuous(breaks = seq(1, 5, 1)) + 
    facet_wrap(~key, scales = 'fixed')
  ggsave(pngName)
  
  
}


doMarkerStacks<-function(sheetDF,fn)
{
  if (!('doMarkerStacks' %in% graphsToPlot)) return(NULL);
  pngName<-paste(fn,"stack.png",sep="")
  allVals<-as.numeric(unlist(sheetDF))
  maxVal<-max(allVals, na.rm=TRUE)
  freq <- apply(sheetDF, 1, function(x) table(factor(x, levels = c(1:maxVal),  ordered = TRUE)))
  freq<- freq[, colSums(freq != 0) > 0]
  coul <- brewer.pal(maxVal, "Set1")
  png(pngName)
  barplot(freq, col=coul, 
          xlab="", 
          ylab="marks", 
          # legend.text=TRUE,
          # args.legend=list(x="bottom", horiz=TRUE, inset=-0.1),
          main=fn)
  legend(x = "bottom",
         horiz=TRUE,
         inset = c(0, -0.1),
         col=coul,
         fill=coul,
         legend = c("1","2","3","4","5"),
         xpd=TRUE
         )
  abline( h = 10, col="white")
  abline( h = 20, col="white")
  dev.off()
  
  # ggplot(propfreq, aes(value)) + 
  #   geom_bar(position="stack", stat="identity") + 
  #   ggtitle(fn)
  # ggsave(pngName)
}

my.cols <- function(maxVal) {
  coul <- brewer.pal(9, "Set1")
  naCol<-as.numeric(maxVal)+1
  coul[naCol]<-"white"
  return (coul)
}

doHeatMaps<-function(sheetDF,fn)
{
  if (!('doHeatMaps' %in% graphsToPlot)) return(NULL);
  pngName<-paste(fn,"_heat.png",sep="")
  names(sheetDF) <- gsub("...", "", names(sheetDF))
  coul <- brewer.pal(6, "Set1")
  coul[6]="white"
  dt2 <- sheetDF %>%
    rownames_to_column() %>%
    gather(colname, value, -rowname)
  
  dt2Max<- max(na.omit(as.numeric(dt2$value)))
  
  
  
  dt2$value<-as.factor(dt2$value)
  dt2$rowname<-as.factor(dt2$rowname)
  dt2$colname<-as.factor(dt2$colname)
  
  
  ggplot(dt2, aes(x = reorder(colname,sort(as.numeric(colname))), 
                  y = rowname, 
                  fill = value)) +
    xlab("Marks") + ylab("Students") + 
    geom_tile()  + scale_fill_manual(values=my.cols(dt2Max))
  ggsave(pngName)
}

#x = reorder(x, sort(as.numeric(x)))

doStacks<-function(sheetDF,fn)
{
  if (!('doStacks' %in% graphsToPlot))  return(NULL);
  pngName<-paste(fn,"stack.png",sep="")
  varz=unlist(strsplit("abcdefghijklmnopqrstuvwxyz", split = ""))
  names(sheetDF)[1:ncol(sheetDF)]<- varz[1:ncol(sheetDF)]
  freq <- sapply(sheetDF, function(x) table(factor(x, levels = c(1,2,3,4,5),  ordered = TRUE)))
  coul <- brewer.pal(5, "Set2")
  png(pngName)
  barplot(freq, col=coul, 
          xlab="markers", 
          ylab="marks", 
          legend.text=TRUE,
          args.legend=list(
            x=ncol(freq) + 3,
            y=max(colSums(freq)),
            bty = "n"),
            main=fn)
  dev.off()
  
  # ggplot(propfreq, aes(value)) + 
  #   geom_bar(position="stack", stat="identity") + 
  #   ggtitle(fn)
  # ggsave(pngName)
}

meanIQR<-function(sheetDF)
{
resultz<-c()
for (i in names(sheetDF)) 
{
  columnVals <-sheetDF[,i]
  columnVals<-na.omit(as.numeric(columnVals))#remove all NAs
  if (length(columnVals)>5)
  {
    p<-IQR(columnVals)
    resultz<-append(resultz,p)
  }
  
} 
return (mean(resultz))
}

meanMad<-function(sheetDF)
{
  resultz<-c()
  for (i in names(sheetDF)) 
  {
    columnVals <-sheetDF[,i]
    columnVals<-na.omit(as.numeric(columnVals))#remove all NAs
    if (length(columnVals)>5)
    {
      p<-mad(columnVals)
      resultz<-append(resultz,p)
    }
    
  } 
  return (mean(resultz))
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
  return (gwet.ac1.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doPA<-function(sheetDF)
{
  return (pa.coeff.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doICCNA<-function(sheetDF)
{
  tmpRes<-iccNA(sheetDF, rho0 = 0, conf = 0.95, detail = FALSE, oneG = TRUE, Cs = 10000)
  #print(tmpRes)
  return (tmpRes$ICCs[6,1])
}

doKrippenAlphaRaw<-function(sheetDF)
{
  return (krippen.alpha.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doFleissKappa<-function(sheetDF)
{
  return(fleiss.kappa.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doCongerKappa<-function(sheetDF)
{
  return (conger.kappa.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doBPRaw<-function(sheetDF)
{
  return (bp.coeff.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doCronbach<-function(sheetDF)
{
  tryCatch(
    {
      return (cronbach.alpha(as.data.frame(sheetDF))$alpha)
    },
    error=function(error_message)
    {
      return(NA)
    } 
  )
}

doKendall<-function(sheetDF)
{
  tryCatch(
    {
      return (kendall(sheetDF)$value)
    },
    error=function(error_message)
    {
      return(NA)
    } 
  )
}


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = FALSE))
  if(!tibble) x <- lapply(x, as.data.frame)
}

doSummativeGraph<-function(sheetsMatrix)
{
  if (!str_detect(graphsToPlot,"doSummativeGraph")) return(NULL);
  tSheetCounts<-t(sheetsMatrix)
  pcSheetCounts<-round( prop.table(as.matrix(tSheetCounts),2) * 100 , 3 )
  coul <- brewer.pal(6, "Set1")
  
  barplot(pcSheetCounts, col=coul, 
          xlab="", 
          ylab="marks", 
          names=q$fn,
          las=2,
          cex.names=0.7,
          space=0.2,
          # legend.text=TRUE,
          # args.legend=list(x="bottom", horiz=TRUE, inset=-0.1),
          main="percentages")
  legend(x = "top",
         horiz=TRUE,
         inset = c(0, -0.25),
         col=coul,
         fill=coul,
         legend = c("1","2","3","4","5","6"),
         xpd=TRUE
  )
  abline( h = 20, col="white")
  abline( h = 40, col="white")
  abline( h = 50, col="white")
  abline( h = 60, col="white")
  abline( h = 80, col="white")
}


#############BODY###########################
#weights: choose from 
#identity/ordinal/quadratic/linear/circular/ratio/radical/bipolar
weightsToUse<-"ratio"
#choose among histogram, markerbars, summative


fileChosen<-file.choose(new = FALSE)
mysheets <- read_excel_allsheets(fileChosen)
tab_names <- excel_sheets(fileChosen)
maxsheets = length(mysheets)

graphsToPlot<-c(
  "doSummativeGraph",
  "doMarkerStacks",
  "doHeatMaps",
  "doStacks",
  "doHists")


masterlist <- list() #create an empty list
looplist <- list()


lapply(mysheets, function(df) {
  looplist <- list()
  tabName<-tab_names[[length(masterlist)+1]]
  looplist[["fn"]]<-tabName

  tmpDf <- cbind(df)
  colnames(tmpDf) <- NULL
  irrDf <- t(tmpDf)
  nullVotesList<-propUniforms(df)
  nullVotesPercent<-length(nullVotesList[nullVotesList>0.05])/length(nullVotesList)
  
  fullRange<-max(as.numeric(irrDf), na.rm=TRUE)-min(as.numeric(irrDf), na.rm=TRUE)
  looplist[["pcUniform"]]<-propUniforms(df)
  looplist[["meanSd"]]<-meanSd(df)
  looplist[["meanIQR"]]<-meanIQR(df)*100/fullRange
  looplist[["meanMad"]]<-meanMad(df)*100/fullRange
  looplist[["gwet"]] <-doGwet(as.data.frame(irrDf))
  looplist[["pa"]] <-doPA(as.data.frame(irrDf))
  looplist[["pcAgg"]]<-doPercentAgg(df)
  looplist[["krippAR"]] <-doKrippenAlphaRaw(as.data.frame(irrDf))
  looplist[["fleissKappa"]] <-doFleissKappa(as.data.frame(irrDf))
  looplist[["bp"]] <-doBPRaw(as.data.frame(irrDf))
  looplist[["kendall"]]<-doKendall(as.data.frame(irrDf))
  looplist[["cronbach"]]<-doCronbach(as.data.frame(irrDf))
  
  
  doHists(df, tabName)
  doMarkerStacks(df,tabName)
  doHeatMaps(df,tabName)
  
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

qq<-cbind(q)

qq$fn<-NULL
qq<-qq[,2:(length(qq)-11)]
res <- cor(qq,use="pairwise.complete.obs")
res<- round(res, 2)
print(res)

##just getting the numeric only columns from the data table
sheetCounts<- q %>% dplyr::select(matches("^[0-9]*$"))
doSummativeGraph(sheetCounts)

