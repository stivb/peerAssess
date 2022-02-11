
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
library(GGally)
library(reshape2)
library(ggcorrplot)


tutorStudentSplitSheet<-function(df)
{
  cnames<-unlist(df[1,-1],use.name=FALSE)
  cnames<-c("marker",cnames)
  colnames(df)<-cnames
  rnames<-df[-1,1]
  empties <- sprintf("empty%s",seq(1:99))
  rnames[is.na(rnames)] <- empties
  df<-df[-1,-1]
  rownames(df)<-rnames
  
  numcols=ncol(df)
  #basically incrementing a new column called markerGroupId whenever there is an empty (all na) row
  df$markerGroupId <- cumsum(rowSums(is.na(df))==numcols)
  #splitting based on those empty rows
  return (split(df, df$markerGroupId))
}

makeGradeSheet<-function(vAveStudentScores,vAveTutorScores,nCriteria,critProportions,tsProportions)
{
  
  
  if (length(vAveStudentScores)!=length(vAveTutorScores)) return (NULL);
  if (length(vAveStudentScores) %% nCriteria!=0) return (NULL);
  
  tutorWeighting=tsProportions[1]/sum(tsProportions)
  studentWeighting=1-tutorWeighting
  
  critProportions<-critProportions/sum(critProportions)
  
  studentScoresNoNames<-as.numeric(vAveStudentScores)
  
  retval<-c()
  
  loopSeq<-seq(from=1, to=length(vAveStudentScores), by=nCriteria)
  print(loopSeq)
  
  for(i in loopSeq)
  {
    #basically going through the marks horizontally nCriteria at a time
    #find the first criteria in each case, then go through the remainders
    #then go on to the next artefact to mark
    tmpGradeSheetList <- list()
    
    originatorId<-names(vAveStudentScores)[i]
    
    tmpGradeSheetList[["originatorId"]]<-originatorId
    
    artefactEnd<-i + (nCriteria-1)
    
    for (j in seq(from=i,to=artefactEnd))
    {
      heading<-getColHead(i,nCriteria,j,"s")
      tmpGradeSheetList[[heading]]<-studentScoresNoNames[j]
      heading<-getColHead(i,nCriteria,j,"t")
      tmpGradeSheetList[[heading]]<-vAveTutorScores[j]
    }
    
    
    studentFactored<-vAveStudentScores[i:artefactEnd]*critProportions
    #tmpGradeSheetList[["factoredStudent"]]<-paste(unlist(studentFactored,use.names=FALSE),sep=" ")
    tutorFactored<-vAveTutorScores[i:artefactEnd]*critProportions
    #tmpGradeSheetList[["factoredTutor"]]<-paste(tutorFactored,sep=" ")
    sSum<-sum(unlist(studentFactored,use.names=FALSE))
    tmpGradeSheetList[["sSum"]]<-sSum
    tSum<-sum(unlist(tutorFactored))
    tmpGradeSheetList[["tSum"]]<-tSum
    totalForGroup <- sSum*studentWeighting + tSum*tutorWeighting
    tmpGradeSheetList[["total"]] <- totalForGroup
    
    gradeSheetList[[length(gradeSheetList)+1]] <<- tmpGradeSheetList
    
    
  }
  
  return (as.data.frame(do.call(rbind, gradeSheetList)))
}

doTblExports<-function(tableList,dStem)
{
  for (name in names(tableList)) {
    tbl<-tableList[[name]]
    doTblExport(tbl,name,dStem)
  }
  
}

doTblExport<-function(data, title, loc)
{
  csvPath<-paste(loc,"/",title,".csv",sep="")
  htmlPath<-paste(loc,"/",title,".htm",sep="")
  tblDf<-apply(data,2,as.character)
  write.csv(tblDf,csvPath)
  print(xtable(tblDf), type = "html",file=htmlPath)
}

getColHead<-function(startPos,nCrit,nowPos,ts)
{
  return(paste(ts, "_crit",(nowPos-startPos)+1,sep=""))
}

getAgreements<-function(tIrrDf,weightsToUse,resultList)
{
  resultList[["gwet"]] <-doGwet(tIrrDf,weightsToUse)
  resultList[["pa"]] <-doPA(tIrrDf,weightsToUse)
  resultList[["krippAR"]] <-doKrippenAlphaRaw(tIrrDf,weightsToUse)
  resultList[["fleissKappa"]] <-doFleissKappa(tIrrDf,weightsToUse)
  resultList[["bp"]] <-doBPRaw(tIrrDf,weightsToUse)
  resultList[["kendall"]]<-doKendall(tIrrDf)
  resultList[["cronbach"]]<-doCronbach(tIrrDf)
  return(resultList)
}



getCustomMeasures<-function(df,maxx,minn,resultList)
{
  fullRange=maxx-minn
  resultList[["pcUniform"]]<-propUniforms(df)
  resultList[["meanSd"]]<-meanSd(df)
  resultList[["meanIQR"]]<-meanIQR(df)*100/fullRange
  resultList[["meanMad"]]<-meanMad(df)*100/fullRange
  resultList[["pcAgg"]]<-doPercentAgg(df)
  resultList[["min"]]<-minn
  resultList[["max"]]<-maxx
  resultList[["raters"]]<-nrow(df)
  resultList[["judgements"]]<-ncol(df)
  return (resultList)
}


doTSCorrReport<-function(vAveStudentScores,vAveTutorScores,nCriteria)
{
  if (length(vAveStudentScores)!=length(vAveTutorScores)) return (NULL);
  if (length(vAveStudentScores) %% nCriteria!=0) return (NULL);
  retval<-list()
  retval[["overallCor"]]= cor(vAveStudentScores,vAveTutorScores)
  for(i in 1:nCriteria)
  {
    subvAveStudentScores<-vAveStudentScores[seq(i, length(vAveStudentScores), nCriteria)]
    subvAveTutorScores<-vAveTutorScores[seq(i, length(vAveTutorScores), nCriteria)]
    critHeader<-paste("Crit",i,sep="")
    retval[[critHeader]] = cor(subvAveStudentScores,subvAveTutorScores)
  }
  
  retval[["tutorMean"]]<-mean(vAveTutorScores,na.rm=TRUE)
  retval[["tutorSd"]]<-sd(vAveTutorScores,na.rm=TRUE)
  retval[["studentMean"]]<-mean(vAveStudentScores,na.rm=TRUE)
  retval[["studentSd"]]<-sd(vAveStudentScores,na.rm=TRUE)
  retval[["fxSize"]]<-(retval[["studentMean"]]-retval[["tutorMean"]])/
                      (retval[["studentSd"]]+retval[["tutorSd"]])/2
  
  retval[] <- lapply(retval,round,2)
  return (as.data.frame(do.call(cbind, retval)))
}


auditIndStudentMarking<-function(studentId, indStudentMarks, tutorAveScores, minMark,maxMark)
{
  tmpMarkerAuditList<-list()
  tMean<-mean(tutorAveScores,na.rm=TRUE)
  tSd<-sd(tutorAveScores,na.rm=TRUE)
  indStudMarking<-as.numeric(indStudentMarks)
  i_sMean<-mean(indStudMarking,na.rm=TRUE)
  i_sSd<-sd(indStudMarking,na.rm=TRUE)
  marksComp<-indStudMarking-tutorAveScores
  
  tmpMarkerAuditList[["studentId"]] <-studentId
  tmpMarkerAuditList[["cor"]] <- cor(indStudMarking,tutorAveScores,use="pairwise.complete.obs")
  tmpMarkerAuditList[["ct"]]<- length(indStudMarking[!is.na(indStudMarking) & indStudMarking>0])
  tmpMarkerAuditList[["fxSize"]]<-(i_sMean-tMean)/(i_sSd+tSd)/2;
  tmpMarkerAuditList[["iqr"]]<-IQR(indStudMarking,na.rm=TRUE)
  tmpMarkerAuditList[["overmarks"]]<-length(marksComp[!is.na(marksComp) & marksComp>2])
  tmpMarkerAuditList[["undermarks"]]<-length(marksComp[!is.na(marksComp) & marksComp<(-2)])
  
  tmpMarkerAuditList<-addMarkCounts(indStudMarking, tmpMarkerAuditList, minMark, maxMark)
  return (tmpMarkerAuditList)
}

addMarkCounts<-function(rawMarks,resultsList,from,to)
{
  srcTbl<-table(rawMarks)
  mycols<-as.list(from:to)
  mycols<-append(mycols,"NA")
  
  for(i in mycols)
  {
    if (as.character(i) %in% names(srcTbl))
    {
      resultsList[[as.character(i)]]<-round(srcTbl[[as.character(i)]],1)
    }
    else
    {
      resultsList[[as.character(i)]]<-0
    }
  }
  return (resultsList)
}





doHist<-function(sheetDF,graphsToPlot,dStem)
{
  if (!('doHists' %in% graphsToPlot))return(NULL);
  
  fn<-basename(dStem)
  pngName<-paste(dStem,".png",sep="")
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

doCGram<-function(sheetDf,dStem)
{
  pngName1<-paste(dStem,"cgram1.png",sep="")
  
  studentCorrMatrix<-cor(t(data.matrix(sheetDf)), 
                         use = "pairwise.complete.obs")
  
  
  ggcorr(data = NULL,
         cor_matrix = studentCorrMatrix,
                      method = "circle",
                      nbreaks = 6,
                      palette = "Spectral")
  ggsave(pngName1)
  
  pngName2<-paste(dStem,"cgram2.png",sep="")
  ggcorrplot(studentCorrMatrix,  type = "lower",
             outline.col = "white",
             #insig = "blank",
             ggtheme = ggplot2::theme_gray,
             colors = c("#E46726", "white", "#6D9EC1"))
  ggsave(pngName2)
  
  
}

doEvtIrrCsv<-function(lst,dStem)
  {
  myList<-lst
  csvName<-paste(dStem,"irr.csv",sep="")
  headings<-paste(names(myList),collapse=",")
  values<-paste(unlist(unname(myList)),collapse=",")
  fileConn<-file(csvName)
  writeLines(c(headings,values), fileConn)
  close( fileConn )
}




doMarkerStack<-function(sheetDF,graphsToPlot,dStem)
{
  if (!('doMarkerStacks' %in% graphsToPlot)) return(NULL);
  fn<-basename(dStem)
  pngName<-paste(dStem,"stack.png",sep="")
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

doPlot<-function(tutorAveScores, studentAveScores, dStem)
{
  tutorAveScores<-as.numeric(tutorAveScores)
  reg_model <- lm(studentAveScores ~ tutorAveScores)
  pngName<-paste(dStem,"_plot.png",sep="")
  png(pngName)
  plot(tutorAveScores,studentAveScores)
  abline(reg_model, col="steelblue")
  dev.off()
}

doCriterionViolins<-function(sheetDF,graphsToPlot,dStem)
{
  pngName<-paste(dStem,"_viol.png",sep="")
  x<-melt(sheetDF,measure.vars=names(sheetDF))
  x$value<-as.numeric(x$value)
  plt<-ggplot(data=x,aes(x=variable,y=value))
  plt+geom_violin()+ theme_minimal() + labs(x="Criteria",y="x")
  ggsave(pngName)
}

doHeatMap<-function(sheetDF,graphsToPlot,dStem)
{
  if (!('doHeatMaps' %in% graphsToPlot)) return(NULL);
  pngName<-paste(dStem,"_heat.png",sep="")
  names(sheetDF) <- gsub('\\.\\.\\.', "", names(sheetDF))
  rownames(sheetDF) <- gsub('^(\\d)$', "0\\1", rownames(sheetDF))
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
    theme_light(base_size = 9) + 
    theme(axis.text.x = element_text(angle = -45, hjust=1)) + 
    geom_tile()  + 
    #scale_fill_manual(values=my.cols(dt2Max))
    scale_fill_manual(breaks = levels(dt2$value),
     values = coul, na.value="white")
  ggsave(pngName)
}

#x = reorder(x, sort(as.numeric(x)))

doStack<-function(sheetDF,graphsToPlot,dStem)
{
  if (!('doStacks' %in% graphsToPlot))  return(NULL);
  fn<-basename(dStem)
  pngName<-paste(dStem,"__stack.png",sep="")
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


doGwet<-function(sheetDF, weightsToUse)
{
  return (gwet.ac1.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doPA<-function(sheetDF, weightsToUse)
{
  return (pa.coeff.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doICCNA<-function(sheetDF, weightsToUse)
{
  tmpRes<-iccNA(sheetDF, rho0 = 0, conf = 0.95, detail = FALSE, oneG = TRUE, Cs = 10000)
  #print(tmpRes)
  return (tmpRes$ICCs[6,1])
}

doKrippenAlphaRaw<-function(sheetDF, weightsToUse)
{
  return (krippen.alpha.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doFleissKappa<-function(sheetDF, weightsToUse)
{
  return(fleiss.kappa.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doCongerKappa<-function(sheetDF, weightsToUse)
{
  return (conger.kappa.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doBPRaw<-function(sheetDF, weightsToUse)
{
  return (bp.coeff.raw(sheetDF, weights=weightsToUse)$est$coeff.val)
}

doCronbach<-function(sheetDF, weightsToUse)
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

doSummativeGraph<-function(sheetsMatrix, graphsToPlot, excelPath)
{
  if (!str_detect(graphsToPlot,"doSummativeGraph")) return(NULL);
  tSheetCounts<-t(sheetsMatrix)
  pcSheetCounts<-round( prop.table(as.matrix(tSheetCounts),2) * 100 , 3 )
  coul <- brewer.pal(6, "Set1")
  png(excelPath,"/","summative.png",sep="")
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
  dev.off()
}


