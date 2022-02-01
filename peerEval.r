source("peerFuncs.r")
library(readxl)
library(readr)
library(xtable)

#############BODY###########################


fileChosen<-file.choose(new = FALSE)
excelPath <- dirname(fileChosen)


mysheets <- read_excel_allsheets(fileChosen)
tab_names <- excel_sheets(fileChosen)
maxsheets = length(mysheets)

##defaults###
weightsToUse<-"ratio"
uniformMarkRange=TRUE
minScore=1
maxScore=5
nCriteria=2
criteriaProprotions=c(1,1)
tutorStudentProportions<-c(80,20)
criteriaShortNames<-sprintf("c%d", 1:nCriteria) 
criteriaLongNames<-sprintf("Criteria%d", 1:nCriteria) 

graphsToPlot<-c(
   "doSummativeGraph",
   "doMarkerStacks",
   "doHeatMaps",
   "doStacks",
   "doHists"
  )


#############various lists used to compile output sheets##################
workSheetList <- list() #create an empty list
tmpWorkSheetList <- list()
markerAuditList<-list()
tmpMarkerAuditList<-list()
gradeSheetList<-list()


##################output sheets###########################################

oSheetMarksHandout<-data.frame()  #single Event (marks given to groups)
oSheetStudentMarkingAudit<-data.frame()  #single Event (checks marker probity/competence)
oSheetIRRAnalytics<-data.frame()  #multiple events (irr for multiple marking events)
oSheetIRRCorr<-data.frame() #multiple events (the above with correlations between all the measures)
oSheetTSComparisonAnalytics<-data.frame() #single Event

#############################################################


lapply(mysheets, function(df) {
  
  topLeftStr<-paste(as.character(df[1,1]))
  numcols<-ncol(df)
  
  tabName<-tab_names[[length(workSheetList)+1]]
  putativePath = paste(excelPath,tabName,sep="/")
  ifelse(!dir.exists(putativePath), dir.create(putativePath), "Folder exists already")
  
  if (str_detect(topLeftStr,regex("[^\\W][0-9]")))
  {

    df_s<-tutorStudentSplitSheet(df)
    
    #now removing the markerGroupId column from the newly created dataframes
    dfStudents<-df_s$`0`[, -ncol(df_s$`0`)]
    dfTutors<-df_s$`1`[-1, -ncol(df_s$`1`)]

    studentAveScores<-colMeans(as.data.frame(sapply(dfStudents,as.numeric)),na.rm=TRUE)
    if (nrow(dfTutors)==1) tutorAveScores<-as.numeric(dfTutors)
    else tutorAveScores<-colMeans(as.data.frame(sapply(dfTutors,as.numeric)),na.rm=TRUE)
    
    
    #am thinking the tscomparison analytics is canonical rather than event
    oSheetTSComparisonAnalytics<-doTSAnalytics(studentAveScores,tutorAveScores,nCriteria)
    oSheetMarksHandout<<-makeGradeSheet(studentAveScores,tutorAveScores,2,criteriaProprotions,tutorStudentProportions)
    
    
    
    doPlot(as.numeric(tutorAveScores), studentAveScores,tabName, putativePath)
    tMean<-mean(tutorAveScores, na.rm=TRUE)
    tSd<-sd(tutorAveScores, na.rm=TRUE)
    
    
    
    for (ct in 1:nrow(dfStudents)) 
      {
      
      indStudMarking<-as.numeric(unlist(dfStudents[ct,],use.names=FALSE))
      
      markerAuditList[[length(markerAuditList)+1]] <<- 
        auditIndStudentMarking(rownames(dfStudents)[ct], indStudMarking, 
                               tutorAveScores, minScore,maxScore)
    }
    
    oSheetStudentMarkingAudit<<-as.data.frame(do.call(rbind, markerAuditList))

    df<-dfStudents
    }
    
  
  
  tmpWorkSheetList <- list()
  tmpWorkSheetList[["fn"]]<-tabName
  
  tmpDf <- cbind(df)
  colnames(tmpDf) <- NULL
  irrDf <- t(tmpDf)
  nullVotesList<-propUniforms(df)
  nullVotesPercent<-length(nullVotesList[nullVotesList>0.05])/length(nullVotesList)

  tmpWorkSheetList<-getAgreements(as.data.frame(irrDf),weightsToUse,tmpWorkSheetList)
  tmpWorkSheetList<-getCustomMeasures(df,maxScore,minScore,tmpWorkSheetList)
  tmpWorkSheetList<-addMarkCounts(unlist(df), tmpWorkSheetList, 1, 5)
  
  
  doHist(df, tabName, graphsToPlot,putativePath)
  doMarkerStack(df,tabName, graphsToPlot,putativePath)
  doHeatMap(df,tabName, graphsToPlot,putativePath)
  #doCGram(df, tabName,putativePath)
  
  #tmpWorkSheetList[["iccNA"]]<-doICCNA(irrDf)
  #print("iccNa")
  #print(tmpWorkSheetList[["iccNA"]])
  
  doEvtIrrCsv(tmpWorkSheetList,tabName,putativePath)
  

  workSheetList[[length(workSheetList)+1]] <<- tmpWorkSheetList
  
})





do.call("rbind", workSheetList)
oSheetIRRAnalytics<-data.table::rbindlist(workSheetList)

print("Completed")




#View(oSheetMarksHandout)
##View(oSheetStudentMarkingAudit)
#View(oSheetIRRAnalytics)
#View(oSheetIRRCorr)

#outputSheets<-list(oSheetMarksHandout,oSheetStudentMarkingAudit,oSheetIRRAnalytics)

outputSheets<-list()
outputSheets[[ "oSheetMarksHandout" ]] <- oSheetMarksHandout
outputSheets[[ "oSheetStudentMarkingAudit" ]] <- oSheetStudentMarkingAudit
outputSheets[[ "oSheetIRRAnalytics" ]] <- oSheetIRRAnalytics


doTables(outputSheets)

mks <- xtable(oSheetMarksHandout)
print(mks,type="html")

# 
# View(q)
# 
# qq<-cbind(q)
# 
# qq$fn<-NULL
# qq<-qq[,2:(length(qq)-11)]
# res <- cor(qq,use="pairwise.complete.obs")
# res<- round(res, 2)
# print(res)

##just getting the numeric only columns from the data table

#sheetCounts<- q %>% dplyr::select(matches("^[0-9]*$"))
#doSummativeGraph(sheetCounts, graphsToPlot)

# do.call("rbind", markerAuditList)
# mAL<-data.table::rbindlist(markerAuditList)
# View(mAL)

