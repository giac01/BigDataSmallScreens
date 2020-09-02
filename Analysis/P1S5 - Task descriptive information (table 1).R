#This won't run on synthetic data untill the output is created for all .Rdata files below from running other scripts. 

source("P1S0 - Load Data.R")


#Load Data
  base::load(file.path(RED_OUTPUTDATA_LOCATION, "TimeTakenTable.RData")) #Data on how long each task takes
  base::load(file.path(RED_OUTPUTDATA_LOCATION, "CTT_PlotData.Rdata")) #Reliability metrics for each task with confidence intervals 
    CTT_PlotData = rbind.data.frame(CTT_PlotData[1,],rep(NA,length(CTT_PlotData)),rep(NA,length(CTT_PlotData)),CTT_PlotData[2:nrow(CTT_PlotData),])
    CTT_PlotData$Tasks3 = CogV_Accuracy_Labels
    CTT_PlotData$Tasks3 = gsub("Go/No-Go","G/N-G",CTT_PlotData$Tasks3)
    CTT_PlotData$Tasks3[12] = "Non-Symbolic Num. Discrim."
  base::load(file.path(RED_OUTPUTDATA_LOCATION, "Cor_list.Rdata"))
  base::load(file.path(RED_OUTPUTDATA_LOCATION, "CogV_NumberSchoolTesting.Rdata"))
  #load("change_R2_out.Rdata")
  base::load(file.path(RED_OUTPUTDATA_LOCATION, "change_R2.Rdata"))
    change_R2 = gsub("^(-?).","\\1",format(change_R2^.5,digits=0,nsmall=2))
  

TimeTakenTable_edit = TimeTakenTable[c(1:4,4,4,5:12),] #Add empty rows for multiple tasks
  rownames(TimeTakenTable_edit)=NULL

Cor_list[[1]]$conf.int[1:2]
Correlations = sapply(Cor_list, function(x) x$estimate)
Correlations_CI =  sapply(Cor_list, function(x) x$conf.int[1:2])

TimeTakenTable
  
#cbind.data.frame(names(Correlations),Correlations_CI,CTT_PlotData$Tasks2,rownames(TimeTakenTable[c(1:4,4,4,5:12),]),names(CogV_NumberSchoolTesting)) #Check correct data is on same rows!
  
TaskData = cbind.data.frame(CTT_PlotData$Tasks3,CogV_NumberSchoolTesting,Correlations,t(Correlations_CI),change_R2,CTT_PlotData[,c(1:3)],TimeTakenTable_edit)
  rownames(TaskData) = NULL
  TaskData = format(TaskData,digits=0,nsmall=2)
#Format Numbers
  TaskData = apply(TaskData,2,function(x)gsub("^[\\s]?(0[\\.])", ".", as.character(x)))
  TaskData = apply(TaskData,2, function(x) gsub("0[\\.]","0.",gsub("-0[\\.]","-.",x)))
  TaskData[TaskData=="  NA"]=" "
  TaskData
  
  
  #"$[\\]Delta R^{2^{1/2}}$"
colnames(TaskData) = c("", "", "r", "LB", "UB", "Δr", "Rel", "LB","UB","10%","50%","90%" )
save(TaskData,file=file.path(RED_OUTPUTDATA_LOCATION, "TaskData.Rdata"))


#Write table to clipboard for copy and pasting into word/excel ... 
write.table(TaskData, file="clip", sep="\t", row.names=FALSE)
clipr::write_clip(TaskData)
# rownames(TaskData)
# TaskData
# ncol(TaskData)
# library(markdown)
# library(formattable)
# library(kableExtra)
# 
# knitr::kable(TaskData) %>%
#   #kableExtra::kable_styling("striped") %>%
#   kableExtra::add_header_above(c("Task Name","N","Correlation With Academic Achievement"=4,"Coefficient H Reliability"=3,"Time Taken (Percentile)"=3))

