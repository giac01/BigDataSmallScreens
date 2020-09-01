
######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### #########   NOTE    ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### 

#This code requires access to raw ipad data which is not provided in the repoisitory! 


#Load Data and Functions Needed 
source("P1S0 - Load Data.R")

#libraries used
library(chron)

#Get data from csv file headers
files = as.character(list.files(path=IPAD_DATA_LOCATION, recursive = TRUE,pattern = ".txt$")) #Extract file names
Tasks = (str_match(files,pattern="\\/([(.)0-9a-zA-Z_]+)_[0-9]+_201[8-9]")[,2]) #Extract task name
Dates = as.character(str_match(files,pattern="201[8-9]-[\\d]{2}-[\\d]{2}")) #Extract date
Dates = gsub("^.{2}","",  gsub("-","/",Dates))
Dates = format(as.Date(Dates,format="%y/%m/%d"),"%m/%d/%y") #format american style
Times = as.character(str_match(files,pattern="([\\d]{2}-[\\d]{2}-[\\d]{2}).txt")[,2]) #Extract time  
Times = gsub("-",":",Times)
Times = chron::chron(times=format(Times,times="$h:$m:$s"))
DateTime = chron::chron(times=Times,dates=Dates)
ParticipantID = as.character(str_match(gsub("_201[8-9].*","",files),"[\\d]*$")) #Extract participant ID info
datafileInfo = as.data.frame(cbind(ParticipantID,files,Tasks),stringsAsFactors = FALSE) #Plug into a single data frame 
datafileInfo$DateTime = DateTime

#Only keep school participants 
datafileInfo = datafileInfo[grep("^0[1-9]",ParticipantID,value=FALSE),] #removes DEMO responses

#Remoev old version of line estimation & liquid conservation tasks!  
datafileInfo=datafileInfo[!grepl("LC[.]|LE[.]",datafileInfo$Tasks),]

#Alter some task labels
datafileInfo$Tasks = gsub("[.]","_", datafileInfo$Tasks)

#Loop for all participants
df.list = list()
for (i in unique(datafileInfo$ParticipantID)){
  df = datafileInfo[datafileInfo$ParticipantID==i,]
  df = df[order(df$DateTime),]
  df$TimeTaken = as.numeric(abs(c((df$DateTime[-nrow(df)]-df$DateTime[-1])*60*24,NA))) #convert from day unit (1=day) to minutes
  df = df[!duplicated(df$Tasks,fromLast = TRUE),]  #remove duplicate files
  df.list[[i]]=df
}

#Find time spent on each task per participant,
timeDF = data.frame(matrix(ncol=length(unique(datafileInfo$Tasks)),nrow=length(unique(datafileInfo$ParticipantID))))
ParticipantID_unique = unique(datafileInfo$ParticipantID)
Tasks_unique = unique(datafileInfo$Tasks)


for (r in seq_along(ParticipantID_unique)){
  df = df.list[[ParticipantID_unique[r]]]
  for (c in seq_along(Tasks_unique)){
    if (Tasks_unique[c] %in% df$Tasks){
      timeDF[r,c]=df[df$Tasks==Tasks_unique[c],"TimeTaken"]
    } else {
      timeDF[r,c] = NA
    }
  }
}
colnames(timeDF) = Tasks_unique ; rownames(timeDF) = ParticipantID_unique

sort(colnames(timeDF))
#Sum time Across certain groups
timeDF$ANS_TOTAL = apply(timeDF[,c("ANS_MainTask","ANS_PracticeRounds")],1,sum)
timeDF$Cancellation_TOTAL = apply(timeDF[,c("CancellationTask","CancellationTaskInvisible")],1,sum)
timeDF$cattell_TOTAL = apply(timeDF[,c("cattell_test_1","cattell_test_2")],1,sum)
timeDF$LC_TOTAL = apply(timeDF[,paste0("LC_MainTask",1:4)],1,sum)
timeDF$LE_TOTAL = apply(timeDF[,paste0("LE_MainTask",c("",1:3))],1,sum)


#apply(timeDF_ordered,2,summary)
#apply(timeDF,2,function(x) length(which(!is.na(x))))

timeDF_ordered = timeDF[,c("Cancellation_TOTAL","Digit_Span","Dot_Matrix","GoNoGoTask","cattell_TOTAL",
                           "ReadingTest", "PhonoAwareness","DavisTest",
                           "SumsTest","ANS_TOTAL","LE_TOTAL","LC_TOTAL")]
#Remove participants with missing data
#timeDF_ordered=timeDF_ordered[match(df0$PpsID,rownames(timeDF_ordered)),]
# timeDF_ordered[is.na(df0[,CogV_Accuracy])]=NA

#Check  
cbind(CogV_Accuracy_Labels[c(-5,-6)],colnames(timeDF_ordered))


TimeTakenTable = t(apply(timeDF_ordered, 2, function(x) quantile(na.omit(x),c(.1,.5,.9),na.rm=TRUE)))
rownames(TimeTakenTable) = CogV_Accuracy_Labels[c(-5,-6)]
TimeTakenTable
save(TimeTakenTable,file=file.path(RED_OUTPUTDATA_LOCATION, "TimeTakenTable.Rdata"))
