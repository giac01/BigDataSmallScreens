#Note that these analyses are just for the school sample!


#Load Data and functions 
  source("P1S0 - Load Data.R")
#IRT for each task
  #Note the IRTcheck function is defined in the script titled "RED_3 - RED Functions (Paper 1)"
  
  df0[df0$SchoolID==99,]=NA #Only use data for school cohort for this! 
  
#Number of cores to use for parallel processing
  NumberOfCores = 1
  mirt_Method = "EM" #Use standard EM algorithm with fixed quadrature #See mirt::mirt(method=)
  mirt_Optimizer = "BFGS"

#Cattell Task
  IRT_Cat=IRTcheck(vars="Cattell_ItemCorrect_",mirtoutput =TRUE, mirt_modeltype="2PL", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/5)
  dropItems=which(IRT_Cat$ResultsTable$mirt_a<0)
  IRT_Cat2=IRTcheck(vars="Cattell_ItemCorrect_",mirtoutput =TRUE, mirt_modeltype="2PL", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/5, dropN = dropItems)
  Cat_Alpha=ltm::cronbach.alpha(na.omit(df0[,which(grepl("Cattell_ItemCorrect_",colnames(df0)))[-dropItems]]),CI=TRUE)

#ANS 
  IRT_ANS=IRTcheck(vars="ans_trialcorrect_",mirtoutput =TRUE, mirt_modeltype="Rasch", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/2)
  #dropItems=which(IRT_ANS$ResultsTable$mirt_a<0)
  # IRT_ANS2=IRTcheck(vars="ans_trialcorrect_",mirtoutput =TRUE, mirt_modeltype="2PL", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/2, dropN = dropItems)
  ANS_Alpha=ltm::cronbach.alpha(na.omit(df0[,which(grepl("ans_trialcorrect_",colnames(df0)))[-dropItems]]),CI=TRUE)
  
#Phonological discrimination
  IRT_PD=IRTcheck(vars="PD_itemcorrect_",mirtoutput =TRUE, mirt_modeltype="2PL", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/2)
  dropItems=which(IRT_PD$ResultsTable$mirt_a<0)
  IRT_PD2=IRTcheck(vars="PD_itemcorrect_",mirtoutput =TRUE, mirt_modeltype="2PL", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/2, dropN = dropItems)
  PD_Alpha=ltm::cronbach.alpha(na.omit(df0[,which(grepl("PD_itemcorrect_",colnames(df0)))[-dropItems]]),CI=TRUE)

  IRT_PD2$mirt_model
  nrow(IRT_PD2$ResultsTable)
  print(plot(IRT_PD2$mirt_model, type="trace",theta_lim=c(-5,5)))
  
#Rhyme judgement - all items
  IRT_RJ=IRTcheck(vars="RJ_itemcorrect_",mirtoutput =TRUE, mirt_modeltype="2PL", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/2)
  #dropItems=which(IRT_RJ$ResultsTable$mirt_a<0)
  #IRT_PD2=IRTcheck(vars="PD_itemcorrect_",mirtoutput =TRUE, ltmoutput=TRUE,mirt_modeltype="2PL", dropN = dropItems)
  IRT_RJ$mirt_plotdata$task="RJComplete_"
  IRT_RJ$ResultsTable

#Rhyme judgement - remaining 6 items
  which(apply(df0[,grepl("RJ_itemcorrect_",colnames(df0))],2,function(x) length(which(!is.na(x))))>300)
  dropItems=c(1,2,6,8,10,12:22)
  IRT_RJ2 = IRTcheck(vars="RJ_itemcorrect_",mirtoutput =TRUE, mirt_modeltype="2PL", nCores=NumberOfCores, mirt_Method=mirt_Method, mirt_Optimizer=mirt_Optimizer, guess_param = 1/2, dropN = dropItems)
  RJ_Alpha=ltm::cronbach.alpha(na.omit(df0[,which(grepl("RJ_itemcorrect_",colnames(df0)))[-dropItems]]),CI=TRUE)
  
#Create Plot data
  #load(file = "IRT_Analyses.RData") #Saving workspace as analyses take a very long time to run!
  PlotTasks = rbind.data.frame(IRT_Cat2$mirt_plotdata,IRT_ANS$mirt_plotdata,IRT_PD2$mirt_plotdata,IRT_RJ2$mirt_plotdata,IRT_RJ$mirt_plotdata)
  PlotTasks$task = tolower(gsub("_","",str_extract(PlotTasks$task,"^[a-zA-Z]+_")))
  PlotTasks$isdash =   Convert(as.numeric(PlotTasks$task == "rjcomplete"), 1:0,c("line","dashed")) #this is using a function in the RED_3 script
  PlotTasks$task=recode(PlotTasks$task, cattell="Matrix Reasoning",rj="Rhyme Judgement (6 items)", rjcomplete="Rhyme Judgement (22 items)",pd="Phonological Discrimination",ans="Non-Symbolic Num. Discrim.")
  
  labels=c("Matrix Reasoning","Rhyme Judgement (6 items)","Rhyme Judgement (22 items)","Phonological Discrimination","Non-Symbolic Num. Discrim.")
  
  # table(PlotTasks$task )

  Colours1=c(
    "Matrix Reasoning"="#E6AB02",
    "Rhyme Judgement (6 items)" = "#D95F02",
    "Rhyme Judgement (22 items)" = "#D95F02" ,
    "Phonological Discrimination"="#7570B3",
    "Non-Symbolic Num. Discrim."="black" 
  )
  LineType1=c(
    "Matrix Reasoning"="solid",
    "Rhyme Judgement (6 items)" = "solid",
    "Rhyme Judgement (22 items)" = "dashed" ,
    "Phonological Discrimination"="solid",
    "Non-Symbolic Num. Discrim."="solid"
  )  
  IRT_Plot=ggplot(PlotTasks,aes(x=UniRange,y=mirt_rel,group=task, color=task)) + 
    scale_color_manual(name="Task", values=Colours1) +  
    scale_linetype_manual(name="Task",values=LineType1) +
    #scale_fill_discrete(name="Task",labels=labels) +
    geom_line(size=1, aes(linetype=task)) + 
    jtools::theme_apa() + 
    geom_hline(yintercept = .6, linetype="dotted") + labs(x="True Score (Percentile)",y="Conditional Reliability", title="") +
    scale_y_continuous(limits=c(0,1),breaks=c(.00,.25,.50,.60,.75,1), labels=c(".00",".25",".50",".60",".75","1.00")) + coord_fixed() +# theme(axis.ticks.x=element_blank())
    scale_x_continuous(limits=c(0,1),breaks=c(.00,.25,.50,.75,1), labels=c("0%","25%","50%","75%","100%")) +
    theme(
      text=element_text(family="Times"),
      plot.title = element_text( size = 12, face = "bold"),
      axis.title = element_text( size = 10),
      legend.position = c(.33,.17),
      legend.background = element_rect(fill=ggplot2::alpha('white', 0.4),
                                      
                                       colour ="white"))
    
  IRT_Plot

  # save(IRT_Plot, file="Plots/IRT_Plot_JustSchoolCohort.pdf")
  pdf(file="Plots/IRTPlot_SchoolCohort.pdf", width=6,height=6)
  IRT_Plot
  dev.off()
  # 
  # theme_SetTextSize = theme(axis.title.x = element_text(size=12),axis.title.y = element_text(size=12),  axis.text = element_text(size=10), legend.text=element_text(size=12))
  # 
  # IRT_Plot + theme_SetTextSize

  # save.image(file = "IRT_Analyses.RData") #Saving workspace as analyses take a very long time to run!
  #rm(list=ls())
  
  #load(file=file.choose())
  #load(file = "IRT_Analyses.RData") #Saving workspace as analyses take a very long time to run!
  
#Calculate Scores from IRT analyses and save for further use
  cattell_IRT_Score = IRT_Cat2$mirt_scores
  cattell_IRT_ScoreNorm = Normalise(IRT_Cat2$mirt_scores)
  
  ans_IRT_Score = IRT_ANS$mirt_scores
  ans_IRT_ScoreNorm = Normalise(IRT_ANS$mirt_scores)
  
  PD_IRT_Score = IRT_PD2$mirt_scores
  PD_IRT_ScoreNorm = Normalise(IRT_PD2$mirt_scores)
  
  RJ_IRT_Score = IRT_RJ2$mirt_scores #I've used data from the shortened version of the task here! 
  RJ_IRT_ScoreNorm = Normalise(IRT_RJ2$mirt_scores)
  
  
  df0_scores = cbind.data.frame(
    df0$PpsID,
    cattell_IRT_Score,cattell_IRT_ScoreNorm,
    ans_IRT_Score,ans_IRT_ScoreNorm,
    PD_IRT_Score,PD_IRT_ScoreNorm,
    RJ_IRT_Score,RJ_IRT_ScoreNorm
  )
  
  
  write.csv(df0_scores,file.path(RED_DATASET_PARENTFOLDER,"red_df0_scoreddata_schoolcohort.csv"),row.names = FALSE)
