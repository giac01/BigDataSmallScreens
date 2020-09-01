#Load Data 
  source("P1S0 - Load Data.R")

#Just keep school cohort 
  df0 = df0[df0$SchoolID!=99,]
  
#Script Parameters 
  N_RESAMPLES = 1000
  

#Split-Half Reliabilities - main tasks 
  cancel = coefH(df0[,c("cancellation_marked_1H_intertime_Norm","cancellation_marked_2H_intertime_Norm")],verbose=TRUE,n_bootstrap=N_RESAMPLES) #search speed ('cancellation') test. 
  
  #Go/no-go
  G1 = coefH(df0[,c("GNG_Dprime_1H","GNG_Dprime_2H")],verbose=TRUE,n_bootstrap=N_RESAMPLES) #d'
  G2 = coefH(df0[,c("GNG_omErr_1H_Norm","GNG_omErr_2H_Norm")],verbose=TRUE,n_bootstrap=N_RESAMPLES) #Ommission errors
  G3 = coefH(df0[,c("GNG_comErr_1H_Norm","GNG_comErr_2H_Norm")],verbose=TRUE,n_bootstrap=N_RESAMPLES) #Commission errors
  
  Cat_data = data.frame(na.omit(df0[,which(grepl("Cattell_ItemCorrect",colnames(df0)))]))
  Cat = coefH(Cat_data,verbose = TRUE,n_bootstrap=N_RESAMPLES, poly_set = FALSE) #Cattell-like Matrix reasoning test

#Split-Half Reliabilities - Literacy Tasks
  Read = coefH(df0[,c("reading_n_total_1H","reading_n_total_2H")],verbose=TRUE,n_bootstrap=N_RESAMPLES) #Reading Fluency 
  RJ = coefH(df0[,paste0("RJ_itemcorrect_",c(3,4,5,7,9,11))],verbose=TRUE,n_bootstrap=N_RESAMPLES, poly_set = FALSE) #Rhyme Judgement
  PD = coefH(na.omit(df0[,paste0("PD_itemcorrect_",c(1:3,5:44))]),verbose=TRUE,n_bootstrap=N_RESAMPLES, poly_set = FALSE) #Phono discrimination
    #In the synthetic dataset the function runs into an error above. This seems to occur on some bootstrap resamples where there is 100% correct on a variable. 

#Reliability - Arithmetic Tasks 
  Sums = coefH(df0[,c("sums_n_total_1H","sums_n_total_2H")],verbose=TRUE,n_bootstrap=N_RESAMPLES) #Arithmetic Fluency Test
  
  ANS = coefH(df0[,paste0("ans_trialcorrect_",1:116)],verbose=TRUE,n_bootstrap=N_RESAMPLES,  poly_set = FALSE) #Approximinate number sense NSND task 
  
  LE_Data = data.frame(na.omit(df0[,which(grepl("LE_itemZDifference",colnames(df0)))])) #Line estimation Tasks
    LE = coefH(LE_Data,verbose=TRUE,n_bootstrap=N_RESAMPLES)
    
  LC_Data = na.omit(data.frame(na.omit(df0[,which(grepl("LC_itemZDifference",colnames(df0)))]))) #Liquid Conservation / Liquid Equalisation Test
    LC = coefH(LC_Data,verbose=TRUE,n_bootstrap=N_RESAMPLES)
  

Reliability = c(
  cancel[[1]],
  
  #NA,NA,
  #.01,.01, #no data for digit span or vswm tasks yet 
  G1[[1]],G2[[1]],G3[[1]], #Go-no-go task 
  Cat[[1]], #Cattell
  
  Read[[1]],
  RJ[[1]], #Rhyme judgement - 6 items
  PD[[1]],
  #Vocab_Alpha[[1]],
  
  Sums[[1]],
  ANS[[1]],
  LE[[1]],
  LC[[1]]
  
)

CI_l = c(
  cancel[[2]][1],
  #NA,NA,
  #.01,.01, #no data for digit span or vswm tasks yet 
  G1[[2]][1],G2[[2]][1],G3[[2]][1], #Go-no-go task 
  Cat[[2]][1], #Cattell
  
  Read[[2]][1],
  RJ[[2]][1], #Rhyme judgement - 6 items
  PD[[2]][1],
  #Vocab_Alpha[[1]],
  
  Sums[[2]][1],
  ANS[[2]][1],
  LE[[2]][1],
  LC[[2]][1]
)

CI_u = c(
  cancel[[2]][3],
  #NA,NA,
  #.01,.01, #no data for digit span or vswm tasks yet 
  G1[[2]][3],G2[[2]][3],G3[[2]][3], #Go-no-go task 
  Cat[[2]][3], #Cattell
  
  Read[[2]][3],
  RJ[[2]][3], #Rhyme judgement - 6 items
  PD[[2]][3],
  #Vocab_Alpha[[1]],
  
  Sums[[2]][3],
  ANS[[2]][3],
  LE[[2]][3],
  LC[[2]][3]
)

NewGroups = c(6,9)
SpaceBetweenGroups=1.8
SpaceBetweenTasks=1.2
SpaceBetweenOtherBars=1
Tasks = gsub("_","",str_extract(CogV_Accuracy[c(-2:-3)],"^[a-zA-Z]+_"))
Tasks2= c("Visual Search Speed","Go/No-Go - D'","Go/No-Go - Ommission Errors","Go/No-Go - Commission Errors","Matrix Reasoning","Reading Fluency","Rhyme Judgement","Phonological Discrimination","Arithmetic Fluency","Non-Symbolic Number Discrimination","Line Estimation","Liquid Equalisation")

X_Pos = (1:length(Reliability))*SpaceBetweenOtherBars
X_Pos = X_Pos[length(X_Pos):1]



for (i in seq_along(X_Pos)){
  if(!duplicated(Tasks)[i]==TRUE){ 
    X_Pos[i:length(X_Pos)]=X_Pos[i:length(X_Pos)]-(SpaceBetweenTasks-SpaceBetweenOtherBars)
  }
}
for (i in NewGroups){
    X_Pos[i:length(X_Pos)]=X_Pos[i:length(X_Pos)]-(SpaceBetweenGroups-SpaceBetweenTasks)
}



CTT_PlotData = cbind.data.frame(Reliability,CI_l,CI_u,X_Pos,Tasks,Tasks2)

# Colours=c("cancellation"="#1B9E77",
#           "digit"="#D95F02",
#           "dot"="#7570B3" ,
#           "GNG"="#E7298A",
#           "cattell"="#E6AB02",
# 
#           "reading"="#1B9E77",
#           "RJ"="#D95F02" ,
#           "PD"="#7570B3",
# 
#           "sums"="#1B9E77",
#           "ans"="#D95F02" ,
#           "LE"="#7570B3",
#           "LC"="#E7298A"
# )

Colours=c("cancellation"="#1B9E77",
          "digit"="#1B9E77",
          "dot"="#1B9E77" ,
          "GNG"="#1B9E77",
          "cattell"="#1B9E77",
          
          "reading"="#D95F02",
          "RJ"="#D95F02" ,
          "PD"="#D95F02",
          
          "sums"="#7570B3",
          "ans"="#7570B3" ,
          "LE"="#7570B3",
          "LC"="#7570B3"
)

Rel_Plot=ggplot(data=CTT_PlotData, aes(x=X_Pos,y=Reliability, fill=Tasks)) + coord_flip()+ geom_hline(yintercept = c(.6,.75), linetype="dotted") + 
  geom_bar(stat="identity") + geom_errorbar(aes(ymin=CI_l, ymax=CI_u, x=X_Pos), width=.5) + jtools::theme_apa()+ theme(legend.position = "none",axis.text=element_text(size=12)) +
  scale_y_continuous(breaks=c(.00,.25,.50,.60,.75,1.00), labels=c(".00",".25",".50",".60",".75","1.00"), limits = c(0,1))  + 
  scale_x_continuous(breaks=X_Pos, labels=Tasks2) +
  labs(x=NULL, y="Reliability (ω)") +
  scale_fill_manual(values=Colours) 
Rel_Plot


save(Rel_Plot,file=file.path("Plots","Rel_Plot.png"))
save(CTT_PlotData,file=file.path(RED_OUTPUTDATA_LOCATION,"CTT_PlotData.Rdata"))


