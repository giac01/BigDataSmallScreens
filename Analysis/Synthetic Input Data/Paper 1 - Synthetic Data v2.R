library(synthpop)
rm(list=ls())

DataLocation = "/imaging/gb01/Paper 1/"

#Read data files
  df0_original = read.csv(file.path(DataLocation,"df0_main_redacted.csv"), stringsAsFactors = FALSE)
  df0_IRT_SCORED_DATA_original = read.csv(file.path(DataLocation,"red_df0_scoreddata_ALLCHILDREN.csv"), stringsAsFactors = FALSE)

#Create 1 synthetic data file
  seedNumber = 100 # Note i have changed this seed number from the original one used 
  df0_combined = cbind.data.frame(df0_original,df0_IRT_SCORED_DATA_original[-1])
  
  # df0_IRT_SCORED_DATA = synthpop::syn(df0_IRT_SCORED_DATA_original, seed=seedNumber)
  df0_synthpop = synthpop::syn(df0_combined, seed=seedNumber)
  
#Split back into original datasets
  df0 = df0_synthpop$syn[,1:471]
  PpsID = df0$PpsID
  df0_IRT_SCORED_DATA = cbind.data.frame(PpsID,df0_synthpop$syn[,472:481])

#Export Synthetic files
  write.csv(df0_IRT_SCORED_DATA,file.path(DataLocation,"red_df0_scoreddata_ALLCHILDREN_synthpop.csv"))
  write.csv(df0,file.path(DataLocation,"df0_main_redacted_synthpop.csv"))



































