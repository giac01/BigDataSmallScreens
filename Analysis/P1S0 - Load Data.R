rm(list=ls())

USE_SYNTHETIC_DATA = TRUE


#Paths of folders containing scripts/datasets
  IPAD_DATA_LOCATION = ""  #Location of RAW ipad data - !!!! please note this isn't provided in the repository!!!!
  RED_DATASET_PARENTFOLDER = "Input Data" #Path to folder containing the RED datasets df0_main_redacted.csv and red_df0_scoreddata.csv 
  RED_OUTPUTDATA_LOCATION = "Output R Data"
  RED_SYNTHETIC_DATA_LOCATION = "Synthetic Input Data"
  
  
#Paths of scripts/datasets
  RED_DATASET_PATH = file.path(RED_DATASET_PARENTFOLDER, "df0_main_redacted_Aug2020.csv") #RED dataset path
  RED_IRT_SCORED_DATA = file.path(RED_DATASET_PARENTFOLDER, "red_df0_scoreddata_ALLCHILDREN_Aug2020.csv") #Data scored from IRT analyses 


#Use renv package to manage r versions
  
  # I have turned off the use of RENV by default on BINDER, but it can be used locally if desired. 
  #
  if (TRUE){
    if (!require("renv")) install.packages("renv") #Install renv if missing
    renv::restore()
  }
  
#Load Libraries
  library(psych)
  source("P1S01 - RED Functions (Paper 1).R")
  
# Load Synthetic Data (if USE_SYNTHETIC_DATA == TRUE )
  if (USE_SYNTHETIC_DATA){
    
    # Load Data
    df0=read.csv(file=file.path(RED_SYNTHETIC_DATA_LOCATION, "df0_main_redacted_synthpop.csv"), stringsAsFactors = FALSE) #All data
    
    # Add IRT Scored Data
    df0_IRT_SCORED_DATA = read.csv(file.path(RED_SYNTHETIC_DATA_LOCATION, "red_df0_scoreddata_ALLCHILDREN_synthpop.csv"), stringsAsFactors = FALSE)
    
    if (all.equal(df0_IRT_SCORED_DATA$PpsID,df0$PpsID)){ #The pps numbers should be matching! 
      df0 = cbind.data.frame(df0,df0_IRT_SCORED_DATA[-1])
    }
    
    ### For some reason, synthpop messes up the Normalised teacher rating outcome, so i swap it here:
    df0$S1_TeachNorm_FSN_Complete = Normalise(df0$S1_TeachNorm_FSN)
    
    rm(df0_IRT_SCORED_DATA)
    
  }
  

# Load REAL Data 
  
  if (!USE_SYNTHETIC_DATA){

# Load Data
  df0=as.data.frame(data.table::fread(file=RED_DATASET_PATH)) #All data
  
# Add IRT Scored Data
  df0_IRT_SCORED_DATA = read.csv(RED_IRT_SCORED_DATA, stringsAsFactors = FALSE)
  
  if (all.equal(df0_IRT_SCORED_DATA$df0.PpsID,df0$PpsID)){ #The pps numbers should be matching!
    df0 = cbind.data.frame(df0,df0_IRT_SCORED_DATA[-1])
  }
  
  rm(df0_IRT_SCORED_DATA)

  }
  
  #df0 = cbind.data.frame(df0,df0_IRT_SCORED_DATA)
  
# Create seperate data-set just for RED school-sample
  df0_school = df0[df0$SchoolID!=99,]
  
# Main Variables Used In Analyses 
  CogV_Accuracy = c(
    "cancellation_marked_intertime_Norm",# "cancellation_marked_interdist", "cancellation_marked_intersect_rate",
    "digit_span_n_correct_Norm","dot_matrix_n_correct_Norm",
    "GNG_Dprime","GNG_omErr_Norm","GNG_comErr_Norm",
    "cattell_IRT_ScoreNorm",
    
    "reading_n_total_Norm",
    "RJ_IRT_ScoreNorm",
    "PD_IRT_ScoreNorm",
    #"vocab_p_correct_Norm",
    
    "sums_n_total_Norm", 
    "ans_IRT_ScoreNorm",
    "LE_Zscore_FSN_Complete_Norm",
    "LC_Zscore_FSN_Complete_Norm"
  )
  
  CogV_Accuracy_Labels= c(
    "Visual Search Speed",
    "Verbal Short-Term Memory",
    "Spatial Short-Term Memory",
    "Go/No-Go - D'", "Go/No-Go - Ommission Errors", "Go/No-Go - Commission Errors",
    "Matrix Reasoning",
    
    "Reading Fluency",
    "Rhyme Judgement",
    "Phonological Discrimination",
    
    "Arithmetic Fluency",
    "Non-Symbolic Number Discrimination",
    "Line Estimation",
    "Liquid Equalisation")
  

