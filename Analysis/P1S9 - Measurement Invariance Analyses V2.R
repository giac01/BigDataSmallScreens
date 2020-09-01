#Load Data and functions 
  rm(list=ls())
  source("P1S0 - Load Data.R")  
  library(lavaan); library(semTools)
  
#core_tasks = c("sums_n_total_Norm","reading_n_total_Norm","digit_span_n_correct_Norm","dot_matrix_n_correct_Norm","cancellation_marked_intertime_Norm","ans_IRT_ScoreNorm","cattell_IRT_ScoreNorm")
  # core_tasks = CogV_Accuracy
  # core_tasks_labels = c(CogV_Accuracy_Labels)
  # N_tasks = length(core_tasks)  
  
#Subset Data
  df0$Group = NA ; 
  df0$Group[df0$SchoolID!=99] = "School" ;  df0$Group[df0$SchoolID==99] = "Lab"
  df0_school = df0[df0$SchoolID!=99,] # School cohort data
  df0_lab = df0[df0$SchoolID==99,] # Lab cohort data
  
    
#Redefine core_tasks for MEASUREMENT INVARIANCE analyses
  core_tasks = c("reading_n_total_Norm","sums_n_total_Norm","digit_span_n_correct_Norm","dot_matrix_n_correct_Norm","cancellation_marked_intertime_Norm","ans_IRT_ScoreNorm","cattell_IRT_ScoreNorm")
  
#Dimensionality in both grouos
  # psych::fa.parallel(df0_school[core_tasks], n.iter=100, error.bars = TRUE) 
  # psych::fa.parallel(df0_lab[core_tasks], n.iter=100, error.bars = TRUE) 
  #   
  # 
#Calculate bootstrapped CFI index
    HS.model = 'IQ =~ reading_n_total_Norm + sums_n_total_Norm  + digit_span_n_correct_Norm + dot_matrix_n_correct_Norm + cancellation_marked_intertime_Norm + ans_IRT_ScoreNorm + cattell_IRT_ScoreNorm' #G-factor model specification for lavaan
    #Create two different dataframes to resample from, removing rows with no tablet task data 
    df0_lab_2 = df0_lab[apply(df0_lab[core_tasks],1,function(x) length(which(!is.na(x))))>1,]
    df0_school_2 = df0_school[apply(df0_school[core_tasks],1,function(x) length(which(!is.na(x))))>1,]
  
    nrow(df0_lab_2); nrow(df0_school_2)
    
    MI_list = list()
    set.seed(3034)
    
    for (i in 1:3000){
      x1 = df0_lab_2[sample(nrow(df0_lab_2), replace = TRUE),]
      x2 = df0_school_2[sample(nrow(df0_school_2), replace = TRUE),]
      dat = rbind.data.frame(x1,x2)
      MI =  semTools::measurementInvariance(model=HS.model, data=dat, group="Group", missing="fiml", quiet=TRUE, strict=TRUE)
      MI_list[[i]]=list()
      MI_list[[i]]$converged = sapply(1:length(MI),function(i)lavInspect(MI[[i]], "converged"))
      if(length(which(MI_list[[i]]$converged))==5){
        MI_list[[i]]$FitMeasures = list(fitMeasures(MI[[1]]),fitMeasures(MI[[2]]),fitMeasures(MI[[3]]),fitMeasures(MI[[4]]),fitMeasures(MI[[5]]))
      }
      rm(x1,x2,dat, MI)
      flush.console()
      print(i)
      # (setTxtProgressBar(txtProgressBar(min = 1, max = 3000) ,i))
    }
    save(MI_list, file=file.path(RED_OUTPUTDATA_LOCATION, "MI_list_Aug2020.Rdata"))
    # base::load(file=file.path(RED_OUTPUTDATA_LOCATION, "MI_list_Aug2020.Rdata"))
    
    
    
    #Remove cases where model failed to converge
      checkConvergence = t(sapply(seq_along(MI_list), function(i) MI_list[[i]]$converged ))
      checkConvergence = apply(checkConvergence,1,function(x) length(which(x))==5) #ALL COVERGED IN MANUSCRUPT 
      keepCases = which(checkConvergence)
      #MI_list2 = MI_list[keepCases] ;



      AIC_Change_Loadings = t(sapply(keepCases, function(i) c(MI_list[[i]]$FitMeasures[[2]][19]-MI_list[[i]]$FitMeasures[[1]][19],
                                                              MI_list[[i]]$FitMeasures[[3]][19]-MI_list[[i]]$FitMeasures[[2]][19],
                                                              MI_list[[i]]$FitMeasures[[4]][19]-MI_list[[i]]$FitMeasures[[3]][19],
                                                              MI_list[[i]]$FitMeasures[[5]][19]-MI_list[[i]]$FitMeasures[[4]][19])))
      
      AIC_Change_Loadings = apply(AIC_Change_Loadings,2, function(x) quantile(x, c(.025,.5,.975), type=6))                       
                             
      CFI_Change_Loadings = t(sapply(keepCases, function(i) c(MI_list[[i]]$FitMeasures[[2]][9] - MI_list[[i]]$FitMeasures[[1]][9],
                                                              MI_list[[i]]$FitMeasures[[3]][9] - MI_list[[i]]$FitMeasures[[2]][9],
                                                              MI_list[[i]]$FitMeasures[[4]][9] - MI_list[[i]]$FitMeasures[[3]][9],
                                                              MI_list[[i]]$FitMeasures[[5]][9] - MI_list[[i]]$FitMeasures[[4]][9])))
      CFI_Change_Loadings = apply(CFI_Change_Loadings,2, function(x) quantile(x, c(.025,.5,.975), type=6))
        
      RMSEA_Change_Loadings = t(sapply(keepCases, function(i) c(MI_list[[i]]$FitMeasures[[2]][23] - MI_list[[i]]$FitMeasures[[1]][23],
                                                                MI_list[[i]]$FitMeasures[[3]][23] - MI_list[[i]]$FitMeasures[[2]][23],
                                                                MI_list[[i]]$FitMeasures[[4]][23] - MI_list[[i]]$FitMeasures[[3]][23],
                                                                MI_list[[i]]$FitMeasures[[5]][23] - MI_list[[i]]$FitMeasures[[4]][23])))
      RMSEA_Change_Loadings = apply(RMSEA_Change_Loadings,2, function(x) quantile(x, c(.025,.5,.975), type=6))       
        
        
#Lavaan Analyses

      
    #enter missing data FML
      MI_Models = list()
      
      MI_Models[[1]] = lavaan::cfa(HS.model, data=df0, group = "Group", missing="fiml")
      MI_Models[[2]] = lavaan::cfa(HS.model, data=df0, group = "Group", group.equal="loadings", missing="fiml")
      MI_Models[[3]] = lavaan::cfa(HS.model, data=df0, group = "Group", group.equal=c("loadings","intercepts"), missing="fiml")
      MI_Models[[4]] = lavaan::cfa(HS.model, data=df0, group = "Group", group.equal=c("loadings","intercepts","residuals"), missing="fiml")
      MI_Models[[5]] = lavaan::cfa(HS.model, data=df0, group = "Group", group.equal=c("loadings","intercepts","residuals","means"), missing="fiml")
      MI_Model_Comaprisons = anova(MI_Models[[1]], MI_Models[[2]], MI_Models[[3]], MI_Models[[4]], MI_Models[[5 ]])
      
      
      # fitMeasures(MI_Models[[1]], "chisq")
      # summary(MI_Models[[1]], fit.measures=TRUE)
      # fitted.values(MI_Models[[4]])
      # 
    #Create Table
      Models = c("1. Configural", "2. Loadings", "3. Intercepts", "4. Residuals", "5. Means")
      ChiSq = format(sapply(MI_Models, function(x) fitMeasures(x, "chisq")), digits=3)
      df = format(sapply(MI_Models, function(x) fitMeasures(x, "df")), digits=0)
      p = gsub("^0.",".",format( MI_Model_Comaprisons$`Pr(>Chisq)`, digits=0,nsmall=3)) ; p[1]="";
      CFI = gsub("^0.",".",format(sapply(MI_Models, function(x) fitMeasures(x, "cfi")), digits=3))
        CFI_numeric = as.numeric(sapply(MI_Models, function(x) fitMeasures(x, "cfi")))
        ΔCFI = format(CFI_numeric[-1]-CFI_numeric[-5], digits=0, nsmall=3)
        ΔCFI = c("", ΔCFI)
        ΔCFI_LB = c("",gsub('^(-)?(\\s)?0[.]', '\\1.', format(CFI_Change_Loadings[1,], digits=0, nsmall=3)))
        ΔCFI_UB = c("",gsub('^(-)?(\\s)?0[.]', '\\1.', format(CFI_Change_Loadings[3,], digits=0, nsmall=3)))
      RMSEA_numeric = sapply(MI_Models, function(x) fitMeasures(x, "rmsea"))
        RMSEA = gsub("^0.",".",format(RMSEA_numeric , digits=3))
        ΔRMSEA = format(RMSEA_numeric[-1]- RMSEA_numeric[-5], digits=0, nsmall=3) 
        ΔRMSEA = c("",ΔRMSEA)
        ΔRMSEA_LB = c("",gsub('^(-)?(\\s)?0[.]', '\\1.', format(RMSEA_Change_Loadings[1,], digits=0, nsmall=3)))
        ΔRMSEA_UB = c("",gsub('^(-)?(\\s)?0[.]', '\\1.', format(RMSEA_Change_Loadings[3,], digits=0, nsmall=3)))
      AIC = format(sapply(MI_Models, function(x) fitMeasures(x, "aic")), digits=1)
      
      MeasurementInvariance_table = cbind.data.frame(Models, ChiSq,df,p, CFI,ΔCFI,ΔCFI_LB,ΔCFI_UB, RMSEA,ΔRMSEA,ΔRMSEA_LB,ΔRMSEA_UB,AIC)
      
      save(MeasurementInvariance_table, file=file.path(RED_OUTPUTDATA_LOCATION,"MeasurementInvariance_table.Rdata"))
      
      write.csv(cbind.data.frame(CFI_numeric,RMSEA_numeric), file=file.path(RED_OUTPUTDATA_LOCATION,"CFI_RMSEA.csv"))
      
      # clipr::write_clip(MeasurementInvariance_table)
      # MI = semTools::measurementInvariance(model=HS.model, data=df0, group="Group", missing="fiml", strict = "TRUE")
      # MI_Boot = bootstrapLavaan(MI_Models[[2]])

#Compare the two groups 
      # residuals(MI_Models[[4]])$School$cov
      # cov2cor(residuals(MI_Models[[4]])$School$cov)
      # summary(MI_Models[[4]], fit.measures=TRUE)
      # fitted(MI_Models[[4]])
      CFA_scores = lavPredict(MI_Models[[4]])
      
      School_Scores = data.frame(CFA_scores$School)
      School_Scores$group = 0
      
      Lab_Scores = data.frame(CFA_scores$Lab)
      Lab_Scores$group = 1

      CFA_scores = as.data.frame(na.omit(rbind.data.frame(School_Scores, Lab_Scores)))
      
      cor.test(CFA_scores$IQ, CFA_scores$group)
        

# Reliability across two groups
        
  CH_Lab = coefH(df0_lab_2[,core_tasks],verbose=TRUE,n_bootstrap=1000) #d'
  CH_School = coefH(df0_school_2[,core_tasks],verbose=TRUE,n_bootstrap=1000) #d'
  
  CH_Lab # omega =  .786
  CH_School # omega = 
  
# Neighbourhood deprivation
  df0$Group_Numeric = as.numeric(df0$Group=="School") # 0 = lab group 1 = school group
  keep = apply(df0[core_tasks],1,function(x) length(which(!is.na(x))))>1 # Keep children who have completed the assessments 
  cor.test(df0$Group_Numeric[keep], df0$DEPRIVATION.Index_of_Multiple_Deprivation_Rank[keep])

  
# Task intercepts reported 
  
  summary(MI_Models[[2]])
  parameterEstimates(MI_Models[[2]])
  parameterEstimates(MI_Models[[2]])$pvalue[45]*7 #Bonferroni corrected p value for MATRIX REASONING 
  parameterEstimates(MI_Models[[2]])$pvalue[41]*7 #BONFERRONI corrected p vlaue for DIGIT SPAN
  
  #Export Parameter estimates 
  
  for (i in 1:5){
    write.csv(data.frame(parameterEstimates(MI_Models[[i]])), file=file.path(RED_OUTPUTDATA_LOCATION, paste0("MeasurementInvarianceCFAModels_NUM",i,".csv")))
  }
  
  summary(MI_Models[[4]])
  
  
        