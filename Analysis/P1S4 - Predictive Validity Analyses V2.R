#Load Data and functions 
  rm(list=ls())
  source("P1S0 - Load Data.R")

#Load Additional Packages
  library(mice)
  library(caret)
  library(parallel)
  
  # library(doParallel)
  library(mixOmics)
  
#Imputation of missing cognitive data - school only
  #What we do here, is to input data for children with one missing value in the "Input Variables" vector. The outcome variable is kept seperate during the imputation. 
  
  InputVariables = c("reading_n_total_Norm", "sums_n_total_Norm", "cancellation_marked_intertime_Norm", "digit_span_n_correct_Norm", "dot_matrix_n_correct_Norm", "cattell_IRT_ScoreNorm", "ans_IRT_ScoreNorm")
  
  #Impute data  
    KeepPpsMatrix = t(sapply(apply(df0_school[,InputVariables],1,function(x) length(which(is.na(x)))) <= 1, function(x) rep(x, length(InputVariables)))) #Select rows with one or less empty cell
    Input_Matrix = mice::complete(mice::mice(df0_school[,InputVariables],m=1,seed=300,printFlag = FALSE,  method="cart", where=KeepPpsMatrix & is.na(df0_school[,InputVariables]))) #matrix of tablet data, imputed missing data
    df0_school_imputed = cbind.data.frame(df0_school[,c("S1_TeachNorm_FSN_Complete")],Input_Matrix)
    colnames(df0_school_imputed)[1] = "S1_TeachNorm_FSN_Complete"
    rm(KeepPpsMatrix)
    
    #Basic checks
    table(df0_school_imputed[,InputVariables]==df0_school[,InputVariables])
    head(df0_school[,InputVariables])
    head(df0_school_imputed)

#Simple linear models - predicting teacher rated AA from tablet metrics
  mod0 = lm(S1_TeachNorm_FSN_Complete ~ reading_n_total_Norm + sums_n_total_Norm, data=df0_school_imputed)
  mod1 = lm(S1_TeachNorm_FSN_Complete ~ cancellation_marked_intertime_Norm + digit_span_n_correct_Norm + dot_matrix_n_correct_Norm + cattell_IRT_ScoreNorm + ans_IRT_ScoreNorm, data=df0_school_imputed)
  mod2 = lm(S1_TeachNorm_FSN_Complete ~ reading_n_total_Norm + sums_n_total_Norm + cancellation_marked_intertime_Norm + digit_span_n_correct_Norm + dot_matrix_n_correct_Norm + cattell_IRT_ScoreNorm + ans_IRT_ScoreNorm, data=df0_school_imputed, na.action = na.exclude)
  
  summary(mod0)
  summary(mod1)
  summary(mod2)

#Bootstrap estimation of adjusted R^2 confidence intervals 
  train_control_boot = caret::trainControl(method="boot", number=3000, savePredictions = TRUE,allowParallel = FALSE)
  
  set.seed(4324)
  Pred1 = caret::train(as.formula("S1_TeachNorm_FSN_Complete ~ reading_n_total_Norm + sums_n_total_Norm"), data=df0_school_imputed, trControl = train_control_boot,method="lm", na.action=na.omit)
    apply(Pred1$resample[1:3],2, function(x) quantile(x, c(.025,.5,.975), type=6))
    adjR2(apply(Pred1$resample[2],2, function(x) quantile(x, c(.025,.5,.975), type=6)), 414, 2) #Adjusted R2 bootstrapped! 
    
  set.seed(4324)
  Pred2 = caret::train(as.formula("S1_TeachNorm_FSN_Complete ~ cancellation_marked_intertime_Norm + digit_span_n_correct_Norm + dot_matrix_n_correct_Norm + cattell_IRT_ScoreNorm + ans_IRT_ScoreNorm"), data=df0_school_imputed, trControl = train_control_boot,method="lm", na.action=na.omit)
    apply(Pred2$resample[1:3],2, function(x) quantile(x, c(.025,.5,.975), type=6))
    adjR2(apply(Pred2$resample[2],2, function(x) quantile(x, c(.025,.5,.975), type=6)), 410, 5) #Adjusted R2 bootstrapped! 
    
    
  set.seed(4324)
  Pred3 = caret::train(as.formula("S1_TeachNorm_FSN_Complete ~ reading_n_total_Norm + sums_n_total_Norm + cancellation_marked_intertime_Norm + digit_span_n_correct_Norm + dot_matrix_n_correct_Norm + cattell_IRT_ScoreNorm + ans_IRT_ScoreNorm"), data=df0_school_imputed, trControl = train_control_boot,method="lm", na.action=na.omit)
    apply(Pred3$resample[1:3],2, function(x) quantile(x, c(.025,.5,.975), type=6))
    adjR2(apply(Pred3$resample[2],2, function(x) quantile(x, c(.025,.5,.975), type=6)), 410, 7) #Adjusted R2 bootstrapped! 
    
    
#Random effect models - predicting teacher rated AA from tablet metrics
    #TeacherResponseID is a number given to each person who filled out a online qualtrics form for a classroom (of 30 children)
    #ClassID is calculated from school-provided data about which classroom each child is meant to be in. 
    #There is pretty much 1-1 correspondence between these variables 
    
    #Create variables representing clusters (teacher responses)
    df0_school$Class_ID = (paste0(df0_school$SchoolID,"_",df0_school$ClassID))
    # df0_school$Teacher_ID[df0_school$Teacher_ID=="1_3"|df0_school$Teacher_ID=="6_NA"]=NA
    table(df0_school$Class_ID,df0_school$TeacherResponseID) 
    
    #Create a single cognition score for random slope analyses (and check coefficient H of cognitive measure)
    df0_school_imputed$Cog_Score = Normalise(calcFactorScore(df1=df0_school_imputed,var= c("reading_n_total_Norm", "sums_n_total_Norm", "cancellation_marked_intertime_Norm", "digit_span_n_correct_Norm", "dot_matrix_n_correct_Norm", "cattell_IRT_ScoreNorm", "ans_IRT_ScoreNorm")))
    coefH(df0_school_imputed[,c("reading_n_total_Norm", "sums_n_total_Norm", "cancellation_marked_intertime_Norm", "digit_span_n_correct_Norm", "dot_matrix_n_correct_Norm", "cattell_IRT_ScoreNorm", "ans_IRT_ScoreNorm")])
    
    df0_school_imputed$TeacherResponseID = df0_school$TeacherResponseID
    
    library(lme4)
    library(MuMIn)
    #By default listwise deletion of missing data is used!
    
    # Pred_LM = (lm(S1_TeachNorm_FSN_Complete ~ 1, data=df0_school_imputed, na.action = na.exclude))
    # Pred1 = lme4::lmer(S1_TeachNorm_FSN_Complete ~  1 + (1|TeacherResponseID), data=df0_school_imputed, REML = TRUE) #Model no fixed effect, single intercept of teacher ID
    #     # Pred1b = lmerTest::lmer(S1_TeachNorm_FSN_Complete ~ 1 +  (1|TeacherResponseID), data=df0_school_imputed) #Model no fixed effect, single intercept of teacher ID
    #     # anova(Pred1b)
    # Pred2 = lme4::lmer(S1_TeachNorm_FSN_Complete ~ Cog_Score + (1|TeacherResponseID), data=df0_school_imputed, REML = TRUE) #Fixed Effect Cognition, single intercept of teacher ID
    # Pred3 = lme4::lmer(S1_TeachNorm_FSN_Complete ~ Cog_Score + (Cog_Score|TeacherResponseID), data=df0_school_imputed, REML = TRUE) #Fixed Effect Cognition, single intercept of teacher ID 
    # Pred4 = lme4::lmer(S1_TeachNorm_FSN_Complete ~ Cog_Score + (Cog_Score||TeacherResponseID), data=df0_school_imputed, REML = TRUE) #Fixed effect Cognition, uncorrelated intercept and slope for teacher ID
    # 
    #Previous models reported in preprint. In Main Manuscript only the following model is relevant:
    Pred5 = lme4::lmer(S1_TeachNorm_FSN_Complete ~ reading_n_total_Norm + sums_n_total_Norm + cancellation_marked_intertime_Norm + digit_span_n_correct_Norm + dot_matrix_n_correct_Norm + cattell_IRT_ScoreNorm + ans_IRT_ScoreNorm + (1|TeacherResponseID), data=df0_school_imputed, REML = TRUE, na.action = na.exclude)

    plot(fitted(Pred5), df0_school_imputed$S1_TeachNorm_FSN_Complete)
    plot(fitted(mod2), df0_school_imputed$S1_TeachNorm_FSN_Complete)
  
    
    #Reported analyses
    summary(as.numeric(table(df0_school$TeacherResponseID)))
    
    #ICC for single intercept model
    # summary(Pred1)
    #   set.seed(23124)
    #   lme4::confint.merMod(Pred1, method="boot")
    #   performance::icc(Pred1, ci=.95)
    #   MuMIn::r.squaredGLMM(Pred1)
    #   summary(aov(S1_TeachNorm_FSN_Complete ~ TeacherResponseID, df0_school_imputed)) #compare to simple ANOVA analysis 
    #   
    #   
    
    # summary(Pred3)
    #   set.seed(23124)
    #   lme4::confint.merMod(Pred3, method="boot") #this does change quite substantially on every rerun
    #   performance::icc(Pred3)
    #   MuMIn::r.squaredGLMM(Pred3)
    
    
    summary(Pred5)
      set.seed(23124)
      lme4::confint.merMod(Pred5, method="boot", nsim=3000,boot.type = 'perc' )
      performance::icc(Pred5)
      MuMIn::r.squaredGLMM(Pred5)
      
      


    performance::check_convergence(Pred5)
    performance::check_heteroscedasticity(Pred5)
    
    #Check Heteroscedasticity
    m=Pred5
    ggplot2::qplot(fitted(Pred5),residuals(Pred5), geom = c("point", "smooth"))
    fitted(Pred5)
    residuals(Pred5)
    
    
    ###Ignore####
      # anova(Pred2,Pred3,Pred4)
      # lme4::splom(Pred3)
      # summary(Pred5)
      # x = data.frame(VarCorr(Pred5))
      # var_r = x[1,4] #Random effect variance
      # var_e = x[2,4] #Residual Variance
      # var_f = var(predict(Pred5), na.rm=TRUE) #Fixed effect variance
      # #Duplicate ICC analyses from MuMIn::r.squaredGLMM
      # var_f / sum(var_r+var_e+var_f)
      # 
      # 
      # confint(Pred3)
      # lme4::confint.merMod(Pred3, method="boot")
      # randef(Pred3)
      # sigma(Pred5)
      # profile(Pred5)
      # terms(Pred3)
      # residuals(Pred3)
      # hist(residuals(Pred3), breaks=20)
      # densityplot(Pred3)
      # plot(Pred3, type = c("p", "smooth"))
      # coef(Pred3)
      # performance::icc(Pred3)
      # summary(Pred5)
      # performance::icc(Pred5)
      # MuMIn::r.squaredGLMM(Pred5)

  
    
#PLS - Tablet WASI relations (CBU small cohort)
    TabletVariables = c("cancellation_marked_intertime_Norm" , "digit_span_n_correct_Norm" , "reading_n_total_Norm" , "sums_n_total_Norm" , "dot_matrix_n_correct_Norm" , "GNG_Dprime" , "GNG_omErr_Norm" , "GNG_comErr_Norm" , "PD_IRT_ScoreNorm" , "cattell_IRT_ScoreNorm" , "ans_IRT_ScoreNorm") 
    df0_lab = df0[df0$SchoolID==99,]
    NoTabletData = apply(df0[TabletVariables],1,function(x) length(which(!is.na(x)))==0)
    NoWASIData = apply(df0[c("MATRIX_RawScore_Norm","VOCAB_RawScore_Norm")],1,function(x) length(which(!is.na(x)))==0)
    table(NoTabletData,NoWASIData)
  
    #Analysis of missing data
      sort(apply(df0_lab[,CogV_Accuracy],2,function(x) length(which(!is.na(x))))) #Complete data N 
      sort(apply(df0_lab[,TabletVariables],2,function(x) length(which(!is.na(x))))) #Complete data N 
      sort(apply(df0_lab[,c("MATRIX_RawScore_Norm","VOCAB_RawScore_Norm")],2,function(x) length(which(!is.na(x))))) #Complete data N 

      # cat(names(sort(apply(df0_lab[,CogV_Accuracy],2,function(x) length(which(!is.na(x)))), decreasing = TRUE)),sep="\" , \"")
      
      N_Cog_MissingTasksPerPerson = apply(df0_lab[,TabletVariables], 1, function(x) length(which(is.na(x)))) #Number of missing cognitive variables per participant
          table(N_Cog_MissingTasksPerPerson)
          
      ExcludePps = N_Cog_MissingTasksPerPerson>3 #Exclude participants because too much missing tablet data 
      KeepPpsMatrix = t(sapply(!ExcludePps,function(x) rep(x,length(TabletVariables))))

      
  #Create matrices for regression analysis
    Tablet_Matrix = complete(mice::mice(df0_lab[,TabletVariables],m=1,seed=300,printFlag = FALSE,  method="cart", where=KeepPpsMatrix & is.na(df0_lab[,TabletVariables]))) #matrix of tablet data, imputed missing data
    WASI_Matrix = cbind.data.frame(df0_lab[,c("MATRIX_RawScore_Norm","VOCAB_RawScore_Norm")])
    
  # Extract Component Scores
    
    Tablet_Score = psych::fa(Tablet_Matrix, missing=FALSE)$scores
    WASI_Score = psych::fa(WASI_Matrix, missing=FALSE)$scores
    
    cor.test(Tablet_Score,WASI_Score)
    
    coefH(Tablet_Matrix,verbose=TRUE,n_bootstrap=500)
    coefH(WASI_Matrix,verbose=TRUE,n_bootstrap=500)
    
    table(!is.na( psych::fa(Tablet_Matrix, missing=TRUE)$scores))
    
#Item level correlations with taechers - this is not in paper anymore, but interesting! 
  cor.test(df0$APQ1_Norm,df0$reading_n_total_Norm)
  cor.test(df0$APQ1_Norm,df0$sums_n_total_Norm)
  psych::paired.r(cor(df0$APQ1_Norm,df0$reading_n_total_Norm,use="pairwise"),cor(df0$APQ1_Norm,df0$sums_n_total_Norm,use="pairwise"),n=403,n2=405)
  
  cor.test(df0$APQ2_Norm,df0$reading_n_total_Norm)
  cor.test(df0$APQ2_Norm,df0$sums_n_total_Norm)
  psych::paired.r(cor(df0$APQ2_Norm,df0$reading_n_total_Norm,use="pairwise"),cor(df0$APQ2_Norm,df0$sums_n_total_Norm,use="pairwise"),n=403,n2=405)
  

#Subset to children who we have teacher rating and at least one cognitive outcome for!
  df0_PV = df0[!is.na(df0$S1_TeachNorm_FSN_Complete) & apply(df0[,CogV_Accuracy],1,function(x) length(which(!is.na(x))))>0,] 
  apply(df0_PV[,CogV_Accuracy],2,function(x) length(which(!is.na(x)))/length(x))
  
#how reliable are the teacher-rated AA measures?
  
  cor(df0[,c("APQ1_Norm","APQ2_Norm","APQ3_Norm")],use="pairwise.complete.obs")
  coefH(na.omit(df0[,c("APQ1_Norm","APQ2_Norm","APQ3_Norm")]),verbose = TRUE)
  cronbach.alpha(na.omit(df0[,c("APQ1_Norm","APQ2_Norm","APQ3_Norm")]))
  
#Linear Correlations for each task - used for table 1 in P1S5 script!
  CogV_NumberSchoolTesting = sapply(CogV_Accuracy,function(x) length(which(!is.na(df0_school[,x]))))
  Cor_list = lapply(CogV_Accuracy,function(x) cor.test(df0_school$S1_TeachNorm_FSN_Complete,df0_school[,x]))
  names(Cor_list) = CogV_Accuracy
  save(Cor_list, file=file.path(RED_OUTPUTDATA_LOCATION, "Cor_list.Rdata"))
  save(CogV_NumberSchoolTesting, file=file.path(RED_OUTPUTDATA_LOCATION, "CogV_NumberSchoolTesting.Rdata"))

#Hierarchial Linear Models Predicting AA from cognitive variables over AGE & SES .
  out_anova=list()
  p_val = vector()
  change_R2 = vector()
  for (i in seq_along(CogV_Accuracy)){
    v = CogV_Accuracy[i] #Cognitive Variable That We Want To Test
    df0_subset = na.omit(df0[,c("S1_TeachNorm_FSN_Complete","PPI","Age1",v)]) #create subset of data with no missing data 
    base_model = lm(S1_TeachNorm_FSN_Complete ~ Age1 + PPI, data=df0_subset) #regress out age and PPI
    cog_model =  lm(paste("S1_TeachNorm_FSN_Complete ~ ",paste(c("Age1","PPI",v), collapse="+"),sep = ""),data=df0_subset)
    out_anova[[i]] = anova(base_model,cog_model)
    p_val[i] = as.numeric(out_anova[[i]]$"Pr(>F)"[2])
    change_R2[i] = (summary(cog_model)$adj.r.squared-summary(base_model)$adj.r.squared)
  }
  
  save(change_R2,file=file.path(RED_OUTPUTDATA_LOCATION, "change_R2.Rdata"))
  #save(p_val_out,file="p_val_out.Rdata")
    change_R2_out = gsub("^0.","0.",format(change_R2^.5,digits=0,nsmall=2))
    p_val_out = rep("",length(p_val))
    p_val_out[.05/length(p_val)<p_val & p_val<.05]="*"
    p_val_out[p_val<.05/length(p_val)]="**"
    change_R2_out = paste0(change_R2_out,"~",p_val_out,"~")
  save(change_R2_out,file=file.path(RED_OUTPUTDATA_LOCATION, "change_R2_out.Rdata")) #output the change in R2 (sqrt) with significance values, formatted nicely! 
  
#Lm with just two AA tablet measures
  lm_model1 = lm(S1_TeachNorm_FSN_Complete ~ sums_n_total_Norm + reading_n_total_Norm,data=df0_PV, na.action = "na.exclude")
  summary(lm_model1)
  plot(df0_PV$S1_TeachNorm_FSN_Complete, predict(lm_model1))
  cor.test(df0_PV$S1_TeachNorm_FSN_Complete, predict(lm_model1))
  


