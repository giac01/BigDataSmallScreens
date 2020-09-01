#Load Data 
  rm(list=ls())
  source("P1S0 - Load Data.R")



#Plots 

#Correlation plot with all children in
Cor_AllData=
  PlotCorrelationMatrix(dat=df0, Variables = c(CogV_Accuracy,"MATRIX_RawScore_Norm", "VOCAB_RawScore_Norm", "S1_TeachNorm_FSN_Complete"),
                      Variables_Labels = c(CogV_Accuracy_Labels, "WASI - Matrix Reasoning", "WASI - Vocabulary", "APQ - Teacher Rating"))
Cor_AllData
#Correlation plot with just large school cohort in
Cor_SchoolData =
  PlotCorrelationMatrix(dat=df0[df0$SchoolID!=99,], Variables = c(CogV_Accuracy,"MATRIX_RawScore_Norm", "VOCAB_RawScore_Norm", "S1_TeachNorm_FSN_Complete"),
                        Variables_Labels = c(CogV_Accuracy_Labels, "WASI - Matrix Reasoning", "WASI - Vocabulary", "APQ - Teacher Rating"))

#Correlation plot with just small lab cohort in
Cor_LabData =
  PlotCorrelationMatrix(dat=df0[df0$SchoolID==99,], Variables = c(CogV_Accuracy,"MATRIX_RawScore_Norm", "VOCAB_RawScore_Norm", "S1_TeachNorm_FSN_Complete"),
                        Variables_Labels = c(CogV_Accuracy_Labels, "WASI - Matrix Reasoning", "WASI - Vocabulary", "APQ - Teacher Rating"))



# Export Plots 

PlotWidth = 6
PlotHeight = 6

pdf(file = file.path("Plots","Cor_AllData_NEW.pdf"), width=PlotWidth, height=PlotHeight)
Cor_AllData
dev.off()

pdf(file = file.path("Plots","Cor_SchoolData.pdf"), width=PlotWidth, height=PlotHeight)
Cor_SchoolData
dev.off()

pdf(file = file.path("Plots","Cor_LabData.pdf"), width=PlotWidth, height=PlotHeight)
Cor_LabData
dev.off()
