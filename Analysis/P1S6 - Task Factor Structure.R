# LOAD_DATA_LOCATION_FILE = file.path(getwd(),"P1S0 - Load Data.R") #We want to load the "P1S0 - Load Data" R-script 
source("P1S0 - Load Data.R")

options(mc.cores=3)

#Useful Function To Plot Parallel Analysis

SexyParallelPlot = function(psych_output, col1="red", col2="green"){
  
  #Get 95% confidence interval (quantiles .025 & .975 for resamples) from resampled data
  ResampledQuantiles=t(apply(psych_output$values,2,function(x) quantile(x, probs=c(0.025,.975), type=6)))
    components=ResampledQuantiles[grepl("C[0-9]",rownames(ResampledQuantiles)),]
    factors=ResampledQuantiles[grepl("F[0-9]",rownames(ResampledQuantiles)),]
  confidence.interval = cbind.data.frame(components,factors) ; colnames(confidence.interval) = c("c_lb","c_ub","f_lb","f_ub")
  
  #Get Eigenvalue Data
  plot =   data.frame(cbind(
    1:length(psych_output$fa.values),
    psych_output$fa.values,
    psych_output$fa.simr,
    psych_output$pc.values,
    psych_output$pc.simr))
  colnames(plot) = c("n","fa.values","fa.simr","pc.values","pc.simr")
  plot=cbind.data.frame(plot,confidence.interval)
  
  out=
    ggplot(data=plot,aes(x=n)) +
    geom_point(aes(x=n,y=fa.values), size=1.5, color=col2) + geom_line(aes(x=n,y=fa.values), color=col2) + geom_line(aes(x=n,y=fa.simr),linetype="longdash", color=col2) + geom_errorbar(aes(ymin=f_lb,ymax=f_ub), color=col2) +
    geom_point(aes(x=n,y=pc.values), shape=17, color=col1, size=1.5) + geom_line(aes(x=n,y=pc.values),color=col1) + geom_line(aes(x=n,y=pc.simr),linetype="longdash", color=col1) + geom_errorbar(aes(ymin=c_lb,ymax=c_ub),color=col1) + 
    labs(x="Number Components/Factors",y="Eigenvalues") + 
    scale_x_continuous(labels=1:nrow(plot),breaks=1:nrow(plot))+
    jtools::theme_apa() + theme(axis.title=element_text(size=30))
  return(out)
}

core_tasks = c("reading_n_total_Norm","sums_n_total_Norm","digit_span_n_correct_Norm","dot_matrix_n_correct_Norm","cancellation_marked_intertime_Norm","ans_IRT_ScoreNorm","cattell_IRT_ScoreNorm")

#Dataframe with all cognition
# df0_AllTasks = df0[df0$SchoolID!=99,CogV_Accuracy[c(-4,-6)]] #all task data without GNG comission errors and d' 
df0_CoreTaskSchoolSample = df0[df0$SchoolID!=99,core_tasks] #Data from SCHOOL sample and using core tests 

#Parrallel Analysis on All tasks
  AllTasks_Parallel2 = psych::fa.parallel(df0_CoreTaskSchoolSample, use="pairwise", n.iter=1000,error.bars = TRUE,fm="minres", sim=FALSE)
    ParallelPlot_AllTasks = SexyParallelPlot(AllTasks_Parallel2, col1="#D95F02", col2="#7570B3")
    ParallelPlot_AllTasks

    ggplot2::ggsave(file.path("Plots", "ParallelPlot.pdf"),ParallelPlot_AllTasks, device="pdf", width=7, height=4.5)
    
#Velicher's MAP  
    
    results = psych::vss(df0_CoreTaskSchoolSample, n=6, use="pairwise", rotate="promax")
    results
  