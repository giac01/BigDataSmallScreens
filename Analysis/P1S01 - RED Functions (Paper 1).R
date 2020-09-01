# if (!"dplyr" %in% installed.packages()){install.packages("dplyr")}
# if (!"ggplot2" %in% installed.packages()){install.packages("ggplot2")}
# if (!"stringr" %in% installed.packages()){install.packages("stringr")}
# if (!"reshape2" %in% installed.packages()){install.packages("reshape2")}
# if (!"ltm" %in% installed.packages()){install.packages("reshape2")}
# if (!"jtools" %in% installed.packages()){install.packages("reshape2")}
# if (!"mirt" %in% installed.packages()){install.packages("reshape2")}
# if (!"psych" %in% installed.packages()){install.packages("reshape2")}
# 
# 
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(ltm)
library(jtools)
library(mirt)
library(psych)


#Calculate adjusted R^2 from R2 (R-squared) n (sample size) & p (number of explanatory variables) 

adjR2 = function(R2, n, p){
  
  output = 1 - (1 - R2) * ( (n - 1) / (n - p - 1) )
  
  return(output)
}

#Function to make pretty correlation plots
PlotCorrelationMatrix = function(dat, Variables, Variables_Labels){
  
  matrix_scores = dat[,Variables] 
  Mat_Cor =gsub("^0","",gsub("^ +","",gsub("^-0","-", format(cor(matrix_scores, use="pairwise.complete.obs"), digits=0, nsmall=2)))) #Correlation matrix
  Mat_Cor_fill = abs(cor(matrix_scores, use="pairwise.complete.obs"))  #Correlation matrix for table fill 
  Mat_Cor_fill[lower.tri(Mat_Cor_fill,diag = TRUE)]=NA
  
  #Matrix on Ns per comparison - lower triag 
  Mat_N = sapply(Variables, function(x) 
    sapply(Variables, function(y)
      nrow(na.omit(data.frame(dat[,unique(c(x,y))])))
    )) 
  
  #Create Dataframe For Ggplot to Read 
  PlotMat = Mat_Cor
  PlotMat[lower.tri(PlotMat, diag=TRUE)]=Mat_N[lower.tri(Mat_N, diag=TRUE)]
  colnames(PlotMat) = Variables_Labels ;  rownames(PlotMat) = Variables_Labels
  
  PlotMat = data.frame(reshape2::melt(PlotMat), stringsAsFactors = FALSE)
  head(PlotMat)
  
  PlotMat$value = (as.character(PlotMat$value))
  PlotMat$ValueFill = as.numeric(t(c(Mat_Cor_fill)))
  PlotMat$Var2 = factor(PlotMat$Var2, levels=rev(levels(PlotMat$Var2)))
  
  
  
  OutPlot = 
    ggplot(data = PlotMat, aes(x=Var1, y=Var2,fill=ValueFill))+# + geom_point(aes(size=value^2,alpha=value^4))+
    geom_tile() + labs(x=NULL, y=NULL) +
    theme(axis.text = element_text(size=5)) + geom_text(aes(label=value), size=1.4) + jtools::theme_apa() +
    #scale_fill_brewer(palette=1,na.value="grey")+
    scale_fill_continuous(na.value="grey",low="#EDCB64", high="#B62A3D")+ 
    theme(legend.position = "#DAECED") + 
    theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2)) + coord_fixed()
  
  OutPlot
  
  return(OutPlot)
}


#Convert factors to numeric

FactorToNumeric=function(x){
  return(as.numeric(levels(x))[x])
}


#calculate OMEGA RELIABILITY (formerly used coefficient H - now changed)
cH_calc = function(data,verbose=FALSE, poly_set=FALSE){ 
  
  # UsePolychoricCorrelation = nrow(table(c(t(data))))==2 
  # 
  # if(UsePolychoricCorrelation){print("Using Polychor")}
  # 
  # rm(coefH_out)
  suppressMessages({suppressWarnings({
  rm(coefH_out)
  
  coefH_out = psych::omega(data, nfactors=1, poly=poly_set)
  })})
  
  coefH_out = coefH_out$omega.tot
  
  return(coefH_out)
}

#calculate coefficientH with bootstrap

coefH = function(data,verbose=FALSE,n_bootstrap=300, poly_set=FALSE){ 
  
  data = na.omit(data) #remove rows with missing data. 
  
  coefH_estimate = cH_calc(data,verbose=verbose,poly_set=poly_set)
  bootstraps = sapply(1:n_bootstrap, function(i) cH_calc(data[sample(nrow(data),replace = TRUE),],poly_set=poly_set))
  Confidence_Interval = (quantile(bootstraps,c(.025,.5,.975), type=6))
  
  
  output = list(coefH_estimate, Confidence_Interval)
  names(output) = c("Omega Total Estimate", "Confidence Interval")
  
  return(output)
}





#Function - taken from mirt source code. Used to convert information to reliability. Credit: Phill Chambers, https://github.com/philchalmers/mirt
    ExtractGroupPars <- function(x){
      if(x@itemclass < 0L) return(list(gmeans=0, gcov=matrix(1)))
      nfact <- x@nfact
      gmeans <- x@par[seq_len(nfact)]
      phi_matches <- grepl("PHI", x@parnames)
      if (x@dentype == "Davidian") {
        phi <- x@par[phi_matches]
        tmp <- x@par[-c(seq_len(nfact), which(phi_matches))]
        gcov <- matrix(0, nfact, nfact)
        gcov[lower.tri(gcov, diag=TRUE)] <- tmp
        if(nfact != 1L)
          gcov <- gcov + t(gcov) - diag(diag(gcov))
        return(list(gmeans=gmeans, gcov=gcov, phi=phi))
      } else {
        par <- x@par
        if(x@dentype == "mixture") par <- par[-length(par)] # drop pi
        tmp <- par[-seq_len(nfact)]
        gcov <- matrix(0, nfact, nfact)
        gcov[lower.tri(gcov, diag=TRUE)] <- tmp
        if(nfact != 1L)
          gcov <- gcov + t(gcov) - diag(diag(gcov))
        return(list(gmeans=gmeans, gcov=gcov))
      }
    }


#Item response theory functions

IRTcheck = function(df1=df0,vars="PD_itemcorrect_", dropN=NULL, mirtoutput=FALSE,
                    mirt_modeltype="2PL", guess_param=.5,nCores=1, maxIterations = 40000,
                    verbose=TRUE, mirt_Method = "EM", mirt_Optimizer = "BFGS"){
  #Set up for parrallel computation - this function behaved to erractically so i've removed it from the code!
    # if (nCores>1){
    #   mirt::mirtCluster(nCores,remove = TRUE)
    #   mirt::mirtCluster(nCores)
    # }
  
  #Rename rows in dataframe
    rownames(df1)=as.character(1:nrow(df1))  
  #Find the relevant variables from dataframe uisng "vars"
    df2=df1[,grepl(vars,colnames(df1))]   
  #Remove variables from input dataframe, (if specified in dropN)
    if (!is.null(dropN)){                 
      df2=df2[,-which((colnames(df2) %in% paste0(vars,dropN)))]
    }
  
  #Remove missing data using listwise deletion
    df2=na.omit(df2)
  
  #MIRT model
    mirt_model = mirt::mirt(df2, 1, itemtype = mirt_modeltype, guess=guess_param, technical = list(NCYCLES = maxIterations,parallel=FALSE), method=mirt_Method,optimizer = mirt_Optimizer)
    mirt_scores = mirt::fscores(mirt_model,method="EAP")[match(rownames(df1),rownames(df2))]
    mirt_coef = mirt::coef(mirt_model,IRTpars=TRUE)
    mirt_coef_table = sapply(1:length(mirt_coef[[1]]), function(c)
                      sapply(mirt_coef, function(x)
                        x[c]
                      ))
    colnames(mirt_coef_table) = paste0("mirt_",colnames(mirt_coef[[1]]))
    mirt_coef_table = mirt_coef_table[which(rownames(mirt_coef_table)!="GroupPars"),]
   
  #Create data for plot
      UniRange = seq(0.001,.999,by=.001)
      mirt_plotdata=as.data.frame(UniRange)
      mirt_plotdata$ThetaRange =  qnorm(UniRange)
      mirt_plotdata$mirt_info = mirt::testinfo(mirt_model, Theta=mirt_plotdata$ThetaRange) #Test Information across theta:
      mirt_plotdata$mirt_se = 1/sqrt(mirt_plotdata$mirt_info)
      #Other ways of converitng test information to reliabiltiy metrics
        #mirt_plotdata$mirt_rel = 1-mirt_plotdata$mirt_se^2
        #mirt_plotdata$mirt_rel2 = 1-(1/mirt_plotdata$mirt_info) #https://stats.stackexchange.com/questions/43378/internal-consistency-reliability-in-item-response-theory-models?rq=1
        #mirt_plotdata$mirt_rel3 = 1-(mirt_plotdata$mirt_se/sd(mirt_scores))^2 #https://stats.stackexchange.com/questions/132738/reliability-in-irt-style
      # mirt_plotdata$mirt_rel = mirt_plotdata$mirt_info/(mirt_plotdata$mirt_info+1) #
      
      #Conversion of information to reliability using mirt source code 
        J <- mirt_model@Data$nitems
        gp <- ExtractGroupPars(mirt_model@ParObjects$pars[[J+1]]) #The extractgrouppars function is taken from mirt! 
        sigma2 = gp$gcov[1L,1L]
        mirt_plotdata$mirt_rel = mirt_plotdata$mirt_info/(mirt_plotdata$mirt_info+ (1/sigma2))  #Phil chambers, email communication 
      
      
      
      
      
      mirt_plotdata$task = vars
    
    #Set up for parrallel computation
    # if (nCores>1){
    #   mirtCluster(remove = TRUE)
    # }
    

  #Print output from MIRT prokect
  if (verbose){
    print(plot(mirt_model, type="rxx", xlim=c(-3.09,3.09)))
    print(ggplot(mirt_plotdata,aes(x=ThetaRange,y=mirt_rel)) + theme_apa() + coord_cartesian(ylim=c(0,1)) + geom_hline(yintercept = .6, col="grey", linetype="dashed")+ geom_line() + labs(x="Latent Ability (Z-Score)", y="Conditional Reliability",title=paste0("MIRT - ",vars)))
    print(ggplot(mirt_plotdata,aes(x=UniRange,y=mirt_rel)) + theme_apa() + coord_cartesian(ylim=c(0,1)) + geom_hline(yintercept = .6, col="grey", linetype="dashed")+ geom_line() + labs(x="Latent Ability (Percentile)", y="Conditional Reliability",title=paste0("MIRT - ",vars)))
    print(plot(mirt_model, type="trace",theta_lim=c(-5,5)))
  }
    

  #Create Results table
      freq_table = as.data.frame(t(apply(df2,2,table)))
        colnames(freq_table) = paste0("freq_",colnames(freq_table))
  
      ResultsTable = cbind.data.frame(mirt_coef_table,freq_table)
      
      ResultsTable$CronbachAlpha = cronbach.alpha(df2)$alpha
      print(ResultsTable)
    
  out=list(mirt_model,mirt_scores, ResultsTable,mirt_plotdata)
  names(out)=c("mirt_model","mirt_scores","ResultsTable","mirt_plotdata")
  return(out)
  rm(df2,TIFout,scores,dropN, scores.list, mirt_model, mirt_plotdata,ResultsTable)
}



#Function which makes all vectors the same size by adding NA's to items (up to N) 

addNA=function(x,N){
  if (length(x)<N){
    out=c(x,rep(NA,N-length(x)))
  }
  if (length(x)==N){
    out=x
  }
  return(out)
}



#Function To Normalise Variables - note that the "normal2" version was used thoughout manuscript. 

Normalise = function(x, return="normal2", MinDim=2, rescale=TRUE){

   if (return=="normal2"){
    percentile2 = (rank(x, na.last="keep", ties.method = "random")-.5)/(length(na.omit(x))) # rank the observations from 0-1. We cannot have the percentile values of 0% & 100% as these would confer to a -/+ infinity on a normal scale, so we add a blank 1/2 observation on each end.
    normalscore = qnorm(percentile2)
    
    #average over z-scores with the same value.
    for(i in unique(x)){
      if(length(which(x==i))>1){
        normalscore[x==i]=median(normalscore[x==i],na.rm = TRUE)
      }
    }
    out=as.numeric(normalscore)
    if (rescale==TRUE){
      out=as.numeric(scale(out,center = TRUE, scale=TRUE))
    }
  } else if (return=="normal"){
    percentile2=  (rank(x, na.last="keep")-.5)/(length(na.omit(x)))  #1 added to prevent infinity occuring!
    normalscore=qnorm(percentile2)
    out=as.numeric(normalscore)
    if (rescale==TRUE){
      out=as.numeric(scale(out,center = TRUE, scale=TRUE))
    }
    
  } else if (return=="percentile"){
    percentile1= ( rank(x, na.last="keep")-1)/(length(na.omit(x))-1) 
    out=as.numeric(percentile1)
    
    if (rescale==TRUE){
      out=as.numeric(scale(out,center = TRUE, scale=TRUE))
    }
    
    
  } else {
    out="Please Specify Output"
  }
  
  #Minumum number of unique values in data allowed. This is so that we can avoid transforming binary data types or likert scales if we don't want them to be transformed 
  ObservedNumberUniqueValues = length(unique(na.omit(x)))
  if (ObservedNumberUniqueValues<=MinDim){
    out=x
  }
  
  return(out)
}



setNA = function(x,vals){
  x=c(t(x))
  x[x %in% vals]=NA
  return(x)
}



swapsies = function(x, input, replacement){
  return(replacement[match(x, input)])
}

#Convenience function that uses "swapsies' but gives output... 
Convert=function(x, input, output){
  
  before=c(t(x))
  after=swapsies(before, input, output)
  #x.rmv = setNA(c(t(x)), rmv)
  # x.swap = swapsies(x, input, output)
  #  cat("Freq Before = \n")
  print(table(before))
  #  cat("\n\nFreq After = \n")
  print(table(after))
  
  return(after)
  
}

plotmat=function(correlationmatrix, varorder=NULL,axis_labels=NULL){
  
  #correlationmatrix=apply(correlationmatrix,2, function(x) (x^2)^.5) #remove negative values
  axis_labels = axis_labels[varorder]
  correlationmatrix=correlationmatrix[varorder,varorder]
  correlationmatrix[lower.tri(correlationmatrix, diag=TRUE)]=NA #create lower matrix
  rownames(correlationmatrix) = axis_labels ; colnames(correlationmatrix) = axis_labels
  melted = reshape2::melt(correlationmatrix)
  melted$value2 = gsub("^-0","-",gsub("NA","",gsub("^0","",formatC(melted$value,digits=2, format="f"))))
  ggplot(data = melted, aes(x=Var1, y=Var2,fill=abs(value)))+# + geom_point(aes(size=value^2,alpha=value^4))+
    geom_tile() + labs(x=NULL, y=NULL) +
    theme(axis.text = element_text(size=5)) + geom_text(aes(label=value2)) + jtools::theme_apa()+ # ++ scale_fill_brewer(palette="Dark2")
    scale_fill_continuous(na.value="white")
}

OrdinalMe = function(x, NCat){
  percentile=(rank(x, na.last="keep"))/(max(rank(x, na.last="keep"), na.rm=TRUE)+1) #calculate percentile score between 0 and 1 (0 and 1 not included as they lead to infinity)
  categories=sapply(0:NCat, function(x) 1/NCat*x)
  return(cut(percentile, categories, labels=FALSE))
}

df0Cor = function(var){
  print(cor(df0[,grepl(var, colnames(df0))], use="pairwise.complete.obs"))
}


elbowPlot = function(df, clusterrange=1:7, nrand=10, MaxIter=100, Algorithm="Hartigan-Wong"){
  #Fit k mean models on real data of varying clusters
  df=na.omit(df)
  totalSS =  kmeans(df, centers=1,iter.max = 100)$totss
  iccTRUE = sapply(clusterrange, function(i) kmeans(df, centers=i,iter.max = MaxIter,algorithm = Algorithm)$betweenss)/totalSS
  
  #randomise data
  randomDF=lapply(1:nrand, function(x) apply(df, 2, function(x) x[sample(length(x))]))
  
  #fit k means to random data
  iccRAND = sapply(clusterrange, function(x)
    mean(sapply(1:nrand, function(y) kmeans(randomDF[[y]], centers=x,iter.max = MaxIter,algorithm = Algorithm)$betweenss), na.rm=TRUE))/totalSS
  
  iccChangeTrue = c(iccTRUE[-1]-iccTRUE[-length(iccTRUE)])
  iccChangeRand = c(iccRAND[-1]-iccRAND[-length(iccTRUE)])
  
  #addedInformationComp = c(0, addedInfomationTRUE*-1+addedInfomationRAND)
  
  
  plot(x=clusterrange, y=iccTRUE, col="gray", type="l",main="Elbow Plot+", xlab="Number of clusters", ylab="Black/RED: ICC increase per cluster real/random data Gray: Total ICC explained real data")
  points(x=2:max(clusterrange), y=iccChangeTrue, type="l")
  points(x=2:max(clusterrange), y=iccChangeTrue)
  points(x=2:max(clusterrange), y=iccChangeRand, col="red")
  points(x=2:max(clusterrange), y=iccChangeRand, col="red", type="l")
  
}


calcFactorScore = function(df1=df0, var, MissingTolerance=1, verbose=FALSE, nfact=1){
  library(psych)
  FAmod=fa(df1[,var] ,nfactors=nfact) #get factor model
  if(verbose){print(FAmod)} #print FA results
  scores=psych::factor.scores(df1[,var] ,FAmod, method="tenBerge", impute="mean")[[1]] #Calculate scores 
  scores[(apply(df1[,var], 1, function(x) length(which(is.na(x)))))>MissingTolerance,] =NA  #remove scores where more than MissingTolereance items are missing
  scores=apply(scores,2,as.numeric)
  if(ncol(scores)==1){
    scores=as.numeric(scores)
  }
  
  return((scores))
}


findMCS = function(x){
  print(grep(x, colnames(df0), value=TRUE))
}


#Read Raw Data Files from iPad App. 

ReadRedFiles = function(FolderLocation="/Users/gb01/ownCloud/Data/Local non-PID data copy/Data/Raw Data/", pattern,removeDuplicates=FALSE, minIDlength=5){
  FileNames = list.files(FolderLocation, pattern=pattern, recursive = TRUE, full.names = TRUE)
  
  ParticipantID = str_match(gsub("_201[8-9].*","",FileNames),"[\\d]*$")
  
  #ParticipantID = gsub(paste0(pattern,"_"),"",
  #                     gsub("_20[0-9]{2}$", "",
  #                          str_match(FileNames, paste0(pattern, "_[0-9]+","_20[0-9]{2}"))
  #                     ))
  #Only keep files with valid PpsIDs
  #Remove files we don't want 
  #Only keep IDs with a length of 6
  keepParticipants1= (nchar(ParticipantID)>=minIDlength)
  #Remove empty files
  keepParticipants2=  as.vector(sapply(FileNames, function(x) file.info(x)$size!=0))
  #Remove participants
  keepParticipants = keepParticipants1 & keepParticipants2
  FileNames2 = FileNames[keepParticipants]
  PpsID = ParticipantID[keepParticipants]
  
  #Remove duplicate tests (i.e. if same person did test twice, keep longer file, or earlier file)
  if(removeDuplicates){
    for(i in unique(PpsID)){
      if (length(which(PpsID %in% i))>1){
        Files = FileNames[which(ParticipantID %in% i)]
        cat("\nDuplicate File: \n",Files)
        rowlengths = sapply(Files, function(x) nrow(read.csv(x)))
        Files[rowlengths<max(rowlengths)]=NA
        keep=which.min(as.POSIXct(gsub(".txt","",str_extract(Files, "[\\d]{1,2}-[\\d]{1,2}-[\\d]{1,2}.txt")),format="%H-%M-%S"))
        remove=which(FileNames2 %in% Files[-keep])
        FileNames2=FileNames2[-remove]
        PpsID=PpsID[-remove]
      }
      suppressWarnings({rm(Files,rowlengths,keep,remove)})
    }
  }
  
  df.list=lapply(FileNames2, function(x) read.csv(x, colClasses = "character"))
  
  #Remove files with 0 row length
  remove3=which(sapply(df.list, function(x) nrow(x)==0))
  if(length(remove3)>0){
    df.list = df.list[-remove3]
    PpsID = PpsID[-remove3]
    FileNames3 = FileNames2[-remove3]
  } 
  if(length(remove3)==0) {
    FileNames3 = FileNames2
  }
  
  
  export=list(df.list, PpsID, FileNames3)
  
  #print out deleted files
  cat("Files Removed because ID length != 6\n")
  print(FileNames[!keepParticipants1][1:min(c(100,length(FileNames[!keepParticipants1])))])
  cat("\n\nFiles Removed because the files were empty\n")
  print(FileNames[!keepParticipants2][1:min(c(100,length(FileNames[!keepParticipants2])))])
  cat("\n\nFiles Removed because 0 row length\n")
  print(FileNames2[remove3])
  
  
  return(export)
}



