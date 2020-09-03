#Load Data and functions 
source("P1S0 - Load Data.R")  

# apply(df0[,findMCS("ans_trialcorrect_")],2,function(x) length(which(x==1))/length(which(x==1|x==0)))




#Sample Decomposition

#How many (in the school cohort) have completed at least one assessment? 

df0_school = df0[!grepl("^99",df0$PpsID),] #Children with school ids (though we may have no cognitive data for them!)
df0_school_tested = df0_school[apply(df0_school[,CogV_Accuracy],1, function(x) length(which(!is.na(x))))>0,] #Dataframe inlcuidng children with at least one assessment (in reality, min was 6) tested in schools

#Standard Error of measurement calculations (See end of discussion)

  reliability = .90
  score_sd = 15
  
  score_sd*(1-reliability)^.5
  
  1.96*score_sd*(1-reliability)^.5


#How many classes have been tested? 

unique_classes = unique(paste0(df0$SchoolID,"_",df0$ClassID))
unique_classes = unique_classes[!grepl("NA",unique_classes)]
length(unique_classes)

#Age range

summary(df0_school_tested$Age1)
sd(df0_school_tested$Age1,na.rm=TRUE)

#How many teachers completed the APQ assessment?
  df0_teacherresponses = df0[,c("SchoolID","ClassID",paste0("APQ",1:3),"S1_TeachNorm_FSN_Complete")]
  df0_teacherresponses$School_Class = paste0(df0_teacherresponses$SchoolID,"_",df0_teacherresponses$ClassID)
    apply(df0_teacherresponses,2,function(x) table(!is.na(x)))
  
  table(!is.na(df0_teacherresponses$APQ1),df0_teacherresponses$School_Class) #data on kids from 16 classes. 

#Teacher APQ rating correlations & reliability
  cor(df0[,paste0("APQ",1:3,"_Norm")], use = "pairwise.complete.obs")
  cors = cor(df0[,paste0("APQ",1:3,"_Norm")], use = "pairwise.complete.obs")
  mean(cors[c(2,3,6)])
  coefH(df0[,paste0("APQ",1:3,"_Norm")], n_bootstrap = 1000)
  cronbach.alpha(na.omit(df0[,paste0("APQ",1:3,"_Norm")]))
  
#Average Number of characters in the reading test

sentences = c(
  "You can eat a banana.",
  "A dog can fly.",
  "Cats have five legs.",
  "A shoe goes on your foot.",
  "An iPad has a screen.",
  "A bird has arms.",
  "The number '5' is a letter.",
  "A circle is round.",
  "Birds can lay eggs.",
  "People hear with their ears.",
  "A car floats on water.",
  "Many children like to play games.",
  "Sometimes the moon is purple.",
  "Elephants are big.",
  "Poop can be stinky.",
  "A kitten is a baby horse.",
  "The sea has a lot of water.",
  "The letter 'B' is the last letter of the alphabet.",
  "A airplane flies in the sky.",
  "A fork can be used for eating",
  "People can see cartoons on a television.",
  "A crayon can be used for drawing.",
  "Ants are big animals.",
  "A cow has four legs.",
  "A child can wear shorts.",
  "Some trees grow fruits.",
  "People like to write with a spoon.",
  "Games can be played on a computer.",
  "October is the month after January.",
  "A lock can be opened with a key.",
  "People can turn lights on with a button.",
  "Some ships have a sail.",
  "A bike has wheels.",
  "Classrooms are always filled with water.",
  "A pot can be empty",
  "People dial a number to phone someone.",
  "'D' is a letter of the alphabet.",
  "A cow can eat grass.",
  "Nobody wears a coat in cold weather.",
  "A glass can be made from shoe laces.",
  "A child may like candy.",
  "Dogs can walk on the ceiling.",
  "Bees eat salmon from the river.",
  "It can be cold inside a freezer.",
  "Tuesday is the first month of the year.",
  "All crabs have three legs.",
  "Many eagles wear bathing suits.",
  "Bread can be used to make sandwiches.",
  "A painting can be hung on a wall.",
  "Most people laugh when they are angry.",
  "People should not eat yellow snow.",
  "Children often go to space.",
  "Some people write books.",
  "Most grass is green.",
  "A bath can hold water.",
  "Fish tend to live on land.",
  "All dogs wear socks and shoes.",
  "Some people wear boots in the rain.",
  "Some children like reading.",
  "Every person has a different name.",
  "Most people sleep in trees.",
  "Most bears ride airplanes to work.",
  "A spoon is needed to tell the time.",
  "A giant is much smaller than a dwarf.",
  "Different plants can grow in a garden.",
  "Some people like to walk through the park.",
  "The number '1' is the biggest number.",
  "Orange juice can be served in a glass for breakfast.",
  "A letter with a stamp cannot be posted.",
  "Cats can eat with their mouths.",
  "A dog usually spends money on art supplies.",
  "People usually sleep on their roof.",
  "Ants are bigger than cats.", 
  "All horse breeds are of the same height and weight.", 
  "Bread rolls can be served at lunch and at dinner.", 
  "Many trees grow hair on their branches.", 
  "Most schools have several classrooms.", 
  "A chicken meows like a cat.", 
  "Traffic lights always have purple and yellow lights.", 
  "A carpenter can use a saw and a hammer.", 
  "The job of a painter is to help you buy groceries.", 
  "An elephant is very light.", 
  "A sailor works on a boat.", 
  "Some people play games on their computer.", 
  "An electrician can fix a broken doorbell.", 
  "A light bulb belongs in the toilet bowl.", 
  "A cupboard can be used to house your parents.", 
  "All chocolate tastes very sour.", 
  "A television can be used to watch cartoons.", 
  "Some trees lose their leaves in winter.", 
  "Telephones can be used to grate cheese.", 
  "Dinosaur bones fall from the sky on Saturdays.", 
  "A teacher can work at a school.", 
  "Some people enjoy reading an interesting book.", 
  "You can see tigers at some zoos.", 
  "Many bankers wear suits and ties.", 
  "Magazines are printed on knives and forks.", 
  "Fish and sharks live under water.", 
  "Horses only ever eat pigeons.", 
  "Cats lay eggs.", 
  "Professors work at universities.")
  
  mean(sapply(gsub("[.]$","",sentences), nchar))
  
  
  #Add citations for r packages
  
  citation("caret")
  packages = c("caret", "spls", "mirt","monomvn", "psych")
    references = unlist( lapply(packages, function(x) toBibtex(citation(x))  ) )
    write(references, file="references.bib")

    
    testdf = data.frame(matrix(sample(100),ncol=10))
    testdf[cbind(sample(10,5),sample(10,5))]=NA
    testdf
    
    complete.cases(testdf)
    na.omit(testdf)
    
    
    # findMCS("")[1:100]
    # 
    # df0_teacherresponses = df0[,c("SchoolID","ClassID",paste0("APQ",1:3))]
    # df0_teacherresponses$School_Class = paste0(df0_teacherresponses$SchoolID,df0_teacherresponses$ClassID)
    # table(df0_teacherresponses$School_Class)
    # 
    # table(!is.na(df0_teacherresponses$APQ1))
    # table(!is.na(df0_teacherresponses$APQ2))
    # table(!is.na(df0_teacherresponses$APQ3))
    # sum(table(df0_teacherresponses$APQ3))
    # 
    # table(is.na(df0$S1_TeachNorm_FSN_Complete))
    
    
    
    