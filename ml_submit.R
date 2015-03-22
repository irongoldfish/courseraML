#setwd("~/scratch/coursera")
library(caret)
library(dplyr)
library(tidyr)
set.seed(2015)

getAccuracy<-function(samplesize){
  if(!(exists("dat.full")))dat.full<-read.csv("pml-training.csv",header=TRUE)
  
  #The original data contain user identifiers. Users probably have distinctive movement patterns, and the classes are not evenly distributed over users, possibly allowing user name to become a spurious predictor effective in the training data only. Training data was anonymized (somewhat) by removing user_name, new_window, and num_window, dropping cvtd_timestamps and converting raw timestamps to relative timestamps.
  cleandf<-function(dat){
    dat.clean<-as.data.frame(dat%>%
                               group_by(user_name)%>%
                               mutate(raw_timestamp_part_1=raw_timestamp_part_1-min(raw_timestamp_part_1))%>%
                               ungroup()%>%
                               dplyr::select(-X,-user_name, -cvtd_timestamp,-new_window,-num_window,-raw_timestamp_part_2) )#dplyr identifier needed on select to avoid clashes with MASS
    
    #There are some variables that look like they're intended to be numeric, but rendered as factors due to the presence of #DIV0 markers or empty strings. Try to convert them, failures to parse as a number convert to NA.
    for(name in names(dat.clean)){
      if(class(dat.clean[,name])=="factor"&&name!="classe")dat.clean[,name]<-as.numeric(as.character(dat.clean[,name]))
      #remove variables with many NA values
      if(sum(is.na(dat.clean[,name]))>=nrow(dat.clean)/4)dat.clean<-dat.clean[,-which(names(dat.clean)==name)]
    }
    
    
    #Also remove uninformative columns (low variation).
    if(length(nearZeroVar(dat.clean)>0))dat.good<-dat.clean[,-nearZeroVar(dat.clean)]#'if' necessary, selecting -<empty vector> cols selects no cols not all of them. :-(
    return(dat.clean)
  }
  
  if("user_name"%in%names(dat.full))dat.full<-cleandf(dat.full)
  
  #fitting to the full dataset is SLOW.
  dat.clean<-sample_n(dat.full,samplesize)
  dat.minitest<-sample_n(dat.full,samplesize)
  
  #also tried lda, it's faster but not as accurate.
  fitrf<-train(classe~.,method="rf",preProcess=c("center","scale"),data=dat.clean)
  
  #try to distinguish between easy and hard cases
  preds<-predict(fitrf,newdata=dat.minitest)
  
  dat.minitest$hardcase<-as.factor(as.character(preds!=dat.minitest$classe))#convert to factor to allow 'train' to run smoothly. Via character for readability.
  
  sobspotter<-train(hardcase~.,method="rf",preProcess=c("center","scale"),data=dat.minitest)
  
  hardcases<-predict(sobspotter,newdata=dat.full)
  
  sobs<-sample_n(dat.full[hardcases=="TRUE",],samplesize)
  sobhandler<-train(classe~.,method="rf",preProcess=c("center","scale"),data=sobs)
  
  dat.test<-cleandf(read.csv("pml-testing.csv"))
  
  easypred<-as.character(predict(fitrf,newdata=dat.test))
  hardpred<-as.character(predict(sobhandler,newdata=dat.test))
  usehard<-as.character(predict(sobspotter,newdata=dat.test))

  to_submit<-ifelse(usehard=="TRUE",hardpred,easypred)
  
  pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
      filename = paste0("problem_id_",i,".txt")
      write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }
  
  #pml_write_files(answers)