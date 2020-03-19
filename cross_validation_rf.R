library(randomForest)
library("caret")
library(pROC)
#k: k fold
#data: the data used for cross validataion
#dir: the directory where the results are saved
#filename: the result files name
cross_validation_rf <- function(k,data,dir,filename){

  #train
  n=length(names(data))
  folds<-createFolds(y=data$Target_label,k=k) #根据training的laber-Species把数据集切分成k等份
  TPR=c(length=k)
  FPR=c(length=k)
  MCC=c(length=k)
  F1=c(length=k)
  ACC=c(length=k)
  precision=c(length=k)
  recall=c(length=k)
  ROCArea=c(length=k)
  PRCArea=c(length=k)
  prob=c(length=nrow(data))
  for(i in 1:k){
    print(paste(i,"-fold"))
    index=1:nrow(data)
    index_train=sample(index[-folds[[i]]])
    train<-data[index_train,]
    index_test=sample(index[folds[[i]]])
    test<-data[index_test,]
    rf <- randomForest(Target_label ~ ., data=train)
    classification=predict(rf,newdata=test)
    pro=predict(rf,newdata=test,type="prob")
    TP <- as.numeric(sum(classification=="m1A_positive" & test$Target_label=="m1A_positive"))
    FP <- as.numeric(sum(classification=="m1A_positive" & test$Target_label=="m1A_negative"))
    TN <- as.numeric(sum(classification=="m1A_negative" & test$Target_label=="m1A_negative"))
    FN <- as.numeric(sum(classification=="m1A_negative" & test$Target_label=="m1A_positive"))
    TPR[i]=TP/(TP+FN)
    FPR[i]=TN/(TN+FP)
    MCC[i]=(TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    F1[i]=(2*TP)/(2*TP+FP+FN)
    ACC[i]=(TP+TN)/(TP+TN+FP+FN)
    precision[i]=TP/(TP+FP)
    recall[i]=TP/(TP+FN)
    ROCArea[i]=auc(roc(test$Target_label,pro[,2]))
    prob[index_test]=pro[,2]
  }
  result=data.frame(mean(TPR),mean(FPR),mean(F1),mean(precision),mean(ACC),mean(recall),mean(MCC),mean(ROCArea))
  colnames(result)<-c("TPR","FPR","F-Measure","Precision","ACC","Recall","MCC","AUC")
  file1=paste(dir,paste(filename,".result.csv"),sep = '/')
  write.csv(result,paste(dir,paste(filename,k,".result.rf.csv"),sep = '/'),row.names = F)
  write.csv(prob,paste(dir,paste(filename,k,".probability.rf.csv"),sep = '/'),row.names = F)
  
  return(result)
}