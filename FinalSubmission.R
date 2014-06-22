setwd("C:/Ambarish/Practical Machine Learning")

traindata <- read.csv("pml-training.csv")

table(traindata$classe)

testdata <- read.csv("pml-testing.csv")

x <- grep("factor",sapply(traindata,class))

traindata.2 <- traindata[,-(x)]

traindata.3 <- traindata.2[, colSums(is.na(traindata.2)) == 0 ]

traindata.3$classe <- traindata$classe

traindata.3 <- traindata.3[,5:57]

tesdata.1 <- testdata[,names(traindata.3[,1:52])]

library(caret)

trControl = trainControl(method = "cv", number = 4)

modelFit <- train(traindata.3$classe ~ .,method="rf",data=traindata.3,trControl = trControl)

predictions <- predict(modelFit,tesdata.1)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

setwd("C:/Ambarish/Practical Machine Learning/Answers")

pml_write_files(predictions)
