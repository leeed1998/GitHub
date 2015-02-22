### Edward Lee
### 2/22/2015
### Coursera Practical Machine Learning

### run_analysis.R script

setwd("C:/Users/ed/Documents/Coursera/Course8_Machine Learning/Assignment")

# Load the need packages
library(randomForest)
library(caret)

# Read in and check out data - iterative - had to take care of #DIV/0 - also to take care
# of the null values

Data <- read.csv("pml-training.csv",na.strings=c('NA','','#DIV/0!'))
DataTwenty <- read.csv("pml-testing.csv",na.strings=c('NA','','#DIV/0!'))

View (Data)
dim(Data);dim(DataTest)
summary(Data)
names(Data)

# Let's delete the useless columns, first 7 columns of Ids and datetime are not useful in a model
# Remember to provide the same treatment to the testing set

Data1 <- Data[-(1:7)]
DataTwenty1 <- DataTwenty[-(1:7)]


# Let's also get columns that are fully populated with Data
goodcol <- colnames(Data1[colSums(is.na(Data1)) == 0])
goodcolTwenty <- colnames(DataTwenty1[colSums(is.na(DataTwenty1)) == 0])

Data2 <- Data1[goodcol]
DataTwenty2 <- DataTwenty1[goodcolTwenty]

View(Data2)
View(DataTwenty2)

# After reading the forum, we will create a traing and testing for cross validation

set.seed(1345345)

inTrain = createDataPartition(Data2$classe, p = 3/4)[[1]]
training = Data2[inTrain,]
testing = Data2[-inTrain,]


# Let's try a RandomForest (try Rfit and it was bad - had to delete and forum the rest)
modFit2 <- randomForest(classe ~ .,data=training,ntree=500, mtry=30)

training_pred2 <- predict(modFit2,training)
print(confusionMatrix(training_pred2, training$classe))

## HeeHaw! - this is 100% accurate on the training - 99% on testing.. gtg
training_pred3 <- predict(modFit2,testing)
print(confusionMatrix(training_pred3, testing$classe))

### Now we can test this boy on the 20 real testing population
answers <- predict(modFit2,DataTwenty2)
answers


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


pml_write_files(answers)
