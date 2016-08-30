setwd ("C:/Users/Renu/MLAssignment")
library(caret)
library(rpart)
library(randomForest)

set.seed(12463)
data <- read.csv("pml-training.csv", stringsAsFactors = FALSE,na.strings=c("#DIV/0!","","NA"))
score <- read.csv("pml-testing.csv", stringsAsFactors = FALSE,na.strings=c("#DIV/0!","","NA"))

str(data)

# outcome variable
classe <- as.factor(data$classe)
table(classe)

#keep columns of interest
filter = grepl("belt|arm|dumbell", names(data))
data2 = data[, filter]


missing= data.frame(colSums(is.na(data2)))
missing= missing[missing$colSums.is.na.data2..>0,] 
missing

# keep columns that do not have missing values
good.cols = colSums(is.na(data2)) == 0
data3 = data2[, good.cols]

# add classe back
data3$classe=classe

str(data3)

# split into train and test
inTrain = createDataPartition(y=classe, p=0.75, list=FALSE)
train = data3[inTrain,]
test = data3[-inTrain,]

set.seed(12345)
# decision trees
modFit1 <- rpart(classe ~ ., data = train, method="class")
#train
confusionMatrix(predict(modFit1,train,type="class"),train$classe)
#test
confusionMatrix(predict(modFit1,test,type="class"),test$classe)

# random forests
modFit2 <- randomForest(classe ~ ., data = train, ntree = 100)

#train
confusionMatrix(predict(modFit2,train,type="class"),train$classe)
#test
confusionMatrix(predict(modFit2,test,type="class"),test$classe)

# Score Dataset
scored=predict(modFit2,score,type="class")
scored


        

