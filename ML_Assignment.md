# MLA Assignment


## Background  

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, my goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Data  

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. Thank you for the use of the data for learning purposes.  

## Setting up  
First we need the following R libraries.  


```r
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
library(rpart)
library(randomForest)
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

## Reading the Data  
Reading the data was a bit tricky because of the way that nulls are coded.  I went through the files and decided that there were three null values.  I coded those in na.strings option.  


```r
data <- read.csv("pml-training.csv", stringsAsFactors = FALSE,na.strings=c("#DIV/0!","","NA"))
score <- read.csv("pml-testing.csv", stringsAsFactors = FALSE,na.strings=c("#DIV/0!","","NA"))
```


## Data Exploration 
I took a look at the outcome variable to make sure that it had the right values.  



```r
classe <- as.factor(data$classe)
table(classe)
```

```
## classe
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

There are 160 variables in the dataset, so the first task was the whittle the number of explanatory variables.  We know that the explantory variables of interest will have measurements related to belt,arm, and dumbbell, so it was easy to grab variables whose names had these keywords in them.  The number of variables now went from 160 to 114.


```r
filter = grepl("belt|arm|dumbell", names(data))
data2 = data[, filter]
```

The data has a lot of missing values.  In fact, variables that have missing values seem to be missing for a vast majority of rows, so it does not make sense to impute these values.  Code below identifies how many missing values per column and then removes these columns from our dataset.  Now we are down to only 39 explanatory variables.  


```r
missing= data.frame(colSums(is.na(data2)))
missing= missing[missing$colSums.is.na.data2..>0,] 
missing
```

```
##  [1] 19226 19248 19622 19225 19248 19622 19216 19216 19226 19216 19216
## [12] 19226 19216 19216 19226 19216 19216 19216 19216 19216 19216 19216
## [23] 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216
## [34] 19216 19216 19294 19296 19227 19293 19296 19227 19216 19216 19216
## [45] 19216 19216 19216 19216 19216 19216 19300 19301 19622 19299 19301
## [56] 19622 19216 19216 19300 19216 19216 19300 19216 19216 19300 19216
## [67] 19216 19216 19216 19216 19216 19216 19216 19216 19216
```

```r
good.cols = colSums(is.na(data2)) == 0
data3 = data2[, good.cols]
```

The following code adds outcome variable back in to the dataset.  


```r
data3$classe=classe
str(data3)
```

```
## 'data.frame':	19622 obs. of  40 variables:
##  $ roll_belt          : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt         : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt           : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt   : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x       : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y       : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z       : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x       : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y       : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z       : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x      : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y      : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z      : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm           : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm          : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm            : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm    : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x        : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y        : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z        : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x        : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y        : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z        : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x       : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y       : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z       : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ roll_forearm       : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
##  $ pitch_forearm      : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm        : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm: int  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x    : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
##  $ gyros_forearm_y    : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
##  $ gyros_forearm_z    : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
##  $ accel_forearm_x    : int  192 192 196 189 189 193 195 193 193 190 ...
##  $ accel_forearm_y    : int  203 203 204 206 206 203 205 205 204 205 ...
##  $ accel_forearm_z    : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
##  $ magnet_forearm_x   : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
##  $ magnet_forearm_y   : num  654 661 658 658 655 660 659 660 653 656 ...
##  $ magnet_forearm_z   : num  476 473 469 469 473 478 470 474 476 473 ...
##  $ classe             : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

## Data Partitioning

I set the seed so that the partitioning is reproducible.  


```r
set.seed(12463)
inTrain = createDataPartition(y=classe, p=0.75, list=FALSE)
train = data3[inTrain,]
test = data3[-inTrain,]
```

## Data Modeling 
I choose decision trees because they are easy to interpret and ranom forests because they are better in accuracy.  I tried using the train function in the caret package, but that took too long to run.  (The random forest model did not finish even after several hours and the decision tree model was quite poor in accuracy.)  So, I used the rpart and random forests packages.  

As can be seen, the decision tree model is good (about 72 percent accurate), but the random forests model is much much better, with an accuracy of 99%!!!  



```r
modFit1 <- rpart(classe ~ ., data = train, method="class")
modFit1
```

```
## n= 14718 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##   1) root 14718 10533 A (0.28 0.19 0.17 0.16 0.18)  
##     2) roll_belt< 130.5 13478  9303 A (0.31 0.21 0.19 0.18 0.11)  
##       4) pitch_forearm< -33.95 1175     8 A (0.99 0.0068 0 0 0) *
##       5) pitch_forearm>=-33.95 12303  9295 A (0.24 0.23 0.21 0.2 0.12)  
##        10) roll_forearm< 126.5 7895  5184 A (0.34 0.25 0.16 0.19 0.069)  
##          20) roll_forearm>=-58.25 5664  3256 A (0.43 0.26 0.17 0.13 0.012)  
##            40) yaw_belt< -2.605 1960   945 A (0.52 0.45 0.011 0.021 0.0041)  
##              80) yaw_belt< -8.8 1016   229 A (0.77 0.2 0.019 0 0.0079) *
##              81) yaw_belt>=-8.8 944   271 B (0.24 0.71 0.0021 0.043 0) *
##            41) yaw_belt>=-2.605 3704  2311 A (0.38 0.16 0.26 0.19 0.016)  
##              82) pitch_belt< -42.55 1280   688 A (0.46 0.42 0.069 0.039 0.007)  
##               164) roll_belt< 123.5 587    47 A (0.92 0.043 0.0017 0.034 0.0017) *
##               165) roll_belt>=123.5 693   177 B (0.075 0.74 0.13 0.043 0.012) *
##              83) pitch_belt>=-42.55 2424  1549 C (0.33 0.019 0.36 0.27 0.02)  
##               166) pitch_belt>=15.05 852   324 A (0.62 0.035 0 0.33 0.015)  
##                 332) pitch_forearm< 37.15 565    66 A (0.88 0.039 0 0.074 0.0035) *
##                 333) pitch_forearm>=37.15 287    48 D (0.1 0.028 0 0.83 0.038) *
##               167) pitch_belt< 15.05 1572   697 C (0.17 0.0095 0.56 0.24 0.023)  
##                 334) yaw_belt< 162.5 927   203 C (0.13 0.0065 0.78 0.044 0.035) *
##                 335) yaw_belt>=162.5 645   313 D (0.23 0.014 0.23 0.51 0.0062)  
##                   670) magnet_belt_x>=176.5 126     6 A (0.95 0 0.016 0 0.032) *
##                   671) magnet_belt_x< 176.5 519   187 D (0.056 0.017 0.29 0.64 0) *
##          21) roll_forearm< -58.25 2231  1501 D (0.14 0.21 0.11 0.33 0.21)  
##            42) yaw_forearm>=-125.5 1718  1282 B (0.16 0.25 0.12 0.22 0.25)  
##              84) pitch_belt>=0.895 1519  1086 B (0.18 0.29 0.14 0.13 0.27)  
##               168) accel_forearm_y>=239.5 494   305 C (0.036 0.36 0.38 0.12 0.11) *
##               169) accel_forearm_y< 239.5 1025   673 E (0.24 0.25 0.024 0.14 0.34)  
##                 338) magnet_belt_y< 585.5 529   293 E (0.44 0.036 0.025 0.051 0.45)  
##                   676) magnet_forearm_z>=-46.5 364   144 A (0.6 0.049 0.011 0.063 0.27) *
##                   677) magnet_forearm_z< -46.5 165    28 E (0.085 0.0061 0.055 0.024 0.83) *
##                 339) magnet_belt_y>=585.5 496   259 B (0.032 0.48 0.024 0.23 0.23) *
##              85) pitch_belt< 0.895 199    22 D (0 0.015 0 0.89 0.095) *
##            43) yaw_forearm< -125.5 513   160 D (0.068 0.074 0.066 0.69 0.1) *
##        11) roll_forearm>=126.5 4408  3073 C (0.067 0.21 0.3 0.21 0.21)  
##          22) accel_forearm_x>=-108.5 3198  2120 C (0.075 0.25 0.34 0.095 0.25)  
##            44) magnet_forearm_z< -245 243    61 A (0.75 0.2 0 0.049 0) *
##            45) magnet_forearm_z>=-245 2955  1877 C (0.02 0.25 0.36 0.098 0.27)  
##              90) roll_belt>=-0.55 2589  1511 C (0.022 0.28 0.42 0.071 0.21)  
##               180) magnet_belt_z>=-381.5 1859   938 C (0.0081 0.31 0.5 0.068 0.12)  
##                 360) accel_forearm_z>=-124.5 193    50 B (0 0.74 0.12 0.052 0.083) *
##                 361) accel_forearm_z< -124.5 1666   769 C (0.009 0.26 0.54 0.07 0.13) *
##               181) magnet_belt_z< -381.5 730   415 E (0.059 0.22 0.22 0.077 0.43) *
##              91) roll_belt< -0.55 366   117 E (0 0.025 0 0.3 0.68)  
##               182) magnet_forearm_z< 288 115     7 D (0 0.061 0 0.94 0) *
##               183) magnet_forearm_z>=288 251     2 E (0 0.008 0 0 0.99) *
##          23) accel_forearm_x< -108.5 1210   576 D (0.047 0.098 0.21 0.52 0.12)  
##            46) magnet_arm_y>=291 365   161 C (0.038 0.14 0.56 0.19 0.066) *
##            47) magnet_arm_y< 291 845   282 D (0.051 0.078 0.063 0.67 0.14) *
##     3) roll_belt>=130.5 1240    10 E (0.0081 0 0 0 0.99) *
```

```r
confusionMatrix(predict(modFit1,train,type="class"),train$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 3515  324   26   97  114
##          B  296 1569  125  196  140
##          C  171  662 2014  287  318
##          D  136  131  236 1772  203
##          E   67  162  166   60 1931
## 
## Overall Statistics
##                                          
##                Accuracy : 0.7339         
##                  95% CI : (0.7266, 0.741)
##     No Information Rate : 0.2843         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.6641         
##  Mcnemar's Test P-Value : < 2.2e-16      
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8399   0.5509   0.7846   0.7347   0.7136
## Specificity            0.9467   0.9362   0.8817   0.9426   0.9621
## Pos Pred Value         0.8624   0.6745   0.5834   0.7151   0.8093
## Neg Pred Value         0.9370   0.8968   0.9509   0.9477   0.9372
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Rate         0.2388   0.1066   0.1368   0.1204   0.1312
## Detection Prevalence   0.2769   0.1580   0.2345   0.1684   0.1621
## Balanced Accuracy      0.8933   0.7436   0.8331   0.8386   0.8379
```

```r
confusionMatrix(predict(modFit1,test,type="class"),test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1157  111   18   38   25
##          B  109  497   43   72   51
##          C   44  227  663   88  103
##          D   63   48   82  591   78
##          E   22   66   49   15  644
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7243          
##                  95% CI : (0.7116, 0.7368)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6522          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8294   0.5237   0.7754   0.7351   0.7148
## Specificity            0.9453   0.9305   0.8859   0.9339   0.9620
## Pos Pred Value         0.8577   0.6438   0.5893   0.6856   0.8090
## Neg Pred Value         0.9331   0.8906   0.9492   0.9473   0.9374
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2359   0.1013   0.1352   0.1205   0.1313
## Detection Prevalence   0.2751   0.1574   0.2294   0.1758   0.1623
## Balanced Accuracy      0.8873   0.7271   0.8307   0.8345   0.8384
```

```r
modFit2 <- randomForest(classe ~ ., data = train, ntree = 100)
confusionMatrix(predict(modFit2,train,type="class"),train$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4185    0    0    0    0
##          B    0 2848    0    0    0
##          C    0    0 2567    0    0
##          D    0    0    0 2412    0
##          E    0    0    0    0 2706
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9997, 1)
##     No Information Rate : 0.2843     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1839
## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1839
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```

```r
confusionMatrix(predict(modFit2,test,type="class"),test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1394    7    4    0    0
##          B    0  937   10    0    0
##          C    1    4  836    5    0
##          D    0    0    5  798    1
##          E    0    1    0    1  900
## 
## Overall Statistics
##                                           
##                Accuracy : 0.992           
##                  95% CI : (0.9891, 0.9943)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9899          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9993   0.9874   0.9778   0.9925   0.9989
## Specificity            0.9969   0.9975   0.9975   0.9985   0.9995
## Pos Pred Value         0.9922   0.9894   0.9882   0.9925   0.9978
## Neg Pred Value         0.9997   0.9970   0.9953   0.9985   0.9998
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2843   0.1911   0.1705   0.1627   0.1835
## Detection Prevalence   0.2865   0.1931   0.1725   0.1639   0.1839
## Balanced Accuracy      0.9981   0.9924   0.9877   0.9955   0.9992
```

## Conclusion and Scoring  
Needless to say, the random forest model was chosen.  
The following code scores the test dataset and prints out the predictions.  


```r
scored=predict(modFit2,score,type="class")
scored
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```


