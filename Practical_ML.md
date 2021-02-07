Predict activity quality from activity monitors
Tanya
07/02/2021
Synopsis
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The goal of this project is to predict the manner in which they did the exercise. This is the classe variable in the training set.

Data description
The outcome variable is classe, a factor variable with 5 levels. For this data set, participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in 5 different fashions:

exactly according to the specification (Class A)
throwing the elbows to the front (Class B)
lifting the dumbbell only halfway (Class C)
lowering the dumbbell only halfway (Class D)
throwing the hips to the front (Class E)
Initial configuration
The initial configuration consists of loading some required packages and initializing some variables.

#Data variables
training.file   <- './data/pml-training.csv'
test.file <- './data/pml-testing.csv'
training.url    <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
test.url  <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

#Directories
if (!file.exists("data")){
  dir.create("data")
}

library("caret")
## Loading required package: lattice
## Loading required package: ggplot2
library("randomForest")
## randomForest 4.6-14
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## The following object is masked from 'package:ggplot2':
## 
##     margin
library("rpart")
library("rpart.plot")
Data prepration
In this section the data is downloaded and processed. Some basic transformations and cleanup will be performed, so that NA values are omitted. Irrelevant columns such as user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, and  num_window (columns 1 to 7) will be removed in the subset.

The pml-training.csv data is used to devise training and testing sets. The pml-test.csv data is used to predict and answer the 20 questions based on the trained model.

# Download data
download.file(training.url, training.file)
download.file(test.url,test.file )
Clean data
training<-read.csv(training.file, na.strings=c("NA","#DIV/0!", ""))
testing <-read.csv(test.file , na.strings=c("NA", "#DIV/0!", ""))
A: Removing Variables which are having nearly zero variance.

non_zero_var <- nearZeroVar(training)

training <- training[,-non_zero_var]
testing<- testing[,-non_zero_var]

dim(training)
## [1] 19622   124
B: Removing Variables which are having NA values. Our threshhold is 95%.

na_val_col <- sapply(training, function(x) mean(is.na(x))) > 0.95

training <- training[,na_val_col == FALSE]
testing <- testing[,na_val_col == FALSE]

dim(training)
## [1] 19622    59
C: We also remove col1 to col7 because they are not related to the model

training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]
Data Partitioning
As per recommendation of the course __ Practical Machine Learning__ , we will be segregating our org_training_data into 2 different parts, one is the training set (consisting 75% of the total data) and validation set (consisting 25% of the total data)

subSamples <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
subTraining <- training[subSamples, ] 
subValidating <- training[-subSamples, ]
Exploratory analysis
The variable classe contains 5 levels. The plot of the outcome variable shows the frequency of each levels in the subTraining data.

barplot(table(subTraining$classe), col="orange", main="Levels of the variable classe", xlab="classe levels", ylab="Frequency")
 The plot above shows that Level A is the most frequent classe. D appears to be the least frequent one.

Prediction models
In this section a decision tree and random forest will be applied to the data.

Decision tree
# Fit model
modFitDT <- train(classe ~ ., data = subTraining, method="rpart")
# Perform prediction
predictDT <- predict(modFitDT, subValidating)
# Plot result
rpart.plot(modFitDT$finalModel, main="Classification Tree", roundint=FALSE)


Following confusion matrix shows the errors of the prediction algorithm.

confusionMatrix(predictDT, as.factor(subValidating$classe))
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   A   B   C   D   E
##          A 993 245  47  96  58
##          B  19 339  26  16 129
##          C 277 175 700 224 267
##          D  93 103  16 438  87
##          E  13  87  66  30 360
## 
## Overall Statistics
##                                          
##                Accuracy : 0.5771         
##                  95% CI : (0.5631, 0.591)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.4659         
##                                          
##  Mcnemar's Test P-Value : < 2.2e-16      
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.7118  0.35722   0.8187  0.54478  0.39956
## Specificity            0.8729  0.95196   0.7671  0.92707  0.95104
## Pos Pred Value         0.6901  0.64083   0.4260  0.59430  0.64748
## Neg Pred Value         0.8840  0.86057   0.9525  0.91217  0.87557
## Prevalence             0.2845  0.19352   0.1743  0.16395  0.18373
## Detection Rate         0.2025  0.06913   0.1427  0.08931  0.07341
## Detection Prevalence   0.2934  0.10787   0.3350  0.15029  0.11338
## Balanced Accuracy      0.7924  0.65459   0.7929  0.73592  0.67530
We can see that the prediction accuracy is 50% which is not upto the desired level.

Random forest
# Fit model
modFitRF <- randomForest(as.factor(classe) ~ ., data=subTraining, method="class")
# Perform prediction
predictRF <- predict(modFitRF, subValidating, type = "class")
# Fit model
#modFitRF <- train(classe ~ ., data = subTraining, method = "rf", ntree=100)
# Perform prediction
#predictRF <- predict(modFitRF, subValidating)
Following confusion matrix shows the errors of the prediction algorithm.

confusionMatrix(predictRF, as.factor(subValidating$classe))
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    2    0    0    0
##          B    0  946    6    0    0
##          C    0    1  849    6    2
##          D    0    0    0  797    7
##          E    0    0    0    1  892
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9949          
##                  95% CI : (0.9925, 0.9967)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9936          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9968   0.9930   0.9913   0.9900
## Specificity            0.9994   0.9985   0.9978   0.9983   0.9998
## Pos Pred Value         0.9986   0.9937   0.9895   0.9913   0.9989
## Neg Pred Value         1.0000   0.9992   0.9985   0.9983   0.9978
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2845   0.1929   0.1731   0.1625   0.1819
## Detection Prevalence   0.2849   0.1941   0.1750   0.1639   0.1821
## Balanced Accuracy      0.9997   0.9977   0.9954   0.9948   0.9949
From the Confusion Matrix, we can clearly see that the prediction accuracy of Random Forest model is 99% which is satisfactory.

Prediction
Now we use it to predict the test set

predict(modFitRF, testing)
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
Conclusion
As we can we from the result, the random forest algorithem far outperforms the decision tree in terms of accuracy. We are getting 99.25% in sample accuracy, while the decision tree gives us only nearly 50% in sample accuracy
