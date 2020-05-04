library('RcmdrMisc')
library('rpart')
# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('ggthemes') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('mice')
# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('rlang') # data manipulation

# specific visualisation
#library('ggfortify') # visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('VIM') # NAs
library('plotly') # interactive
library('ggforce') # visualisation
library('corrplot')
library('MLmetrics') # gini metric

library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('caret')
library('RcmdrMisc')
library('rpart')



# Reading data
Original_data <- as.tibble(fread('/home/suraj/Desktop/R_project Dataset/porto-seguro-safe-driver-prediction/train.csv', na.strings=c("-1","-1.0")))



View(train)
dim(train)
str(train)  # Shows us the structure(attribute type) of data.
# Removing extra attribuute in training set
train = within(Original_data, {target = NULL}) 

# Analyzing missing Values
sum(is.na(train))
sapply(train, function(x)(sum(is.na(x)))) # Gives us count of NA's column wise 

list_na <- colnames(train)[ apply(train, 2, anyNA) ]
list_na  # Shows us missing values


#reading test dataset
test <- as.tibble(fread('/home/suraj/Desktop/R_project Dataset/porto-seguro-safe-driver-prediction/test.csv', na.strings=c("-1","-1.0")))

View(test)
dim(test)
str(test)
    
combined_dataset = rbind(train,test)
combined_dataset = within(combined_dataset, {id = NULL}) 
View(combined_dataset)
dim(combined_dataset)
  

# Analyzing missing Values of combined data
sum(is.na(combined_dataset))
sapply(combined_dataset, function(x)(sum(is.na(x)))) # Gives us count of NA's column wise 

list_na1 <- colnames(combined_dataset)[ apply(combined_dataset, 2, anyNA) ]
list_na1 # Shows us missing values

# Summarizing and Formatting our training set
summary(combined_dataset)
combined_dataset <- combined_dataset %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical))
str(combined_dataset) # Shows us the structure(attribute type) of data.

# Removing redundant attribute(ps_car_03_cat and ps_car_05_cat)....
combined_dataset = within(combined_dataset, {ps_car_03_cat = NULL})
combined_dataset = within(combined_dataset, {ps_car_05_cat = NULL}) 


imp1 = mice(combined_dataset[,1:20], m = 5, method = 'pmm')
complete1 = complete(imp1,3)

imp2 = mice(combined_dataset[,22:33], m = 5, method = 'pmm')
complete2 = complete(imp2,3)

partly_Imputed = cbind(complete1,complete2)

imp3 = mice(combined_dataset[,35:55], m = 5, method = 'pmm')
complete3 = complete(imp3,3)

fully_Imputed = cbind(partly_Imputed, complete3)

sum(is.na(fully_Imputed))

fully_Imputed <- as.tibble(fread('/home/suraj/Desktop/R_project Dataset/porto-seguro-safe-driver-prediction/Final_Imputed.csv'))
dataset = fully_Imputed[1:595212,]  #Taking out training dataset again from complete imputed data


dataset = cbind(dataset, Original_data[,2]) # Adding target variable again
dataset$target = as.factor(dataset$target)

sum(is.na(dataset))

# require(caTools)  # loading caTools library
# set.seed(123)   #  set seed to ensure you always have same random numbers generated
# sample = sample.split(dataset, SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
# train1 =subset(dataset,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
# test1=subset(dataset, sample==FALSE)
# library(caret)
# set.seed(3456)
# trainIndex <- createDataPartition(dataset$target, p = .75,
#                                   list = FALSE,
#                                   times = 1)
# train <- dataset[ trainIndex,]
# valid <- dataset[-trainIndex,]

train = dataset[1:476169,]   # Dividing into train and test set
valid = dataset[476170:595212,]

# Modelling using Naive Bayes
library('naivebayes')
model <- naive_bayes(target~., data = train)
prediction <- predict(model, training, type="class")# Showing how well model is predicting on training set
conf_mat = table(prediction, training$target,dnn = c("Prediction","Actual"))
confusionMatrix(conf_mat)

prediction <- predict(model, valid, type="class") # Predicting on test data0
conf_mat = table(prediction, valid$target, dnn = c("prediction", "actual")) # table showing actual and predicted values
confusionMatrix(conf_mat)
# find precision and recall in R

# Modelling using decision Tree
library(rpart)
model <- rpart(target ~ .,method="class", data = train)
prediction <- predict(model, valid, type="class")
cm = as.matrix(table(prediction, valid$target,dnn = c("Prediction","Actual")))

# Code to find Precision, Recall and F1 score
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall)
data.frame(precision, recall, f1)
# Result : Increased accuracy by 1.7 percent for ind_05_cat 
# Result : less than 0.5 increase in accuracy for car_14 
# Result : again no significant increase due to ps_reg_03
