library('RcmdrMisc')
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


# Reading data
train <- as.tibble(fread('/home/suraj/Desktop/R_project Dataset/porto-seguro-safe-driver-prediction/train.csv', na.strings=c("-1","-1.0")))
View(train)
dim(train)
str(train)  # Shows us the structure(attribute type) of data.

# Removing irrelevant attribuute(id)
train = within(train, {id = NULL}) 
dim(train)

# Summarizing and Formatting our training set
summary(train)
train <- train %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical)) %>%
  mutate(target = as.factor(target))
summary(train)  # Better Summary than before
numSummary(train[,c("ps_calc_01", "ps_calc_02", "ps_calc_03", "ps_calc_04", "ps_calc_05", "ps_calc_06", "ps_calc_07", "ps_calc_08", 
                          "ps_calc_09", "ps_calc_10", "ps_calc_11", "ps_calc_12", "ps_calc_13", "ps_calc_14", "ps_car_11", "ps_car_12", "ps_car_13", "ps_car_14", "ps_car_15", 
                          "ps_ind_01", "ps_ind_03", "ps_ind_14", "ps_ind_15", "ps_reg_01", "ps_reg_02", "ps_reg_03"), drop=FALSE], statistics=c("mean", "median", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))

# Analyzing missing Values
sum(is.na(train))
sapply(train, function(x)(sum(is.na(x)))) # Gives us count of NA's column wise 

list_na <- colnames(train)[ apply(train, 2, anyNA) ]
list_na  # Shows us missing values


# Visualising Missing Values....
train %>% select(which(colMeans(is.na(.)) > 0)) %>%aggr(prop = FALSE, combined = TRUE, numbers = TRUE, bars = FALSE, cex.axis = 0.7)

#The features ps_car_03_cat and ps_car_05_cat have the largest number of NAs. 
#They also share numerous instances where NAs occur in both of them for the same row.
#There are features that share a lot of NA rows with other features, for instance ps_reg_03. 
#Others are exclusive, like ps_car_12, or almost exclusive like ps_car_11 or *ps_car_02.cat.


# Percentage of missing values in Training Set
sum(is.na(train))/(nrow(train)*ncol(train))*100
#About 2.5% of values are missing in total in the train set

# Handling Missing Values.....

# Method 1:--ignoring the tuples which have one or more missing values
train_drop <- train%>%na.omit()
# Comparing the change in tuples for this case	
dim(train_drop)   # Data highly reduced, Info lost
dim(train)  

# Removing redundant attribute(ps_car_03_cat)....
train_modified = within(train, {ps_car_03_cat = NULL})
train_modified = within(train_modified, {ps_car_05_cat = NULL}) 
train_modified = within(train_modified, {ps_reg_03 = NULL})
#train_modified = within(train_modified, {ps_car_14 = NULL})

dim(train_modified)

# Imputing Missing Values with mean/median for numerical attributes...
summary(train_modified) # talk on relationship between mean and median
sum(is.na(train_modified$ps_car_11))
train_modified <- data.frame(sapply(train_modified, function(x) ifelse(is.na(x),mean(x, na.rm = TRUE),x)))
sum(is.na(train_modified$ps_car_11))

# Correlation between numeric attributes 
cor(train_modified[, c(2, 4, 15:16, 20:22, 33:51)])  # Observe ps_reg_2 and ps_reg_03, ps_car_12, ps_car_13
var = cor(train_modified)
# Visualising training set...

# Visualising Correlation between attributes
train_modified %>%
  
  select(-starts_with("ps_calc"), -ps_ind_10_bin, -ps_ind_11_bin, -ps_car_10_cat) %>%
  
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  
  mutate(target = as.integer(target)) %>%
  
  cor(use="complete.obs", method = "spearman") %>%
  
  corrplot(type="lower", tl.col = "black",  diag=FALSE)
# Most features appear to be primarily correlated with others in their group. We can see this by studying the upper right region near where the diagonal would be and comparing it to the lower left area of the plot.

# There is no obvious correlation with the target feature in the left-most column. This could be caused by the sparsity of the target == 1 values.

# Visualising outliers using Boxplots
boxplot(train_modified$ps_calc_01, train_modified$ps_calc_02, train_modified$ps_calc_03, train_modified$ps_calc_04, train_modified$ps_calc_05, train_modified$ps_calc_06, train_modified$ps_calc_07, train_modified$ps_calc_08, train_modified$ps_calc_09, train_modified$ps_calc_10, train_modified$ps_calc_11, train_modified$ps_calc_12, names = c("ps_calc_01", "ps_calc_02", "ps_calc_03", "ps_calc_04", "ps_calc_05", "ps_calc_06", "ps_calc_07", "ps_calc_08", "ps_calc_09", "ps_calc_10", "ps_calc_11", "ps_calc_12"), border = "black", main = "Multiple boxplots for comparision")
boxplot(train_modified$ps_calc_13, train_modified$ps_calc_14, train_modified$ps_car_11, train_modified$ps_car_12, train_modified$ps_car_13, train_modified$ps_car_14, train_modified$ps_car_15, train_modified$ps_ind_01, train_modified$ps_ind_03, train_modified$ps_ind_14, train_modified$ps_ind_15, train_modified$ps_reg_01, train_modified$ps_reg_02, train_modified$ps_reg_03, names = c("ps_calc_13", "ps_calc_14", "ps_car_11", "ps_car_12", "ps_car_13", "ps_car_14", "ps_car_15", "ps_ind_01", "ps_ind_03","ps_ind_14", "ps_ind_15", "ps_reg_01", "ps_reg_02", "ps_reg_03"), border = "black", main = "Multiple boxplots for comparision")

# Visualising Individual Features

# Binary Features Part 1
p1 <- train %>%
  ggplot(aes(ps_ind_06_bin, fill = ps_ind_06_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_ind_07_bin, fill = ps_ind_07_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_ind_08_bin, fill = ps_ind_08_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_ind_09_bin, fill = ps_ind_09_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_ind_10_bin, fill = ps_ind_10_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_ind_11_bin, fill = ps_ind_11_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p7 <- train %>%
  ggplot(aes(ps_ind_12_bin, fill = ps_ind_12_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_ind_13_bin, fill = ps_ind_13_bin)) +
  geom_bar() +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout=layout)

## Binary features part 2




p1 <- train %>%
  
  ggplot(aes(ps_ind_16_bin, fill = ps_ind_16_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p2 <- train %>%
  
  ggplot(aes(ps_ind_17_bin, fill = ps_ind_17_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p3 <- train %>%
  
  ggplot(aes(ps_ind_18_bin, fill = ps_ind_18_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p4 <- train %>%
  
  ggplot(aes(ps_calc_15_bin, fill = ps_calc_15_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p5 <- train %>%
  
  ggplot(aes(ps_calc_16_bin, fill = ps_calc_16_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p6 <- train %>%
  
  ggplot(aes(ps_calc_17_bin, fill = ps_calc_17_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p7 <- train %>%
  
  ggplot(aes(ps_calc_18_bin, fill = ps_calc_18_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p8 <- train %>%
  
  ggplot(aes(ps_calc_19_bin, fill = ps_calc_19_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p9 <- train %>%
  
  ggplot(aes(ps_calc_20_bin, fill = ps_calc_20_bin)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



layout <- matrix(c(1,2,3,4,5,6,7,8,9,9),2,5,byrow=TRUE)

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, layout=layout)

## Categorical features part 1



p1 <- train %>%
  
  ggplot(aes(ps_ind_02_cat, fill = ps_ind_02_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p2 <- train %>%
  
  ggplot(aes(ps_ind_04_cat, fill = ps_ind_04_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p3 <- train %>%
  
  ggplot(aes(ps_ind_05_cat, fill = ps_ind_05_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p4 <- train %>%
  
  ggplot(aes(ps_car_01_cat, fill = ps_car_01_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p5 <- train %>%
  
  ggplot(aes(ps_car_02_cat, fill = ps_car_02_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p6 <- train %>%
  
  ggplot(aes(ps_car_03_cat, fill = ps_car_03_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)

multiplot(p1, p2, p3, p4, p5, p6, layout=layout)





#We find that some categorical features have only very few levels, down to 2 levels (+ NA) for three of them. In others we have up to 11 levels, some of which are clearly dominating the (logarithmic) plots.





## Categorical features part 2



#Note again the logarithmic y-axes:

p1 <- train %>%
  
  ggplot(aes(ps_car_04_cat, fill = ps_car_04_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p2 <- train %>%
  
  ggplot(aes(ps_car_05_cat, fill = ps_car_05_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p3 <- train %>%
  
  ggplot(aes(ps_car_06_cat, fill = ps_car_06_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p4 <- train %>%
  
  ggplot(aes(ps_car_07_cat, fill = ps_car_07_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p5 <- train %>%
  
  ggplot(aes(ps_car_08_cat, fill = ps_car_08_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p6 <- train %>%
  
  ggplot(aes(ps_car_09_cat, fill = ps_car_09_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p7 <- train %>%
  
  ggplot(aes(ps_car_10_cat, fill = ps_car_10_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p8 <- train %>%
  
  ggplot(aes(ps_car_11_cat, fill = ps_car_11_cat)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



layout <- matrix(c(1,1,2,3,4,4,5,5,6,6,7,7,8,8,8,8),4,4,byrow=TRUE)

multiplot(p1, p2, p4, p3, p5, p6, p7, p8, layout=layout)





#We find that also here the number of levels is mostly low. Recall that the *car* features are related to the automobile itself. Feature "11" has lots of levels. This is the one number shared by a (supposedly) categorical and an integer feature. Maybe there has been a mixup in naming these features?
  
  
  
  
  
  ## Integer features part 1: "ind" and "car"
  
  
  
#  The integer features for the "ind" and "car" groups are best visualised in a categorical-style barplot, because their ranges are not very large. We are using log axes for some.



p1 <- train %>%
  
  mutate(ps_ind_01 = as.factor(ps_ind_01)) %>%
  
  ggplot(aes(ps_ind_01, fill = ps_ind_01)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p2 <- train %>%
  
  mutate(ps_ind_03 = as.factor(ps_ind_03)) %>%
  
  ggplot(aes(ps_ind_03, fill = ps_ind_03)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p3 <- train %>%
  
  mutate(ps_ind_14 = as.factor(ps_ind_14)) %>%
  
  ggplot(aes(ps_ind_14, fill = ps_ind_14)) +
  
  geom_bar() +
  
  scale_y_log10() +
  
  theme(legend.position = "none")



p4 <- train %>%
  
  mutate(ps_ind_15 = as.factor(ps_ind_15)) %>%
  
  ggplot(aes(ps_ind_15, fill = ps_ind_15)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p5 <- train %>%
  
  mutate(ps_car_11 = as.factor(ps_car_11)) %>%
  
  ggplot(aes(ps_car_11, fill = ps_car_11)) +
  
  geom_bar() +
  
  theme(legend.position = "none")





layout <- matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)

multiplot(p1, p2, p3, p4, p5, layout=layout)





#We find that again there are large differences in frequencies, in particular for *ps\_ind\_14* and *ps\_car\_11* where "0" and "3" are the dominating values, respectively. 





## Integer features part 2: "calc"



#Whereas most of the "calc" integer features can still be visualised best using barplots, for three of them a histogram is a better choice:
  

p1 <- train %>%
  
  mutate(ps_calc_04 = as.factor(ps_calc_04)) %>%
  
  ggplot(aes(ps_calc_04, fill = ps_calc_04)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p2 <- train %>%
  
  mutate(ps_calc_05 = as.factor(ps_calc_05)) %>%
  
  ggplot(aes(ps_calc_05, fill = ps_calc_05)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p3 <- train %>%
  
  mutate(ps_calc_06 = as.factor(ps_calc_06)) %>%
  
  ggplot(aes(ps_calc_06, fill = ps_calc_06)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p4 <- train %>%
  
  mutate(ps_calc_07 = as.factor(ps_calc_07)) %>%
  
  ggplot(aes(ps_calc_07, fill = ps_calc_07)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p5 <- train %>%
  
  mutate(ps_calc_08 = as.factor(ps_calc_08)) %>%
  
  ggplot(aes(ps_calc_08, fill = ps_calc_08)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p6 <- train %>%
  
  mutate(ps_calc_09 = as.factor(ps_calc_09)) %>%
  
  ggplot(aes(ps_calc_09, fill = ps_calc_09)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p7 <- train %>%
  
  ggplot(aes(ps_calc_10, fill = ps_calc_10)) +
  
  geom_histogram(fill = "blue", binwidth = 1) +
  
  theme(legend.position = "none")



p8 <- train %>%
  
  ggplot(aes(ps_calc_11, fill = ps_calc_11)) +
  
  geom_histogram(fill = "blue", binwidth = 1) +
  
  theme(legend.position = "none")



p9 <- train %>%
  
  mutate(ps_calc_12 = as.factor(ps_calc_12)) %>%
  
  ggplot(aes(ps_calc_12, fill = ps_calc_12)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p10 <- train %>%
  
  mutate(ps_calc_13 = as.factor(ps_calc_13)) %>%
  
  ggplot(aes(ps_calc_13, fill = ps_calc_13)) +
  
  geom_bar() +
  
  theme(legend.position = "none")



p11 <- train %>%
  
  ggplot(aes(ps_calc_14, fill = ps_calc_14)) +
  
  geom_histogram(fill = "blue", binwidth = 1) +
  
  theme(legend.position = "none")



layout <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,11),3,4,byrow=TRUE)

multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, layout=layout)




#We find that the histogram features "10", "11", and "14" have close to normal looking distributions with possibly more pronounced tails towards larger values. The other features are not far from a normal or log-normal distribution either and consequently display significant ranges in frequency.





## Float features part 1: "reg" and "calc"



#For the floating point features we choose histograms to get a first impression of their distributions:
  
  
  

p1 <- train %>%
  
  ggplot(aes(ps_reg_01, fill = ps_reg_01)) +
  
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  
  theme(legend.position = "none")



p2 <- train %>%
  
  ggplot(aes(ps_reg_02, fill = ps_reg_02)) +
  
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  
  theme(legend.position = "none")



p3 <- train %>%
  
  ggplot(aes(ps_reg_03, fill = ps_reg_03)) +
  
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  
  theme(legend.position = "none")



p4 <- train %>%
  
  ggplot(aes(ps_calc_01, fill = ps_calc_01)) +
  
  geom_histogram(fill = "blue", binwidth = 0.1) +
  
  theme(legend.position = "none")



p5 <- train %>%
  
  ggplot(aes(ps_calc_02, fill = ps_calc_02)) +
  
  geom_histogram(fill = "blue", binwidth = 0.1) +
  
  theme(legend.position = "none")



p6 <- train %>%
  
  ggplot(aes(ps_calc_03, fill = ps_calc_03)) +
  
  geom_histogram(fill = "blue", binwidth = 0.1) +
  
  theme(legend.position = "none")







layout <- matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)

multiplot(p1, p2, p3, p4, p5, p6, layout=layout)




#We find that while the (green) "reg" features show distributions that are clearly skewed toward a prominent peak, the (blue) "calc" features appear to be pretty uniformly distributed.





## Float features part 2: "car"



#Also the second part of these features will be visualised using histograms:
  
  
  

p1 <- train %>%
  
  ggplot(aes(ps_car_12, fill = ps_car_12)) +
  
  geom_histogram(fill = "red", binwidth = 0.05) +
  
  theme(legend.position = "none")



p2 <- train %>%
  
  ggplot(aes(ps_car_13, fill = ps_car_13)) +
  
  geom_histogram(fill = "red", binwidth = 0.1) +
  
  theme(legend.position = "none")



p3 <- train %>%
  
  ggplot(aes(ps_car_14, fill = ps_car_14)) +
  
  geom_histogram(fill = "red", binwidth = 0.01) +
  
  theme(legend.position = "none")



p4 <- train %>%
  
  ggplot(aes(ps_car_15, fill = ps_car_15)) +
  
  geom_histogram(fill = "red", binwidth = 0.1) +
  
  theme(legend.position = "none")



layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)

multiplot(p1, p2, p3, p4, layout=layout)




#We find that the two features on the left show interesting sub-structure in their distributions, while *ps\_car\_15* appears to take only quite distinct values until after `ps_car_15 == 3`when the gaps decrease notably.





## Target variable



#Finally, this is what it is all about: whether a claim has been filed ("1") or not ("0"):
  
  
  

train %>%
  
  ggplot(aes(target, fill = target)) +
  
  geom_bar() +
  
  theme(legend.position = "none")





#We find:
#  - The majority of cases has no filed claim:
#With less than 4% of policy holders filing a claim the problem is heavily imbalanced.

# Simple Data sampling without replacement
data.sample = train_modified[sample(nrow(train_modified),10),]

# Stratified Data sampling
set.seed(1)

data.stratified <- train_modified %>%
  
  group_by(train_modified$target) %>%
  
  sample_n(10)
fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ), na.rm = TRUE)
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}
fun( train_modified )
train_modified = squish(train_modified, quantile(train_modified, c(.05, .95), , na.rm = TRUE))
View(train_modified)

train_modified1 = mice(train_modified[,3:5], m = 5, method = NULL)
train_modified1 = mice(train_modified[,6:7], m = 5, method = NULL)
train_modified1 = mice(train_modified[,23:24], m = 5, method = NULL)
train_modified1 = mice(train_modified[,29:30], m = 5, method = NULL)
train_modified1 = mice(train_modified[,31:36], m = 5, method = NULL)
df <- data.frame(matrix(unlist(train_modified1[["data"]]), nrow=length(train_modified1[["data"]]), byrow=T))
