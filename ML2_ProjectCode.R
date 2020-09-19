
library(ggplot2)
library(gridExtra)
library(clv)
library(mvtnorm) 
library(cluster)  
library(stats)
library(corrplot)
library(mice)
library(reshape2)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(knitr)
library(rmarkdown)
library(qcc)
library(nnet)
library(NeuralNetTools)
library(devtools)
library(gtable)
library(stringr)
library(tibble)
library(rpart)
library(rpart.plot)
library(clue)
library(flexclust)
library(cclust)
library(class)
library(DeducerExtras)
library(writexl)
library(e1071)
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library(GGally)
library(randomForest)
library(pROC)


#-----------------------------------------------------------

filePath=choose.files() 
DataSet<-read.csv(filePath,header=TRUE)


#complete unknown data

DataSet$native.country <- as.character(DataSet$native.country)
DataSet$native.country[DataSet$native.country!=" United-States"] <- " Other"
DataSet$native.country <- as.factor(DataSet$native.country)

DataSet$workclass[DataSet$workclass==" Never-worked"] <- " ?"
DataSet$workclass[DataSet$workclass==" Without-pay"] <- " ?"


DataSet[DataSet==" ?"] <- NA
DataSet <- mice(data=DataSet, m=5, method="pmm", maxit=1, seed=50)
DataSet <- complete(DataSet)


# --------- Data Understanding------------------------------

Y_apriori_Over <- length(which(DataSet$Y==1))/length(DataSet$Y)
Y_apriori_Under <- 1-Y_apriori_Over


DataSet$Y_Yes_No[DataSet$Y == " >50K"] <- "yes"
DataSet$Y_Yes_No[DataSet$Y == " <=50K"] <- "no"
DataSet$Y_Yes_No <- as.factor(DataSet$Y_Yes_No)

colnames(DataSet)[which(names(DataSet) == "marital.status")] <- "marital"

hist(DataSet$age, freq=FALSE)
age_apriori_under_25 <- as.numeric(sqldf("select count (age) from DataSet where age <=25" ))/length(DataSet$age)
age_apriori_26_30 <- as.numeric(sqldf("select count (age) from DataSet where age between 26 and 30" ))/length(DataSet$age)
age_apriori_31_40 <- as.numeric(sqldf("select count (age) from DataSet where age between 31 and 40" ))/length(DataSet$age)
age_apriori_41_55 <- as.numeric(sqldf("select count (age) from DataSet where age between 41 and 55" ))/length(DataSet$age)
age_apriori_above_56 <- as.numeric(sqldf("select count (age) from DataSet where age >=56" ))/length(DataSet$age)




barplot(prop.table(table(DataSet$workclass)))
Workclass_apriori_Private <- length(which(DataSet$workclass==" Private"))/length(DataSet$workclass)


hist(DataSet$fnlwgt, freq=FALSE)
fnlwgt_apriori_under_90k <- as.numeric(sqldf("select count (fnlwgt) from DataSet where fnlwgt <=90000" ))/length(DataSet$fnlwgt)
fnlwgt_apriori_90k_170k <- as.numeric(sqldf("select count (fnlwgt) from DataSet where fnlwgt between 90001 and 170000" ))/length(DataSet$fnlwgt)
fnlwgt_apriori_170k_250k <- as.numeric(sqldf("select count (fnlwgt) from DataSet where fnlwgt between 170001 and 250000" ))/length(DataSet$fnlwgt)
fnlwgt_apriori_above_250k <- as.numeric(sqldf("select count (fnlwgt) from DataSet where fnlwgt >=250001" ))/length(DataSet$fnlwgt)


barplot(prop.table(table(DataSet$education)))
education_apriori_HS_Grad <- length(which(DataSet$education==" HS-grad"))/length(DataSet$education)
education_apriori_Some_College <- length(which(DataSet$education==" Some-college"))/length(DataSet$education)
education_apriori_Bachelors <- length(which(DataSet$education==" Bachelors"))/length(DataSet$education)


hist(DataSet$education.num, freq=FALSE)
educationNum_apriori_under_9 <- as.numeric(sqldf("select count ([education.num]) from DataSet where [education.num] <=9" ))/length(DataSet$education.num)
educationNum_apriori_10_11 <- as.numeric(sqldf("select count ([education.num]) from DataSet where [education.num] between 10 and 11" ))/length(DataSet$education.num)
educationNum_above_12 <- as.numeric(sqldf("select count ([education.num]) from DataSet where [education.num] >=12" ))/length(DataSet$education.num)


barplot(prop.table(table(DataSet$marital)))
maritalStatus_apriori_Divorced <- length(which(DataSet$marital==" Divorced"))/length(DataSet$marital)
maritalStatus_apriori_MarriedCivSpouse <- length(which(DataSet$marital==" Married-civ-spouse"))/length(DataSet$marital)
maritalStatus_apriori_NeverMarried <- length(which(DataSet$marital==" Never-married"))/length(DataSet$marital)


barplot(prop.table(table(DataSet$occupation)))



barplot(prop.table(table(DataSet$relationship)))
relationship_apriori_Husband <- length(which(DataSet$relationship==" Husband"))/length(DataSet$relationship)
relationship_apriori_NotInFamily <- length(which(DataSet$relationship==" Not-in-family"))/length(DataSet$relationship)
relationship_apriori_OtherRelative <- length(which(DataSet$relationship==" Other-relative"))/length(DataSet$relationship)
relationship_apriori_OwnChild <- length(which(DataSet$relationship==" Own-child"))/length(DataSet$relationship)
relationship_apriori_UnMarried <- length(which(DataSet$relationship==" Unmarried"))/length(DataSet$relationship)
relationship_apriori_Wife <- length(which(DataSet$relationship==" Wife"))/length(DataSet$relationship)



barplot(prop.table(table(DataSet$race)))
race_apriori_White <- length(which(DataSet$race==" White"))/length(DataSet$race)
race_apriori_Black <- length(which(DataSet$race==" Black"))/length(DataSet$race)


barplot(prop.table(table(DataSet$sex)))
sex_apriori_Male <- length(which(DataSet$sex==" Male"))/length(DataSet$sex)
sex_apriori_Female <- length(which(DataSet$sex==" Female"))/length(DataSet$sex)



DataSet$capital.gain[DataSet$capital.gai>0] <- "1"
DataSet$capital.gain[DataSet$capital.gai==0] <- "0"
barplot(prop.table(table(DataSet$capital.gain)))
capital_gain_apriori_Over <- length(which(DataSet$capital.gain=="1"))/length(DataSet$capital.gain)
capital_gain_apriori_Zero <- length(which(DataSet$capital.gain=="0"))/length(DataSet$capital.gain)


DataSet$capital.loss[DataSet$capital.loss>0] <- "1"
DataSet$capital.loss[DataSet$capital.loss==0] <- "0"
barplot(prop.table(table(DataSet$capital.loss)))
capital_loss_apriori_Over <- length(which(DataSet$capital.loss=="1"))/length(DataSet$capital.loss)
capital_loss_apriori_Zero <- length(which(DataSet$capital.loss=="0"))/length(DataSet$capital.loss)



hist(DataSet$hours.per.week, freq=FALSE)
hours_pre_week_apriori_under_35 <- as.numeric(sqldf("select count ([hours.per.week]) from DataSet where [hours.per.week] <=35" ))/length(DataSet$hours.per.week)
hours_pre_week_apriori_36_45 <- as.numeric(sqldf("select count ([hours.per.week]) from DataSet where [hours.per.week] between 36 and 45" ))/length(DataSet$hours.per.week)
hours_pre_week_apriori_above_46 <- as.numeric(sqldf("select count ([hours.per.week]) from DataSet where [hours.per.week] >=46" ))/length(DataSet$hours.per.week)



barplot(prop.table(table(DataSet$native.country)))
native_country_apriori_US <- length(which(DataSet$native.country==" United-States"))/length(DataSet$native.country)


#----------------

plot(DataSet$age, DataSet$Y_Yes_No, xlab="Age", ylab="Y")
DataSet$grouped_age <- findInterval(DataSet$age,c(0,26,31,41,56,120))
plot(as.factor(DataSet$grouped_age), DataSet$Y_Yes_No, xlab="Age", ylab="Y")
plot(DataSet$workclass, DataSet$Y_Yes_No, xlab="WorkClass", ylab="Y")
plot(DataSet$fnlwgt, DataSet$Y_Yes_No, xlab="fnlwgt", ylab="Y")
plot(as.factor(DataSet$education.num), DataSet$Y_Yes_No, xlab="Education Num", ylab="Y")
chisq.test(as.factor(DataSet$education.num), DataSet$Y_Yes_No)
plot(DataSet$marital, DataSet$Y_Yes_No, xlab="Marital Status", ylab="Y")
plot(DataSet$occupation, DataSet$Y_Yes_No, xlab="Occupation", ylab="Y")
plot(DataSet$race, DataSet$Y_Yes_No, xlab="Race", ylab="Y")
plot(DataSet$sex, DataSet$Y_Yes_No, xlab="Sex", ylab="Y")
plot(as.factor(DataSet$capital.gain), DataSet$Y_Yes_No, xlab="Capital Gain", ylab="Y")
plot(as.factor(DataSet$capital.loss), DataSet$Y_Yes_No, xlab="Capital Loss", ylab="Y")
DataSet$grouped_hoursPerWeek <- findInterval(DataSet$hours.per.week,c(0,36,46,120))
plot(as.factor(DataSet$grouped_hoursPerWeek), DataSet$Y_Yes_No, xlab="Hours per Week", ylab="Y")
plot(as.factor(DataSet$native.country), DataSet$Y_Yes_No, xlab="Native Country", ylab="Y")

plot(DataSet$education.num, DataSet$age, xlab="Education", ylab="Age")

plot(DataSet$marital,DataSet$age, xlab="Marital Status", ylab="Age")

#------------------

apriori_workclass_unknown <- length(which(DataSet$workclass==" ?"))/(length(DataSet$workclass))
apriori_occupation_unknown <- length(which(DataSet$occupation==" ?"))/(length(DataSet$occupation))
apriori_native.country_unknown <- length(which(DataSet$native.country==" ?"))/(length(DataSet$native.country))


#------------------

DataSet$race[DataSet$race==" Asian-Pac-Islander"] <- " Other"
DataSet$race[DataSet$race==" Amer-Indian-Eskimo"] <- " Other"

DataSet$marital <- as.character(DataSet$marital)
DataSet$marital[DataSet$marital==" Divorced"] <- " Not_Married"
DataSet$marital[DataSet$marital==" Never-married"] <- " Not_Married"
DataSet$marital[DataSet$marital==" Widowed"] <- " Not_Married"
DataSet$marital[DataSet$marital==" Separated"] <- " Not_Married"
DataSet$marital <- as.factor(DataSet$marital)


#-------------------

#------------------Data preparation-----------------------------------

Y <- DataSet$Y_Yes_No
deletion <- c(4,8,13,15,16,17)
DataSet <- DataSet[,-deletion]
DataSet <- as.data.frame(cbind(DataSet,Y))

set.seed(123)
Rows_Numbers_for_Test_Set <- sample(1:nrow(DataSet),0.2*nrow(DataSet),replace = FALSE)
Train_Set <- DataSet[-Rows_Numbers_for_Test_Set,]
Test_Set <-DataSet[Rows_Numbers_for_Test_Set,]

# K-fold configuration
n.folds <- 5
samples <- sample(1:n.folds, nrow(Train_Set), replace=T)
Train_and_Val <- cbind(Train_Set, samples)
k_fold_sets <- list()
for(fold in 1:n.folds){
  k_fold_sets[[fold]] <- Train_and_Val[Train_and_Val$samples==fold,1:ncol(Train_and_Val)-1]
}

k_folds_Train_sets <- list()
k_folds_Valid_sets <- list()

k_folds_Train_sets[[1]] <- rbind(k_fold_sets[[2]],k_fold_sets[[3]],k_fold_sets[[4]],k_fold_sets[[5]])
k_folds_Train_sets[[2]] <- rbind(k_fold_sets[[1]],k_fold_sets[[3]],k_fold_sets[[4]],k_fold_sets[[5]])
k_folds_Train_sets[[3]] <- rbind(k_fold_sets[[1]],k_fold_sets[[2]],k_fold_sets[[4]],k_fold_sets[[5]])
k_folds_Train_sets[[4]] <- rbind(k_fold_sets[[1]],k_fold_sets[[2]],k_fold_sets[[3]],k_fold_sets[[5]])
k_folds_Train_sets[[5]] <- rbind(k_fold_sets[[1]],k_fold_sets[[2]],k_fold_sets[[3]],k_fold_sets[[4]])

k_folds_Valid_sets[[1]] <- k_fold_sets[[1]]
k_folds_Valid_sets[[2]] <- k_fold_sets[[2]]
k_folds_Valid_sets[[3]] <- k_fold_sets[[3]]
k_folds_Valid_sets[[4]] <- k_fold_sets[[4]]
k_folds_Valid_sets[[5]] <- k_fold_sets[[5]]


#--------------------------------------------------

# neural networks

#-------------------------------------------------- 

net_DataSet <- DataSet

net_DataSet$workclass <- as.character(net_DataSet$workclass)
net_DataSet$workclass[net_DataSet$workclass==" State-gov"] <- " Gov"
net_DataSet$workclass[net_DataSet$workclass==" Federal-gov"] <- " Gov"
net_DataSet$workclass[net_DataSet$workclass==" Local-gov"] <- " Gov"
net_DataSet$workclass[net_DataSet$workclass==" Self-emp-not-inc"] <- " Self"
net_DataSet$workclass[net_DataSet$workclass==" Self-emp-inc"] <- " Self"
net_DataSet$workclass[net_DataSet$workclass==" Never-worked"] <- " Private"
net_DataSet$workclass[net_DataSet$workclass==" Without-pay"] <- " Private"
net_DataSet$workclass <- as.factor(net_DataSet$workclass)

# Dummy variables

#workclass
net_DataSet$workclass_D1 <- ifelse(net_DataSet$workclass==" Self",1,-1)
net_DataSet$workclass_D2 <- ifelse(net_DataSet$workclass==" Gov",1,-1)
net_DataSet$workclass_D1 <- as.factor(net_DataSet$workclass_D1)
net_DataSet$workclass_D2 <- as.factor(net_DataSet$workclass_D2)

#occupation
net_DataSet$occupation_D1 <- ifelse(net_DataSet$occupation==" Exec-managerial",1,-1)
net_DataSet$occupation_D2 <- ifelse(net_DataSet$occupation==" Handlers-cleaners",1,-1)
net_DataSet$occupation_D3 <- ifelse(net_DataSet$occupation==" Prof-specialty",1,-1)
net_DataSet$occupation_D4 <- ifelse(net_DataSet$occupation==" Other-service",1,-1)
net_DataSet$occupation_D5 <- ifelse(net_DataSet$occupation==" Sales",1,-1)
net_DataSet$occupation_D6 <- ifelse(net_DataSet$occupation==" Craft-repair",1,-1)
net_DataSet$occupation_D7 <- ifelse(net_DataSet$occupation==" Transport-moving",1,-1)
net_DataSet$occupation_D8 <- ifelse(net_DataSet$occupation==" Farming-fishing",1,-1)
net_DataSet$occupation_D9 <- ifelse(net_DataSet$occupation==" Machine-op-inspct",1,-1)
net_DataSet$occupation_D10 <- ifelse(net_DataSet$occupation==" Tech-support",1,-1)
net_DataSet$occupation_D11 <- ifelse(net_DataSet$occupation==" Protective-serv",1,-1)
net_DataSet$occupation_D12 <- ifelse(net_DataSet$occupation==" Armed-Forces",1,-1)
net_DataSet$occupation_D13 <- ifelse(net_DataSet$occupation==" Priv-house-serv",1,-1)
net_DataSet$occupation_D1 <- as.factor(net_DataSet$occupation_D1)
net_DataSet$occupation_D2 <- as.factor(net_DataSet$occupation_D2)
net_DataSet$occupation_D3 <- as.factor(net_DataSet$occupation_D3)
net_DataSet$occupation_D4 <- as.factor(net_DataSet$occupation_D4)
net_DataSet$occupation_D5 <- as.factor(net_DataSet$occupation_D5)
net_DataSet$occupation_D6 <- as.factor(net_DataSet$occupation_D6)
net_DataSet$occupation_D7 <- as.factor(net_DataSet$occupation_D7)
net_DataSet$occupation_D8 <- as.factor(net_DataSet$occupation_D8)
net_DataSet$occupation_D9 <- as.factor(net_DataSet$occupation_D9)
net_DataSet$occupation_D10 <- as.factor(net_DataSet$occupation_D10)
net_DataSet$occupation_D11 <- as.factor(net_DataSet$occupation_D11)
net_DataSet$occupation_D12 <- as.factor(net_DataSet$occupation_D12)
net_DataSet$occupation_D13 <- as.factor(net_DataSet$occupation_D13)

#sex
net_DataSet$sex_D1 <- ifelse(net_DataSet$sex==" Male",1,-1)
net_DataSet$sex_D1 <- as.factor(net_DataSet$sex_D1)

#race
net_DataSet$race_D1 <- ifelse(net_DataSet$race==" Black",1,-1)
net_DataSet$race_D2 <- ifelse(net_DataSet$race==" Other",1,-1)
net_DataSet$race_D1 <- as.factor(net_DataSet$race_D1)
net_DataSet$race_D2 <- as.factor(net_DataSet$race_D2)

#capital-gain
net_DataSet$capitalGain_D1 <- ifelse(net_DataSet$capital.gain==" 1",1,-1)
net_DataSet$capitalGain_D1 <- as.factor(net_DataSet$capitalGain_D1)

#capital-loss
net_DataSet$capitalLoss_D1 <- ifelse(net_DataSet$capital.loss==" 1",1,-1)
net_DataSet$capitalLoss_D1 <- as.factor(net_DataSet$capitalLoss_D1)

#marital
net_DataSet$marital_D1 <- ifelse(net_DataSet$marital==" Married-spouse-absent",1,-1)
net_DataSet$marital_D2 <- ifelse(net_DataSet$marital==" Not_Married",1,-1)
net_DataSet$marital_D1 <- as.factor(net_DataSet$marital_D1)
net_DataSet$marital_D2 <- as.factor(net_DataSet$marital_D2)

#hours per week
net_DataSet$grouped_hoursPerWeek_D1 <- ifelse(net_DataSet$grouped_hoursPerWeek==" 2",1,-1)
net_DataSet$grouped_hoursPerWeek_D2 <- ifelse(net_DataSet$grouped_hoursPerWeek==" 3",1,-1)
net_DataSet$grouped_hoursPerWeek_D1 <- as.factor(net_DataSet$grouped_hoursPerWeek_D1)
net_DataSet$grouped_hoursPerWeek_D2 <- as.factor(net_DataSet$grouped_hoursPerWeek_D2)

#native country
net_DataSet$nativeCountry_D1 <- ifelse(net_DataSet$native.country==" United-States",1,-1)
net_DataSet$nativeCountry_D1 <- as.factor(net_DataSet$nativeCountry_D1)




##scale

#age 
net_DataSet$age <- scale(net_DataSet$age)

#fnlwgt
net_DataSet$fnlwgt <- scale(net_DataSet$fnlwgt)

#education num
net_DataSet$education.num <- scale(net_DataSet$education.num)

Y <- net_DataSet$Y
deletion <- c(2,5,6,7,8,9,10,11,12,13)
net_DataSet <- net_DataSet[,-deletion]
net_DataSet <- as.data.frame(cbind(net_DataSet,Y))


net_Train_Set <- net_DataSet[-Rows_Numbers_for_Test_Set,]
net_Test_Set <-net_DataSet[Rows_Numbers_for_Test_Set,]

# K-fold configuration
n.folds <- 5
samples <- sample(1:n.folds, nrow(net_Train_Set), replace=T)
net_Train_and_Val <- cbind(net_Train_Set, samples)
net_k_fold_sets <- list()
for(fold in 1:n.folds){
  net_k_fold_sets[[fold]] <- net_Train_and_Val[net_Train_and_Val$samples==fold,1:ncol(net_Train_and_Val)-1]
}

net_k_folds_Train_sets <- list()
net_k_folds_Valid_sets <- list()

net_k_folds_Train_sets[[1]] <- rbind(net_k_fold_sets[[2]],net_k_fold_sets[[3]],net_k_fold_sets[[4]],net_k_fold_sets[[5]])
net_k_folds_Train_sets[[2]] <- rbind(net_k_fold_sets[[1]],net_k_fold_sets[[3]],net_k_fold_sets[[4]],net_k_fold_sets[[5]])
net_k_folds_Train_sets[[3]] <- rbind(net_k_fold_sets[[1]],net_k_fold_sets[[2]],net_k_fold_sets[[4]],net_k_fold_sets[[5]])
net_k_folds_Train_sets[[4]] <- rbind(net_k_fold_sets[[1]],net_k_fold_sets[[2]],net_k_fold_sets[[3]],net_k_fold_sets[[5]])
net_k_folds_Train_sets[[5]] <- rbind(net_k_fold_sets[[1]],net_k_fold_sets[[2]],net_k_fold_sets[[3]],net_k_fold_sets[[4]])

net_k_folds_Valid_sets[[1]] <- net_k_fold_sets[[1]]
net_k_folds_Valid_sets[[2]] <- net_k_fold_sets[[2]]
net_k_folds_Valid_sets[[3]] <- net_k_fold_sets[[3]]
net_k_folds_Valid_sets[[4]] <- net_k_fold_sets[[4]]
net_k_folds_Valid_sets[[5]] <- net_k_fold_sets[[5]]





#default net

set.seed(123)
nn <- nnet(x=net_Train_Set[,1:28], y=class.ind(net_Train_Set[,29]), size=1, linout=FALSE, softmax=T) 
preds_nn_train <- factor(predict(nn, newdata=net_Train_Set[,1:28], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=net_Test_Set[,1:28], type='class')) # prediction for test set

nn_default_train_percision <- sum(preds_nn_train==net_Train_Set$Y)/nrow(net_Train_Set)
nn_default_test_percision <- sum(preds_nn_test==net_Test_Set$Y)/nrow(net_Test_Set)

plotnet(nn)
summary(nn)
olden(nn)

# find the num of neurons


set.seed(123)
#nnum <- seq(1,101,5)
nnum <- c(1,2,3,4,5,6,7,8,9,10)
acc  <- matrix(0,length(nnum),1)
i    <- 1 
for(neurons in nnum){
  temp_total_precision <- 0
  for (fold in 1:5){
    
    nn <- nnet(x=net_k_folds_Train_sets[[fold]][,1:28], y=class.ind(net_k_folds_Train_sets[[fold]]$Y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000) 
    
    preds_nn_val <- factor(predict(nn, newdata=net_k_folds_Valid_sets[[fold]][,1:28], type='class'))
    preds_nn_val <- factor(preds_nn_val, levels=levels(net_k_folds_Valid_sets[[fold]]$Y))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==net_k_folds_Valid_sets[[fold]]$Y)/nrow(net_k_folds_Valid_sets[[fold]]))
  }
  acc[i] <-  temp_total_precision/5
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=acc)) + geom_line(aes(x,y), color='purple') + 
  xlab("Number of Neurons") + ylab("Validation Accuracy")

print(acc)

neurons <- nnum[which.max(acc)] # find the size with maximum accuracy
neurons <- 5
valid_percision_after_neurons_num <- max(acc)
set.seed(123)
nn <- nnet(x=net_Train_Set[,1:28], y=class.ind(net_Train_Set[,29]), size=neurons, linout=FALSE, softmax=T) 
preds_nn_train <- factor(predict(nn, newdata=net_Train_Set[,1:28], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=net_Test_Set[,1:28], type='class')) # prediction for test set
nn_after_neuronsNum_train_percision <- sum(preds_nn_train==net_Train_Set$Y)/nrow(net_Train_Set)
nn_after_neuronsNum_test_percision <- sum(preds_nn_test==net_Test_Set$Y)/nrow(net_Test_Set)
plotnet(nn)

# weight decay

set.seed(123)
nnum <- seq(0,1,0.05)
decay_val  <- matrix(0,length(nnum),1)
i    <- 1 
for(n_decay in nnum){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    nn <- nnet(x=net_k_folds_Train_sets[[fold]][,1:28], y=class.ind(net_k_folds_Train_sets[[fold]]$Y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, decay = n_decay ) 
    
    preds_nn_val <- factor(predict(nn, newdata=net_k_folds_Valid_sets[[fold]][,1:28], type='class'))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==net_k_folds_Valid_sets[[fold]]$Y)/nrow(net_k_folds_Valid_sets[[fold]]))
  }
  decay_val[i] <-  temp_total_precision/n.folds
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=decay_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("Weight Decay") + ylab("Validation Accuracy")

weight_decay <- nnum[which.max(decay_val)]
weight_decay <- 0.2
nn_Configure1_valid_percision <- max(decay_val)

set.seed(123)
nn <- nnet(x=net_Train_Set[,1:28], y=class.ind(net_Train_Set[,29]), size=neurons, linout=FALSE, softmax=T, decay = weight_decay ) # train
preds_nn_train <- factor(predict(nn, newdata=net_Train_Set[,1:28], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=net_Test_Set[,1:28], type='class')) # prediction for test set
nn_Configure1_train_percision <- sum(preds_nn_train==net_Train_Set$Y)/nrow(net_Train_Set)
nn_Configure1_test_percision <- sum(preds_nn_test==net_Test_Set$Y)/nrow(net_Test_Set)
plotnet(nn)


# max iteration number

set.seed(123)
nnum <- seq(10,300,10)
iteration_val  <- matrix(0,length(nnum),1)
i    <- 1 
for(n_iteration in nnum){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    nn <- nnet(x=net_k_folds_Train_sets[[fold]][,1:28], y=class.ind(net_k_folds_Train_sets[[fold]]$Y), size=neurons, linout=FALSE, softmax=T, MaxNWts=100000, decay = weight_decay, maxit = n_iteration ) 
    
    preds_nn_val <- factor(predict(nn, newdata=net_k_folds_Valid_sets[[fold]][,1:28], type='class'))
    temp_total_precision  <- temp_total_precision+(sum(preds_nn_val==net_k_folds_Valid_sets[[fold]]$Y)/nrow(net_k_folds_Valid_sets[[fold]]))
  }
  iteration_val[i] <-  temp_total_precision/n.folds
  i              <- i + 1
}

ggplot(data.frame(x=nnum, y=iteration_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("Iteration Number") + ylab("Validation Accuracy")

iteration_number <- nnum[which.max(iteration_val)]
iteration_number <- 200
nn_Configure2_valid_percision <- max(iteration_val)


set.seed(123)
nn <- nnet(x=net_Train_Set[,1:28], y=class.ind(net_Train_Set[,29]), size=neurons, linout=FALSE, softmax=T, decay = weight_decay, maxit = iteration_number ) # train
preds_nn_train <- factor(predict(nn, newdata=net_Train_Set[,1:28], type='class')) # prediction for train set
preds_nn_test  <- factor(predict(nn, newdata=net_Test_Set[,1:28], type='class')) # prediction for test set
nn_Configure2_train_percision <- sum(preds_nn_train==net_Train_Set$Y)/nrow(net_Train_Set)
nn_Configure2_test_percision <- sum(preds_nn_test==net_Test_Set$Y)/nrow(net_Test_Set)
plotnet(nn)
preds_0_1_net <- as.numeric(preds_nn_test)
preds_0_1_net <- preds_0_1_net-1

# final net
set.seed(123)
nn <- nnet(x=net_Train_Set[,1:28], y=class.ind(net_Train_Set[,29]), size=5, linout=FALSE, softmax=T, decay = 0.2, maxit = 200 ) 
final_net_train <- factor(predict(nn, newdata=net_Train_Set[,1:28], type='class')) # prediction for train set
final_net_test  <- factor(predict(nn, newdata=net_Test_Set[,1:28], type='class')) # prediction for test set
final_net_train_percision <- sum(final_net_train==net_Train_Set$Y)/nrow(net_Train_Set)
final_net_test_percision <- sum(final_net_test==net_Test_Set$Y)/nrow(net_Test_Set)

#--------------------------------------------------

# K-Means

#-------------------------------------------------- 

K_means_DataSet_train <- net_Train_Set[,-29]





#Train_Set_Original <- Bank_Data_Original[-Rows_Numbers_for_Test_Set,]
#Test_Set_Original <-Bank_Data_Original[Rows_Numbers_for_Test_Set,]



k <- 2
set.seed(123)
clust_data <- kmeans(K_means_DataSet_train, centers=k)
K_means_DataSet_train$clust <- factor(clust_data$cluster)

K_means_DataSet_train$clust_0_1 <- ifelse(K_means_DataSet_train$clust==1,0,1)
Y_0_1_Train <- as.data.frame(ifelse(net_Train_Set$Y=="no",0,1))
k_means_default_train_percision <- sum(K_means_DataSet_train$clust_0_1==Y_0_1_Train)/nrow(Y_0_1_Train)


K_means_DataSet_train <- K_means_DataSet_train[,-30]
K_means_DataSet_train <- data.matrix(K_means_DataSet_train)


#ggpairs(K_means_DataSet_train, columns=c('age', 'fnlwgt', 'education.num', 'workclass_D1', 'workclass_D2', 'occupation_D1', 'occupation_D2', 'occupation_D3', 'occupation_D4', 'occupation_D5', 'occupation_D6', 'occupation_D7', 'occupation_D8', 'occupation_D9', 'occupation_D10', 'occupation_D11', 'occupation_D12', 'occupation_D13', 'sex_D1', 'race_D1', 'race_D2', 'capitalGain_D1', 'capitalLoss_D1', 'marital_D1', 'marital_D2', 'grouped_hoursPerWeek_D1', 'grouped_hoursPerWeek_D2', 'nativeCountry_D1'), colour='clust', lower=list(continuous='points'), axisLabels='none', upper=list(continuous='blank'))
#ggpairs(K_means_DataSet_train, columns=c( 'fnlwgt', 'education.num'), colour='clust', lower=list(continuous='points'), axisLabels='none', upper=list(continuous='blank'))


scatt_data    <- cls.scatt.data((K_means_DataSet_train), clust=clust_data$cluster, dist='euclidean')
dunn_train          <- clv.Dunn(scatt_data, 'centroid', 'centroid')
DB_train            <- clv.Davies.Bouldin(scatt_data, 'centroid', 'centroid')


p1 <- ggplot(Train_Set ,aes(x= Train_Set$age, y=Train_Set$education.num,  color=ifelse(clust_data$cluster=="1","Green","Red") ,size=10 )) + geom_point(shape = ifelse(Y_0_1_Train==1,"Y","N")) + guides(color=F, size=F) 

grid.arrange(p1)

## FINDING THE BEST K

K_means_DataSet_train <- net_Train_Set[,-29]
K_means_DataSet_train <- data.matrix(K_means_DataSet_train)

# Dunn & DB

set.seed(123)
dunn <- c(); DB <- c(); K <- 9
for(k in 2:K){
  clust_data    <- kmeans(K_means_DataSet_train, centers=k)
  scatt_data    <- cls.scatt.data(K_means_DataSet_train, clust=clust_data$cluster, dist='euclidean')
  dunn          <- c(dunn, clv.Dunn(scatt_data, 'centroid', 'centroid'))
  DB            <- c(DB,   clv.Davies.Bouldin(scatt_data, 'centroid', 'centroid'))
}

clust_metrics <- data.frame(K = rep(seq(2,K,1),2), value = c(dunn, DB), metric = c(rep('Dunn',K-1), rep('DB',K-1)))
ggplot(clust_metrics, aes(x=K, y=value, color=factor(metric))) + geom_point() + geom_line()

k <- c(2,3,4,5,6,7,8,9)
print(cbind(k,DB,dunn, DB-dunn))



# kmeans results 


clust_data_2 <-  kcca(K_means_DataSet_train[,-29], k=2, kccaFamily("kmeans"))
pred_train <- predict(clust_data_2)
pred_test <- predict(clust_data_2, newdata=data.matrix(net_Test_Set[,-29]))
pred_test <- pred_test-1
pred_0_1_Kmeans <- pred_test
Y_0_1_Test <- as.data.frame(ifelse(net_Test_Set$Y=="no",0,1))
k_means_default_test_percision <- sum(pred_test==Y_0_1_Test)/nrow(Y_0_1_Test)


#--------------------------------------------------

# Fuzzy K-Means

#-------------------------------------------------- 

K_means_DataSet_train <- net_Train_Set[,-29]
K_means_DataSet_train <- data.matrix(K_means_DataSet_train)

res.fcm <- fcm(K_means_DataSet_train, centers=2)
as.data.frame(res.fcm$u)
res.fcm$v0
res.fcm$v
summary(res.fcm)
plotcluster(res.fcm, cp=1, trans=TRUE)



#--------------------------------------------------

# Random Forest

#-------------------------------------------------- 

Forest_DataSet <- Train_Set

set.seed(123)
RandomForest <- randomForest(Forest_DataSet$Y ~ ., data = Forest_DataSet, importance = TRUE)
RandomForest
# Predicting on Validation set
predValid <- predict(RandomForest, Test_Set, type = "class")
# Checking classification accuracy
mean(predValid == Test_Set$Y)                    
table(predValid,Test_Set$Y)


# finding best mtry

options <- c(1,2,3,4,5)
mtry_val  <- matrix(0,length(options),1)
i <- 1
for(n_mtry in options){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    RandomForest <- randomForest(k_folds_Train_sets[[fold]]$Y ~ ., data = k_folds_Train_sets[[fold]], importance = TRUE, mtry= n_mtry)
    predValid <- predict(RandomForest, k_folds_Valid_sets[[fold]], type = "class")
    temp_total_precision <- temp_total_precision + mean(predValid ==  k_folds_Valid_sets[[fold]]$Y)
  }
  mtry_val[i] <-  temp_total_precision/n.folds
  i  <- i + 1
}
print(mtry_val)
n_mtry <- options[which.max(mtry_val)]
n_mtry <- 3
ggplot(data.frame(x=options, y=mtry_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("mtry") + ylab("Validation Accuracy")



# finding best ntree

options <- seq(200,600,50)
ntree_val  <- matrix(0,length(options),1)
i <- 1
for(n_ntree in options){
  temp_total_precision <- 0
  for (fold in 1:n.folds){
    RandomForest <- randomForest(k_folds_Train_sets[[fold]]$Y ~ ., data = k_folds_Train_sets[[fold]], importance = TRUE, mtry= n_mtry, ntree= n_ntree)
    predValid <- predict(RandomForest, k_folds_Valid_sets[[fold]], type = "class")
    temp_total_precision <- temp_total_precision + mean(predValid ==  k_folds_Valid_sets[[fold]]$Y)
  }
  ntree_val[i] <-  temp_total_precision/n.folds
  print(i)
  i  <- i + 1
}
print(ntree_val)
ggplot(data.frame(x=options, y=ntree_val)) + geom_line(aes(x,y), color='purple') + 
  xlab("ntree") + ylab("Validation Accuracy")
n_ntree <- 450






## Final Random Forest

set.seed(123)
RandomForest <- randomForest(Forest_DataSet$Y ~ ., data = Forest_DataSet, importance = TRUE,  mtry= 3, ntree= 450)
RandomForest

final_forest_train <- predict(RandomForest, Train_Set, type = "class")
final_forest_test <- predict(RandomForest, Test_Set, type = "class")
# Checking classification accuracy
mean(final_forest_train == Train_Set$Y)  
mean(final_forest_test == Test_Set$Y) 


#------------------------------------------------------

# SVM

#------------------------------------------------------

## classification mode
# default with factor response:
set.seed(123)
model_svm <- svm(as.factor(Train_Set$Y) ~ ., data = Train_Set)



print(model_svm)
summary(model_svm)

# train perciosion
pred_train_svm <- predict(model_svm, Train_Set[,1:12])
svm_train <- ifelse(pred_train_svm=="no",0,1)


# test perciosion
pred_test_svm <- predict(model_svm, Test_Set[,1:12])
svm_test <- ifelse(pred_test_svm=="no",0,1)


# confusion table:
table(pred_test_svm, Test_Set$y)



#ROC
Yobs <- ifelse(Test_Set$Y=="no",0,1)
pred_randomForest_0_1 <- as.numeric(predValid)
pred_randomForest_0_1 <- pred_randomForest_0_1-1
roc(Yobs,pred_randomForest_0_1, plot = TRUE, legacy.axes=TRUE, percent = TRUE, 
    xlab="False Positive Percentage", ylab="True Positive Percentage", col= "purple", lwd= 3, print.auc= TRUE)
plot.roc(Yobs, preds_0_1_net, percent=TRUE, col= "#4daf4a", lwd=3, print.auc=TRUE, add=TRUE, print.auc.y=40)
plot.roc(Yobs, pred_0_1_Kmeans, percent=TRUE, col= "pink", lwd=3, print.auc=TRUE, add=TRUE, print.auc.y=30)
legend("bottomright", legend = c("Neural Net", "Random Forest", "Kmeans"), col= c("purple","#4daf4a","pink"), lwd=3)



# Ensemble

net_train <- ifelse(final_net_train=="no",0,1)
net_test <- ifelse(final_net_test=="no",0,1)
forest_train <- ifelse(final_forest_train=="no",0,1)
forest_test <- ifelse(final_forest_test=="no",0,1)


ensemble_train <- cbind(net_train,forest_train,svm_train)
ensemble_test <- cbind(net_test,forest_test,svm_test)

sum_train <- net_train+forest_train+svm_train
sum_test <- net_test+forest_test+svm_test

length(which(sum_train==0 | sum_train==3))/length(sum_train)
length(which(sum_test==0 | sum_test==3))/length(sum_test)

majority_train <- ifelse(sum_train>=2,1,0)
majority_test <- ifelse(sum_test>=2,1,0)

ensemble_train <- cbind(ensemble_train,majority_train)
ensemble_test <- cbind(ensemble_test,majority_test)

Y_train_0_1 <- ifelse(Train_Set$Y=="no",0,1) 
Y_test_0_1 <- ifelse(Test_Set$Y=="no",0,1) 
mean(majority_train == Y_train_0_1)
mean(majority_test == Y_test_0_1)

