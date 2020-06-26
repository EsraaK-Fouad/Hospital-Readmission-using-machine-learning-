library(naniar)
library(dummies)
library(readr)
library(caret)
library("ggpubr") #bt3t cor
library(stringr)
library(dplyr)
dataset <- readmission[1:50,]
dataset<-na.omit(dataset)



dim(dataset)

dataset<-replace(dataset,dataset=="?",NA)


########### decide which column to drop based on NA percentage & drop ID  #######

data_NA <- sapply(dataset, function(x) sum(is.na(x)))
data_NA_sorted <- sort(data_NA,decreasing = TRUE)
barplot(data_NA_sorted,xlab="Attributes",ylab="# NA",main="Count_NA")
drops <- c("weight","medical_specialty","payer_code",'encounter_id','patient_nbr','admission_type_id',
           'discharge_disposition_id','admission_source_id')
dataset_dropped <- dataset[ , !(names(dataset) %in% drops)]


########################### gender ##################

dataset <- replace_with_na(dataset_dropped,replace = list("gender" = c('Unknown','Invalid')))
dataset<-na.omit(dataset)

################################### DIAG_1&2&3 ##################
col <- c('diag_1','diag_2','diag_3')

for (i in col) {
  
  dataset[i]<-replace(dataset[i], dataset[i] == 250, 'Diabetes')
  dataset[i]<-replace(dataset[i],(dataset[i]>=390 & dataset[i]<=458)| dataset[i] == 785 , 'Circulatory')
  dataset[i]<-replace(dataset[i], (dataset[i]>=460 & dataset[i]<=519) | dataset[i]==786, 'Respiratory')
  dataset[i]<-replace(dataset[i], (dataset[i]>=520 & dataset[i]<=579) | dataset[i]==787, 'Digestive')
  dataset[i]<-replace(dataset[i], (dataset[i]>=580 & dataset[i]<=629) | dataset[i]==788, 'Genitourinary')
  dataset[i]<-replace(dataset[i], (dataset[i]>=800) & (dataset[i]<=999), 'Injury')
  dataset[i]<-replace(dataset[i], (dataset[i]>=710) & (dataset[i]<=739), 'Muscoloskeletal')
  dataset[i]<-replace(dataset[i], (dataset[i]>=140) & (dataset[i]<=239), 'Neoplasms')
  dataset[i]<-replace(dataset[i],(dataset[i] !='Diabetes') & (dataset[i] !='Circulatory')&(dataset[i] !='Respiratory')& (dataset[i] !='Digestive')& (dataset[i] !='Genitourinary')&(dataset[i] !='Injury')&(dataset[i] !='Muscoloskeletal')&(dataset[i] !='Neoplasms'), 'Others')
dataset[i]<-replace(dataset[i],(dataset[i] !='Diabetes') & (dataset[i] !='Circulatory')&(dataset[i] !='Respiratory')& (dataset[i] !='Digestive')& (dataset[i] !='Genitourinary')&(dataset[i] !='Injury')&(dataset[i] !='Muscoloskeletal')&(dataset[i] !='Neoplasms'), 'Others')
dataset[i]<-replace(dataset[i],(dataset[i] !='Diabetes') & (dataset[i] !='Circulatory')&(dataset[i] !='Respiratory')& (dataset[i] !='Digestive')& (dataset[i] !='Genitourinary')&(dataset[i] !='Injury')&(dataset[i] !='Muscoloskeletal')&(dataset[i] !='Neoplasms'), 'Others')


}




########################################  Age  ##############

s<-str_split_fixed(dataset$age, "-", 2)
s1<-as.numeric(gsub("[[]","",s[,1]))
s2<-as.numeric(gsub("[)]","",s[,2]))
age_new<-(s1+s2)/2
dataset<- cbind(dataset,age_new)


############################################# max_glu_serum ##############

dataset['max_glu_serum']<-replace(dataset['max_glu_serum'], dataset['max_glu_serum'] == 'None', 0)
dataset['max_glu_serum']<-replace(dataset['max_glu_serum'], dataset['max_glu_serum'] == 'Norm', 1)
dataset['max_glu_serum']<-replace(dataset['max_glu_serum'], dataset['max_glu_serum'] == '>200', 2)
dataset['max_glu_serum']<-replace(dataset['max_glu_serum'], dataset['max_glu_serum'] == '>300', 3)


################  A1 ########################

dataset['A1Cresult']<-replace(dataset['A1Cresult'], dataset['A1Cresult'] == 'None', 0)
dataset['A1Cresult']<-replace(dataset['A1Cresult'], dataset['A1Cresult'] == 'Norm', 1)
dataset['A1Cresult']<-replace(dataset['A1Cresult'], dataset['A1Cresult'] == '>7', 2)
dataset['A1Cresult']<-replace(dataset['A1Cresult'], dataset['A1Cresult'] == '>8', 3)



##################### change medication ###########3

dataset['change']<-replace(dataset['change'], dataset['change'] == 'No', 0)
dataset['change']<-replace(dataset['change'], dataset['change'] == 'Ch', 1)



dataset['diabetesMed']<-replace(dataset['diabetesMed'], dataset['diabetesMed'] == 'No', 0)
dataset['diabetesMed']<-replace(dataset['diabetesMed'], dataset['diabetesMed'] == 'Yes', 1)


############################## 24 Features ####################


col <- c('metformin','repaglinide','nateglinide','chlorpropamide',
       'glimepiride','acetohexamide','glipizide','glyburide',
       'tolbutamide','pioglitazone','rosiglitazone','acarbose',
       'miglitol','troglitazone','tolazamide','examide',
       'citoglipton','insulin','glyburide-metformin','glipizide-metformin',
       'glimepiride-pioglitazone','metformin-rosiglitazone','metformin-pioglitazone')



for (i in col){
dataset[i] <- replace(dataset[i], dataset[i] == 'Up', 0)
dataset[i] <- replace(dataset[i], dataset[i] == 'Down', 1)
dataset[i] <- replace(dataset[i], dataset[i] == 'Steady', 2)
dataset[i] <- replace(dataset[i], dataset[i] == 'No', 3)

}



########################################## readmitted ########################

dataset['readmitted']<-replace(dataset['readmitted'], (dataset['readmitted'] != 'NO' ) , 1)
dataset['readmitted']<-replace(dataset['readmitted'], dataset['readmitted'] == 	'NO', 0)

####################### time in hospital 


########################## dummy variables ######################

c1<-dummy(dataset$gender)
c2<-dummy(dataset$race)
c3<-dummy(dataset$diag_1)
c4<-dummy(dataset$diag_2)
c5<-dummy(dataset$diag_3)

dataset<- cbind(dataset,c1,c2,c3,c4,c5)

###################################### drops ###########################


drops <- c('gender','race','diag_1','diag_2','diag_3','age')
dataset <- dataset[ , !(names(dataset) %in% drops)]


################################## normalize ###################################


dataset <- as.data.frame(sapply(dataset, as.numeric))
dataset<-na.omit(dataset)



col <- c('time_in_hospital','num_lab_procedures','num_medications',
       'number_emergency','number_inpatient',
       'number_diagnoses','age_new')

normalize <- function(x) {

return ((x - min(x)) / (max(x) - min(x)))
}


for (i in col){
dataset[i] <-  normalize(dataset[i])
}








#####################################feature selection  ##############################

View(dataset)

drops <- c('readmitted')
dd <- dataset[ , !(names(dataset) %in% drops)]
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results<-rfe(dd[,1:62],dataset[,49], sizes=c(1:5), rfeControl=control)
xs<-predictors(results)
plot(results, type=c("g", "o"))


# according to the plot and comparing importance with xs's values , we selected the follwing features to go on with which is exactly like the plot from data'paper attached in blog post 





View(dataset)
dataset<-select(dataset,'diag_1Circulatory','diag_1Genitourinary','diag_1Injury',
              'diag_1Circulatory','diag_1Others','age_new','time_in_hospital','A1Cresult',
              'raceAfricanAmerican','raceCaucasian','raceOther',"diag_2Circulatory",'readmitted')
      









################################################################################
dataset$readmitted <- as.factor(dataset$readmitted)

levels(dataset$readmitted) <- c(0, 1)
set.seed(777)
folds<-createFolds(dataset$readmitted,k=10)

model=lapply(folds, function(x){

training_set=dataset[-x,]
test_set=dataset[x,]

##################### knn model*******************************
KNN=train(readmitted ~ .,data= training_set ,method="knn")

drops <- c('readmitted')
test_set_drop <- dataset[ , !(names(dataset) %in% drops)]

pred=predict(KNN,newdata=test_set_drop)


cm=table(dataset[,12],pred)
accuarcy=(cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
sensitivity=cm[2,2]/(cm[2,2]+cm[2,1])
specificity=cm[1,1]/(cm[1,2]+cm[1,1])

#################### logestic ****************
LOGESTIC=train(readmitted ~ .,data= training_set ,method="glm")
pred1=predict(LOGESTIC,newdata=test_set_drop)
cm1=table(dataset[,12],pred1)
accuarcy1=(cm1[1,1]+cm1[2,2])/(cm1[1,1]+cm1[2,2]+cm1[1,2]+cm1[2,1])
sensitivity1=cm1[2,2]/(cm1[2,2]+cm1[2,1])
specificity1=cm1[1,1]/(cm1[1,2]+cm1[1,1])

conf.mat=as.data.frame(c(accuarcy,sensitivity,specificity,accuarcy1,sensitivity1,specificity1))
return(conf.mat)
}
)
rowMeans(as.data.frame(model))









################### test importance of features to make less lab tests ##########


set.seed(7)
library(mlbench)
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(readmitted~., data=dataset, method="glm", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance)








