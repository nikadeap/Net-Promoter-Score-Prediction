#Manipal Hospital
#Multiclass Model

#Load required libraries
library(stats)    #for binary logistic without wald statistics
#library(Deducer)  #for ROC plot
library(ROCR)     #for ROC plot (other way)
library(caret)    #for data partition. Model building
library(ResourceSelection)
library(car)
library(MASS)
library(lsr)


#Import trainig data for 3-class model

setwd('C:/Users/Harish/Desktop/BDA/Assignment_Module_2')
train.data <- read.csv("Train_MultiClass.csv",header = TRUE)
test.data <- read.csv("Test_MultiClass.csv",header=TRUE)

#Structure of training data
str(train.data)
View(train.data)

#Remove serial number, hospital ID and dates from training data
train.data <- train.data[,3:52]
#Convert ratings into factors
train.data[,11:45]<- lapply(train.data[,11:45],as.factor)

#Convert Response variables in ordinal factors such as detractors<passive<promoters
train.data[,50]<- as.ordered(train.data[,50])

#New training data after identifying significant variable individually regressed with response variales
lg.train.data <- as.data.frame(train.data[,c(#'MaritalStatus',
  'AgeYrs',
  #'Sex',
  #'BedCategory',
  'Department',
  #'Estimatedcost',
  'InsPayorcategory',
  #'State',
  #'STATEZONE',
  'CE_ACCESSIBILITY',
  #'CE_CSAT',
  'CE_VALUEFORMONEY',
  'EM_IMMEDIATEATTENTION',
  'EM_NURSING',	
  #'EM_DOCTOR',
  #'EM_OVERALL',
  #'AD_TIME',
  'AD_TARRIFFPACKAGESEXPLAINATION',
  #'AD_STAFFATTITUDE',
  #'INR_ROOMCLEANLINESS',
  #'INR_ROOMPEACE',
  #'INR_ROOMEQUIPMENT',
  'INR_ROOMAMBIENCE',
  'FNB_FOODQUALITY',	
  'FNB_FOODDELIVERYTIME',
  #'FNB_DIETICIAN',
  #'FNB_STAFFATTITUDE',
  'AE_ATTENDEECARE',
  #'AE_PATIENTSTATUSINFO',
  'AE_ATTENDEEFOOD',	
  #'DOC_TREATMENTEXPLAINATION',
  #'DOC_ATTITUDE',	
  #'DOC_VISITS',
  'DOC_TREATMENTEFFECTIVENESS',
  'NS_CALLBELLRESPONSE',	
  #'NS_NURSESATTITUDE',	
  #'NS_NURSEPROACTIVENESS',
  #'NS_NURSEPATIENCE',
  #'OVS_OVERALLSTAFFATTITUDE',
  'OVS_OVERALLSTAFFPROMPTNESS',
  #'OVS_SECURITYATTITUDE',
  #'DP_DISCHARGETIME',	
  # 'DP_DISCHARGEQUERIES',	
  'DP_DISCHARGEPROCESS',
  #'AdmissionDate',
  #'DischargeDate',
  #'LengthofStay',
  'NPS_Status')])

#Apply same on test data

test.data <- read.csv("Test_MultiClass.csv",header=TRUE)

test.data <- test.data[,3:52]

test.data[,11:45]<- lapply(test.data[,11:45],as.factor)

test.data[,50]<- as.ordered(test.data[,50])


lg.test.data <- as.data.frame(test.data[,c(#'MaritalStatus',
  'AgeYrs',
  #'Sex',
  #'BedCategory',
  'Department',
  #'Estimatedcost',
  'InsPayorcategory',
  #'State',
  #'STATEZONE',
  'CE_ACCESSIBILITY',
  #'CE_CSAT',
  'CE_VALUEFORMONEY',
  'EM_IMMEDIATEATTENTION',
  'EM_NURSING',	
  #'EM_DOCTOR',
  #'EM_OVERALL',
  #'AD_TIME',
  'AD_TARRIFFPACKAGESEXPLAINATION',
  #'AD_STAFFATTITUDE',
  #'INR_ROOMCLEANLINESS',
  #'INR_ROOMPEACE',
  #'INR_ROOMEQUIPMENT',
  'INR_ROOMAMBIENCE',
  'FNB_FOODQUALITY',	
  'FNB_FOODDELIVERYTIME',
  #'FNB_DIETICIAN',
  #'FNB_STAFFATTITUDE',
  'AE_ATTENDEECARE',
  #'AE_PATIENTSTATUSINFO',
  'AE_ATTENDEEFOOD',	
  #'DOC_TREATMENTEXPLAINATION',
  #'DOC_ATTITUDE',	
  #'DOC_VISITS',
  'DOC_TREATMENTEFFECTIVENESS',
  'NS_CALLBELLRESPONSE',	
  #'NS_NURSESATTITUDE',	
  'NS_NURSEPROACTIVENESS',
  #'NS_NURSEPATIENCE',
  #'OVS_OVERALLSTAFFATTITUDE',
  'OVS_OVERALLSTAFFPROMPTNESS',
  #'OVS_SECURITYATTITUDE',
  #'DP_DISCHARGETIME',	
  # 'DP_DISCHARGEQUERIES',	
  'DP_DISCHARGEPROCESS',
  #'AdmissionDate',
  #'DischargeDate',
  #'LengthofStay',
  'NPS_Status')])


#Null Model
noModel <- polr(NPS_Status ~ 1,data = lg.train.data, Hess=TRUE)

#Full Model
lgGlmModelFull = polr(NPS_Status ~ .
                     , data = lg.train.data, Hess=TRUE)


#Stepwise - Forward selection backward elimination
lgGlmModelStepwise <- step(noModel, list(lower = formula(noModel),
                                         upper = formula(lgGlmModelFull)),
                           direction = "both",trace = 0)

#Find out significant variables based on p-value
#Identify coefficients
ctable <- coef(summary(lgGlmModelStepwise))
#Identify p-values
p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
(ctable<-cbind(ctable,"p value" = p))

#Rebuild the models only with significant variables identified from above model

lgGlmModelFull = polr(NPS_Status ~CE_VALUEFORMONEY+FNB_FOODDELIVERYTIME+CE_ACCESSIBILITY+AD_TARRIFFPACKAGESEXPLAINATION+DP_DISCHARGEPROCESS+
                        INR_ROOMAMBIENCE+Department+NS_CALLBELLRESPONSE+DOC_TREATMENTEFFECTIVENESS+AE_ATTENDEEFOOD
                      , data = lg.train.data, Hess=TRUE)

#Calculate regression coefficient
ctable <- coef(summary(lgGlmModelFull))
#Calculate p-value
p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE)*2
#Bind the p-vale in table

ctable<-cbind(ctable,"p value" = p)
#Print the table to observe p-values
print(ctable)



#predict probability

pred <- predict(lgGlmModelFull,lg.train.data)

#Confusion Matrix and error for training data
cat("confusion matrix for training data")
cat("\n")
tab <- table(pred,lg.train.data$NPS_Status)
print(tab)


#Confusion Matrix and error for test data
pred <- predict(lgGlmModelFull,lg.test.data)
cat("confusion matrix for test data")
cat("\n")
(tab1 <- table(pred,lg.test.data$NPS_Status))
print(tab1)


#Overall Accuracy
accuracy<- sum(diag(tab1))/sum(tab1)
cat("overall accuracy is ",accuracy*100,"%")



