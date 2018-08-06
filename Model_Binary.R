
#Binary Model

#Load required libraries
library(stats)    #for binary logistic without wald statistics
#library(Deducer)  #for ROC plot
library(ROCR)     #for ROC plot (other way)
library(caret)    #for data partition. Model building
library(ResourceSelection)
library(car)

library(lsr)


#Import trainig data for binary model

setwd('C:/......')
train.data <- read.csv("Train_Binary.csv",header = TRUE)
test.data <- read.csv("Test_Binary.csv",header=TRUE)

#Structure of training data
str(train.data)
View(train.data)

#Remove the variables such as serial number, ID and dates from training data
train.data <- train.data[,3:52]

#Convert the ratings in ordinal factors

train.data[,11:45]<- lapply(train.data[,11:45],as.factor)


#Select the actual training data by selecting the variables
#Variable selection is done on the basis 
#of performing regression of response variable with every single independent variable
#and only those variable which were significant when regressed individually 
#with response varaiable are taken into consideration
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

test.data <- test.data[,3:52]
test.data[,11:45]<- lapply(test.data[,11:45],as.factor)


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
noModel <- glm(NPS_Status ~ 1,data = lg.train.data, family = binomial)

#Full Model
lgGlmModelFull = glm(NPS_Status ~ .
                     , data = lg.train.data, family = binomial)



#Stepwise - Forward selection backward elimination
lgGlmModelStepwise <- step(noModel, list(lower = formula(noModel),
                                         upper = formula(lgGlmModelFull)),
                           direction = "both",trace = 0)

cat("\n")

#Check for multicollinearity
print(vif(lgGlmModelStepwise))


## Model Evaluation

## Diagnostic test for logistic model validation
hoslem.test(lgGlmModelStepwise$y, fitted(lgGlmModelStepwise)) #Hosmer lemeshow test
summary(lgGlmModelStepwise)


#Optimal cutoff frequency

#creating empty vectors to store the results. 
msclaf.cost <- c()
youden.index <- c()
cutoff <- c()
P11 <- c() #correct classification of positive as positive
P00 <- c() #correct classification of negative as negative
P10 <- c() #misclassification of positive class to negative class
P01 <- c() #misclassification of negative class to positive class


#Select the optimal cut-off value

#lgGlmTrainPredictedProbability = predict.glm(lgGlmModelStepwise, lg.train.data, type = "response")

costs = matrix(c(0,1,1, 0), ncol = 2)
colnames(costs) = rownames(costs) = c("Promoter", "Detractor")
as.table(costs)

#The misclassification cost table is:

# defining log odds in favor of Non Default
for (i in seq(0.05, 1, .05)) {
  predicted.y = rep("Promoter", n)
  predicted.y[lgGlmTrainPredictedProbability < i] = "Detractor"
  tbl <- table(lg.train.data$NPS_Status, predicted.y)
  if ( i <= 1) {
    #Classifying Promoters and detractors
    P10[20*i] <- tbl[2]/(tbl[2] + tbl[4])
    
    P11[20*i] <- tbl[4]/(tbl[2] + tbl[4])
    
    #Classifying Joined as Not Joined
    P01[20*i] <- tbl[3]/(tbl[1] + tbl[3])
    
    P00[20*i] <- tbl[1]/(tbl[1] + tbl[3])
    
    cutoff[20*i] <- i
    msclaf.cost[20*i] <- P10[20*i]*costs[2] + P01[20*i]*costs[3]
    youden.index[20*i] <- P11[20*i] + P00[20*i] - 1
  }
}
#Following table has Youden index information
#Based on the maximum value of Youden's index, chose cutoff probability
#Rationnal behind using Youden's index
# As the causes leading to the NPS score are important, identifying promoters and detractors carry same weightage
df.cost.table <- cbind(cutoff,P10,P01,msclaf.cost, P11, P00, youden.index)

# 3. Confusion Matrix on the test data

lgGlmTestPredictedProbability = predict(lgGlmModelStepwise, lg.test.data, type = "response")

#variable with all the values as promoters
n <- length(lg.test.data$NPS_Status)
predicted.y = rep("Promoter", n)

# defining log odds in favor of detractors
#Cut off probability is identified as 0.6 from youden's index table
predicted.y[lgGlmTestPredictedProbability < 0.60] = "Detractor"

#add the model_precition in the data
lg.test.data$predicted.y <- predicted.y

###Create the confusionmatrix###
ConfusionMatrix<-addmargins(table(lg.test.data$NPS_Status, lg.test.data$predicted.y))
print("Below is confusion matrix")
print(ConfusionMatrix)
cat("\n")
#Calculate sensitivity, specificity and overall accuracy of the model

accuracy <- ((ConfusionMatrix[1]+ConfusionMatrix[5])/ConfusionMatrix[9])*100
recall <-ConfusionMatrix[2,2]/ConfusionMatrix[2,3]
specificity <- ConfusionMatrix[1,1]/ConfusionMatrix[1,3]
cat("Sensitivity of the model is ",recall*100,"%")
cat("\n")
cat("specificity of the model is ",specificity*100,"%")
cat("\n")

cat("overall accuracy of the model is ",accuracy,"%")


