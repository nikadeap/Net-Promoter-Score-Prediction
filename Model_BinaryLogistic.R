library(stats)    #for binary logistic without wald statistics
#library(Deducer)  #for ROC plot
library(ROCR)     #for ROC plot (other way)
library(caret)    #for data partition. Model building
library(ResourceSelection) #Hosmer lemeshow GoF test

setwd('C:/Users/Harish/Desktop/BDA/Assignment_Module_2')
#raw.data <- read.csv("/Users/Harish/Desktop/BDA/Assignment_Module_2/Manipal NPS data file")

library(readxl)
raw_data <- read_excel("Manipal NPS data file.xlsx", sheet = 'Training Data for Multi-Class M')
test.data <-read_excel("Manipal NPS data file.xlsx", sheet = 'Test Data for Multi-Class Model')

str(raw_data)
raw_data <- data.frame(raw_data)

for(i in 1:length(raw_data$CE_NPS)){

if(raw_data$CE_NPS[i]>=9 ){
  
  raw_data$NPS_newStatus[i]<-"Promoter"
 
  }else{
    raw_data$NPS_newStatus[i]<-"Detractor"
 
  }
}

for(i in 1:length(test.data$CE_NPS)){
  
  if(test.data$CE_NPS[i]>=9 ){
    
    test.data$NPS_newStatus[i]<-"Promoter"
    
  }else{
    test.data$NPS_newStatus[i]<-"Detractor"
    
  }
}



raw_data$NPS_newStatus <- relevel(as.factor(raw_data$NPS_newStatus), ref = "Promoter")

test.data$NPS_newStatus <- relevel(as.factor(test.data$NPS_newStatus), ref = "Promoter")

filter.data <- subset(raw_data, select=-c(SN,HospitalNo2,Country,AdmissionDate,DischargeDate,NPS_Status,CE_NPS))

testFilter.data <- subset(test.data, select=-c(SN,HospitalNo2,Country,AdmissionDate,DischargeDate,NPS_Status,CE_NPS))

#temp<-subset(raw_data,select=c(AgeYrs,Estimatedcost,LengthofStay,Sex,MaritalStatus,Department,BedCategory,InsPayorcategory,State,STATEZONE,NPS_newStatus))
#temp[,4:10]<-lapply(temp[,4:10],as.factor)
#temp[,1:3]<-lapply(temp[,1:3],as.integer)

#temp<-subset(raw_data,select=c(AgeYrs,NPS_newStatus))
#temp[,2]<-lapply(temp[,2],as.factor)
#temp[,1]<-lapply(temp[,1],as.integer)

testFilter.data <- subset(test.data, select=-c(SN,HospitalNo2,AgeYrs,MaritalStatus,BedCategory,Estimatedcost,State,Country,STATEZONE,AdmissionDate,DischargeDate,NPS_Status,CE_NPS))
testFilter.data[,1:38]<-lapply(testFilter.data[,1:38],as.factor)
testFilter.data$LengthofStay <- as.integer(testFilter.data$LengthofStay)
testFilter.data$NPS_newStatus <- as.factor(testFilter.data$NPS_newStatus)

filter.data <- subset(raw_data, select=-c(SN,HospitalNo2,AgeYrs,MaritalStatus,BedCategory,Estimatedcost,State,Country,STATEZONE,AdmissionDate,DischargeDate,NPS_Status,CE_NPS))
filter.data[,1:38]<-lapply(filter.data[,1:38],as.factor)
filter.data$LengthofStay <- as.integer(filter.data$LengthofStay)
filter.data$NPS_newStatus <- as.factor(filter.data$NPS_newStatus)

raw_data$NPS_newStatus <- relevel(as.factor(raw_data$NPS_newStatus), ref = "Detractor")

#test.data$NPS_newStatus <- relevel(as.factor(test.data$NPS_newStatus), ref = "Promoter")
#No Model

noModel <- glm(NPS_newStatus ~ 1,data = filter.data, family = binomial)
#Full Model
lgGlmModelFull = glm(NPS_newStatus ~ .
                     , data = filter.data, family = binomial)

#Stepwise - Forward selection backward elimination
lgGlmModelStepwise <- step(noModel, list(lower = formula(noModel),
                                         upper = formula(lgGlmModelFull)),
                           direction = "both",trace = 0)



## Model Evaluation

### 1. ROC plot and Model summary of Train Data

hoslem.test(lgGlmModelStepwise$y, fitted(lgGlmModelStepwise)) #Hosmer lemeshow test
#rocplot(lgGlmModelStepwise)
summary(lgGlmModelStepwise)


### 2. The optimal cut-off

msclaf.cost <- c()
youden.index <- c()
cutoff <- c()
P11 <- c() #correct classification of positive as positive
P00 <- c() #correct classification of negative as negative
P10 <- c() #misclassification of positive class to negative class
P01 <- c() #misclassification of negative class to positive class




lgGlmTrainPredictedProbability = predict.glm(lgGlmModelStepwise, filter.data, type = "response")
#variable with all the values as joined
n <- length(filter.data$NPS_newStatus)

costs = matrix(c(0,2,1, 0), ncol = 2)
colnames(costs) = rownames(costs) = c("Promoter", "Detractor")
as.table(costs)



for (i in seq(0.05, 1, .05)) {
  predicted.y = rep("Promoter", n)
  predicted.y[lgGlmTrainPredictedProbability > i] = "Detractor"
  tbl <- table(filter.data$NPS_newStatus, predicted.y)
  if ( i <= 1) {
    #Classifying Not Joined as Joined
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
df.cost.table <- cbind(cutoff,P10,P01,msclaf.cost, P11, P00, youden.index)
```
df.cost.table