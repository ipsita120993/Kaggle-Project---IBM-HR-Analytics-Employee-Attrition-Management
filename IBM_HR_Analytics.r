#All the data will be coming as numbers instead of exponential
options(scipen=999)

#Get the directory
getwd()

#Set the directory
setwd("D:/data science")

#Important packages to be loaded from memory
library(dplyr)

#Importing the dataset
IBM_HR<- read.csv("D:/data science/IBM HR Analytics/WA_Fn-UseC_-HR-Employee-Attrition.csv",header=TRUE)

#Summarize the data
summary(IBM_HR)

#Checking the data types of all the variables of the dataset
myfun1<- function(x)
{
  class(x)
}
myfun1out<- t(lapply(IBM_HR,FUN=myfun1))
write.csv(myfun1out,file="D:/data science/IBM HR Analytics/data_types.csv")

#Checking the structure of the dataframe 
str(IBM_HR)
View(IBM_HR)
names(IBM_HR)

#Renaming the variable of the dataframe
colnames(IBM_HR)[colnames(IBM_HR)=="ï..Age"]<-"Age"

#Changing the data type of the variables of the dataframe as per requirement
IBM_HR$Age<- as.numeric(IBM_HR$Age)
IBM_HR$DailyRate<- as.numeric(IBM_HR$DailyRate)
IBM_HR$DistanceFromHome <- as.numeric(IBM_HR$DistanceFromHome)
IBM_HR$Education <- as.factor(IBM_HR$Education)
IBM_HR$EmployeeCount <- as.numeric(IBM_HR$EmployeeCount)
IBM_HR$EmployeeNumber <- as.numeric(IBM_HR$EmployeeNumber)
IBM_HR$EnvironmentSatisfaction <- as.factor(IBM_HR$EnvironmentSatisfaction)
IBM_HR$HourlyRate <- as.numeric(IBM_HR$HourlyRate)
IBM_HR$JobInvolvement <- as.factor(IBM_HR$JobInvolvement)
IBM_HR$JobLevel <- as.factor(IBM_HR$JobLevel)
IBM_HR$JobSatisfaction<- as.factor(IBM_HR$JobSatisfaction)
IBM_HR$MonthlyIncome <- as.numeric(IBM_HR$MonthlyIncome)
IBM_HR$MonthlyRate <- as.numeric(IBM_HR$MonthlyRate)
IBM_HR$NumCompaniesWorked <- as.numeric(IBM_HR$NumCompaniesWorked)
IBM_HR$PerformanceRating <- as.factor(IBM_HR$PerformanceRating)
IBM_HR$RelationshipSatisfaction <- as.factor(IBM_HR$RelationshipSatisfaction)
IBM_HR$StandardHours <- as.numeric(IBM_HR$StandardHours)
IBM_HR$StockOptionLevel <- as.factor(IBM_HR$StockOptionLevel)
IBM_HR$TrainingTimesLastYear<- as.numeric(IBM_HR$TrainingTimesLastYear)
IBM_HR$WorkLifeBalance <- as.numeric(IBM_HR$WorkLifeBalance)
IBM_HR$PercentSalaryHike <- as.numeric(IBM_HR$PercentSalaryHike)
IBM_HR$TotalWorkingYears <- as.numeric(IBM_HR$TotalWorkingYears)
IBM_HR$YearsAtCompany <- as.numeric(IBM_HR$YearsAtCompany)
IBM_HR$YearsInCurrentRole <- as.numeric(IBM_HR$YearsInCurrentRole)
IBM_HR$YearsSinceLastPromotion <- as.numeric(IBM_HR$YearsSinceLastPromotion)
IBM_HR$YearsWithCurrManager <- as.numeric(IBM_HR$YearsWithCurrManager)

#Label Encoding for the response variable
IBM_HR<- transform(IBM_HR,Attrition=ifelse(Attrition=='Yes',1,0))
IBM_HR$Attrition <- as.factor(IBM_HR$Attrition)

#Remove the variables with zero variance
IBM_HR$EmployeeCount <- NULL
IBM_HR$EmployeeNumber<- NULL
IBM_HR$StandardHours <- NULL

#Separate the categorical and numerical data
numericVars <- which(sapply(IBM_HR, is.numeric))
num_data <- IBM_HR[,numericVars]
facVars <- which(sapply(IBM_HR, is.factor))
fac_data<- IBM_HR[,facVars]

#Understanding the numerical data
myfun3<- function(x)
{
  var_type=class(x)   
  nmiss<- sum(is.na(x))
  MEAN<- mean(x,na.rm=T)
  SD<- sd(x,na.rm=T)
  pctl<- quantile(x,na.rm=T,p=c(0.01,0.05,0.5,0.09,0.95,0.98,0.99))
  MAX<- max(x,na.rm=T)
  MIN<- min(x,na.rm=T)
  return(c(var_type=var_type,nmiss=nmiss,MEAN=MEAN,SD=SD,MAX=MAX,MIN=MIN,pctl=pctl))
}
num_IBM_data<- t(data.frame(lapply(num_data,FUN=myfun3)))
write.csv(num_IBM_data,file="D:/data science/IBM HR Analytics/descriptive.csv") 

#Missing Value Detection
missing_treat1<- function(x)
{
  NMISSpct<- (sum(is.na(x))*100)/1470
  return(c(NMISSpct=NMISSpct))
}
out<-lapply(IBM_HR,FUN=missing_treat1)
print(out)
colSums(IBM_HR=='NA')
#There is no missing value in the whole dataset

#From the dataset, it is observed that outliers are present.
#Since, all the variables are non symmetric, do Outlier treatment using 99 percentile as upper cap and 1 percentile as lower cap
outlier_treat2<- function(x)
{
  p99<-quantile(x,na.rm=T,p=c(0.99))
  p1<-quantile(x,na.rm=T,p=c(0.01))
  x[x>p99]<- p99
  x[x<p1]<-p1
  return (x)
}
num_data<-lapply(num_data,FUN=outlier_treat2)
num_IBM_data<- t(data.frame(lapply(num_data,FUN=myfun3)))
write.csv(num_IBM_data,file="D:/data science/IBM HR Analytics/descriptive.csv") 

#Bind both num and fac datasets
IBM_HR<- cbind(num_data,fac_data)

#Checking for the significance of the numerical variables with response variable using anova test
anova_func<- function(x)
{
  anova_out<- aov(x~Attrition,data=IBM_HR)
  o<-summary(anova_out)
  return (o)
}
lapply(num_data,FUN=anova_func)

#Remove the insignificant variables as got from anova test
IBM_HR$HourlyRate <- NULL
IBM_HR$MonthlyRate <- NULL
IBM_HR$NumCompaniesWorked <- NULL
IBM_HR$PercentSalaryHike <- NULL
IBM_HR$YearsSinceLastPromotion <- NULL

#checking the significance of categorical variables with response variable
chisq.test(IBM_HR$Attrition,IBM_HR$Gender,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$BusinessTravel,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$Department,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$Education,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EducationField,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$StockOptionLevel,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$RelationshipSatisfaction,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EnvironmentSatisfaction,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobInvolvement,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobLevel,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobSatisfaction,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$MaritalStatus,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$Over18,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$OverTime,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$PerformanceRating,simulate.p.value = TRUE)

#Remove the insignificant categorical variables
IBM_HR$Gender<- NULL
IBM_HR$Education <- NULL
IBM_HR$RelationshipSatisfaction<- NULL
IBM_HR$PerformanceRating <- NULL
IBM_HR$Over18 <- NULL

#Create dummy variables for the significant categorical variables
library(fastDummies)
IBM_HR<-dummy_cols(IBM_HR,select_columns =c("BusinessTravel","Department","EducationField","EnvironmentSatisfaction","JobInvolvement","JobLevel","JobRole","JobSatisfaction","MaritalStatus","StockOptionLevel"),remove_first_dummy = TRUE )
write.csv(IBM_HR,file="D:/data science/IBM HR Analytics/IBM_HR.csv")

#Remove the parent categorical variables for which you have created dummy
IBM_HR$BusinessTravel <- NULL
IBM_HR$Department <- NULL
IBM_HR$EducationField <- NULL
IBM_HR$EnvironmentSatisfaction<- NULL
IBM_HR$JobInvolvement <- NULL
IBM_HR$JobLevel <- NULL
IBM_HR$JobRole <- NULL
IBM_HR$JobSatisfaction <- NULL
IBM_HR$MaritalStatus <- NULL
IBM_HR$StockOptionLevel <- NULL

#Label Encoding for the predictor categorical variable Overtime
IBM_HR<- transform(IBM_HR,OverTime=ifelse(OverTime=='Yes',1,0))
IBM_HR$OverTime <- as.factor(IBM_HR$OverTime)

#Some columns need to be renamed after creating dummies
#Rename some columns which are required
colnames(IBM_HR)[colnames(IBM_HR)=="Department_Research & Development"]<-"Department_Research_and_Development"
colnames(IBM_HR)[colnames(IBM_HR)=="Department_Human Resources"]<-"Department_Human_and_Resources"
colnames(IBM_HR)[colnames(IBM_HR)=="EducationField_Technical Degree"]<-"EducationField_Technical_and_Degree"
colnames(IBM_HR)[colnames(IBM_HR)=="EducationField_Human Resources"]<-"EducationField_Human_and_Resources"
colnames(IBM_HR)[colnames(IBM_HR)=="JobRole_Research Scientist"]<-"JobRole_ResearchScientist"
colnames(IBM_HR)[colnames(IBM_HR)=="JobRole_Laboratory Technician"]<-"JobRole_LaboratoryTechnician"
colnames(IBM_HR)[colnames(IBM_HR)=="JobRole_Manufacturing Director"]<-"JobRole_ManufacturingDirector"
colnames(IBM_HR)[colnames(IBM_HR)=="JobRole_Healthcare Representative"]<-"JobRole_HealthcareRepresentative"
colnames(IBM_HR)[colnames(IBM_HR)=="JobRole_Sales Representative"]<-"JobRole_SalesRepresentative"
colnames(IBM_HR)[colnames(IBM_HR)=="JobRole_Research Director"]<-"JobRole_ResearchDirector"
colnames(IBM_HR)[colnames(IBM_HR)=="JobRole_Human Resources"]<-"JobRole_HumanResources"

#Dummies created are of integer type
#Change the datatype of the dummies
IBM_HR$BusinessTravel_Travel_Frequently<- as.factor(IBM_HR$BusinessTravel_Travel_Frequently)
IBM_HR$BusinessTravel_Non.Travel<- as.factor(IBM_HR$BusinessTravel_Non.Travel)
IBM_HR$Department_Research...Development<- as.factor(IBM_HR$Department_Research...Development)
IBM_HR$Department_Human.Resources<- as.factor(IBM_HR$Department_Human.Resources)
IBM_HR$EducationField_Other<- as.factor(IBM_HR$EducationField_Other)
IBM_HR$EducationField_Medical<- as.factor(IBM_HR$EducationField_Medical)
IBM_HR$EducationField_Marketing<- as.factor(IBM_HR$EducationField_Marketing)
IBM_HR$EducationField_Technical.Degree<- as.factor(IBM_HR$EducationField_Technical.Degree)
IBM_HR$EducationField_Human.Resources<- as.factor(IBM_HR$EducationField_Human.Resources)
IBM_HR$EnvironmentSatisfaction_3<- as.factor(IBM_HR$EnvironmentSatisfaction_3)
IBM_HR$EnvironmentSatisfaction_4<- as.factor(IBM_HR$EnvironmentSatisfaction_4)
IBM_HR$EnvironmentSatisfaction_1<- as.factor(IBM_HR$EnvironmentSatisfaction_1)
IBM_HR$JobInvolvement_2<- as.factor(IBM_HR$JobInvolvement_2)
IBM_HR$JobInvolvement_4<- as.factor(IBM_HR$JobInvolvement_4)
IBM_HR$JobInvolvement_1<- as.factor(IBM_HR$JobInvolvement_1)
IBM_HR$JobLevel_1<- as.factor(IBM_HR$JobLevel_1)
IBM_HR$JobLevel_3<- as.factor(IBM_HR$JobLevel_3)
IBM_HR$JobLevel_4<- as.factor(IBM_HR$JobLevel_4)
IBM_HR$JobLevel_5<- as.factor(IBM_HR$JobLevel_5)
IBM_HR$JobRole_Research.Scientist<- as.factor(IBM_HR$JobRole_Research.Scientist)
IBM_HR$JobRole_Laboratory.Technician<- as.factor(IBM_HR$JobRole_Laboratory.Technician)
IBM_HR$JobRole_Manufacturing.Director<- as.factor(IBM_HR$JobRole_Manufacturing.Director)
IBM_HR$JobRole_Healthcare.Representative<- as.factor(IBM_HR$JobRole_Healthcare.Representative)
IBM_HR$JobRole_Manager<- as.factor(IBM_HR$JobRole_Manager)
IBM_HR$JobRole_Sales.Representative<- as.factor(IBM_HR$JobRole_Sales.Representative)
IBM_HR$JobRole_Research.Director<- as.factor(IBM_HR$JobRole_Research.Director)
IBM_HR$JobRole_Human.Resources<- as.factor(IBM_HR$JobRole_Human.Resources)
IBM_HR$JobSatisfaction_2<- as.factor(IBM_HR$JobSatisfaction_2)
IBM_HR$JobSatisfaction_3<- as.factor(IBM_HR$JobSatisfaction_3)
IBM_HR$JobSatisfaction_1<- as.factor(IBM_HR$JobSatisfaction_1)
IBM_HR$MaritalStatus_Married<- as.factor(IBM_HR$MaritalStatus_Married)
IBM_HR$MaritalStatus_Divorced<- as.factor(IBM_HR$MaritalStatus_Divorced)
IBM_HR$StockOptionLevel_1<- as.factor(IBM_HR$StockOptionLevel_1)
IBM_HR$StockOptionLevel_3<- as.factor(IBM_HR$StockOptionLevel_3)
IBM_HR$StockOptionLevel_2<- as.factor(IBM_HR$StockOptionLevel_2)

#Check the significance of the dummies
chisq.test(IBM_HR$Attrition,IBM_HR$BusinessTravel_Travel_Frequently,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$BusinessTravel_Non.Travel,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$Department_Research...Development,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$Department_Human.Resources,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EducationField_Other,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EducationField_Medical,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EducationField_Marketing,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EducationField_Technical.Degree,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EducationField_Human.Resources,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EnvironmentSatisfaction_3,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EnvironmentSatisfaction_4,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$EnvironmentSatisfaction_1,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobInvolvement_2,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobInvolvement_4,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobInvolvement_1,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobLevel_1,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobLevel_3,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobLevel_4,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobLevel_5,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Research.Scientist,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Laboratory.Technician,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Manufacturing.Director,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Healthcare.Representative,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Manager,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Sales.Representative,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Research.Director,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobRole_Human.Resources,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobSatisfaction_2,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobSatisfaction_3,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$JobSatisfaction_1,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$MaritalStatus_Married,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$MaritalStatus_Divorced,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$StockOptionLevel_1,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$StockOptionLevel_3,simulate.p.value = TRUE)
chisq.test(IBM_HR$Attrition,IBM_HR$StockOptionLevel_2,simulate.p.value = TRUE)

#Remove the insignificant dummy variables
IBM_HR$Department_Human.Resources<- NULL
IBM_HR$EducationField_Other<- NULL
IBM_HR$EducationField_Medical<- NULL
IBM_HR$EducationField_Human.Resources<- NULL
IBM_HR$EnvironmentSatisfaction_3<- NULL
IBM_HR$EnvironmentSatisfaction_4<- NULL
IBM_HR$JobInvolvement_2<-NULL
IBM_HR$JobLevel_3<-NULL
IBM_HR$StockOptionLevel_3<-NULL
IBM_HR$JobSatisfaction_3<-NULL
IBM_HR$JobSatisfaction_2<-NULL
IBM_HR$JobRole_Human.Resources<-NULL
IBM_HR$JobRole_Research.Scientist<-NULL

#Multicollinearity check
numericVars <- which(sapply(IBM_HR, is.numeric))
num_data <- IBM_HR[,numericVars]
correlation_matrix<- cor(num_data,use = "complete.obs")
print(correlation_matrix)
library(xtable)
print(xtable(correlation_matrix), type="html")

#Remove the highly correlated predictors
IBM_HR$YearsWithCurrManager <- NULL
IBM_HR$YearsInCurrentRole<- NULL
IBM_HR$TotalWorkingYears<- NULL

#Split the data into training and testing dataset
smp_size<-floor(0.70*nrow(IBM_HR))
set.seed(123)
train_ind<- sample(seq_len(nrow(IBM_HR)),size=smp_size)
train<- IBM_HR[train_ind,]
test<-IBM_HR[-train_ind,]

#Build the logistic regression model on the training dataset
fit<- glm(Attrition~Age+DailyRate+DistanceFromHome+MonthlyIncome+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+OverTime+BusinessTravel_Travel_Frequently+BusinessTravel_Non.Travel+Department_Research...Development+EducationField_Marketing+EducationField_Technical.Degree+EnvironmentSatisfaction_1+JobInvolvement_4+JobInvolvement_1+JobLevel_1+JobLevel_4+JobLevel_5+JobRole_Laboratory.Technician+JobRole_Manufacturing.Director+JobRole_Healthcare.Representative+JobRole_Manager+JobRole_Sales.Representative+JobRole_Research.Director+JobSatisfaction_1+MaritalStatus_Married+MaritalStatus_Divorced+StockOptionLevel_1+StockOptionLevel_2,data=train,family=binomial(logit))
summary(fit)
ls(fit)
fit$model
coeff<-fit$coef

#Stepwise regression on the model
library(MASS)
fit1<- glm(Attrition~Age+DailyRate+DistanceFromHome+MonthlyIncome+TrainingTimesLastYear+WorkLifeBalance+YearsAtCompany+OverTime+BusinessTravel_Travel_Frequently+BusinessTravel_Non.Travel+Department_Research...Development+EducationField_Marketing+EducationField_Technical.Degree+EnvironmentSatisfaction_1+JobInvolvement_4+JobInvolvement_1+JobLevel_1+JobLevel_4+JobLevel_5+JobRole_Laboratory.Technician+JobRole_Manufacturing.Director+JobRole_Healthcare.Representative+JobRole_Manager+JobRole_Sales.Representative+JobRole_Research.Director+JobSatisfaction_1+MaritalStatus_Married+MaritalStatus_Divorced+StockOptionLevel_1+StockOptionLevel_2,data=train,family=binomial(logit))
step<-stepAIC(fit1,direction="both")
summary(fit1)

#After stepwise regression, build model with the rest of the variables
fit2<- glm(Attrition ~ Age + DailyRate + DistanceFromHome + TrainingTimesLastYear + 
             WorkLifeBalance + YearsAtCompany + OverTime + BusinessTravel_Travel_Frequently + 
             BusinessTravel_Non.Travel + Department_Research...Development + 
             EducationField_Marketing + EducationField_Technical.Degree + 
             EnvironmentSatisfaction_1 + JobInvolvement_1 + JobLevel_1 + 
             JobLevel_5 + JobRole_Laboratory.Technician + JobRole_Manager + 
             JobRole_Research.Director + JobSatisfaction_1 + StockOptionLevel_1 + 
             StockOptionLevel_2+MonthlyIncome, data=train,family=binomial(logit))
summary(fit2)
ls(fit2)
fit2$model
coeff<-fit2$coef
write.csv(coeff, file="D:/data science/IBM HR Analytics/coeff2.csv")

#Concordance Checking
source("D:/data science/BA Class 7,8/Regression - Class Exercises/Concordance.R")
Concordance(fit2)

#After building the model equation, we will find probabilities (p-value) for training dataset
prob= predict(fit2,train,type="response")

#add the probability column to your training dataset
train1<- cbind(train,prob)
View(train1)

#Next, after finding the p-value for training dataset, do the decile analysis for training dataset
train1$Attrition<- as.numeric(as.character(train1$Attrition))
decLocations<- quantile(train1$prob,probs=seq(0.1,0.9,by=0.1))
train1$decile<- findInterval(train1$prob,c(-Inf,decLocations,Inf))
summary(train1$decile)
train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(Attrition), 
                             non_default_cnt=total_cnt -default_cnt )
decile_summ_train<-arrange(decile_summ_train,desc(decile))
View(decile_summ_train)
names(train1)
write.csv(decile_summ_train,file="D:/data science/IBM HR Analytics/fit_train_DA1.csv",row.names = F)

#Next, predict the values for testing dataset
prob=predict(fit2,test,type="response")
test1<- cbind(test,prob)
View(test1)
#Next, do the decile analysis for testing dataset
test1$Attrition<- as.numeric(as.character(test1$Attrition))
decLocations<- quantile(test1$prob,probs=seq(0.1,0.9,by=0.1))
test1$decile<- findInterval(test1$prob,c(-Inf,decLocations,Inf))
summary(test1$decile)
test1$decile<-factor(test1$decile)
decile_grp<-group_by(test1,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(Attrition), 
                            non_default_cnt=total_cnt -default_cnt )
decile_summ_test<-arrange(decile_summ_test,desc(decile))
View(decile_summ_test)
write.csv(decile_summ_test,file="D:/data science/IBM HR Analytics/fit_test_DA1.csv",row.names = F)

#So, from decile analysis report of training and testing dataset, find the cut off and then use that cut off to find predicted values for training dataset
#Finding the predicted values
train1$predicted_Attrition<- ifelse(train1$prob>0.25521,1,0)

#Building the confusion matrix for training dataset
table(train1$prob>0.25521,train1$Attrition)

#ROC Curve for training dataset
require(ROCR)
pred_train_fit<- prediction(train1$prob,train1$Attrition)
performance_fit<- performance(pred_train_fit,"tpr","fpr")
plot(performance_fit)
abline(0, 1)
performance(pred_train_fit, "auc")@y.values

#So, from decile analysis report of training and testing dataset, find the cut off and then use that cut off to find predicted values for testing dataset
#Finding the predicted values
test1$predicted_Attrition<- ifelse(test1$prob>0.1961,1,0)

#ROC Curve for testing dataset
require(ROCR)
pred_test_fit<- prediction(test1$prob,test1$Attrition)
performance_fit<- performance(pred_test_fit,"tpr","fpr")
plot(performance_fit)
abline(0, 1)
performance(pred_train_fit, "auc")@y.values

#Building the confusion matrix for testing dataset
table(test1$prob>0.1961,test1$Attrition)
#=============================#==========================#==================================#
#Report which we got from logitic regression model building
#For training dataset------
#Cut off value for training dataset is 0.25521
#Sensitivity is 53%
#Specificity is 93%
#AUC is 86%
#Accuracy is 85%
#For testing dataset------
#Cut off value for testing dataset is 0.1961
#Sensitivity is 43%
#Specificity is 93%
#AUC is 86%
#Accuracy is 78%
#So, this is an overfitting issue
#============================#===================#===============#=========================#

#Now, with logistic regression model we got accuracy for training dataset as 85% and for
#testing dataset it is 78%.
#Now, we will build the same logistic regression model, but will apply SMOTE before that because 
#this dataset is an imbalanced dataset, which is an imbalanced classification problem.
table(train$Attrition)
#So, Attrition rate is 16% and no attrition rate is 83%
#Now, smote the whole dataset
install.packages("DMwR")
library(DMwR)
train_smote<- SMOTE(Attrition ~ Age + DailyRate + DistanceFromHome + TrainingTimesLastYear + 
                      WorkLifeBalance + YearsAtCompany + OverTime + BusinessTravel_Travel_Frequently + 
                      BusinessTravel_Non.Travel + Department_Research...Development + 
                      EducationField_Marketing + EducationField_Technical.Degree + 
                      EnvironmentSatisfaction_1 + JobInvolvement_1 + JobLevel_1 + 
                      JobLevel_5 + JobRole_Laboratory.Technician + JobRole_Manager + 
                      JobRole_Research.Director + JobSatisfaction_1 + StockOptionLevel_1 + 
                      StockOptionLevel_2+MonthlyIncome,data=train,perc.over = 200 ,perc.under = 200)
table(train_smote$Attrition)

fit3<- glm(Attrition ~ Age + DailyRate + DistanceFromHome + TrainingTimesLastYear + 
             WorkLifeBalance + YearsAtCompany + OverTime + BusinessTravel_Travel_Frequently + 
             BusinessTravel_Non.Travel + Department_Research...Development + 
             EducationField_Marketing + EducationField_Technical.Degree + 
             EnvironmentSatisfaction_1 + JobInvolvement_1 + JobLevel_1 + 
             JobLevel_5 + JobRole_Laboratory.Technician + JobRole_Manager + 
             JobRole_Research.Director + JobSatisfaction_1 + StockOptionLevel_1 + 
             StockOptionLevel_2+MonthlyIncome, data=train_smote,family=binomial(logit))
summary(fit3)
ls(fit3)
fit3$model
coeff<-fit3$coef
write.csv(coeff, file="D:/data science/IBM HR Analytics/coeff3.csv")

#Concordance Checking
source("D:/data science/BA Class 7,8/Regression - Class Exercises/Concordance.R")
Concordance(fit3)
#Concordance is 85% for training dataset

#After building the model equation, we will find probabilities (p-value) for training dataset
prob= predict(fit3,train_smote,type="response")

#add the probability column to your training dataset
train_smote1<- cbind(train_smote,prob)
View(train_smote1)

#Next, after finding the p-value for training dataset, do the decile analysis for training dataset
train_smote1$Attrition<- as.numeric(as.character(train_smote1$Attrition))
decLocations<- quantile(train_smote1$prob,probs=seq(0.1,0.9,by=0.1))
train_smote1$decile<- findInterval(train_smote1$prob,c(-Inf,decLocations,Inf))
summary(train_smote1$decile)
train_smote1$decile<-factor(train_smote1$decile)
decile_grp<-group_by(train_smote1,decile)
decile_summ_train_smote<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(Attrition), 
                             non_default_cnt=total_cnt -default_cnt )
decile_summ_train_smote<-arrange(decile_summ_train_smote,desc(decile))
View(decile_summ_train_smote)
names(train_smote1)
write.csv(decile_summ_train_smote,file="D:/data science/IBM HR Analytics/fit_train_smote_DA1.csv",row.names = F)

#Next, predict the values for testing dataset
prob=predict(fit3,test,type="response")
test2<- cbind(test,prob)
View(test2)
#Next, do the decile analysis for testing dataset
test2$Attrition<- as.numeric(as.character(test2$Attrition))
decLocations<- quantile(test2$prob,probs=seq(0.1,0.9,by=0.1))
test2$decile<- findInterval(test2$prob,c(-Inf,decLocations,Inf))
summary(test2$decile)
test2$decile<-factor(test2$decile)
decile_grp<-group_by(test2,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(Attrition), 
                            non_default_cnt=total_cnt -default_cnt )
decile_summ_test<-arrange(decile_summ_test,desc(decile))
View(decile_summ_test)
write.csv(decile_summ_test,file="D:/data science/IBM HR Analytics/fit_test_smote_DA1.csv",row.names = F)

#So, from decile analysis report of training and testing dataset, find the cut off and then use that cut off to find predicted values for training dataset
#Finding the predicted values
train_smote1$predicted_Attrition<- ifelse(train_smote1$prob>0.49193,1,0)

#Building the confusion matrix for training dataset
table(train_smote1$prob>0.49193,train_smote1$Attrition)

#ROC Curve for training dataset
require(ROCR)
pred_train_smote_fit<- prediction(train_smote1$prob,train_smote1$Attrition)
performance_fit<- performance(pred_train_smote_fit,"tpr","fpr")
plot(performance_fit)
abline(0, 1)
performance(pred_train_smote_fit, "auc")@y.values


#So, from decile analysis report of training and testing dataset, find the cut off and then use that cut off to find predicted values for testing dataset
#Finding the predicted values
test2$predicted_Attrition<- ifelse(test2$prob>0.50116,1,0)

#ROC Curve for testing dataset
require(ROCR)
pred_test_fit<- prediction(test2$prob,test2$Attrition)
performance_fit<- performance(pred_test_fit,"tpr","fpr")
plot(performance_fit)
abline(0, 1)
performance(pred_train_fit, "auc")@y.values

#Building the confusion matrix for testing dataset
table(test2$prob>0.50116,test2$Attrition)

#so after SMOTE, accuracy for training dataset is 79% and accuracy for testing dataset is 78%
#AUC for training datset is 85%
#AUC for testing dataset is 86%
#So in SMOTE, perform SMOTE on training dataset, then build the model and then validate 
#on the original testing dataset without using SMOTE on testing dataset.
