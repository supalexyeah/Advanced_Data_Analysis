library(woe)
library(smbinning)
library(tcltk)
library(InformationValue)
library(car)

###### Import Data ######
HOS=read.csv(file="~/desktop/employees_v2.csv")

###### Check Class Bias ######
table(HOS$IS_TURNOVER)

###### Create Training and Test Sample ######
# Create Training Data
HOS1=HOS[which(HOS$IS_TURNOVER==1),]  #all 1"s
HOS0=HOS[which(HOS$IS_TURNOVER==0),]  #all 0's
set.seed(100)  #for repeatability of samples
HOS1_TrainingRows=sample(1:nrow(HOS1),0.7*nrow(HOS1))  #1's for training
HOS0_TrainingRows=sample(1:nrow(HOS0),0.7*nrow(HOS1))  #0'S for training. pick as many 0's as 1's
Training1=HOS1[HOS1_TrainingRows,]
Training0=HOS0[HOS0_TrainingRows,]
TrainingData=rbind(Training1, Training0)  #row bind the 1's and 0's
# Create Test Data
Test1=HOS1[-HOS1_TrainingRows, ]
Test0=HOS0[-HOS0_TrainingRows, ]
TestData=rbind(Test1, Test0)  # row bind the 1's and 0's 

###### Compute information value to find out important variables ######
# segregate continuous and factor variables
factor_vars=c("PEER_GROUP_CAT_ABBR","PARTNERSHIP_TYPE","SKILL_CATEGORY_ABBR")
continuous_vars=c("FTE", "SENIORITY", "STAFF_COUNT", "UTO_COUNT","PTO_COUNT","TARDY_COUNT","TOTAL_HOURS","OT_HRS","LEAKAGE_HRS","EXTRA_HRS","CANCELL_COUNT","TRAIN_COUNT","TIMEOFF_REQ_COUNT","EXCUSED_TIMEOFF_COUNT","CRITICAL_STAFFING_COUNT","HIGH_STAFFING_COUNT","OPEN_SHIFT_PICK_COUNT","WEEKEND_COUNT","WEEKEND_HOURS")
iv_df=data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(22))  # init for IV results
# compute IV for categoricals
for(factor_var in factor_vars){
  smb=smbinning.factor(TrainingData, y="IS_TURNOVER", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"]=smb$iv
  }
}
# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb=smbinning(TrainingData, y="IS_TURNOVER", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"]=smb$iv
  }
}
iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df

###### Build Logit Models and Predict ######
LogitMod=glm(IS_TURNOVER ~ TOTAL_HOURS + OT_HRS + LEAKAGE_HRS + WEEKEND_HOURS + WEEKEND_COUNT + CRITICAL_STAFFING_COUNT + SENIORITY, data=TrainingData, family=binomial(link="logit"))
predicted=plogis(predict(LogitMod, TestData))  # predicted scores
###### Decide on optimal prediction probability cutoff for the model ######
optCutOff=optimalCutoff(TestData$IS_TURNOVER, predicted)[1]
#=> 0.85

###### Model Diagnostics ######
# Hypothesis test
summary(LogitMod)
# remove insignificant x variables
LogitMod2=glm(IS_TURNOVER ~ TOTAL_HOURS + OT_HRS + WEEKEND_HOURS + WEEKEND_COUNT + CRITICAL_STAFFING_COUNT, data=TrainingData, family=binomial(link="logit"))
summary(LogitMod2)
# VIF
vif(LogitMod2)
# Remove WEEKEND_HOURS
LogitMod3=glm(IS_TURNOVER ~ TOTAL_HOURS + OT_HRS + WEEKEND_COUNT + CRITICAL_STAFFING_COUNT, data=TrainingData, family=binomial(link="logit"))
summary(LogitMod3)
vif(LogitMod3)
predicted1=plogis(predict(LogitMod3, TestData))  # predicted scores
optCutOff1=optimalCutoff(TestData$IS_TURNOVER, predicted1)
# Remove WEEKEND_COUNT
LogitMod4=glm(IS_TURNOVER ~ TOTAL_HOURS + OT_HRS + WEEKEND_HOURS + CRITICAL_STAFFING_COUNT, data=TrainingData, family=binomial(link="logit"))
summary(LogitMod4)
vif(LogitMod4)
predicted2=plogis(predict(LogitMod4, TestData))  # predicted scores
optCutOff2=optimalCutoff(TestData$IS_TURNOVER, predicted2)
# Misclassification Error
misClassError2 = misClassError(TestData$IS_TURNOVER, predicted2, threshold = optCutOff2)
misClassError1 = misClassError(TestData$IS_TURNOVER, predicted1, threshold = optCutOff1)
# ROC
plotROC(TestData$IS_TURNOVER, predicted1)
plotROC(TestData$IS_TURNOVER, predicted2) 
# Concordance
Concordance2 = Concordance(TestData$IS_TURNOVER, predicted2)
# Specificity and Sensitivity
sensitivity(TestData$IS_TURNOVER, predicted2, threshold = optCutOff2)
specificity(TestData$IS_TURNOVER, predicted2, threshold = optCutOff2)
# Confusion Matrix
confusionMatrix(TestData$IS_TURNOVER, predicted2, threshold = optCutOff2)

##########################
t.test(HOS$TOTAL_HOURS~HOS$IS_TURNOVER)
t.test(HOS$OT_HRS~HOS$IS_TURNOVER)
t.test(HOS$WEEKEND_HOURS~HOS$IS_TURNOVER)
t.test(HOS$WEEKEND_COUNT~HOS$IS_TURNOVER)
t.test(HOS$LEAKAGE_HRS~HOS$IS_TURNOVER)
t.test(HOS$SENIORITY~HOS$IS_TURNOVER)
LogitMod0=glm(IS_TURNOVER ~ TOTAL_HOURS + OT_HRS + WEEKEND_HOURS + CRITICAL_STAFFING_COUNT + LEAKAGE_HRS + SENIORITY, data=TrainingData, family=binomial(link="logit"))
vif(LogitMod0)
predicted0=plogis(predict(LogitMod0, TestData)) 
plotROC(TestData$IS_TURNOVER, predicted0)
