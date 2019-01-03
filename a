library(data.table)
library(InformationValue)
library(car)
library(caret)
library(dplyr)
options(scipen = 999)

cust <- fread("~/Case studies/Logistic Regression/Proactive Attrition Management-Logistic Regression Case Study.csv",stringsAsFactors = T)

str(cust)

# Changing the class of the variables to the right one
cust$CUSTOMER <- as.factor(cust$CUSTOMER)
# customerID and CSA values are removed because they can overfit the model because of the numerous levels and to avoid overfitting. 
cust$CUSTOMER <- NULL
cust$CSA <- NULL
# Variables that show value is missing or not will be removed`
cust$INCMISS <- NULL
cust$SETPRCM <- NULL

# Getting to know the variables better
about <- function(x){
  
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = (mean(is.na(x)))*100
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
  
}

about_int <- function(x){
  
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = (mean(is.na(x)))*100
  range = max(x, na.rm=T)-min(x, na.rm=T)
  fre = table(x)
  prop = prop.table(table(x))
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct,range=range))
  
}

integs <- names(cust)[sapply(cust, is.integer)]
abt_nums <- as.data.frame(t(sapply(cust[,!integs, with = F],about)))
# all the missing values here are less than 1% of the dataset, so they are imputed with mean
cust_nums <- cust[,!integs, with = F]
churndep <- cust_nums$CHURNDEP
cust_nums$CHURNDEP <- NULL
fwrite(abt_nums,"~/Case studies/Logistic Regression/abt_nums.csv",row.names = T)

sapply(cust_nums, boxplot)
sapply(cust_nums, hist)
# None of the variables are normally distributed. Percentiles will be used to cap
# Percentiles have shown that all variables need to be capped with 99 percntile values

# Capping with 99 percentile value
outlier_treat_99 <- function(x){
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  return(x)
}
cust_nums <- data.table(apply(cust_nums,2, FUN = outlier_treat_99))

miss_treat_num = function(x){
  x[is.na(x)] = mean(x,na.rm=T) # replace missings with mean
  return(x)
}
cust_nums <- data.table(apply(cust_nums,2, FUN = miss_treat_num))

# now looking at integers
abt_ints <- as.data.frame(t(sapply(cust[,integs, with = F],about_int)))
# missing values in age will be imputed with mean and the rest of the missing will values be removed 
cust_ints <- cust[,integs, with = F]
cust_ints$AGE1[is.na(cust_ints$AGE1)] <- ceiling(mean(cust_ints$AGE1, na.rm = T))
cust_ints$AGE2[is.na(cust_ints$AGE2)] <- ceiling(mean(cust_ints$AGE2, na.rm = T))

cust_ <- cbind(cust_ints,cust_nums) 
cust_ <- cbind(cust_,churndep)
# Remaining missing values are removed
cust_ <- cust_[complete.cases(cust_[,1:25])]
# Removing Insignificant variables
cust_$CHURN <- NULL
cust_$marryun <- NULL

# Creating validation and calibration data
valid <- cust_[CALIBRAT == 0]
calib <- cust_[CALIBRAT == 1]

calib$CALIBRAT <- NULL
valid$CALIBRAT <- NULL


table(calib$churndep)
# as both the groups are equal, there is no bias in the data.

#form <- churndep~MONTHS+UNIQSUBS+ACTVSUBS+PHONES+MODELS+EQPDAYS+AGE1+AGE2+CHILDREN+CREDITA+CREDITAA+CREDITB+
#  CREDITC+CREDITGY+CREDITZ+PRIZMRUR+PRIZMUB+PRIZMTWN+REFURB+WEBCAP+TRUCK+RV+OCCPROF+OCCCLER+OCCCRFT+OCCSTUD+
#  OCCHMKR+OCCRET+OCCSELF+OWNRENT+MARRYUN+MARRYYES+MARRYNO+MAILORD+MAILRES+MAILFLAG+TRAVEL+PCOWN+CREDITCD+RETCALLS+
#  RETACCPT+NEWCELLY+NEWCELLN+REFER+INCOME+MCYCLE+CREDITAD+RETCALL+REVENUE+MOU+RECCHRGE+DIRECTAS+OVERAGE+ROAM+CHANGEM+
#  CHANGER+DROPVCE+BLCKVCE+UNANSVCE+CUSTCARE+THREEWAY+MOUREC+OUTCALLS+INCALLS+PEAKVCE+OPEAKVCE+DROPBLK+CALLFWDV+CALLWAIT+
#  SETPRC

fit1 <- glm(churndep~.,data = calib,family = binomial(logit),maxit=100)
summary(fit1)

form <- churndep~MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+AGE2+CHILDREN+CREDITB+CREDITDE+PRIZMUB+REFURB+
  WEBCAP+MARRYUN+RETACCPT+NEWCELLY+INCOME+CREDITAD+RETCALL+REVENUE+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+
  CUSTCARE+THREEWAY+INCALLS+PEAKVCE+DROPBLK+CALLWAIT+SETPRC

fit2 <- glm(form,data = calib,family = binomial(logit),maxit=100)
summary(fit2)

form <- churndep~MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+AGE2+CHILDREN+CREDITB+CREDITDE+PRIZMUB+REFURB+
  WEBCAP+MARRYUN+RETACCPT+NEWCELLY+CREDITAD+RETCALL+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+
  CUSTCARE+THREEWAY+INCALLS+PEAKVCE+DROPBLK+SETPRC

fit3 <- glm(form,data = calib,family = binomial(logit),maxit=100)
summary(fit3)

# VIF values are looked to see if there is any high multicollinearity
vif(fit3)
# VIF of all values are below required levels

# Calculating the psudo R-squared value of Logistic regression
pR2 <- 1- fit3$deviance/fit3$null.deviance
# The psudo R-squared value for this model is very low

predicted <- predict(fit3, calib, type="response")

cutoff <- optimalCutoff(calib$churndep,predicted)


# Getting the confusion matrix
ConfMat <- InformationValue::confusionMatrix(calib$churndep, predicted, threshold = cutoff)
ConfMat

ConfMat <- as.data.frame(ConfMat,row.names = c("Actual NO","Actual YES"))
colnames(ConfMat) <- c("Predicted NO","Predicted YES")
ConfMat

# Testing the model accuracy
InformationValue::sensitivity(calib$churndep, predicted, threshold = cutoff)

InformationValue::specificity(calib$churndep, predicted, threshold = cutoff)

InformationValue::misClassError(calib$churndep, predicted, threshold = cutoff)

InformationValue::plotROC(calib$churndep, predicted,Show.labels = T)

InformationValue::AUROC()

# In this model it is seen that both sensitivity and specificity are greater than 0.5 but the accuracy is still low as
# the values are still below the desierable 0.7. The misclassification error is high but not enough to make the model invalid.
# The AUROC is 0.62 which is still less than a 0.7 desierable level.

# Conducting decile analysis

calib <- cbind(calib,pred = predicted)

decLocations <- quantile(calib$pred, probs = seq(0.1,0.9,by=0.1))
calib$decile <- findInterval(calib$pred,c(-Inf,decLocations, Inf))

calib <- data.table(calib)

fit_train_DA <- calib %>% group_by(decile) %>% dplyr::summarize(Min_prob = min(pred),
                                                                 Max_prob = max(pred),
                                                                 churn_Count = sum(churndep),
                                                                 Non_churn_Count = (length(decile)-sum(churndep))
                                                                ) %>% arrange(decile)
fit_train_DA <- dplyr::arrange(fit_train_DA, desc(decile))  
fit_train_DA



DecilePlot <-  ggplot2::ggplot(data = fit_train_DA,aes(x = factor(-decile), y = churn_Count))
DecilePlot <- DecilePlot + geom_bar(stat = 'identity',fill = "red") 
DecilePlot <- DecilePlot + xlab("The Deciles") + ylab("No of churn Customers")
DecilePlot <- DecilePlot + ggtitle("Decile Analyis Graph for customers")

DecilePlot
calib$pred_churn <- NULL
calib$pred_churn <- ifelse(calib$pred>cutoff, 1,0)

# The declile plot shows that the deciles do form step shape and can be concluded that the model is still suitable to use for prediction.

valid<-cbind(valid, Pred=predict(fit3, valid, type="response"))
valid$churn <- ifelse(valid$Pred>cutoff, 1,0)
sum(valid$churn)

# This model says that there are 13,888 customers who are about to churn.


# Inorder to get better accuracy and ROC ML models are used.

calib <- cust_[CALIBRAT == 1]

calib$CALIBRAT <- NULL

calib$churndep <- factor(calib$churndep, levels = c(0,1), labels = c("No_Churn","Churn"))
library(rsample)

set.seed(123)
split <- initial_split(calib, prop = .7, strata = "churndep")
train <- training(split)
test <- testing(split)

# Setting parameters and building the model
control <- trainControl(method = "cv", number = 5, classProbs = TRUE)

model1 <- train(churndep~MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+AGE2+CHILDREN+CREDITB+CREDITDE+PRIZMUB+
                  WEBCAP+RETACCPT+NEWCELLY+CREDITAD+MOU+RECCHRGE+ROAM+CHANGEM+CHANGER+
                  CUSTCARE+INCALLS+PEAKVCE+DROPBLK+SETPRC,
                data = train,method = "rf",metric = "ROC",tuneLength = 5,trControl = control, preProcess=c("scale","center"))
model1

aa <- predict(model1,test, type = "prob")
confusionMatrix(aa$Churn,test$churndep)
test$churndep <- as.numeric(test$churndep)
test$churndep <- ifelse(test$churndep==1,0,1)

InformationValue::AUROC(test$churndep,aa$Churn)


model2 <- train(churndep~MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+AGE2+CHILDREN+CREDITB+CREDITDE+PRIZMUB+REFURB+
                  WEBCAP+MARRYUN+RETACCPT+NEWCELLY+CREDITAD+RETCALL+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+CHANGER+
                  CUSTCARE+THREEWAY+INCALLS+PEAKVCE+DROPBLK+SETPRC
                ,data = train,method = "rpart",metric = "ROC",tuneLength = 5,trControl = control, preProcess="scale")
model2

aa <- predict(model2,test)
confusionMatrix(aa,test$churndep)

varImp(model2)

control <- trainControl(method = "repeatedcv", number = 5)
model3 <- train(stars~.,data = train,method = "svm",metric = "Accuracy",tuneLength = 5,trControl = control, preProcess="scale")
model3


# -----------------------------------------------------------
modelchi <- fit3$null.deviance-fit2$deviance
chidf <- fit3$df.null-fit2$df.residual
chisq.prob <- 1-pchisq(modelchi,chidf)



######### create ordinal factors for all ordinals
