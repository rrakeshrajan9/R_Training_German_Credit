#==============================================================================================
# WILL THE CUSTOMER DEFAULT ON THE LOAN?
#==============================================================================================


#INTRODUCTION
#=============================================================================================






#1. DATA (GERMAN CREDIT DATA)
#=============================================================================================

credit = read.csv('C:\\Users\\HP\\Desktop\\assignments\\Model Interpretation\\4th Session\\germancredit.csv')
View(credit)



#2. Get a basic understanding of the dataset
#=============================================================================================

dim(credit)
str(credit)

table(credit$Default_)



#3. ANALYZE THE BAD RATE ACCROSS SEVERAL INDIVIDUAL VARIABLES
#=============================================================================================

install.packages("sqldf")

require(sqldf)

#Bad rate for credit history

sqldf("SELECT history, round(avg(Default_)*100,2) AS 'Bad Rate',
      (100 - round(avg(Default_)*100,2)) AS 'Good Rate'
      FROM credit GROUP BY history")



#Calculating the Bad Rate for all the categorical variables

for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    string = paste("SELECT", varname, ", round(avg(Default_)*100,2) AS 'Bad Rate', (100 - round(avg(Default_)*100,2)) AS 'Good Rate' FROM credit GROUP BY", varname)
    
    #print(sqldf(string))
    
    a = sqldf(string)
    print(a)
    
  }
}



#A better way...
#Export the tables in an excel workbook in separate worksheets

install.packages("xlsx")
library(xlsx)
wb = createWorkbook()

for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    
    varname = names(credit)[i]
    
    sheet = createSheet(wb, varname)
    
    string = paste("SELECT", varname, ", round(avg(Default_)*100,2) AS 'Bad Rate', (100 - round(avg(Default_)*100,2)) AS 'Good Rate' FROM credit GROUP BY", varname)
    
    addDataFrame(sqldf(string), sheet = sheet, startColumn = 2, row.names = F)
    
  }
}

saveWorkbook(wb, "Bad Rate.xlsx")





#3. EXPLORING THE DATA VISUALLY
#=============================================================================================

#A. UNIVARIATE ANALYSIS

#Histogram or barplots for numerical variables


for(i in 1:ncol(credit))
{
  if(is.numeric(credit[,i]))
  {
    if(length(unique(credit[,i])) > 10)
    {
      hist(credit[,i], main = names(credit)[i], xlab = names(credit)[i])
    }
      
    else if(length(unique(credit[,i])) < 10)
    {
      barplot(table(credit[,i]), main=names(credit)[i], xlab = names(credit)[i])
    }
  }
}


#Barplots for categorical varibales
for(i in 1:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
     barplot(table(credit[,i]), main=names(credit)[i], xlab = names(credit)[i])
  }
}



#B. BIVARIATE ANALYSIS

#Side-by-side Boxplots for numerical variables
for(i in 2:ncol(credit))
{
  if(is.numeric(credit[,i]))
  {
    if(length(unique(credit[,i])) > 10)
    {
      boxplot(credit[,i] ~ credit$Default_, main = names(credit)[i], ylab = names(credit)[i])
    }
      
    else if(length(unique(credit[,i])) < 10)
    {
      barplot(table(credit[,i], credit$Default_), main=names(credit)[i], 
              xlab = names(credit)[i], beside = T, legend = rownames(table(credit[,i])))
    }
  }
}






#4. RE-GROUPING THE LEVELS OF THE CATEGORICAL VARIABLES
#=============================================================================================


library(xlsx)
library(InformationValue)



#Calculate the WOE table for the variable history

WOETable(credit$history, credit$Default_)



#Exporting the WOE Table for every categorical variables in excel workbook
wb = createWorkbook()
getwd()
for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    
    varname = names(credit)[i]
    
    sheet = createSheet(wb, varname)
    
    woe = WOETable(credit[,varname], credit$Default_)
    
    addDataFrame(woe, sheet = sheet, startColumn = 2, row.names = F)
    
  }
}

saveWorkbook(wb, "WOE Table 2.xlsx")




#re-level the credit history and a few other variables
credit2 = credit
str(credit2)

#VARIABLE: Credit_Default


credit2$Default_ = factor(credit2$Default_)

#VARIABLE: Credit history
credit2$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit2$history) = c("good","good","poor","poor","terrible")

#VARIABLE; Credit_foreign
credit2$foreign_ = factor(credit2$foreign_, levels=c("A201","A202"),
                          labels=c("foreign","german"))
table(credit2$foreign_)
table(credit$foreign_)

#VARIABLE: Credit$Property
credit2$property_ = factor(credit2$property, levels=c("A121","A122","A123","A124"),
                          labels=c("real_estate","building society savings","car", "No property"))
#VARIABLE: Purpose
credit2$purpose = factor(credit$purpose, levels=c("A41","A48","A43","A42","A44","A49","A45","A40","A410","A46"))
levels(credit2$purpose) = c("Re-training","Used-car","Radio TV", rep("Furniture and Domestic app.",3),
                            rep("Business or Repairs",2), "New car", "Education and Others")   #[Check the no. of levels]

#Varaible Savings
credit2$savings = factor(credit2$savings, levels = c("A61","A62","A63","A64","A65"))
levels(credit2$savings) = c("Very low", "low", "Average",'Good', 'No savings acc')


#Variable Checking account
credit2$checkingstatus1 = factor(credit2$checkingstatus1, levels = c("A11","A12","A13","A14"))
levels(credit2$checkingstatus1) = c("Low_Deposit", "Average_Deposit", "High_Deposit","No_Deposit")

View(credit2)






#UNDERSTANDING VARIABLE IMPORTANCE
#=============================================================================================
library(InformationValue)

#Using IV to understand the imporance of the categorical variables

for(i in 2:ncol(credit))
{
  if(is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    print(varname)
    print(IV(X=factor(credit[,varname]), Y=credit$Default_))
    
  }
}




#Using t-test to understand the imporance of the numerical variables

importance = c()

for(i in 2:ncol(credit))
{
  if(!is.factor(credit[,i]))
  {
    varname = names(credit)[i]
    
    p_value = t.test(credit[,varname] ~ credit$Default_)$p.value
    
    importance[varname] = round(p_value,5)
  
  }
}

(importance)



#Note: You can categorize the numerical variables in bins and calculate the WOE for
#separate bins. Group the bins with similar WOE in sequential manner. Finally calculate
#The IV to determine the variable importance.



#TRAIN-TEST SPLIT
#=============================================================================================
library(caTools)
set.seed(88)
split <- sample.split(credit2$Default_, SplitRatio = 0.75)

#get training and test data
train <- subset(credit2, split == TRUE)
test  <- subset(credit2, split == FALSE)

View(train)



#FITTING A LOGISTIC REGRESSION MODEL
#=============================================================================================
names(credit2)

#Using some important variables to fit a Logistic Regression Model

#IMPORTANT CATEGORICAL VARIABLES: 
#("checkingstatus1", "history", "purpose_new", "savings", "property")

#IMPORTANT NUMERICAL VARIABLES: 
#("duration", "amount", "installment", "age")


cat_var = c("checkingstatus1", "history", "purpose", "savings", "property", 'Default_')
num_var = c("duration", "amount", "installment", "age")
credit_new = credit2[,c(cat_var, num_var)]
names(credit_new)


#Creating dummy variables
str(credit_new)
x = model.matrix(Default_ ~ ., credit_new)[,-1] #model.matrix automatically creates dummy variables for factor variables
View(x)   
y = credit_new$Default_


#get training and test data

library(caTools)
set.seed(88)
split <- sample.split(credit_new$Default_, SplitRatio = 0.75)
train <- subset(credit_new, split == TRUE)
test  <- subset(credit_new, split == FALSE)

View(train)
X_train = (model.matrix(Default_ ~ ., train)[,-1])
View(X_train)
X_test = (model.matrix(Default_ ~ ., test)[,-1])
y_train = (train$Default_)
y_test = test$Default_
#MODEL SELECTION
#=================================================================================================
#Refer to this artical:
#http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf

names(X_test)
#Logistic regression on train dataset, fitting all the variables 
head(X_train)
fullmod = glm(Default_ ~ ., family = binomial, data = data.frame(Default_ = y_train, X_train))
?glm
summary(fullmod)

#AIC: 749.87 for full model

#Significant variables for loan default from this model are checkingstatus1High_Deposit, checkingstatus1No_Deposit
#historypoor, historyterrible, purposeFurniture and Domestic app., purposeBusiness or Repairs, purposeEducation and Others
# savingsNo savings acc and SavingsGood

#Comparing the model with Null model to check the AIC value and deviance values 

nothing <- glm(Default_ ~ 1, family = binomial, data = data.frame(Default_ = y_train, X_train))
summary(nothing) #AIC value is 916.3 greater than fullmod 

#Now using stepwise logistic regression


backwards = step(fullmod,direction = 'backward')

summary(backwards)
all = step(fullmod)

summary(all)
plot(all,scale='bic')

coef(all,8)

# Both backward and all in method of stepwise algorithm lead to a same AIC value of 739.77 with the variables
#checkingstatus1High_Deposit, checkingstatus1No_Deposit 
#historypoor  , historyterrible  , purposeFurniture.and.Domestic.app. 
#purposeBusiness.or.Repairs   , purposeEducation.and.Others , savingsGood 
#savingsNo.savings.acc  , duration , amount 
#installment and age 




#Predict and evaluate on test data set 

pred = predict(all, newdata = data.frame(X_test), type = 'response')
data.frame(y_test,pred)

#CHOICE OF CUT-OFF
#==================================================================================================

#Assume that lending into default is 5 times as costly as not lending to a good debtor
#(Assume that this later cost is 1). Here the default is taken as "success". suppose we 
#estimate a certain p for probability of default.
#
#Then, Expected Cost = 5p, if we make a loan
#      Expected Cost = 1(1 - p), if we refuse the loan
#
#if 5p < (1 - p), we expect to lose less by loaning than by turning away business
#
#i.e. make loan if the probability of default is < 1/6
#Or,  predict 1, if p > 1/6
#
#[This is an example where knowledge about the relative cost of misclassification impacts
#the choice of probability cutoff]
#
#Reference:
#Book: Data Mining and Business Analytics with R - Ledolter
#(Section 7.9 para: 2)






library(ROCR)
ROCpred <- prediction(pred,test$Default_)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)
plot(ROCperf, colorize=T, 
     print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

auroc = AUROC(test$Default_, pred)
auroc
#AUROC is 0.77







#MODEL VALIDATION
#=================================================================================================

#threshold = 0.16
pred1 = ifelse(pred > 0.16, 1, 0)


















#PASSESSING MODEL PERFORMANCE
#=================================================================================================


#Confusion matrix
table(test$Default_, pred1)
table(credit$Default_)
#Sensitivity
sensitivity(test$Default_, pred1)

#Specificity
specificity(test$Default_, pred1)

#Precision
precision(test$Default_, pred1)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(test$Default_, pred1)

#Mis-classification Error
misClassError(test$Default_, pred)






#Life chart and Gain Chart
#--------------------------------------------------------------------------



newdata = data.frame(y_test,pred)
View(newdata)
newdata = newdata[order(-newdata$pred), ]


nrow(newdata)/10
?rep
groups = rep(1:10,each=floor(nrow(newdata)/10))

extra = rep(10, nrow(newdata)-length(groups))
length(groups)
nrow(newdata)

groups = c(groups,extra)


newdata$groups = groups
View(newdata)
newdata

library(sqldf)
gainTable = sqldf("select groups, count(pred) as N, sum(pred) as N1 from newdata group by groups ")

gainTable$cumN1 = cumsum(gainTable$N1)
gainTable$Gain = round(gainTable$cumN1/sum(gainTable$N1)*100,3)
gainTable$Lift = round(gainTable$Gain/((1:10)*10),3)

gainTable


plot(gainTable$groups, gainTable$Gain, type="b", 
     main = "Gain Plot",
     xlab = "Groups", ylab = "Gain")



plot(gainTable$groups, gainTable$Lift, type="b", 
     main = "Lift Plot",
     xlab = "Groups", ylab = "Lift")




#K-S
ks = sqldf("select groups, count(pred) as N, sum(pred) as N1, 
           count(pred)-sum(pred) as N0 from newdata group by groups ")

ks$cumN1 = cumsum(ks$N1)
ks$cumN2 = cumsum(ks$N0)
ks$cumPerN1 = round(ks$cumN1/sum(ks$cumN1)*100,3)
ks$cumPerN2 = round(ks$cumN2/sum(ks$cumN2)*100,3)
ks$KS = ks$cumPerN1 - ks$cumPerN2
ks

plot(ks$groups, ks$cumPerN1, type="l", col="red")
lines(ks$groups, ks$cumPerN2, col="blue")

plot(ks$groups, ks$cumPerN2, type="l", col="red")
lines(ks$groups, ks$cumPerN1, col="blue")

library(InformationValue)
ks_plot(y_test, pred)


#Hosmer - Lameshow Goodness of Fit
#------------------------------------------------------------------------

install.packages("MKmisc")


library(MKmisc)
HLgof.test(fit = pred, obs = test$Default_)


install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(test$Default_, pred, g=10)















#HANDLING CLASS IMBALANCEDNESS
#=================================================================================================
library(ROSE)  #Randomly Over sampling examples
table(train$Default_)

#Oversampling
over = ovun.sample(Default_~., data = train, method = "over", N = 1050)$data
table(over$Default_)
?ovun.sample


summary(over)

#Building model on this oversampled data and checking for improvement in sensitivity 

X_over = (model.matrix(Default_ ~ ., over)[,-1])
View(X_over)
y_over = (over$Default_)
over_mod = glm(Default_ ~ ., family = binomial, data = data.frame(Default_ = y_over, X_over))
summary(over_mod)

back_over = step(over_mod,direction = 'backward')

summary(back_over)
all = step(over_mod)

summary(all)  #We can see that AIC value has increased to 1050 and it is performing worse than the original model without sampling technique
plot(all,scale='bic')

pred = predict(all, newdata = data.frame(X_test), type = 'response')
#threshold = 0.16
pred1 = ifelse(pred > 0.16, 1, 0)
#Checking the sensitivity, precision and confusion matrix
#Confusion matrix
table(test$Default_, pred1)

#Sensitivity
sensitivity(test$Default_, pred1)  #Sensitivity increased from 0.85 to 0.93 though the overall accuracy had dropped.

#Specificity
specificity(test$Default_, pred1)

#Precision
precision(test$Default_, pred1)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(test$Default_, pred1)

#Mis-classification Error
misClassError(test$Default_, pred)


#Undersampling
under =  ovun.sample(Default_~., data = train, method = "under", N = 450)$data
table(under$Default_)

summary(under)

#Building model on this undersampled data and checking for improvement in sensitivity 

X_under = (model.matrix(Default_ ~ ., under)[,-1])
View(X_over)
y_under = (under$Default_)
under_mod = glm(Default_ ~ ., family = binomial, data = data.frame(Default_ = y_under, X_under))
summary(under_mod)

back_under = step(under_mod,direction = 'backward')

summary(back_under)
all_under = step(under_mod)

summary(all_under)
#plot(all,scale='bic')
pred = predict(all_under, newdata = data.frame(X_test), type = 'response')
#threshold = 0.16
pred1 = ifelse(pred > 0.16, 1, 0)
#Checking the sensitivity, precision and confusion matrix
#Confusion matrix
table(test$Default_, pred1)

#Sensitivity
sensitivity(test$Default_, pred1)  #Sensitivity increased from 0.85 to 0.973 for undersampling
#though the overall accuracy had dropped.

#Specificity
specificity(test$Default_, pred1)

#Precision
precision(test$Default_, pred1)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(test$Default_, pred1)

#Mis-classification Error
misClassError(test$Default_, pred)


#Both sampling techniques 
both = ovun.sample(Default_~., data = train, method = 'both', N = 750)$data
table(both$Default_)

summary(both)

#Building model on this undersampled data and checking for improvement in sensitivity 

X_both = (model.matrix(Default_ ~ ., both)[,-1])
View(X_both)
y_both = (both$Default_)
both_mod = glm(Default_ ~ ., family = binomial, data = data.frame(Default_ = y_both, X_both))
summary(both_mod)

back_both = step(both_mod,direction = 'backward')

summary(back_both)
all_both = step(both_mod)

summary(all_both)
#plot(all,scale='bic')
pred = predict(all_both, newdata = data.frame(X_test), type = 'response')
#threshold = 0.16
pred1 = ifelse(pred > 0.16, 1, 0)
#Checking the sensitivity, precision and confusion matrix
#Confusion matrix
table(test$Default_, pred1)

#Sensitivity
sensitivity(test$Default_, pred1)  #Sensitivity increased from 0.85 to 0.946 for both sampling together
#though the overall accuracy had dropped.

#Specificity
specificity(test$Default_, pred1)

#Precision
precision(test$Default_, pred1)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(test$Default_, pred1)

#Mis-classification Error
misClassError(test$Default_, pred)

# Synthetic sampling technique

rose = ROSE(Default_ ~., data = train, N = 1000, seed = 123)$data
table(rose$Default_)

summary(rose)

#Building model on this undersampled data and checking for improvement in sensitivity 

X_rose = (model.matrix(Default_ ~ ., rose)[,-1])
View(X_rose)
y_rose = (rose$Default_)
rose_mod = glm(Default_ ~ ., family = binomial, data = data.frame(Default_ = y_rose, X_rose))
summary(rose_mod)

back_rose = step(rose_mod,direction = 'backward')

summary(back_rose)
all_rose = step(rose_mod)

summary(all_rose)
#plot(all,scale='bic')
pred = predict(all_rose, newdata = data.frame(X_test), type = 'response')
#threshold = 0.16
pred1 = ifelse(pred > 0.16, 1, 0)
#Checking the sensitivity, precision and confusion matrix
#Confusion matrix
table(test$Default_, pred1)

#Sensitivity
sensitivity(test$Default_, pred1)  #Sensitivity increased from 0.85 to 0.973 for synthetic sampling
#though the overall accuracy had dropped.

#Specificity
specificity(test$Default_, pred1)

#Precision
precision(test$Default_, pred1)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(test$Default_, pred1)

#Mis-classification Error
misClassError(test$Default_, pred)

