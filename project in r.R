setwd("/Users/mrinalgaur/desktop/Rdir")
mydata= read.csv("LoanPredictiontrain.csv",header= TRUE)
mydata[mydata==""]=NA
mydata$Loan_ID=as.character(mydata$Loan_ID)
mydata$Gender=as.character(mydata$Gender)
mydata$Married=as.character(mydata$Married)
mydata$Dependents=as.character(mydata$Dependents)
mydata$Education=as.character(mydata$Education)
mydata$Self_Employed=as.character(mydata$Self_Employed)
mydata$Credit_History=as.character(mydata$Credit_History)
mydata$Property_Area=as.character(mydata$Property_Area)
mydata$Loan_Status=as.character(mydata$Loan_Status)
outlierapplicantIncome=mydata$ApplicantIncome>30000
newdata=mydata[!outlierapplicantIncome,]
cor(LoanAmount,CoapplicantIncome,use="complete.obs")
cor(ApplicantIncome,CoapplicantIncome,use="complete.obs")
cor(ApplicantIncome,LoanAmount,use = "complete.obs")
outliercoapplicantIncome= newdata$CoapplicantIncome>30000
newdata= newdata[!outliercoapplicantIncome,]
cor(newdata[,7:9],use = "complete.obs")
glm.fit=glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,data = newdata, family=binomial)
glm.fit=glm(Loan_Status~Married+Education+Self_Employed+LoanAmount+Credit_History+Property_Area,data = newdata, family=binomial)
glm.probs=predict(glm.fit,type='response')
glm.pred=rep("N",614)
glm.pred[glm.probs>0.5]="Y"
lda.fit=lda(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Credit_History+Property_Area,data = newdata)
lda.pred=predict(lda.fit,newdata1)
table(lda.pred$class,newdata1$Loan_Status)
lda.fit=lda(Loan_Status~LoanAmount+Credit_History,data = newdata1)
lda.fit=lda(Loan_Status~Married+Education+Self_Employed+LoanAmount+Credit_History+Property_Area,data = newdata1)
qda.fit=qda(Loan_Status~Married+Education+Self_Employed+LoanAmount+Credit_History+Property_Area,data = newdata1)
lda.fit=lda(Loan_Status~LoanAmount+Credit_History,data = newdata1)
tree.loanstatus= tree(Loan_Status~.,newdata)
testdata=read.csv("LoanPredictiontrain.csv",header= TRUE)
lda.predtestdata=predict(lda.fit,testdata)
table(lda.predtestdata$class,testdata$Loan_Status,dnn = c("predicted","actual"))
dtree=rpart(Loan_Status~LoanAmount+Credit_History+Married+Property_Area+Dependents,data = mydata)
dtree$cptable
#library(rpart.plot)
#prp(dtree, type = 2, extra = 104,
#   fallen.leaves = TRUE, main="Decision Tree")
dtree.pred=predict(dtree,data=mydata,type="class")
dtree.perf=table(mydata$Loan_Status,dtree.pred,dnn = c("actual","predicted"))
table(dtree.pred,newdata$Loan_Status)
#Loan_Status
#glm.pred   N   Y
#N  36  66
#Y 156 356
#with all variables
#Loan_Status 
#glm.pred   N   Y
#N  28  63
#Y 164 359

#lda on all coolumns without missing data 
#N   Y
#N  63   6
#Y  83 323

#lda on loanamount, credithistory
#N   Y
#N  62   6
#Y  84 323

#qda on loanamount,credithistory
#N   Y
#N  65  11
#Y  81 318
GenderMale              3.18e-01   3.35e-01    0.95   0.3436    
MarriedYes              5.89e-01   2.99e-01    1.97   0.0486 *  
        Dependents1            -2.75e-01   3.56e-01   -0.77   0.4395    
Dependents2             2.81e-01   3.83e-01    0.73   0.4636    
Dependents3+            9.07e-02   4.85e-01    0.19   0.8516    
EducationNot Graduate  -4.19e-01   3.06e-01   -1.37   0.1706    
Self_EmployedYes       -1.84e-01   3.65e-01   -0.51   0.6135    
ApplicantIncome         2.93e-05   5.23e-05    0.56   0.5744    
CoapplicantIncome       1.75e-06   6.63e-05    0.03   0.9790    
LoanAmount             -4.37e-03   2.23e-03   -1.96   0.0501 .  
Loan_Amount_Term       -1.14e-04   2.01e-03   -0.06   0.9549    
Credit_History          3.79e+00   4.59e-01    8.26   <2e-16 ***
        Property_AreaSemiurban  9.69e-01   3.08e-01    3.14   0.0017 ** 
        Property_AreaUrban      1.28e-01   3.05e-01    0.42   0.6737    

 "Loan_ID"           "Gender"            "Married"           "Dependents"        "Education"        
[6] "Self_Employed"     "ApplicantIncome"   "CoapplicantIncome" "LoanAmount"        "Loan_Amount_Term" 
[11] "Credit_History"    "Property_Area"     "Loan_Status"      