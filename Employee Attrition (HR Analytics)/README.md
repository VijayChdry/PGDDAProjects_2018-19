#To Build Employee Attrition Logistice Model in HR Department of IT Industry.

######Problem Statement
To understand the employee attrition pattern and identify the key variables on which the organisation should focus to curb the Attrition.   


#######Business Objective
To build a model to predict the probability of attrition using a logistic regression. The results thus obtained will be used by the management to understand what changes they should make to their workplace, in order to get most of their employees to stay. 


#######Business Impact
To maintain organisation reputation among consumers and partners by avoiding unnecessary delay in meeting timelines which usually occurred due to attrition.
Cost benefit in terms of not maintaining separate recruitment department and training. 

######Data
#4410 unique employee data consist of basic information, Performance review, survey results and login behaviours for a year 
#Around 15% attrition rate every year
#There are 5 different csv files given with around 520 total variables/Attributes



#####Model Building and Evaluation
#We have successfully built a logistic regression model to predict the probability of attrition with below characteristics
#Final model was achieved in 25 iterations at cutoff value is 0.1636

################################ Model Statistics#################### 
######Key driving factors are below
#NumCompaniesWorked
#TotalWorkingYears 
#YearsWithCurrManager
#BusinessTravel.xTravel_Frequently
#EnvironmentSatisfaction.xLow  
#Department.xHuman.Resources
#MaritalStatus.xDivorced 
#MaritalStatus.xMarried + 
#JobSatisfaction.xLow
#emp_login_cat.xearly.logout
#emp_login_cat.xovertime


#####
#Accuracy: 75%
#Sensitivity: 72%
#Specificity: 75%

####
#Gain at 4th decile =81.9%
#Lift at 4th decile =2.05%
#KS Stat= 47.69%

########################### Thank You ################################


