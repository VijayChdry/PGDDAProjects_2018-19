##########........Gramener Case Study........###########
#Objective is to understand the driving factors (or driver variables) behind loan default, i.e. the variables which are strong indicators of default.  The company can utilise this knowledge for its portfolio and risk assessment.

#Loading the required library 
library(tidyverse)
library(lubridate)
library(corrplot)


#Load the Dataset
dataset <-loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)


##############.......................Data Cleansing............######################

#Check duplicates
sum(duplicated(loan))


#Check "NA" Values 
sum(is.na(loan))
colSums(is.na(loan))

#Dropping all the columns which are having ONLY "NA" values.
loan <- loan[ , colSums(is.na(loan)) != nrow(loan)]
colSums(is.na(loan))

#Clean the columns which are having only unique values
Unique_Col<-sapply(loan, function(col) length(unique(col)))
Unique_Col
loan<-loan[,Unique_Col>1]

#Dropping columns which are having sum 0 from the dataset. That means the column is having only "O" or "0 and NA".
loan <- loan[,colSums(loan != 0,na.rm = T)>0] 


#Computing Columns where significant "NA" values are available 
colSums(is.na(loan))
loan<-loan[ ,-c(29,30,52)]

#As the dataset contains some fields which are related to customer behaviours which are not possible to collect at the time of application.
#Thus it makes no use to keep such fields for analysis.Through Applying domain expertise and data dictionary
#Removing all of such fields from the dataset. 

Unwanted_col<- c("last_pymnt_d","next_pymnt_d","last_credit_pull_d","url","desc","title","zip_code","next_pymnt_d","mths_since_last_delinq","last_credit_pull_d","pub_rec_bankruptcies")
loan<-loan[, !(colnames(loan) %in% Unwanted_col)]

#Removing text "Months" from the column "Term" and converting it into Numeric data type. 
loan$term<- as.numeric(gsub('months',"",loan$term))


#Removing pattern "%" from the column "int_rate" and "revol_util" of the dataset.
loan$int_rate<-as.numeric(gsub('%', "", loan$int_rate))
loan$revol_util<-as.numeric(gsub('%', "", loan$revol_util))


#Converting date formatting issue for a column "issue_d"
class(loan$issue_d)
loan$issue_d<-as.Date(paste(loan$issue_d,"-01",sep=""), format="%y-%b-%d")
class(loan$issue_d)


#As there are "n/a" values in the column "emp_length" so to compute that, we are extracting years from the column
#"earliest_cr_line" and computing the column "emp_length" to fill n/a values.
#We ahve taken 'Median' of the emp_length and computed the 'n/a' values. 
loan$earliest_cr_line<- regmatches( loan$earliest_cr_line,gregexpr('[0-9]+',loan$earliest_cr_line))
loan$earliest_cr_line <- as.numeric(loan$earliest_cr_line)
loan$emp_length[which(loan$emp_length == 'n/a'  & loan$earliest_cr_line<1  )] <- "10+ years"
loan$emp_length[which(loan$emp_length == 'n/a'  & loan$earliest_cr_line>10  )] <- "10+ years"
loan$emp_length[which(loan$emp_length == 'n/a'  & loan$earliest_cr_line<=10 & loan$earliest_cr_line>=1  )] <- "5 years"



#Replacing blank fields in the column "emp_title" 
loan$emp_title[which(loan$emp_title == "")] <- "Unknown"


#We have 50 NA in Revol_util out of which 49 are 0 , so after watching the trend which rest values assigning all the 
#NA values to 0 
loan$revol_util[is.na(loan$revol_util)] <- 0
 

#Checking Outliers 
boxplot(loan$annual_inc)
#Anual income has few outliers but as we won't be computing mean or average we are not removing the outliers


#There are no "NA" values in the final dataset
#Summary for the final dataset
summary(loan)
sum(is.na(loan)) #0



#############..........Derived Matrics Analysis........#################################

#Deriving Year of the loan issued
loan$loan_issued_Year <- format(loan$issue_d, "%Y")


#Bucketing Annual income to understand the distribution properly
loan$Customer_annual_earning <- ifelse(loan$annual_inc <30000,"Income<30K",ifelse(loan$annual_inc <60000,"Income<60K",ifelse(loan$annual_inc <90000,"Income<90K","Above 90K")))


#Bucketing interest rate to understand the distributions
loan$loan_Interest_rate <- ifelse(loan$int_rate <10,"Interest_Rate<10%",
                                  ifelse(loan$int_rate <15,"Interest_Rate between 10%-15%",
                                         ifelse(loan$int_rate<20,"Interest_Rate between 15%-20%","More than 20%")))

#Bucketing Loan Amount 
loan$loan_Amount_Cap <- ifelse(loan$loan_amnt <5000,"Loan<5K",ifelse(loan$loan_amnt <10000,"Loan<10K",
                                                                     ifelse(loan$loan_amnt <15000,"Loan<15K",ifelse(loan$loan_amnt <20000,"Loan<20K",
                                                                                                                    ifelse(loan$loan_amnt <25000,"Loan<25K",ifelse(loan$loan_amnt <30000,"Loan<30K","Above 30K"))))))


#Bucketing Installement Amount
loan$installment_cap <- ifelse(loan$installment<300, "EMI<300",ifelse(loan$installment<600, "EMI<600",
                                                                      ifelse(loan$installment<900, "EMI<900",ifelse(loan$installment<1200, "EMI<1200",
                                                                                                                    ifelse(loan$installment<1500, "EMI<1500")))))

#Bucketing DTI amount
loan$dti_cap <-ifelse(loan$dti<5, "dti<5%",ifelse(loan$dti<10, "dti<10%",ifelse(loan$dti<15, "dti<15%",
                                                                                ifelse(loan$dti<20, "dti<20%",ifelse(loan$dti<25, "dti<25%",ifelse(loan$dti<30, "dti<30%"))))))




###########..............Univariate Analysis .....###########################################
summary(loan$loan_status)

#Understanding the loan status distribution and found that 15% loans are the "defaulters" and 83% are "fully paid" 
Chargedoff_percentage<- print(sum(loan$loan_status=="Charged Off")/length(loan$loan_status)*100)
fullypaid_percentage<- print(sum(loan$loan_status=="Fully Paid")/length(loan$loan_status)*100)
Current_percentage<- print(sum(loan$loan_status=="Current")/length(loan$loan_status)*100)


#Plotting Loan_Status distribution
ggplot(loan, aes(x=factor(loan$loan_status), fill=loan$loan_status))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Loan Status Distribution", x="Loan Status", y="count")


#Plotting statewise loan Distribution
ggplot(loan, aes(x=factor(loan$addr_state), fill=loan$addr_state))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="statewise loan Distribution", x="state", y="count")



#Plotting interest rate wise distribution 
ggplot(loan, aes(x=factor(loan$loan_Interest_rate), fill=loan$loan_Interest_rate))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Interest Rate Distribution", x="Interest rate", y="count")


#Plotting Term DISTRIBUTION
ggplot(loan, aes(x=factor(loan$term), fill=factor(loan$term)))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Term wise Distribution", x="Term", y="count")


#plotting DTI distribution
ggplot(loan, aes(x=factor(loan$dti_cap), fill=factor(loan$dti_cap)))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Depth TO Income ratio Distribution", x="dti", y="count")


#plotting Home owned distribution
ggplot(loan, aes(x=factor(loan$home_ownership), fill=factor(loan$home_ownership)))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Home Ownership Loan Distribution", x="Home ownership", y="count")


#plotting emp length distribution
ggplot(loan, aes(x=factor(loan$emp_length), fill=factor(loan$emp_length)))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Emp length wise Distribution", x="emp length", y="count")


#Plotting Purpose distribution
ggplot(loan, aes(x=factor(loan$purpose), fill=factor(loan$purpose)))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Loan purpose wise Distribution", x="Purpose", y="count")



#Plotting Verification status distribution
ggplot(loan, aes(x=factor(loan$verification_status), fill=factor(loan$verification_status)))+geom_bar(stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..),position = position_dodge(width = 1))+
  labs(title="Verification status distribution", x="Verification Status", y="count")
 




#####................Bivariate Analysis.........######################

#As per problem statmenet , excluding loan status as "Current" for the further analysis. 
loan_fully_charged<-subset(loan, loan$loan_status!="Current")


######...........Corelation Understanding for the variables.........
loan_cor <- loan_fully_charged
loan_cor<-loan_cor[ ,c("loan_status","loan_amnt","int_rate","installment","annual_inc","dti" ,"grade","home_ownership","verification_status","purpose","emp_length","term")]
loan_cor$loan_status<-as.numeric(factor(loan_cor$loan_status))
loan_cor$grade<-as.numeric(factor(loan_cor$grade))
loan_cor$home_ownership<-as.numeric(factor(loan_cor$home_ownership))
loan_cor$verification_status<-as.numeric(factor(loan_cor$verification_status))
loan_cor$purpose<-as.numeric(factor(loan_cor$purpose))
loan_cor$emp_length<-as.numeric(factor(loan_cor$emp_length))
loan_cor$emp_length<-as.numeric(factor(loan_cor$term))

Corr<-cor(loan_cor)
corrplot(Corr,method = "circle",type="upper")



# Graph between Loan Status and Verification status
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$verification_status)) +
geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and Purpose
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$purpose)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and DTI
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$dti_cap)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and Annual Income
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = factor(loan_fully_charged$Customer_annual_earning))) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and House ownership
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$home_ownership)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and emp length
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$emp_length)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and grade
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$grade)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and Interest rate
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$loan_Interest_rate)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Graph between Loan Status and Loan Amount
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$loan_Amount_Cap)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=scales::percent((..count..)/sum(..count..))),   position = position_dodge(width = 1))


# Graph between Loan Status and Loan Issued Year
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$loan_issued_Year)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))



# Graph between Loan Status and Loan Issued Year
ggplot(loan_fully_charged,aes(x= loan_fully_charged$loan_status ,fill = loan_fully_charged$loan_issued_Year)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


######### .................. MultiVariate Analysis.........##################
#The analysis says that "Not Verified" from "Varification status", "depth Consolidation" from "Purpose"
#Annual income between 30k to 90K, home_ownership as "Mortgage and Rent", 10+ YEARS of experrience.

# Loan Status vs Anual Income for debt_consolidation
loan_debt_consolidation<-subset(loan_fully_charged, loan_fully_charged$purpose =="debt_consolidation")
ggplot(loan_debt_consolidation,aes(x= loan_debt_consolidation$loan_status ,fill = loan_debt_consolidation$Customer_annual_earning)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Loan Status vs Anual Income for CA state
loan_addr_state<-subset(loan_fully_charged, loan_fully_charged$addr_state =="CA")
ggplot(loan_addr_state,aes(x= loan_addr_state$loan_status ,fill = loan_addr_state$Customer_annual_earning)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))


# Loan Status vs Anual Income for Not Verified
loan_verification_status<-subset(loan_fully_charged, loan_fully_charged$verification_status =="Not Verified")
ggplot(loan_verification_status,aes(x= loan_verification_status$loan_status ,fill = loan_verification_status$dti_cap)) +
  geom_bar(position = "fill") + scale_y_continuous(name="Percentile") +
  geom_bar(stat="count",position = "dodge") + geom_text(stat='count',aes(label=..count..),   position = position_dodge(width = 1))




#########################################THANK YOU###################################################################











                                        
