# Global Mart Assignment.

library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)

require(graphics)


# IMPORTING Global Mart data

global_mart <- read.csv("Global Superstore.csv",stringsAsFactors = F)



################################## Data Prepararion 

# Checking for missing values
sum(is.na(global_mart))

sapply(global_mart, function(x) sum(is.na(x)))

# All the Missing values are in Postal code and the missing values are for market other than US so after segmentation we
# will ignore it for other markets

# Getting Month and year from order date
global_mart$Order.Date  <- as.Date(global_mart$Order.Date ,"%d-%m-%Y")
global_mart$Order.Date <- format(global_mart$Order.Date,"%Y-%m")

# No. of months of which we have data
global_mart$Order.Date<- as.factor(global_mart$Order.Date)
length(levels(global_mart$Order.Date))

# We have 48 months of data


# Segmentation of data based on Market & segment 

# No. of Markets
market_type <- as.factor(global_mart$Market)
levels(market_type)
#There are 7 markets namely "APAC"   "Africa" "Canada" "EMEA"   "EU"     "LATAM"  "US" 

# No. of segment 
Segment_type <- as.factor(global_mart$Segment)
levels(Segment_type)
#There are 3 segments : "Consumer"    "Corporate"   "Home Office"

#APAC data  
apac_Consumer <- global_mart[which(global_mart$Market == "APAC" & global_mart$Segment == "Consumer" ),]
apac_Corporate <- global_mart[which(global_mart$Market == "APAC" & global_mart$Segment == "Corporate" ),]
apac_Home <- global_mart[which(global_mart$Market == "APAC" & global_mart$Segment == "Home Office" ),]

#Africa data
africa_Consumer <- global_mart[which(global_mart$Market == "Africa" & global_mart$Segment == "Consumer"),]
africa_Corporate <- global_mart[which(global_mart$Market == "Africa" & global_mart$Segment == "Corporate" ),]
africa_Home <- global_mart[which(global_mart$Market == "Africa" & global_mart$Segment == "Home Office" ),]

#Canada data
canada_Consumer <- global_mart[which(global_mart$Market == "Canada" & global_mart$Segment == "Consumer"),]
canada_Corporate <- global_mart[which(global_mart$Market == "Canada" & global_mart$Segment == "Corporate" ),]
canada_Home <- global_mart[which(global_mart$Market == "Canada" & global_mart$Segment == "Home Office" ),]

#EMEA data
emea_Consumer <- global_mart[which(global_mart$Market == "EMEA" & global_mart$Segment == "Consumer"),]
emea_Corporate <- global_mart[which(global_mart$Market == "EMEA" & global_mart$Segment == "Corporate" ),]
emea_Home <- global_mart[which(global_mart$Market == "EMEA" & global_mart$Segment == "Home Office" ),]

#EU data
eu_Consumer <- global_mart[which(global_mart$Market == "EU" & global_mart$Segment == "Consumer"),]
eu_Corporate <- global_mart[which(global_mart$Market == "EU" & global_mart$Segment == "Corporate" ),]
eu_Home <- global_mart[which(global_mart$Market == "EU" & global_mart$Segment == "Home Office" ),]

#LATAM data
latam_Consumer <- global_mart[which(global_mart$Market == "LATAM" & global_mart$Segment == "Consumer"),]
latam_Corporate <- global_mart[which(global_mart$Market == "LATAM" & global_mart$Segment == "Corporate" ),]
latam_Home <- global_mart[which(global_mart$Market == "LATAM" & global_mart$Segment == "Home Office" ),]

#US DATA
us_Consumer <- global_mart[which(global_mart$Market == "US" & global_mart$Segment == "Consumer"),]
us_Corporate <- global_mart[which(global_mart$Market == "US" & global_mart$Segment == "Corporate" ),]
us_Home <- global_mart[which(global_mart$Market == "US" & global_mart$Segment == "Home Office" ),]


#Aggregating Sales, Quantity & Profit based on order month

#APAC

#Consumer
apac_Consumer_Sales <-  (aggregate(apac_Consumer$Sales, by=list(Month=apac_Consumer$Order.Date), FUN=sum))
apac_Consumer_Quantity <- (aggregate(apac_Consumer$Quantity, by=list(Month=apac_Consumer$Order.Date), FUN=sum))
apac_Consumer_Profit <- (aggregate(apac_Consumer$Profit, by=list(Month=apac_Consumer$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("apac_Consumer_Sales",sd(apac_Consumer_Sales$x, na.rm=TRUE)/mean(apac_Consumer_Sales$x, na.rm=TRUE))
b<-c("apac_Consumer_Quantity",sd(apac_Consumer_Quantity$x, na.rm=TRUE)/mean(apac_Consumer_Quantity$x, na.rm=TRUE))
c<-c("apac_Consumer_Profit",sd(apac_Consumer_Profit$x, na.rm=TRUE)/mean(apac_Consumer_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(a)
coff_of_var_quantity<- rbind(b)
coff_of_var_profit<- rbind(c)

#Corporate
apac_Corporate_Sales <- (aggregate(apac_Corporate$Sales, by=list(Month=apac_Corporate$Order.Date), FUN=sum))
apac_Corporate_Quantity <- (aggregate(apac_Corporate$Quantity, by=list(Month=apac_Corporate$Order.Date), FUN=sum))
apac_Corporate_Profit <- (aggregate(apac_Corporate$Profit, by=list(Month=apac_Corporate$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("apac_Corporate_Sales",sd(apac_Corporate_Sales$x, na.rm=TRUE)/mean(apac_Corporate_Sales$x, na.rm=TRUE))
b<-c("apac_Corporate_Quantity",sd(apac_Corporate_Quantity$x, na.rm=TRUE)/mean(apac_Corporate_Quantity$x, na.rm=TRUE))
c<-c("apac_Corporate_Profit",sd(apac_Corporate_Profit$x, na.rm=TRUE)/mean(apac_Corporate_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Home
apac_Home_Sales <- (aggregate(apac_Home$Sales, by=list(Month=apac_Home$Order.Date), FUN=sum))
apac_Home_Quantity <- (aggregate(apac_Home$Quantity, by=list(Month=apac_Home$Order.Date), FUN=sum))
apac_Home_Profit <- (aggregate(apac_Home$Profit, by=list(Month=apac_Home$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("apac_Home_Sales",sd(apac_Home_Sales$x, na.rm=TRUE)/mean(apac_Home_Sales$x, na.rm=TRUE))
b<-c("apac_Home_Quantity",sd(apac_Home_Quantity$x, na.rm=TRUE)/mean(apac_Home_Quantity$x, na.rm=TRUE))
c<-c("apac_Home_Profit",sd(apac_Home_Profit$x, na.rm=TRUE)/mean(apac_Home_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#africa

#Consumer
africa_Consumer_Sales <- (aggregate(africa_Consumer$Sales, by=list(Month=africa_Consumer$Order.Date), FUN=sum))
africa_Consumer_Quantity <- (aggregate(africa_Consumer$Quantity, by=list(Month=africa_Consumer$Order.Date), FUN=sum))
africa_Consumer_Profit <- (aggregate(africa_Consumer$Profit, by=list(Month=africa_Consumer$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("africa_Consumer_Sales",sd(africa_Consumer_Sales$x, na.rm=TRUE)/mean(africa_Consumer_Sales$x, na.rm=TRUE))
b<-c("africa_Consumer_Quantity",sd(africa_Consumer_Quantity$x, na.rm=TRUE)/mean(africa_Consumer_Quantity$x, na.rm=TRUE))
c<-c("africa_Consumer_Profit",sd(africa_Consumer_Profit$x, na.rm=TRUE)/mean(africa_Consumer_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Corporate
africa_Corporate_Sales <- (aggregate(africa_Corporate$Sales, by=list(Month=africa_Corporate$Order.Date), FUN=sum))
africa_Corporate_Quantity <- (aggregate(africa_Corporate$Quantity, by=list(Month=africa_Corporate$Order.Date), FUN=sum))
africa_Corporate_Profit <- (aggregate(africa_Corporate$Profit, by=list(Month=africa_Corporate$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("africa_Corporate_Sales",sd(africa_Corporate_Sales$x, na.rm=TRUE)/mean(africa_Corporate_Sales$x, na.rm=TRUE))
b<-c("africa_Corporate_Quantity",sd(africa_Corporate_Quantity$x, na.rm=TRUE)/mean(africa_Corporate_Quantity$x, na.rm=TRUE))
c<-c("africa_Corporate_Profit",sd(africa_Corporate_Profit$x, na.rm=TRUE)/mean(africa_Corporate_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Home
africa_Home_Sales <- (aggregate(africa_Home$Sales, by=list(Month=africa_Home$Order.Date), FUN=sum))
africa_Home_Quantity <- (aggregate(africa_Home$Quantity, by=list(Month=africa_Home$Order.Date), FUN=sum))
africa_Home_Profit <- (aggregate(africa_Home$Profit, by=list(Month=africa_Home$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("africa_Home_Sales",sd(africa_Home_Sales$x, na.rm=TRUE)/mean(africa_Home_Sales$x, na.rm=TRUE))
b<-c("africa_Home_Quantity",sd(africa_Home_Quantity$x, na.rm=TRUE)/mean(africa_Home_Quantity$x, na.rm=TRUE))
c<-c("africa_Home_Profit",sd(africa_Home_Profit$x, na.rm=TRUE)/mean(africa_Home_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)


#canada

#Consumer
canada_Consumer_Sales <- (aggregate(canada_Consumer$Sales, by=list(Month=canada_Consumer$Order.Date), FUN=sum))
canada_Consumer_Quantity <- (aggregate(canada_Consumer$Quantity, by=list(Month=canada_Consumer$Order.Date), FUN=sum))
canada_Consumer_Profit <- (aggregate(canada_Consumer$Profit, by=list(Month=canada_Consumer$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("canada_Consumer_Sales",sd(canada_Consumer_Sales$x, na.rm=TRUE)/mean(canada_Consumer_Sales$x, na.rm=TRUE))
b<-c("canada_Consumer_Quantity",sd(canada_Consumer_Quantity$x, na.rm=TRUE)/mean(canada_Consumer_Quantity$x, na.rm=TRUE))
c<-c("canada_Consumer_Profit",sd(canada_Consumer_Profit$x, na.rm=TRUE)/mean(canada_Consumer_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Corporate
canada_Corporate_Sales <- (aggregate(canada_Corporate$Sales, by=list(Month=canada_Corporate$Order.Date), FUN=sum))
canada_Corporate_Quantity <- (aggregate(canada_Corporate$Quantity, by=list(Month=canada_Corporate$Order.Date), FUN=sum))
canada_Corporate_Profit <- (aggregate(canada_Corporate$Profit, by=list(Month=canada_Corporate$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("canada_Corporate_Sales",sd(canada_Corporate_Sales$x, na.rm=TRUE)/mean(canada_Corporate_Sales$x, na.rm=TRUE))
b<-c("canada_Corporate_Quantity",sd(canada_Corporate_Quantity$x, na.rm=TRUE)/mean(canada_Corporate_Quantity$x, na.rm=TRUE))
c<-c("canada_Corporate_Profit",sd(canada_Corporate_Profit$x, na.rm=TRUE)/mean(canada_Corporate_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Home
canada_Home_Sales <- (aggregate(canada_Home$Sales, by=list(Month=canada_Home$Order.Date), FUN=sum))
canada_Home_Quantity <- (aggregate(canada_Home$Quantity, by=list(Month=canada_Home$Order.Date), FUN=sum))
canada_Home_Profit <- (aggregate(canada_Home$Profit, by=list(Month=canada_Home$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("canada_Home_Sales",sd(canada_Home_Sales$x, na.rm=TRUE)/mean(canada_Home_Sales$x, na.rm=TRUE))
b<-c("canada_Home_Quantity",sd(canada_Home_Quantity$x, na.rm=TRUE)/mean(canada_Home_Quantity$x, na.rm=TRUE))
c<-c("canada_Home_Profit",sd(canada_Home_Profit$x, na.rm=TRUE)/mean(canada_Home_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)


#emea

#Consumer
emea_Consumer_Sales <- (aggregate(emea_Consumer$Sales, by=list(Month=emea_Consumer$Order.Date), FUN=sum))
emea_Consumer_Quantity <- (aggregate(emea_Consumer$Quantity, by=list(Month=emea_Consumer$Order.Date), FUN=sum))
emea_Consumer_Profit <- (aggregate(emea_Consumer$Profit, by=list(Month=emea_Consumer$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("emea_Consumer_Sales",sd(emea_Consumer_Sales$x, na.rm=TRUE)/mean(emea_Consumer_Sales$x, na.rm=TRUE))
b<-c("emea_Consumer_Quantity",sd(emea_Consumer_Quantity$x, na.rm=TRUE)/mean(emea_Consumer_Quantity$x, na.rm=TRUE))
c<-c("emea_Consumer_Profit",sd(emea_Consumer_Profit$x, na.rm=TRUE)/mean(emea_Consumer_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Corporate
emea_Corporate_Sales <- (aggregate(emea_Corporate$Sales, by=list(Month=emea_Corporate$Order.Date), FUN=sum))
emea_Corporate_Quantity <-( aggregate(emea_Corporate$Quantity, by=list(Month=emea_Corporate$Order.Date), FUN=sum))
emea_Corporate_Profit <- (aggregate(emea_Corporate$Profit, by=list(Month=emea_Corporate$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("emea_Corporate_Sales",sd(emea_Corporate_Sales$x, na.rm=TRUE)/mean(emea_Corporate_Sales$x, na.rm=TRUE))
b<-c("emea_Corporate_Quantity",sd(emea_Corporate_Quantity$x, na.rm=TRUE)/mean(emea_Corporate_Quantity$x, na.rm=TRUE))
c<-c("emea_Corporate_Profit",sd(emea_Corporate_Profit$x, na.rm=TRUE)/mean(emea_Corporate_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Home
emea_Home_Sales <- (aggregate(emea_Home$Sales, by=list(Month=emea_Home$Order.Date), FUN=sum))
emea_Home_Quantity <- (aggregate(emea_Home$Quantity, by=list(Month=emea_Home$Order.Date), FUN=sum))
emea_Home_Profit <- (aggregate(emea_Home$Profit, by=list(Month=emea_Home$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("emea_Home_Sales",sd(emea_Home_Sales$x, na.rm=TRUE)/mean(emea_Home_Sales$x, na.rm=TRUE))
b<-c("emea_Home_Quantity",sd(emea_Home_Quantity$x, na.rm=TRUE)/mean(emea_Home_Quantity$x, na.rm=TRUE))
c<-c("emea_Home_Profit",sd(emea_Home_Profit$x, na.rm=TRUE)/mean(emea_Home_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)


#eu

#Consumer
eu_Consumer_Sales <- (aggregate(eu_Consumer$Sales, by=list(Month=eu_Consumer$Order.Date), FUN=sum))
eu_Consumer_Quantity <- (aggregate(eu_Consumer$Quantity, by=list(Month=eu_Consumer$Order.Date), FUN=sum))
eu_Consumer_Profit <- (aggregate(eu_Consumer$Profit, by=list(Month=eu_Consumer$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("eu_Consumer_Sales",sd(eu_Consumer_Sales$x, na.rm=TRUE)/mean(eu_Consumer_Sales$x, na.rm=TRUE))
b<-c("eu_Consumer_Quantity",sd(eu_Consumer_Quantity$x, na.rm=TRUE)/mean(eu_Consumer_Quantity$x, na.rm=TRUE))
c<-c("eu_Consumer_Profit",sd(eu_Consumer_Profit$x, na.rm=TRUE)/mean(eu_Consumer_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Corporate
eu_Corporate_Sales <- (aggregate(eu_Corporate$Sales, by=list(Month=eu_Corporate$Order.Date), FUN=sum))
eu_Corporate_Quantity <- (aggregate(eu_Corporate$Quantity, by=list(Month=eu_Corporate$Order.Date), FUN=sum))
eu_Corporate_Profit <- (aggregate(eu_Corporate$Profit, by=list(Month=eu_Corporate$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("eu_Corporate_Sales",sd(eu_Corporate_Sales$x, na.rm=TRUE)/mean(eu_Corporate_Sales$x, na.rm=TRUE))
b<-c("eu_Corporate_Quantity",sd(eu_Corporate_Quantity$x, na.rm=TRUE)/mean(eu_Corporate_Quantity$x, na.rm=TRUE))
c<-c("eu_Corporate_Profit",sd(eu_Corporate_Profit$x, na.rm=TRUE)/mean(eu_Corporate_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Home
eu_Home_Sales <- (aggregate(eu_Home$Sales, by=list(Month=eu_Home$Order.Date), FUN=sum))
eu_Home_Quantity <- (aggregate(eu_Home$Quantity, by=list(Month=eu_Home$Order.Date), FUN=sum))
eu_Home_Profit <- (aggregate(eu_Home$Profit, by=list(Month=eu_Home$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("eu_Home_Sales",sd(eu_Home_Sales$x, na.rm=TRUE)/mean(eu_Home_Sales$x, na.rm=TRUE))
b<-c("eu_Home_Quantity",sd(eu_Home_Quantity$x, na.rm=TRUE)/mean(eu_Home_Quantity$x, na.rm=TRUE))
c<-c("eu_Home_Profit",sd(eu_Home_Profit$x, na.rm=TRUE)/mean(eu_Home_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)


#latam

#Consumer
latam_Consumer_Sales <-( aggregate(latam_Consumer$Sales, by=list(Month=latam_Consumer$Order.Date), FUN=sum))
latam_Consumer_Quantity <- (aggregate(latam_Consumer$Quantity, by=list(Month=latam_Consumer$Order.Date), FUN=sum))
latam_Consumer_Profit <- (aggregate(latam_Consumer$Profit, by=list(Month=latam_Consumer$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("latam_Consumer_Sales",sd(latam_Consumer_Sales$x, na.rm=TRUE)/mean(latam_Consumer_Sales$x, na.rm=TRUE))
b<-c("latam_Consumer_Quantity",sd(latam_Consumer_Quantity$x, na.rm=TRUE)/mean(latam_Consumer_Quantity$x, na.rm=TRUE))
c<-c("latam_Consumer_Profit",sd(latam_Consumer_Profit$x, na.rm=TRUE)/mean(latam_Consumer_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Corporate
latam_Corporate_Sales <- (aggregate(latam_Corporate$Sales, by=list(Month=latam_Corporate$Order.Date), FUN=sum))
latam_Corporate_Quantity <- (aggregate(latam_Corporate$Quantity, by=list(Month=latam_Corporate$Order.Date), FUN=sum))
latam_Corporate_Profit <- (aggregate(latam_Corporate$Profit, by=list(Month=latam_Corporate$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("latam_Corporate_Sales",sd(latam_Corporate_Sales$x, na.rm=TRUE)/mean(latam_Corporate_Sales$x, na.rm=TRUE))
b<-c("latam_Corporate_Quantity",sd(latam_Corporate_Quantity$x, na.rm=TRUE)/mean(latam_Corporate_Quantity$x, na.rm=TRUE))
c<-c("latam_Corporate_Profit",sd(latam_Corporate_Profit$x, na.rm=TRUE)/mean(latam_Corporate_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Home
latam_Home_Sales <- (aggregate(latam_Home$Sales, by=list(Month=latam_Home$Order.Date), FUN=sum))
latam_Home_Quantity <- (aggregate(latam_Home$Quantity, by=list(Month=latam_Home$Order.Date), FUN=sum))
latam_Home_Profit <-( aggregate(latam_Home$Profit, by=list(Month=latam_Home$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("latam_Home_Sales",sd(latam_Home_Sales$x, na.rm=TRUE)/mean(latam_Home_Sales$x, na.rm=TRUE))
b<-c("latam_Home_Quantity",sd(latam_Home_Quantity$x, na.rm=TRUE)/mean(latam_Home_Quantity$x, na.rm=TRUE))
c<-c("latam_Home_Profit",sd(latam_Home_Profit$x, na.rm=TRUE)/mean(latam_Home_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)


#us

#Consumer
us_Consumer_Sales <- (aggregate(us_Consumer$Sales, by=list(Month=us_Consumer$Order.Date), FUN=sum))
us_Consumer_Quantity <- (aggregate(us_Consumer$Quantity, by=list(Month=us_Consumer$Order.Date), FUN=sum))
us_Consumer_Profit <- (aggregate(us_Consumer$Profit, by=list(Month=us_Consumer$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("us_Consumer_Sales",sd(us_Consumer_Sales$x, na.rm=TRUE)/mean(us_Consumer_Sales$x, na.rm=TRUE))
b<-c("us_Consumer_Quantity",sd(us_Consumer_Quantity$x, na.rm=TRUE)/mean(us_Consumer_Quantity$x, na.rm=TRUE))
c<-c("us_Consumer_Profit",sd(us_Consumer_Profit$x, na.rm=TRUE)/mean(us_Consumer_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Corporate
us_Corporate_Sales <- (aggregate(us_Corporate$Sales, by=list(Month=us_Corporate$Order.Date), FUN=sum))
us_Corporate_Quantity <- (aggregate(us_Corporate$Quantity, by=list(Month=us_Corporate$Order.Date), FUN=sum))
us_Corporate_Profit <- (aggregate(us_Corporate$Profit, by=list(Month=us_Corporate$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("us_Corporate_Sales",sd(us_Corporate_Sales$x, na.rm=TRUE)/mean(us_Corporate_Sales$x, na.rm=TRUE))
b<-c("us_Corporate_Quantity",sd(us_Corporate_Quantity$x, na.rm=TRUE)/mean(us_Corporate_Quantity$x, na.rm=TRUE))
c<-c("us_Corporate_Profit",sd(us_Corporate_Profit$x, na.rm=TRUE)/mean(us_Corporate_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Home
us_Home_Sales <- (aggregate(us_Home$Sales, by=list(Month=us_Home$Order.Date), FUN=sum))
us_Home_Quantity <- (aggregate(us_Home$Quantity, by=list(Month=us_Home$Order.Date), FUN=sum))
us_Home_Profit <- (aggregate(us_Home$Profit, by=list(Month=us_Home$Order.Date), FUN=sum))

# calculating Coefficient of variation
a<-c("us_Home_Sales",sd(us_Home_Sales$x, na.rm=TRUE)/mean(us_Home_Sales$x, na.rm=TRUE))
b<-c("us_Home_Quantity",sd(us_Home_Quantity$x, na.rm=TRUE)/mean(us_Home_Quantity$x, na.rm=TRUE))
c<-c("us_Home_Profit",sd(us_Home_Profit$x, na.rm=TRUE)/mean(us_Home_Profit$x, na.rm=TRUE))

coff_of_var_sales<- rbind(coff_of_var_sales,a)
coff_of_var_quantity<- rbind(coff_of_var_quantity,b)
coff_of_var_profit<- rbind(coff_of_var_profit,c)

#Converting into data frame
coff_of_var_sales <- data.frame(coff_of_var_sales)
coff_of_var_quantity <- data.frame(coff_of_var_quantity)
coff_of_var_profit <- data.frame(coff_of_var_profit)

# Getting top 3 segments Profit , Quantity and sales
coff_of_var_profit <- coff_of_var_profit[order(coff_of_var_profit$X2,decreasing = FALSE),]
profit_top_3 <- coff_of_var_profit[1:3,]
profit_top_3

coff_of_var_quantity <- coff_of_var_quantity[order(coff_of_var_quantity$X2,decreasing = FALSE),]
quantity_top_3 <- coff_of_var_quantity[1:3,]
quantity_top_3

coff_of_var_sales <- coff_of_var_sales[order(coff_of_var_sales$X2,decreasing = FALSE),]
sales_top_3 <- coff_of_var_profit[1:3,]
sales_top_3

# eu_Consumer_Profit and apac_Consumer_Profit is appering in all 3 segments so these 2 are most profitable and consistently 
# profitable segments and we will use there 2 segments for our modeling.


################### Time Series modeling and forecasting #################

###########   eu_Consumer_Quantity Model #############

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
eu_Consumer_Quantity$Month <- as.numeric(eu_Consumer_Quantity$Month)
total_timeser <- ts(data=eu_Consumer_Quantity$x )
indata <- eu_Consumer_Quantity[1:42,]
timeser <- ts(indata$x)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)


#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.1*Month) * poly(Month,1)
            , data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

accuracy(global_pred, smootheddf$Quantity)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
armafit

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- eu_Consumer_Quantity[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#28.76053

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")



##########   ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
#30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

# In this case classical decomposition method has better MAPE value so we will go with it and forcaste for next 6 months

next_months <- c(49:54)

eu_Consumer_Quantity_forecast <- predict(lmfit,data.frame(Month =next_months))

eu_Consumer_Quantity_forecast_df <- data.frame(c(49:54) ,eu_Consumer_Quantity_forecast)

#forecast for next 6 months
eu_Consumer_Quantity_forecast_df




###########   eu_Consumer_Sale Model #############

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
eu_Consumer_Sales$Month <- as.numeric(eu_Consumer_Sales$Month)
total_timeser <- ts(data=eu_Consumer_Sales$x )
indata <- eu_Consumer_Sales[1:42,]
timeser <- ts(indata$x)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)


#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month , data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

accuracy(global_pred, smootheddf$Sales)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
armafit

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- eu_Consumer_Sales[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec
#92.95788

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
#28.9226

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

# In this case ARIMA fit has better MAPE value so we will go with it and forcaste for next 6 months

next_months <- c(49:54)

fcast_eu_Consumer_Sale <- predict(autoarima, n.ahead = 6)

fcast_eu_Consumer_Sale_df <- data.frame(c(49:54) ,fcast_eu_Consumer_Sale$pred)

#forecast for next 6 months
fcast_eu_Consumer_Sale_df




################### Time Series modeling and forecasting #################

###########   apac_Consumer_Quantity Model #############

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
apac_Consumer_Quantity$Month <- as.numeric(apac_Consumer_Quantity$Month)
total_timeser <- ts(data=apac_Consumer_Quantity$x )
indata <- apac_Consumer_Quantity[1:42,]
timeser <- ts(indata$x)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)


#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.1*Month) * poly(Month,1)
            , data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

accuracy(global_pred, smootheddf$Quantity)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
armafit

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- apac_Consumer_Quantity[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec
#22.45494

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
#26.24458

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

# In this case classical decomposition method has better MAPE value so we will go with it and forcaste for next 6 months

next_months <- c(49:54)

apac_Consumer_Quantity_forecast <- predict(lmfit,data.frame(Month =next_months))

apac_Consumer_Quantity_forecast_df <- data.frame(c(49:54) ,apac_Consumer_Quantity_forecast)

#forecast for next 6 months
apac_Consumer_Quantity_forecast_df



###########   apac_Consumer_Sale Model #############

#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later
apac_Consumer_Sales$Month <- as.numeric(apac_Consumer_Sales$Month)
total_timeser <- ts(data=apac_Consumer_Sales$x )
indata <- apac_Consumer_Sales[1:42,]
timeser <- ts(indata$x)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)


#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]

for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}


#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month , data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

accuracy(global_pred, smootheddf$Sales)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
armafit

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- apac_Consumer_Sales[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec
#31.07429

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima
#27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black")
lines(auto_arima_pred, col = "red")

# In this case ARIMA fit has better MAPE value so we will go with it and forcaste for next 6 months

next_months <- c(49:54)

fcast_apac_Consumer_Sale <- predict(autoarima, n.ahead = 6)

fcast_apac_Consumer_Sale_df <- data.frame(c(49:54) ,fcast_apac_Consumer_Sale$pred)

#forecast for next 6 months
fcast_apac_Consumer_Sale_df


############ Forcasted vaules ####################

#Europe
# Europe Consumer Quantity Forecast : 
eu_Consumer_Quantity_forecast_df

#Europe Consumer Sales Forecast : 
fcast_eu_Consumer_Sale_df

# APAC
# APAC Consumer Quantity Forecast : 
apac_Consumer_Quantity_forecast_df

#APAC Consumer Sales Forecast : 
fcast_apac_Consumer_Sale_df



############################## Thank you ##################################################################



