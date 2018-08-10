############################LINEAR REGRESSION MODEL###############################################
#To build a model to predict the price of cars with the available independent variables
#Business Understanding is that we need to predict the key drivers variables to predict the price.

#Import the required library
library(tidyverse)
library(MASS) # stepAIC
library(car)# VIF 
library(corrplot)


#Import the data set
carprice<-dataset<-read.csv("CarPrice_Assignment.csv", header = TRUE, stringsAsFactors = FALSE)


#There are total 205 observations of 26 variables including target variable
View(carprice)
str(carprice)

##############Data Preparation/EDA Analysis################
#Checking missing values
sum(is.na(carprice)) ## no missing values

# check for duplicated rows
carprice[which(duplicated(carprice)), ] # no duplicated rows


#As per Business rule , splitting CarName colum and extracting only car company name as "CarName". 
carprice<-separate(carprice,3,into = c("CarName", "Car Model"), sep = " ", remove = TRUE, extra = "merge", fill = "right")
carprice<-carprice[,-4]


#Replacing some mis-spelled word for column "CarName"
carprice$CarName<-tolower(carprice$CarName)
carprice$CarName<-carprice$CarName %>% str_replace("maxda", "mazda")
carprice$CarName<-carprice$CarName %>% str_replace("porcshce", "porsche")
carprice$CarName<-carprice$CarName %>% str_replace("toyouta", "toyota")
carprice$CarName<-carprice$CarName %>% str_replace("vokswagen", "volkswagen")
carprice$CarName<-carprice$CarName %>% str_replace("vw", "volkswagen")


#Converting all the Character data types into Factor
carprice <-mutate_if(carprice, is.character, as.factor)


#Ensuring that all the varibales are in right format
str(carprice)

####################### outlier treatment  for numeric types ############################# 

##Wheelbase variable to check 
boxplot(carprice$wheelbase)
quantile(carprice$wheelbase, seq(0,1,0.01))
#computing at 98% and above by replacing the value as "114.200"
carprice$wheelbase[which(carprice$wheelbase>114.200)]<-114.200


##Carlength Variable 
boxplot(carprice$carlength)
quantile(carprice$carlength, seq(0,1,0.01))
#computing at 0%, 1% and 2% by replacing the value as "155.900"
carprice$carlength[which(carprice$carlength<155.900)] <-155.900


##carwidth variable 
boxplot(carprice$carwidth)
quantile(carprice$carwidth, seq(0,1,0.01))
#computing at 97% and above by replacing value as "70.852"
carprice$carwidth[which(carprice$carwidth>70.852)] <-70.852

##carheight variable
boxplot(carprice$carheight) #No outlier


##curbweight variable No outlier
boxplot(carprice$curbweight) #No outlier


##enginesize variable 
boxplot(carprice$enginesize)
quantile(carprice$enginesize, seq(0,1,0.01))
#compute 95% onwards by replacing values as "194.00"
carprice$enginesize[which(carprice$enginesize>194.00)]<-194.00

##boreratio variable
boxplot(carprice$boreratio)#No outlier


##Stroke varibale
boxplot(carprice$stroke)
quantile(carprice$stroke, seq(0,1,0.01))
#imputing for 0% and 1 % by replacing values as "2.6400"
carprice$stroke[which(carprice$stroke<2.6400)] <-2.6400
carprice$stroke[which(carprice$stroke>3.8600)] <-3.8600


##compressionratio variable
boxplot(carprice$compressionratio)
quantile(carprice$compressionratio, seq(0,1,0.01))
#imputing 90% and above by replacing values as "10.9400"
carprice$compressionratio[which(carprice$compressionratio>10.9400)]<-10.9400


##horsepower variable
boxplot(carprice$horsepower)
quantile(carprice$horsepower, seq(0,1,0.01))
#compute 94% and above by replacing value as "162.00"
carprice$horsepower[which(carprice$horsepower>162.00)]<-162.00


##peakrpm variable
boxplot(carprice$peakrpm)
quantile(carprice$peakrpm, seq(0,1,0.01))
#compute at 100% by replacing value as "6000" 
carprice$peakrpm[which(carprice$peakrpm>6000)]<-6000


##citympg variable
boxplot(carprice$citympg)
quantile(carprice$citympg, seq(0,1,0.01))
#compute at 98% and above by replacing value as "38.00" 
carprice$citympg[which(carprice$citympg>38.00)]<-38.00


##highwaympg variable
boxplot(carprice$highwaympg)
quantile(carprice$highwaympg, seq(0,1,0.01))
#compute at 98% and above by replacing value as "46.92" 
carprice$highwaympg[which(carprice$highwaympg>46.92)]<-46.92


#Based on Business understanding, correcting few factor varibales into Numeric
levels(carprice$cylindernumber)<-c(8,5,4,6,3,12,2)
carprice$cylindernumber<- as.numeric(levels(carprice$cylindernumber))[carprice$cylindernumber]


levels(carprice$doornumber)<-c(4,2)
carprice$doornumber<-as.numeric(levels(carprice$doornumber))[carprice$doornumber]


#To see Summary of all the Factor variables
#There are total 6 varibales that needs to be converted using dummy variable logic
summary(carprice[which(sapply(carprice, class)=='factor')])


############Derived Matrix##########################
#Categorizing carname based on demographic orginator

europe <- c('alfa-romeo','audi','bmw','jaguar','peugeot','porsche','renault','saab','volkswagen','volvo')
apac <- c('honda','isuzu','mazda','mitsubishi','nissan','subaru','toyota')
us <- c('buick','chevrolet','dodge','mercury','plymouth')
carprice$car_origin<-ifelse(carprice$CarName %in% europe , "europe", ifelse(carprice$CarName %in% us , "us", "apac"))
carprice$car_origin<-as.factor(carprice$car_origin)


#To check the overall distribution of derived column
table(carprice$car_origin)


#############Dummy variable creation##################################
#Dummy variable creation for level 2 variables
levels(carprice$fueltype) <-c(0,1)
carprice$fueltype<-as.numeric(levels(carprice$fueltype))[carprice$fueltype]
levels(carprice$aspiration)<-c(0,1)
carprice$aspiration<-as.numeric(levels(carprice$aspiration))[carprice$aspiration]
levels(carprice$enginelocation)<-c(0,1)
carprice$enginelocation<-as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]


#Dummy Variable creation for variables which are having more than 2 levels
library(modelr) #This library comes with base package

#Fuel system - level 8
levels(carprice$fuelsystem)
dummy_1<-data.frame(model.matrix(~ fuelsystem, data = carprice))

dummy_1<-dummy_1[, -1]
#combine the dummy variable
carprice_final<-cbind(carprice[,-c(18)],dummy_1)
str(carprice_final)


#car_origin - level 3
dummy_2<-data.frame(model.matrix(~ car_origin, data = carprice))
dummy_2<-dummy_2[, -1]
carprice_final1<-cbind(carprice_final[,-c(26)],dummy_2)
str(carprice_final1)


#enginetype - level 7
dummy_3<-data.frame(model.matrix(~enginetype, data = carprice))
dummy_3<-dummy_3[, -1]
carprice_final3<-cbind(carprice_final1[,-c(15)],dummy_3)
str(carprice_final3)


#drivewheel- 3 levels
dummy_4<-data.frame(model.matrix(~drivewheel, data = carprice))
dummy_4<-dummy_4[, -1]
carprice_final4<-cbind(carprice_final3[,-c(8)],dummy_4)
str(carprice_final4)



#Carbody - levels 5
dummy_5<-data.frame(model.matrix(~ carbody, data = carprice))
dummy_5<-dummy_5[, -1]
carprice_final5<-cbind(carprice_final4[,-c(7)],dummy_5)

str(carprice_final5)


#Carname- level 22
dummy_6<-data.frame(model.matrix(~ CarName, data = carprice))


dummy_6<-dummy_6[, -1]

carprice_final6<-cbind(carprice_final5[,-c(3)],dummy_6)


#Creating final dataset which would be used for model building
Carprice_last<-carprice_final6


#Final dataset includes total 205 observations of 63 variables including target variable.  
str(Carprice_last)

##############################Model Building##########################


#set the seed to 1000
set.seed(1000)


#Save 70% of the data set as training dayaset and the remaining 30% as the testing dataset
trainindices<-sample(1:nrow(Carprice_last), 0.7*nrow(Carprice_last))
train.Carprice_last<-Carprice_last[trainindices, ]
test=Carprice_last[-trainindices,]



#Apply lm() and check summary
model<-lm(price~., data=train.Carprice_last) #R-squared:  0.9416
summary(model)


### Using StepAIC to select important features ##
step<-stepAIC(model,direction = 'both')
step

## creating second model based on stepAIC selection

model_1<-lm(price ~ car_ID + aspiration + doornumber + enginelocation + wheelbase + 
              carlength + carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + compressionratio + peakrpm + highwaympg + 
              fuelsystem4bbl + fuelsystemmpfi + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf + carbodyhatchback + carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamerenault + CarNamesaab + CarNametoyota, data=train.Carprice_last)

summary(model_1) ##R-squared:  0.961,	Adjusted R-squared:  0.9468

#Checking VIF values-Multicolinearity
vif(model_1)


# remove aspiration: it is not significant p-value=0.176144    > 0.05
model_2<-lm(price ~ car_ID + doornumber + enginelocation + wheelbase + 
              carlength + carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + compressionratio + peakrpm + highwaympg + 
              fuelsystem4bbl + fuelsystemmpfi + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf + carbodyhatchback + carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamerenault + CarNamesaab + CarNametoyota, data=train.Carprice_last)

summary(model_2)  ##R-squared:  0.9604,	Adjusted R-squared:  0.9464 
vif(model_2)


#Removing carlength: it is not significant p-value=0.081514    > 0.05
model_3<-lm(price ~ car_ID + doornumber + enginelocation + wheelbase+ carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + compressionratio + peakrpm + highwaympg + 
              fuelsystem4bbl + fuelsystemmpfi + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf + carbodyhatchback + carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamerenault + CarNamesaab + CarNametoyota, data=train.Carprice_last) 

summary(model_3) ##R-squared:  0.9592,	Adjusted R-squared:  0.9453 
vif(model_3)


#Removing CarNamerenault: it is not significant p-value=0.178418    > 0.05
model_4<-lm(price ~ car_ID + doornumber + enginelocation + wheelbase+ carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + compressionratio + peakrpm + highwaympg + 
              fuelsystem4bbl + fuelsystemmpfi + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf + carbodyhatchback + carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamesaab + CarNametoyota, data=train.Carprice_last)                

summary(model_4)  ##R-squared:  0.9585,	Adjusted R-squared:  0.9449
vif(model_4)


#Removing wheelbase: it is not significant p-value=0.211124    > 0.05
model_5<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + compressionratio + peakrpm + highwaympg + 
              fuelsystem4bbl + fuelsystemmpfi + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf + carbodyhatchback + carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamesaab + CarNametoyota, data=train.Carprice_last)                         

summary(model_5)  ##R-squared:  0.9579,	Adjusted R-squared:  0.9446
vif(model_5)


#Removing fuelsystemmpfi: it is not significant p-value=0.114070    > 0.05
model_6<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + compressionratio + peakrpm + highwaympg + 
              fuelsystem4bbl + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf + carbodyhatchback + carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamesaab + CarNametoyota, data=train.Carprice_last)                  

summary(model_6)   ##R-squared:  0.9569,	Adjusted R-squared:  0.9438
vif(model_6)


#Removing carbodyhatchback: it is not significant p-value=0.270730    > 0.05
model_7<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + compressionratio + peakrpm + highwaympg + 
              fuelsystem4bbl + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf +carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamesaab + CarNametoyota, data=train.Carprice_last)                  


summary(model_7)   ##R-squared:  0.9564,	Adjusted R-squared:  0.9437
vif(model_7)


#Removing compressionratio: it is not significant p-value=0.070761    > 0.05
model_8<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + peakrpm + highwaympg + 
              fuelsystem4bbl + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf +carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamesaab + CarNametoyota, data=train.Carprice_last)                 

summary(model_8)  ##R-squared:  0.9551,	Adjusted R-squared:  0.9425
vif(model_8)


#Removing highwaympg: it is not significant p-value=0.085104    > 0.05
model_9<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
              boreratio + stroke + peakrpm +  fuelsystem4bbl + car_origineurope + car_originus + 
              enginetypel + enginetypeohcf +carbodywagon + 
              CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
              CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
              CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNamesaab + CarNametoyota, data=train.Carprice_last)                


summary(model_9)  ##R-squared:  0.9538,	Adjusted R-squared:  0.9415
vif(model_9)


#Removing fuelsystem4bbl: it is not significant p-value=0.289293    > 0.05
model_10<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + stroke + peakrpm + car_origineurope + car_originus + 
               enginetypel + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
               CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNamesaab + CarNametoyota, data=train.Carprice_last)                     

summary(model_10) ##R-squared:  0.9534,	Adjusted R-squared:  0.9414
vif(model_10)


#Removing peakrpm: it is not significant p-value=0.119851    > 0.05
model_11<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + stroke + car_origineurope + car_originus + 
               enginetypel + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
               CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNamesaab + CarNametoyota, data=train.Carprice_last)                  

summary(model_11)  ##R-squared:  0.9524,	Adjusted R-squared:  0.9407
vif(model_11)


##Removing enginetypel: it is not significant p-value=0.054976    > 0.05 , vif=6.821571
model_12<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + stroke + car_origineurope + car_originus + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
               CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNamesaab + CarNametoyota, data=train.Carprice_last)               

summary(model_12)  ##R-squared:  0.9508,	Adjusted R-squared:  0.9392
vif(model_12)


###Removing stroke: it is not significant p-value=0.022130, vif=5.108544 >2
model_13<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + car_originus + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
               CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNamesaab + CarNametoyota, data=train.Carprice_last)               

summary(model_13)  ##R-squared:  0.9485,	Adjusted R-squared:  0.9369
vif(model_13)


#Removing CarNamesaab: it is not significant p-value=0.076842  >0.05
model_14<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + car_originus + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
               CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
              CarNametoyota, data=train.Carprice_last)                                  

summary(model_14)  ##R-squared:  0.9471,	Adjusted R-squared:  0.9358
vif(model_14)


##Removing CarNamedodge: it is not significant p-value=0.040163 , VIF -2.973985   >2
model_15<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + car_originus + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamehonda + CarNameisuzu + CarNamejaguar + 
               CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNametoyota, data=train.Carprice_last)                               

summary(model_15) ##R-squared:  0.9451,	Adjusted R-squared:  0.934
vif(model_15)


###Removing CarNamehonda: it is not significant p-value=0.016158 * , VIF= 9.276109   >2
model_16<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + car_originus + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNameisuzu + CarNamejaguar + 
               CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNametoyota, data=train.Carprice_last)                              

summary(model_16) ##R-squared:  0.9424,	Adjusted R-squared:  0.9312
vif(model_16)



#Removing CarNamemazda: it is not significant p-value=0.339950   >0.05
model_17<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + car_originus + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNameisuzu + CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNametoyota, data=train.Carprice_last)                              

summary(model_17)  ##R-squared:  0.9419,	Adjusted R-squared:  0.9313
vif(model_17)



#Removing CarNameisuzu: it is not significant p-value=0.437182   >0.05
model_18<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + car_originus + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNametoyota, data=train.Carprice_last)                                

summary(model_18)  ##R-squared:  0.9416,	Adjusted R-squared:  0.9315 
vif(model_18)


#Removing car_originus: it is not significant p-value=0.212769   >0.05
model_19<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNamemitsubishi + CarNamenissan + CarNameporsche + 
               CarNametoyota, data=train.Carprice_last)                                    

summary(model_19) ##R-squared:  0.9409,	Adjusted R-squared:  0.9312
vif(model_19)


#Removing CarNamenissan: it is not significant p-value=0.161980    >0.05
model_20<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNamemitsubishi + CarNameporsche + 
               CarNametoyota, data=train.Carprice_last)

summary(model_20) ##R-squared:  0.9399,	Adjusted R-squared:  0.9306 
vif(model_20)


#Removing CarNamemitsubishi: it is not significant p-value=0.134822    >0.05
model_21<-lm(price ~ car_ID + doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNameporsche + CarNametoyota, data=train.Carprice_last)

summary(model_21) ##R-squared:  0.9388,	Adjusted R-squared:  0.9299
vif(model_21)


#Removing car_ID: it is not significant p-value=0.129716    >0.05
model_22<-lm(price ~ doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + car_origineurope + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNameporsche + CarNametoyota, data=train.Carprice_last)

summary(model_22)  ##R-squared:  0.9376,	Adjusted R-squared:  0.9292
vif(model_22)


#Removing car_origineurope: it is not significant p-value=0.27841    >0.05
model_23<-lm(price ~ doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNameporsche + CarNametoyota, data=train.Carprice_last)

summary(model_23)  ##R-squared:  0.9371,	Adjusted R-squared:  0.9291 
vif(model_23)


#Removing CarNametoyota: it is not significant p-value=0.122875    >0.05
model_24<-lm(price ~ doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamechevrolet + 
               CarNamejaguar + CarNameporsche, data=train.Carprice_last)

summary(model_24) ##R-squared:  0.9359,	Adjusted R-squared:  0.9283 
vif(model_24)


#Removing CarNamechevrolet: it is not significant p-value=0.074596    >0.05
model_25<-lm(price ~doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar + CarNameporsche, data=train.Carprice_last)

summary(model_25) ##R-squared:  0.9342,	Adjusted R-squared:  0.927 
vif(model_25)


#Removing CarNameporsche: it is not significant p-value=0.044701 , VIF = 4.496995  >2
model_26<-lm(price ~ doornumber + enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar, data=train.Carprice_last)

summary(model_26) ##R-squared:  0.9321,	Adjusted R-squared:  0.9253 
vif(model_26)


#Removing doornumber: it is not significant p-value=0.011305 * , VIF = 1.369562 
model_27<-lm(price ~ enginelocation +carwidth + curbweight + cylindernumber + enginesize + 
               boreratio + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar, data=train.Carprice_last)

summary(model_27) ##R-squared:  0.9286,	Adjusted R-squared:  0.922
vif(model_27)


#Removing carwidth: it is not significant p-value=0.003883 **  , VIF = 5.941995  >2 
model_28<-lm(price ~ enginelocation +curbweight + cylindernumber + enginesize + 
               boreratio + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar, data=train.Carprice_last)

summary(model_28) ##R-squared:  0.9239,	Adjusted R-squared:  0.9175 
vif(model_28)


#Removing enginesize: it is not significant p-value=0.000174 *** but VIF = 14.767382 which is high. 
model_29<-lm(price ~ enginelocation +curbweight + cylindernumber +boreratio + enginetypeohcf +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar, data=train.Carprice_last)

summary(model_29) ##R-squared:  0.9152,	Adjusted R-squared:  0.9088
vif(model_29)


#Removing enginetypeohcf: it is not significant p-value=0.041918 * VIF = 2.133544  
model_30<-lm(price ~ enginelocation +curbweight + cylindernumber +boreratio +carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar, data=train.Carprice_last)

summary(model_30) ##R-squared:  0.9125,	Adjusted R-squared:  0.9066
vif(model_30)


#Removing boreratio: it is not significant p-value=0.052087  VIF = 2.567443  
model_31<-lm(price ~ enginelocation +curbweight + cylindernumber+carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar, data=train.Carprice_last)

summary(model_31) ##R-squared:   0.91,	Adjusted R-squared:  0.9046
vif(model_31)


#Removing cylindernumber: it is not significant p-value=0.01259 *   VIF = 2.135432  
model_32<-lm(price ~ enginelocation +curbweight + carbodywagon + 
               CarNameaudi + CarNamebmw + CarNamebuick + CarNamejaguar, data=train.Carprice_last)

summary(model_32) ##R-squared:  0.9057,	Adjusted R-squared:  0.9008 
vif(model_32)


###############Final Model with below statics ##############
#Multiple R-squared:  0.9057,	Adjusted R-squared:  0.9008
#VIF - all the variabls are less than 2. 
#All the variables are having P values <0.05 and 3 stars (***). 



#Predict the testing data
predict_1<-predict(model_32, test[-21])

#Amending the exisitng test data set with the predicted one
test$predicted_price<- predict_1

#Calculating R^2 for the Model
r<- cor(test$price, test$predicted_price)
rsquared<- r^2
rsquared



#######As Both the R^2 values (Train data- 0.9008 , test data-0.868486) are closer so this can be considered the BEST MODEL##################
# Driver factors are:
# 1. enginelocation
# 2. curbweight
# 3. carbodywagon
# 4. CarNameaudi
# 5. CarNamebmw
# 6. CarNamebuick

###################################Thank you############################################################
