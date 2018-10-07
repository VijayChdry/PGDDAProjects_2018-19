################################# SVM handwritten digit recognition Model #######################################

#To develop a model using Support Vector Machine which should correctly classify the handwritten digits based on the pixel values given as features.

#1. Business Understanding
#2. Data Understanding
#3. Data Preparation
#4. Model Building
#   4.1 Linear Model
#   4.2 Polynomial Model
#   4.3 Non linear(RBF) Model
#5. Hyperparameter tuning and cross validation
#6. Result

####################################################################################################

#1. Business Understanding

#A classic problem in the field of pattern recognition is that of handwritten digit recognition. 
#Suppose that you have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices. 
#The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image.

#####################################################################################################

#2.Data Understanding

#Train dataset has 60000 Obs. of 785 Variables
#Test dataset has 10000 Obs of 785 Variables
#First column of both the dataset are the target variable (Digits to be predicted)
#It was advised by Student Mentors to sample 15% of the train dataset to avoid any computaion challenges due to big data

######################################################################################################

#3. Data Preparation

#Loading Neccessary libraries
library(kernlab)
library(readr)
library(caret)
library(tidyverse)
library(gridExtra)
library(skimr)
library(caTools)

#reading the provided CSV files and converting to data frame
mnist_train<-read.csv("mnist_train.csv", stringsAsFactors = FALSE, header = FALSE)
mnist_test<-read.csv("mnist_test.csv", stringsAsFactors = FALSE, header = FALSE)


#Renaming target variable (first column)in both the dataset as 'label' and convert to factor
colnames(mnist_train)[1]<-"label"
colnames(mnist_test)[1]<-"label"
mnist_train$label<-factor(mnist_train$label)
mnist_test$label<-factor(mnist_test$label)


#data summary statistics 
skim(mnist_train)


#checking missing value
sapply(mnist_train, function(x) sum(is.na(x)))
sum(is.na(mnist_train)) #No missing values
sum(is.na(mnist_test))  #No Misisng values


#Sampling 15% of the mnist_train dataset
set.seed(1)
trainindices<-sample(1:nrow(mnist_train), 0.15*nrow(mnist_train))
train<-mnist_train[trainindices,]


#As the variables are numeric , scaling the train and test dataset
train[, -1]<-train[, -1]/255
mnist_test[, -1]<-mnist_test[, -1]/255


########################################################################################################

#4.   Model Building



#4.1 Using Linear model
Model_linear <- ksvm(label~ ., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, mnist_test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear, mnist_test$label)     #Accuracy : 0.9211

#Evaluation on training set
Eval_linear_trainset<- predict(Model_linear, train)
confusionMatrix(Eval_linear_trainset, train$label) #Accuracy : 0.9966 

###there is larger gap between train accuracy and test accuracy with linear model.
###it gives tha sign of overfitting.





#4.2 Using Polynomial Kernel

#Degree =2
Model_poly<-ksvm(label ~., data=train, scale = FALSE, kernel = "polydot", kpar=list(degree=2))
Eval_poly <- predict(Model_poly, mnist_test)

#confusion matrix - polynomial Kernel
confusionMatrix(Eval_poly, mnist_test$label)  #Accuracy : 0.963


#Degree = 3
Model_poly1<-ksvm(label ~., data=train, scale = FALSE, kernel = "polydot", kpar=list(degree=3))
Eval_poly1 <- predict(Model_poly1, mnist_test)

#confusion matrix - polynomial Kernel
confusionMatrix(Eval_poly1, mnist_test$label)  #Accuracy : 0.9572

#Degree = 4
Model_poly2<-ksvm(label ~., data=train, scale = FALSE, kernel = "polydot", kpar=list(degree=4))
Eval_poly2 <- predict(Model_poly2, mnist_test)

#confusion matrix - polynomial Kernel
confusionMatrix(Eval_poly2, mnist_test$label)  #Accuracy : 0.9465

###As the accuracy is continuously decreasing hence it will not be worth to check with higher degree here






#4.3 Using RBF kernel
Model_RBF <- ksvm(label~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF, mnist_test$label) #Accuracy : 0.9587

#Evaluation on training set
Eval_RBF_trainset<- predict(Model_RBF, train)
confusionMatrix(Eval_RBF_trainset, train$label) #Accuracy : 0.9808 

### With RBF Kernel (default parameters), the gap is shorter, and hence
### we can tune the model through cross validation for better results.



#5.############ Hyperparameter tuning and Cross Validation #######################################

# We will use the train function from caret package to perform crossvalidation
#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.
trainControl <- trainControl(method="cv", number=5, verboseIter=TRUE)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"


#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.
fit.svm <- train(label~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)



# Printing cross validation result
print(fit.svm)   

# sigma  C    Accuracy   Kappa    
#0.025  0.1  0.9269979  0.9188420
#0.025  0.5  0.9565546  0.9517020
#0.025  1.0  0.9624431  0.9582483
#0.025  2.0  0.9646652  0.9607185
#0.050  0.1  0.8655548  0.8505305
#0.050  0.5  0.9505543  0.9450334
#0.050  1.0  0.9607768  0.9563968
#0.050  2.0  0.9613326  0.9570146

###Accuracy was used to select the optimal model using largest values
###In the final model the optimal values are used sigma = 0.025, C = 2


########################### 
# Lets build a model with sigma = 0.025, C = 2
Model_RBF_final <- ksvm(label~.,data=train, kernel="rbfdot",scale=FALSE,C=2,kpar=list(sigma=0.025))
Eval_RBF_final<- predict(Model_RBF_final, mnist_test)

#confusion matrix - final RBF Kernel[Accuracy on test dataset]
confusionMatrix(Eval_RBF_final, mnist_test$label) #Accuracy : 0.969
#Overall accuracy is 0.969 for the final model on test dataset


# Lets check accuracy on training set
eval_RBF_final_trainset <- predict(Model_RBF_final, train)
confusionMatrix(eval_RBF_final_trainset,train$label) #Accuracy : 0.9991
#Overall accuracy is 0.9991 for trianing dataset


# There is very less difference between train and test accuracy, hence we can say that over-fitting
# is not the case, our model is able to predict correct digits using Non-Linear SVM to a large extent.


#################################################################################################################



## Result

#There seems to be non-linearity in data as the value of hyper parameter
# sigma is of the order of 0.025. However, it is also seen that having even this magnitude of non-lineariry
# is helping in increasing the performance (accuracy).
# After performing cross validation (five fold) for "rbfdot" kernel, it has been seen that
# maximum accuracy can be seen with C=2 and sigma=0.025, the train accuracy is also very good.
# Also with these hyper parameters, the test accuracy is comparable with train accuracy, eliminating
# chances of over fitting as well. Also the specificity and sensitivity across different levels (1,2,3,etc.)
# is very good for both train and test.


# Final Hyper Parameters :
# C=2
# sigma = 0.025


############################################### THANK YOU ###################################################
