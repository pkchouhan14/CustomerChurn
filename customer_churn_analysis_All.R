library(stats)
library(caTools)
library(Amelia)
library(plyr)
library(dplyr) # to analyse the data
#select(), which returns a subset of the columns,
#filter(), that is able to return a subset of the rows, 
#arrange(), that reorders the rows according to single or multiple variables,
#mutate(), used to add columns from existing data,
#summarise(), which reduces each group to a single row by calculating aggregate measures.

library(tidyr)  #to tidy the data
library(caret)  #for revalue function

# set your working directory

# read the telecom dataset input file
telecomDataframe <- read.csv(file="WA_Fn-UseC_-Telco-Customer-Churn.csv")

#####ways to looking into data for analysis#####
# print the structure of the dataframe
print(str(telecomDataframe))

#check summary of data
summary(telecomDataframe)

#check dimmension of the dataframe (rows, columns)
dim(telecomDataframe)

#display on few columns to teh console not crashing the window
glimpse(telecomDataframe)
#telecomDataframe <- tbl_df(telecomDataframe)
#telecomDataframe
#############################################

#####work on NA values in data #####
# check for the NA values 
any(is.na(telecomDataframe))

#count how many NA values
sum(is.na(telecomDataframe))

## relatively small in number then ignore those rows from the analysis
#telecomDataframe <- na.omit(telecomDataframe)

# visualize the missing values using the missing map from the Amelia package
missmap(telecomDataframe,col=c("yellow","red"))

#create new dataframe with rows with any NA in data
new_DF <- telecomDataframe[rowSums(is.na(telecomDataframe)) > 0,]

#changed total charges from 0 to monthly data, found after looking at new_DF
telecomDataframe <- within(telecomDataframe, TotalCharges[tenure==0] <- MonthlyCharges) 

# visualize again the missing values using the missing map from the Amelia package
missmap(telecomDataframe,col=c("yellow","red"))

###########Plot Variables####################
hist(telecomDataframe$tenure, xlab = "Tenure", main = "Dataset")
hist(telecomDataframe$SeniorCitizen, xlab = "SeniorCitizen", main = "Dataset")
hist(telecomDataframe$MonthlyCharges, xlab = "MonthlyCharges", main = "Dataset")
hist(telecomDataframe$TotalCharges, xlab = "TotalCharges", main = "Dataset")

#############################################
#check the number of unique values for each variable
apply(telecomDataframe, 2,function(x) length(unique(x)))

#how many obersevation for each variable type (Yes, No, other)
table(telecomDataframe$Churn)
table(telecomDataframe$gender)
table(telecomDataframe$SeniorCitizen)
table(telecomDataframe$Partner)
table(telecomDataframe$Dependents)
#table(telecomDataframe$tenure)
table(telecomDataframe$PhoneService)
table(telecomDataframe$MultipleLines)
table(telecomDataframe$InternetService)
table(telecomDataframe$OnlineSecurity)
table(telecomDataframe$OnlineBackup)
table(telecomDataframe$DeviceProtection)
table(telecomDataframe$TechSupport)
table(telecomDataframe$StreamingTV)
table(telecomDataframe$StreamingMovies)
table(telecomDataframe$Contract)
table(telecomDataframe$PaperlessBilling)
table(telecomDataframe$PaymentMethod)
#table(telecomDataframe$MonthlyCharges)
#table(telecomDataframe$TotalCharges)
#############################################

#storing telecommDataframe for my model use
original_tele <- telecomDataframe


# set the seed it will output same output when ever the model is executed
set.seed(123)

#####################website_code#########

# create new column "tenure_interval" from the tenure column
group_tenure <- function(tenure){
  if (tenure >= 0 && tenure <= 6){
    return('0-6 Month')
  }else if(tenure > 6 && tenure <= 12){
    return('6-12 Month')
  }else if (tenure > 12 && tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 && tenure <=36){
    return('24-36 Month')
  }else if (tenure > 36 && tenure <=48){
    return('36-48 Month')
  }else if (tenure > 48 && tenure <= 62){
    return('48-62 Month')
  }else if (tenure > 62){
    return('> 62 Month')
  }
}

# apply group_tenure function on each row of dataframe
telecomDataframe$tenure_interval <- sapply(telecomDataframe$tenure,group_tenure)
telecomDataframe$tenure_interval <- as.factor(telecomDataframe$tenure_interval)

# Ignore the variables with more levels while predicting the model
# Columns "customerID" and "tenure" having more levels
telecomDataframe <- select(telecomDataframe,-customerID,-tenure)

# The value of the following columns affecting the model and introducing the NA value for "No phone service" and  and "No internet service" need to cleanup the data for these columns MultipleLine,OnlineSecurity,OnlineBackup,DeviceProtection,TechSupport,StreamingTV,StreamingMovies
telecomDataframe$MultipleLines <- as.character(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.character(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.character(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.character(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.character(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.character(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.character(telecomDataframe$StreamingMovies)

# convert factor variables into character variables before changing the values
telecomDataframe$MultipleLines[telecomDataframe$MultipleLines=="No phone service"] <- "No"
telecomDataframe$OnlineSecurity[telecomDataframe$OnlineSecurity=="No internet service"] <- "No"
telecomDataframe$OnlineBackup[telecomDataframe$OnlineBackup=="No internet service"] <- "No"
telecomDataframe$DeviceProtection[telecomDataframe$DeviceProtection=="No internet service"] <- "No"
telecomDataframe$TechSupport[telecomDataframe$TechSupport=="No internet service"] <- "No"
telecomDataframe$StreamingTV[telecomDataframe$StreamingTV=="No internet service"] <- "No"
telecomDataframe$StreamingMovies[telecomDataframe$StreamingMovies=="No internet service"] <- "No"

# converting character variables into factor variables
telecomDataframe$MultipleLines <- as.factor(telecomDataframe$MultipleLines)
telecomDataframe$OnlineSecurity <- as.factor(telecomDataframe$OnlineSecurity)
telecomDataframe$OnlineBackup <- as.factor(telecomDataframe$OnlineBackup)
telecomDataframe$DeviceProtection <- as.factor(telecomDataframe$DeviceProtection)
telecomDataframe$TechSupport <- as.factor(telecomDataframe$TechSupport)
telecomDataframe$StreamingTV <- as.factor(telecomDataframe$StreamingTV)
telecomDataframe$StreamingMovies <- as.factor(telecomDataframe$StreamingMovies)

# check the number of NA rows if it is relatively small in number then ignore those rows from the analysis
telecomDataframe <- na.omit(telecomDataframe)

# set the seed it will output same output when ever the model is executed
set.seed(123)

# sample the input data with 70% for training and 30% for testing
sample <- sample.split(telecomDataframe$Churn,SplitRatio=0.70)
trainData <- subset(telecomDataframe,sample==TRUE)
testData <- subset(telecomDataframe,sample==FALSE)

# logistic regression model on top training the data
telecomModel <- glm(Churn ~ .,family=binomial(link="logit"),data=trainData)
print(summary(telecomModel))

# test the model with test dataset
test.predictions <- predict(telecomModel,newdata=testData,type="response")

# if the prediction probability is greater than 0.5 then those 
# customers are classified as churned customer less than 0.5 are classified as not churning customer
fitted.results <- ifelse(test.predictions > 0.5,1,0)
testData$Churn <- as.character(testData$Churn)
testData$Churn[testData$Churn=="No"] <- "0"
testData$Churn[testData$Churn=="Yes"] <- "1"

# calculating the misclassfication rate
misClasificationError <- mean(fitted.results!=testData$Churn)
print(misClasificationError)

# calculating the accuracy rate
accuracyRate <- 1-misClasificationError
print(accuracyRate)

# confusion matrix
table(testData$Churn,test.predictions > 0.5)

# cbinding actual results with the predicted results
results <- cbind(fitted.results,testData$Churn)
colnames(results) <- c("predicted","actual")
results <- as.data.frame(results)
#print(results)
#############################################

#get back orginal data for Random Forest impelmentation
telecomDataframe <- original_tele

###############Convert Variables format #####

telecomDataframe$gender <- revalue(telecomDataframe$gender, c("Male"=1))
telecomDataframe$gender <- revalue(telecomDataframe$gender, c("Female"=0))

telecomDataframe$Churn <- revalue(telecomDataframe$Churn, c("Yes"=1))
telecomDataframe$Churn <- revalue(telecomDataframe$Churn, c("No"=0))

telecomDataframe$Partner <- revalue(telecomDataframe$Partner, c("Yes"=1))
telecomDataframe$Partner <- revalue(telecomDataframe$Partner, c("No"=0))
#################Dependents
telecomDataframe$Dependents <- revalue(telecomDataframe$Dependents, c("Yes"=1))
telecomDataframe$Dependents <- revalue(telecomDataframe$Dependents, c("No"=0))

telecomDataframe$PhoneService <- revalue(telecomDataframe$PhoneService, c("Yes"=1))
telecomDataframe$PhoneService <- revalue(telecomDataframe$PhoneService, c("No"=0))

telecomDataframe$OnlineBackup <- revalue(telecomDataframe$OnlineBackup, c("Yes"=1))
telecomDataframe$OnlineBackup <- revalue(telecomDataframe$OnlineBackup, c("No"=0))
telecomDataframe$OnlineBackup <- revalue(telecomDataframe$OnlineBackup, c("No internet service"=2))

telecomDataframe$OnlineSecurity <- revalue(telecomDataframe$OnlineSecurity, c("Yes"=1))
telecomDataframe$OnlineSecurity <- revalue(telecomDataframe$OnlineSecurity, c("No"=0))
telecomDataframe$OnlineSecurity <- revalue(telecomDataframe$OnlineSecurity, c("No internet service"=2))

telecomDataframe$DeviceProtection <- revalue(telecomDataframe$DeviceProtection, c("Yes"=1))
telecomDataframe$DeviceProtection <- revalue(telecomDataframe$DeviceProtection, c("No"=0))
telecomDataframe$DeviceProtection <- revalue(telecomDataframe$DeviceProtection, c("No internet service"=2))

telecomDataframe$TechSupport <- revalue(telecomDataframe$TechSupport, c("Yes"=1))
telecomDataframe$TechSupport <- revalue(telecomDataframe$TechSupport, c("No"=0))
telecomDataframe$TechSupport <- revalue(telecomDataframe$TechSupport, c("No internet service"=2))

telecomDataframe$StreamingMovies <- revalue(telecomDataframe$StreamingMovies, c("Yes"=1))
telecomDataframe$StreamingMovies <- revalue(telecomDataframe$StreamingMovies, c("No"=0))
telecomDataframe$StreamingMovies <- revalue(telecomDataframe$StreamingMovies, c("No internet service"=2))

telecomDataframe$StreamingTV <- revalue(telecomDataframe$StreamingTV, c("Yes"=1))
telecomDataframe$StreamingTV <- revalue(telecomDataframe$StreamingTV, c("No"=0))
telecomDataframe$StreamingTV <- revalue(telecomDataframe$StreamingTV, c("No internet service"=2))

telecomDataframe$PaperlessBilling <- revalue(telecomDataframe$PaperlessBilling, c("Yes"=1))
telecomDataframe$PaperlessBilling <- revalue(telecomDataframe$PaperlessBilling, c("No"=0))

telecomDataframe$MultipleLines <- revalue(telecomDataframe$MultipleLines, c("Yes"=1))
telecomDataframe$MultipleLines <- revalue(telecomDataframe$MultipleLines,  c("No"=0))
telecomDataframe$MultipleLines <- revalue(telecomDataframe$MultipleLines,  c("No phone service"=2))

telecomDataframe$InternetService <- revalue(telecomDataframe$InternetService, c("DSL"=1))
telecomDataframe$InternetService <- revalue(telecomDataframe$InternetService,  c("No"=0))
telecomDataframe$InternetService <- revalue(telecomDataframe$InternetService,  c("Fiber optic"=2))

telecomDataframe$Contract <- revalue(telecomDataframe$Contract, c("One year"=1))
telecomDataframe$Contract <- revalue(telecomDataframe$Contract,  c("Month-to-month"=0))
telecomDataframe$Contract <- revalue(telecomDataframe$Contract,  c("Two year"=2))

telecomDataframe$PaymentMethod <- revalue(telecomDataframe$PaymentMethod, c("Bank transfer (automatic)"=1))
telecomDataframe$PaymentMethod <- revalue(telecomDataframe$PaymentMethod,  c("Mailed check"=0))
telecomDataframe$PaymentMethod <- revalue(telecomDataframe$PaymentMethod,  c("Credit card (automatic)"=2))
telecomDataframe$PaymentMethod <- revalue(telecomDataframe$PaymentMethod,  c("Electronic check"=3))

#############################################

# Column "customerID" is removed as it is not playing any role for making decision
telecomDataframe <- select(telecomDataframe,-customerID)


######################
## Decision Tree    ##
######################
library(tree)

#select_rows = createDataPartition(telecomDataframe$Churn, p =2/3, list =FALSE)
select_rows<-sample(1:nrow(telecomDataframe), round(0.2*nrow(telecomDataframe)), replace = F)
ChurnTest<-telecomDataframe[select_rows,]
ChurnTrain<-telecomDataframe[-(select_rows),]


modelRegTree<-tree(Churn~tenure+MonthlyCharges+Dependents,data = ChurnTrain)
plot(modelRegTree)
text(modelRegTree,pretty = 5,ces=1)
pred<-predict(modelRegTree,newdata = ChurnTest, type = "class")
#convert possibility to return the label
#head(pred, n=5)
#pred
str(ChurnTest)
str(pred)
confusionMatrix(pred,ChurnTest$Churn)


######################
#####random forest 
######################
library(MASS)
library(randomForest)

# setting the seed so that we get same result each time we run the random forest
set.seed(123)

#check NA in data
new_DF <- telecomDataframe[rowSums(is.na(telecomDataframe)) > 0,]

#changed total charges from 0 to monthly data
telecomDataframe <- within(telecomDataframe, TotalCharges[tenure==0] <- MonthlyCharges) 

###drops <- c("customerID")
###telecomDataframe[, !(names(telecomDataframe) %in% drops)]

#####remove customerID
telecomDataframe <- subset(telecomDataframe,select = -c(customerID))

sample <- sample.split(telecomDataframe$Churn,SplitRatio=0.70)
trainData <- subset(telecomDataframe,sample==TRUE)
testData <- subset(telecomDataframe,sample==FALSE)

################ RANDOM FOREST CODE

####Cross validation

ControlParameters <- trainControl(method = "cv", number = 5, savePredictions = TRUE, classProbs = TRUE)
parameterGrid <- expand.grid(mtry=c(2,3,4))
modelRandom <- train(Churn~., data = trainData, method = "rf", trControl = ControlParameters, tuneGrid = parameterGrid)

rf <- randomForest(Churn ~., data=trainData)
rf <- randomForest(Churn ~., data=trainData, ntree =50, mtry =2 , importance = TRUE, proximity = TRUE)


print(rf)
####check the attributes of random forest
attributes(rf)

####chekc the value of any random forest using $ sign, here its confusion matrix
rf$confusion

#Prediction and consufion matrix
p1<-predict(rf,trainData)
confusionMatrix(p1,trainData$Churn)


#rf <- randomForest(Churn ~., data=testData)
p2<-predict(rf,testData)
confusionMatrix(p2,testData$Churn)

#error rate of random forest
plot(rf)


#tune mtry
t<- tuneRF(trainData[,-20],trainData[,20], stepFactor = 0.5, plot = TRUE, ntreeTry = 50, trace = TRUE, improve =0.05)


#no. of nodes for each trees
hist(treesize(rf), main = "No. of Nodes for the trees", col = "green")

varImpPlot(rf)

importance(rf)
varUsed(rf)

#Patial dependence plot

partialPlot(rf, trainData, TotalCharges, "1")
partialPlot(rf, trainData, TotalCharges, "0")

#extract single tree from random forest
getTree(rf, 1, labelVar = TRUE)

#Multi-dimensional scaling plot of proximity matrix
#MDSplot(rf,trainData$Churn)


################

#split data in test and training 
library(caTools)

#ind<-sample.split(Y = telecomDataframe, SplitRatio = 0.7)
#trainData <-telecomDataframe[ind,]
#testData <- telecomDataframe[!ind,]

####fitting the model
###mtry = number of variables selected at each split
modelRandom <- randomForest(Churn~tenure+MonthlyCharges+Dependents+Partner, data = ChurnTrain)
plot(modelRandom)
#text(modelRandom,pretty = 5,ces=1)
predForest<-predict(modelRandom,newdata = ChurnTest, type = "class")
#convert possibility to return the label
head(predForest, n=5)
#pred
library(caret)
str(ChurnTest)
str(predForest)
confusionMatrix(predForest,ChurnTest$Churn)

