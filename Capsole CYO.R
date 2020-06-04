# Install all needed libraries if it is not present

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(e1071)) install.packages("e1071")
if(!require(class)) install.packages("class")
if(!require(ROCR)) install.packages("ROCR")
if(!require(randomForest)) install.packages("randomForest")
if(!require(PRROC)) install.packages("PRROC")
if(!require(reshape2)) install.packages("reshape2")
if(!require(funModeling)) install.packages("funModeling")
if(!require(corrplot)) install.packages("corrplot")
if(!require(dplyr)) install.packages("dplyr")
if(!require(gridExtra))install.packages("gridExtra")
# Loading all needed libraries

library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)
library(class)
library(ROCR)
library(randomForest)
library(PRROC)
library(reshape2)
library(funModeling)
library(corrplot)
library(dplyr)

## Loading the dataset from my github profile

pdata <- read.csv("https://raw.githubusercontent.com/danielhaye17/Harvard-X-Capsole-Project/master/datasets_indian_liver_patient.csv")


#********************* Data Evaluation **********************

#Finding the amount of observations and variables in the datset
dim(pdata)


#Viewing the datatypes of the variables in the dataset
str(pdata)

#Finding how many different ages are in the dataset
n_distinct(pdata$Age)


#Finding the number of males and females in the dataset
pdata%>%group_by(Gender)%>%summarise(count=n())


#Finding the top 5 ages represented in the dataset
pdata%>%group_by(Age)%>%summarise(count=n())%>%
  arrange(desc(count))%>%head(n=5)


#Checking to see the number ofliver disease patients
pdata%>%group_by(Dataset)%>%summarise(count=n())


#Finding out if there any data with N/As
sapply(pdata, function(g){
  sum(is.na(g))
})


#Viewing the NAs in the Albumin_and_Globulin_Ratio variable
pdata%>%filter(is.na(Albumin_and_Globulin_Ratio))


#Finding the mean of the Albumin_and_Globulin_Ratio variable
mean(pdata$Albumin_and_Globulin_Ratio,na.rm = TRUE)


#Viewing the first 10 rows of the dataset
pdata%>%head(n=10)


#***************** Data Cleaning **************************

#1.	Replace the NAs in the Albumin and Globulin ratio with 0s

pdata$Albumin_and_Globulin_Ratio[is.na(pdata$Albumin_and_Globulin_Ratio)]<- 
  mean(pdata$Albumin_and_Globulin_Ratio,na.rm = TRUE)

sapply(pdata, function(g){
  sum(is.na(g))})

#2.	Replacing the 2s in the Dataset column with 0s

pdata<- pdata%>%mutate(Dataset= ifelse(Dataset ==2,0,1))


#3.	Create a column for Indirect Bilirubin 

pdata<- pdata%>%mutate(Indirect_Bilirubin= 
                         Total_Bilirubin - Direct_Bilirubin )

#Changing the order of the dataset
pdata<- pdata%>%select(Age,Gender,Total_Bilirubin,
                       Direct_Bilirubin,Indirect_Bilirubin,
                       Alkaline_Phosphotase, Alamine_Aminotransferase,
                       Aspartate_Aminotransferase,Total_Protiens,
                       Albumin,Albumin_and_Globulin_Ratio, Dataset)


#4. Change Dataset variable name to Disease_Status

pdata<- pdata%>%mutate(Disease_Status=Dataset)%>%select(-Dataset)




#Print the first 10 rows in the dataset   

pdata%>%head(n=10)  

#*************************** Data Visualization *****************************

#Frequency between Male and Female

pdata %>%
  ggplot(aes(Gender, fill= Gender)) +
  theme_minimal()  +
  geom_bar() +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Frequency between Male and Female",
       x = "Gender",
       y = "Frequency")


#Liver Patient Status vs Density

pdata%>%ggplot( aes(Disease_Status)) + 
  geom_histogram(aes(y = ..density..),binwidth = 1, colour = "black", fill = "white")+
  geom_density(alpha = 0.2, fill = "#FF6666") + ggtitle("Liver Disease Status vs Density")

#Table showing the distribution of liver patients by Gender
pdata %>%group_by(Gender)%>%summarise(Num_Liver_Patients = sum(Disease_Status))   



# Plotting all numeric data
plot_num(pdata %>% select(-Gender), bins=10)  



# Check the correlation between chemicals 
correlationMatrix <- cor(pdata[,3:ncol(pdata)])
corrplot(correlationMatrix, type = "upper", order = "hclust", 
         tl.col = "black",tl.cex = 1, addrect = 8) 


#Selecting which parameter to take out
#Since we started correlationMatrix at column 3 of pdata, to get back the
#Index, we added 2 to the index so it would start at 3

highl_Correlated <- findCorrelation(correlationMatrix, cutoff=0.9)
head(pdata[highl_Correlated+2])


#Correlation between Total_Bilirubin and Indirect_Bilirubin
ggplot(pdata, aes(x=Total_Bilirubin, y=Indirect_Bilirubin, fill=Disease_Status)) +
  geom_point(size=2, shape=23)


#*********************** Modelling ************************

#Calculating and plotting PCA
pca_pdata <- prcomp(pdata[,3:ncol(pdata)], center = TRUE, scale = TRUE)
plot(pca_pdata, type="l")


#Finding the level of Importance for each paramter
summary(pca_pdata)


#Removing Total Biliribin from the model
pdata<-pdata%>%select(-Total_Bilirubin)


#Calculating and plotting PCA
pca_pdata <- prcomp(pdata[,3:ncol(pdata)], center = TRUE, scale = TRUE)
plot(pca_pdata, type="l")


#Finding the level of Importance for each paramter
summary(pca_pdata)


# Setting outcome as a 2 level factor variable 
pdata<- pdata%>%mutate(Disease_Status= 
                         ifelse(Disease_Status ==1,"yes","no"))
pdata$Disease_Status <- as.factor(pdata$Disease_Status)



# Split the dataset into train and test set
# Set seed as a starting point

set.seed(1, sample.kind='Rounding')
train_index <- createDataPartition(
  y = pdata$Disease_Status, 
  p = 0.2, 
  list = F
)

training_set <- pdata[-train_index,]
test_set <- pdata[train_index,]


#The dimension of the training set      
dim(training_set)   

#The dimension of the test set      
dim(test_set)


# Define training control
#method = cv -> Cross Validation method
#number = 10 -> The number of folds
# Set seed as a starting point

set.seed(1, sample.kind='Rounding')
train.control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)



#******************* Logistic Regression model **************************

set.seed(1, sample.kind='Rounding')

glm_fit<- train(Disease_Status ~., data = training_set , method = "glm",
                metric = "ROC",
                preProcess = c("scale", "center"),  # in order to normalize the data
                trControl= train.control,
                family = "binomial")

# Predciting on the test data
glm_pred<- predict( glm_fit, test_set)

# Calculating the confusion matrix
glm_conf_matrix <- confusionMatrix(glm_pred, test_set$Disease_Status, positive = "yes")

#Storing the Accuracy and sensitivity level for glm model
glm_Accuracy <- glm_conf_matrix$overall[['Accuracy']]
glm_Sensitivity <- sensitivity(glm_pred, test_set$Disease_Status, positive="yes")


#Calculating F1 Score

precision <- posPredValue(glm_pred, test_set$Disease_Status, positive="yes")
recall <- sensitivity(glm_pred, test_set$Disease_Status, positive="yes")

glm_F1_Score <- (2 * precision * recall) / (precision + recall)


# Creating a data frame to store the results from our models
model_results <- data.frame(model="Logistic Regression Model", 
                            Accuracy =glm_Accuracy,
                            Sensitivity = glm_Sensitivity ,
                            F1_Score = glm_F1_Score)

table("Accuracy" = glm_Accuracy, "Sensitivity" = glm_Sensitivity, 
      "F1_Score" = glm_F1_Score  )



#********************** K-nearest neighbors (kNN) Model ***************

set.seed(1, sample.kind='Rounding')

knn_fit<- train(Disease_Status ~., data = training_set , method = "knn",
                metric = "ROC",
                preProcess = c("scale", "center"),  # in order to normalize the data
                trControl= train.control,
                tuneLength=10)


# Predciting on the test data
knn_pred<- predict( knn_fit, test_set)

# Calculating the confusion matrix
knn_conf_matrix <- confusionMatrix(knn_pred, test_set$Disease_Status, positive = "yes")


#Storing the Accuracy and sensitivity level for knn model
knn_Accuracy <- knn_conf_matrix$overall[['Accuracy']]
knn_Sensitivity <- sensitivity(knn_pred, test_set$Disease_Status, positive="yes")


#Calculating F1 Score

precision <- posPredValue(knn_pred, test_set$Disease_Status, positive="yes")
recall <- sensitivity(knn_pred, test_set$Disease_Status, positive="yes")

knn_F1_Score <- (2 * precision * recall) / (precision + recall)

#Adding the result to the data frame created earlier
model_results <- model_results %>% add_row(model="K-Nearest Neighbors Model",
                                           Accuracy =knn_Accuracy,
                                           Sensitivity = knn_Sensitivity ,
                                           F1_Score = knn_F1_Score)

#Table of results                                    
table("Accuracy" = glm_Accuracy, "Sensitivity" = glm_Sensitivity, 
      "F1_Score" = glm_F1_Score  )

#********************  Random Forrest Model ********************************     

set.seed(1, sample.kind='Rounding')

rf_fit<- train(Disease_Status ~., data = training_set , method = "rf",
               metric = "ROC",
               preProcess = c("scale", "center"),  # in order to normalize the data
               trControl= train.control,
               tuneLength=10)


# Predciting on the test data
rf_pred<- predict( rf_fit, test_set)

# Calculating the confusion matrix
rf_conf_matrix <- confusionMatrix(rf_pred, test_set$Disease_Status, positive = "yes")

#Storing the Accuracy and sensitivity level for knn model
rf_Accuracy <- rf_conf_matrix$overall[['Accuracy']]
rf_Sensitivity <- sensitivity(rf_pred, test_set$Disease_Status, positive="yes")


#Calculating F1 Score

precision <- posPredValue(rf_pred, test_set$Disease_Status, positive="yes")
recall <- sensitivity(rf_pred, test_set$Disease_Status, positive="yes")

rf_F1_Score <- (2 * precision * recall) / (precision + recall)

#Adding the result to the data frame created earlier
model_results <- model_results %>% add_row(model="Random Forrest Model",
                                           Accuracy =rf_Accuracy,
                                           Sensitivity = rf_Sensitivity ,
                                           F1_Score = rf_F1_Score)
#Table of results
table("Accuracy" = glm_Accuracy, "Sensitivity" = glm_Sensitivity, 
      "F1_Score" = glm_F1_Score  )


#******************** Support Vector Machines Algorithm ******************  

set.seed(1, sample.kind='Rounding')

svm_fit<- svm(Disease_Status~., data = training_set, kernel = "linear")
svm_fit
summary(svm_fit)

svm_pred = predict(svm_fit,test_set)
svm_conf_matrix <- confusionMatrix(reference = 
                                     as.factor(test_set$Disease_Status), data = svm_pred)

svm_Accuracy <- svm_conf_matrix$overall[['Accuracy']]
svm_Sensitivity <- sensitivity(svm_pred, test_set$Disease_Status, positive="yes")


#Calculating F1 Score

precision <- posPredValue(svm_pred, test_set$Disease_Status, positive="yes")
recall <- sensitivity(svm_pred, test_set$Disease_Status, positive="yes")

svm_F1_Score <- (2 * precision * recall) / (precision + recall)

#Adding the result to the data frame created earlier
model_results <- model_results %>% add_row(model="Support Vector Model",
                                           Accuracy =svm_Accuracy,
                                           Sensitivity = svm_Sensitivity ,
                                           F1_Score = svm_F1_Score)
#Table of results
table("Accuracy" = svm_Accuracy, "Sensitivity" = svm_Sensitivity, 
      "F1_Score" = svm_F1_Score)


# **************************** Results ********************************** 
print(model_results)










