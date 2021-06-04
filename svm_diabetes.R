#Pima Indians Diabetes 
#A data frame with 768 observations on 9 variables.

#Load the necessary packages

library(mlbench)
library(e1071)
library(caret)
library(caTools)


#Load the data
data("PimaIndiansDiabetes2")


# inspect the data --------------------------------------------------------

head(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)


#Remove the columns with highest frequency of NAs 

df <- PimaIndiansDiabetes2
df$triceps <- NULL
df$insulin <- NULL

#Omit NAs from other columns
df <- na.omit(df)



# Normalise the features --------------------------------------------------

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df[1:6] <- as.data.frame(lapply(df[1:6], normalize))

summary(df)


# Create training and test set --------------------------------------------

splitcc <- sample.split(df, SplitRatio = 0.7)
train <- subset(df, splitcc == "TRUE")
test <- subset(df, splitcc == "FALSE")


# Testing Models SVM ------------------------------------------------------

#Minimum Accuracy
table(train$diabetes)
minimum <- 264/414

#Create a random svm model with kernel = linear and cost = 0.1
set.seed(123)
model <- svm(diabetes~., data = train, kernel = "linear", cost = 0.1, scale = FALSE)
summary(model)
pred <- predict(model, train)

#Misclassification error = % 25.8

confusionMatrix( train$diabetes, pred)


# Tuning ------------------------------------------------------------------

#tune for optimal kernel  and cost value

set.seed(123)
tuned <- tune(svm, diabetes~., data = train, 
              ranges = list(epsilon = seq(0,1,0.1),  
                            cost = (1:10)))

summary(tuned)


#darker regions equal to lower misclassification errors

plot(tuned)

#choosing best model

best_svm_model <- tuned$best.model

# The kernel is radial and cost = 8 for the best model

summary(best_svm_model)

#Comapre on training set

pred_2 <- predict(best_svm_model, train)

#misclassification error = % 11.8
#the model has improved significantly 

confusionMatrix( train$diabetes, pred_2)

# Compare on test set

test_pred <- predict(best_svm_model, test)

confusionMatrix(test$diabetes, test_pred)

#Accuracy % 76.77
#This is still not accurate enough to diagnose diabetes


