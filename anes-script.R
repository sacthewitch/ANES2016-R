# install packages ----

install.packages("readxl")
install.packages('writexl')
install.packages("tidyverse")
install.packages('dplyr')
install.packages('klaR')

# load libraries ----
library(readxl)
library(writexl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(GGally)
library(car)
library(nnet)
library(MASS)
library(klaR)


# Load Data
path <- "df_anes.xlsx"
data_file <- read_excel(path)
data_file
typeof(data_file)


# Creating the matrix of dataset
df_anes <- data.frame(data_file)
summary(df_anes)

length(df_anes$Trump)

### clean data ----

# Based on the assumption that partner and spouse education do not affect,
# DROP Partner and SpouseEdu columns
df_anes <- select(df_anes, -c(Partner, SpouseEdu))

# Drop rows with negative values
df_anes <- df_anes %>%
  filter( Media != -9 & Media!= -8 & FamSize != -9 & 
           Hillary != -9 & Hillary != -8 & Trump !=-9 & Trump !=-8 &
           Age != -9 & Age != -8 &
           Education != -9 & Employment != -9 & Birthplace != -9 & Birthplace != -8 & 
            GBirth != -9 & GBirth != -8 & Dependent != -9 & Housing != -9 & Housing != -8 & 
           Income != -9 & Income != -5, )

# Drop Rows with NA using 
df_anes <- df_anes %>%
          na.omit()

# export cleaned dataset into a excel so that the lecturer can run the program.
write_xlsx(df_anes, 'df_anes.xlsx')
# please un-comment below lines
#df_anes <- data.frame(read_excel('df_anes.xlsx'))


# split dataset into training set and testing set
# for the use of Question (b)
copy_df <- data.frame(df_anes) 

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(copy_df), replace=TRUE, prob=c(0.7,0.3))
train <- copy_df[sample, ]
test <- copy_df[!sample, ] 

# Recode the variable Trump as follows. 
# Denote Slightly liberal to Extremely liberal (levels 1-3) as “Liberal”, 
# and Moderate to Extremely conservative (levels 4-7) as “Conservative” ----

### Re-code the Trump variable to categorical values
for(i in 1 : length(df_anes$Trump)){
  trump_column_value <- df_anes$Trump[i]
  if( trump_column_value == 1 |  trump_column_value == 2 |  trump_column_value == 3 ) {
    df_anes$Trump[i] <- "Liberal"
  }
  else if(trump_column_value == 4 | trump_column_value == 5 | trump_column_value == 6 | trump_column_value == 7){
    df_anes$Trump[i] <- "Conservative"
  }
  
}

df_anes

# transform categorical variables into factors
df_anes$Media <- as.factor(df_anes$Media)
df_anes$Trump <- as.factor(df_anes$Trump)
df_anes$Education <- as.factor(df_anes$Education)
df_anes$Employment <- as.factor(df_anes$Employment)
df_anes$Birthplace <- as.factor(df_anes$Birthplace)
df_anes$GBirth <- as.factor(df_anes$GBirth)
df_anes$Housing <- as.factor(df_anes$Housing)
df_anes$Income <- as.factor(df_anes$Income)
df_anes$Education2 <- as.factor(df_anes$Education2)
df_anes$PartyID <- as.factor(df_anes$PartyID)
df_anes$Marital <- as.factor(df_anes$Marital)

# Check data
sapply(df_anes, class)

levels(df_anes$Media)
levels(df_anes$Trump)
levels(df_anes$Education)
levels(df_anes$Employment)
levels(df_anes$Birthplace)
levels(df_anes$GBirth)
levels(df_anes$Housing)
levels(df_anes$Income)
levels(df_anes$Education2)
levels(df_anes$PartyID)
levels(df_anes$Marital)

str(df_anes$Media)
str(df_anes$Trump)
str(df_anes$Education)
str(df_anes$Employment)
str(df_anes$Birthplace)
str(df_anes$GBirth)
str(df_anes$Housing)
str(df_anes$Income)
str(df_anes$Education2)
str(df_anes$PartyID)
str(df_anes$Marital)

#fit logistic regression model
glm_fit <- glm(
  df_anes$Trump ~  df_anes$Media + df_anes$Age  + df_anes$Education + 
    df_anes$Employment + df_anes$Birthplace + df_anes$GBirth  + df_anes$Housing + 
    df_anes$Income + df_anes$Education2 + df_anes$PartyID + df_anes$Marital , 
    data = df_anes , family = binomial)

# view summary of data
summary(glm_fit)

# The p-value for the predictor variable “Media” is high. So, remove it.
glm_fit_x <- glm(
  df_anes$Trump ~  df_anes$Age  + df_anes$Education + df_anes$Employment + 
    df_anes$Birthplace + df_anes$GBirth  + df_anes$Housing + df_anes$Income + 
    df_anes$Education2 + df_anes$PartyID + df_anes$Marital , 
    data = df_anes , family = binomial)

summary(glm_fit_x)

# The p-value for the predictor variable “Education” is high. So, remove it.
glm_fit_x <- glm(
  df_anes$Trump ~  df_anes$Age + df_anes$Birthplace + df_anes$GBirth  + 
    df_anes$Housing + df_anes$Income + df_anes$Education2 + df_anes$PartyID + 
    df_anes$Marital , data = df_anes , family = binomial)

summary(glm_fit_x)

# The p-value for the predictor variable “Age” is high. So, remove it.
glm_fit_x <- glm(
  df_anes$Trump ~  df_anes$Employment + 
    df_anes$Birthplace + df_anes$GBirth  + df_anes$Housing + df_anes$Income + 
    df_anes$Education2 + df_anes$PartyID + df_anes$Marital , data = df_anes , 
    family = binomial)

summary(glm_fit_x)

# The p-value for the predictor variable “Employment” is high. So, remove it.
glm_fit_x <- glm(
  df_anes$Trump ~  df_anes$Birthplace + df_anes$GBirth  + df_anes$Housing + 
    df_anes$Income + df_anes$Education2 + df_anes$PartyID + df_anes$Marital , 
    data = df_anes , family = binomial)

summary(glm_fit_x)

# variable importance
caret::varImp(glm_fit_x)

max(caret::varImp(glm_fit_x))
# most important predictor variable: Education2
min(caret::varImp(glm_fit_x))
# least important predictor variable: Housing


# calculate VIF values for each predictor variable in the model
vif(glm_fit_x)


# Building a prediction model to predict an individual’s party identification 
#using the respective individual’s personal, and family characteristics. ----

# fit LDA model
model_partyID <- lda(PartyID ~ . , data = train)

# view model output
model_partyID

# use LDA model to make predictions on test data
predicted_partyID <- predict(model_partyID, data = test)

names(predicted_partyID)

# view predicted class for the first six observations in the test set
head(predicted_partyID$class)

# view posterior probabilities for first six observations in test set
head(predicted_partyID$posterior)

# view linear discriminant for first six observations in test set
head(predicted_partyID$x)

# create plots

# stacked histogram
ldahist(predicted_partyID$x[,1], g = predicted_partyID$class) # data from LD1

ldahist(predicted_partyID$x[,2], g = predicted_partyID$class) # data from LD2

ldahist(predicted_partyID$x[,3], g = predicted_partyID$class) # data from LD3


#find accuracy of model

# Confusion Matrix and Accuracy of test data
cm <- predict(model_partyID, test)$class
cm_table <- table(Predicted = cm, Actual = test$PartyID)
accuracy1 <- sum(diag(cm_table))/sum(cm_table)
accuracy1

# Quadratic Discriminant Analysis
quadratic <- qda(PartyID~., data=train)

qd <- predict(quadratic, test)$class
qd_table <- table(Predicted = qd, Actual = test$PartyID)
accuracy2 <- sum(diag(qd_table))/sum(qd_table)
accuracy2





