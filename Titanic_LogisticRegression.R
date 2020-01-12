# We are working on the titanic dataset to build a Logistic Regression model to predict the probablity of survival of passengers based on the information like their sex, age and passenger class.

### Importing Libraries ###

install.packages('Amelia')
library(Amelia)
library(ggplot2)
library(dplyr)


### Importing data ###


df.train <- read.csv('/Users/ck250894/Desktop/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_train.csv')
head(df.train)
str(df.train)
df.test <- read.csv('/Users/ck250894/Desktop/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/titanic_test.csv')
head(df.test)
str(df.test)


### EDA ##


# Checking for missing values in the dataset

missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)


missmap(df.test, main="Titanic Test Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

# We can see that around 20% of Age data is missing in train dataset



# Count of people survived in train dataset

ggplot(df.train,aes(Survived)) + geom_bar()


# Count of different class of passengers in train dataset

ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)),alpha=0.5)


# Count of sex of passengers in train dataset

ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=factor(Sex)),alpha=0.5)


# Distribution of age of passengers in train dataset

ggplot(df.train,aes(Age)) + geom_histogram(fill='blue',bins=20,alpha=0.5)


# Distribution with respect to the number of siblings or spouses of passengers on board in train dataset

ggplot(df.train,aes(SibSp)) + geom_bar(fill='red',alpha=0.5)

# Distribution of fare paid by passengers in train dataset

ggplot(df.train,aes(Fare)) + geom_histogram(fill='green',color='black',alpha=0.5)




### Data Cleaning ####


pl <- ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))

# Imputation of age based on class in train and test datasets

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

fixed.ages.train <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages.train

fixed.ages.test <- impute_age(df.test$Age,df.test$Pclass)
df.test$Age <- fixed.ages.test

missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

missmap(df.test, main="Titanic Test Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

# Now we can see that there are no missing values in Age column of train and test data
# We can ignore the missing values in fare column of test data as we are not using it for building the model



### Building Logistic Regression Model ####


str(df.train)
head(df.train,3)

# We will remove columns that are not needed


df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train,3)
str(df.train)

df.test <- select(df.test,-PassengerId,-Name,-Ticket,-Cabin)
head(df.test,3)
str(df.test)

# Converting columns from int type to factor
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

df.test$Pclass <- factor(df.test$Pclass)
df.test$Parch <- factor(df.test$Parch)
df.test$SibSp <- factor(df.test$SibSp)

str(df.train)
str(df.test)


## Training the model

log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = df.train)
summary(log.model)


## Predicting using test data


# Applying model on test data
fitted.probabilities <- predict(log.model,newdata=df.test,type='response')

# Predicting values
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

# Calculating accuracy
misClasificError <- mean(fitted.results != final.test$Survived)
print(paste('Accuracy',1-misClasificError))
# "Accuracy 0.798507462686567"


# Creating Confusion matrix
table(final.test$Survived, fitted.probabilities > 0.5)
#    FALSE TRUE
# 0   140   25
# 1    29   74