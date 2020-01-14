## We have a dataset of incomes of adults along with their occupation, employment type, marital status, age, hours worked per week and the country they reside in
## Using this data, we will build  a supervised machine learning model to predict the incomes of individuals

## Importing libraries

library(dplyr)
library(Amelia)
library(ggplot2)
library(caTools)


## Importing data

adult <- read.csv('/Users/ck250894/Desktop/R-Course-HTML-Notes/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv')
head(adult)


# Removing double index
adult <- select(adult,-X)

str(adult)
summary(adult)

### Data Cleaning ###

table(adult$type_employer)

# We can see that type_employer has 9 levels
# We will group some of those levels together for better analysis

# Grouping Never worked and without pay into unemployed column

unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,unemp)
table(adult$type_employer)

# Grouping Gov jobs and self employed records together
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}

adult$type_employer <- sapply(adult$type_employer,group_emp)
table(adult$type_employer)

# We can do similar operation for marital column
# Grouping records into Not-Married, Never-Married and Married categories
table(adult$marital)
group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

table(adult$country)
levels(adult$country)

# Grouping the countries according to the continents

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')


group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)
table(adult$country)

str(adult)

# Converting '?' into NA values

adult[adult == '?'] <- NA
table(adult$type_employer)

# Converting type_employer, marital, occupation and country columns into factors

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

# Renaming country column to 'region' as we have grouped them by regions
names(adult)[names(adult)=="country"] <- "region"

# Plotting missing values using missmap function
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# We can see that there are only 1% of missing values
# Thus we can drop them without having much affect on our results

adult <- na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

str(adult)



### EDA ###

# Plotting histogram of age, colored by income

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

# We can see that the number of people earing >50k increases after 25 years of age and gradually decreases after 50 years of age

# Plotting histogram of hours worked per week

ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()

# We can see a spike at 40 hours per week mark

# Creating a barplot of region filled with color of income

ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### Building Model ###

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

# Training the model
model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)

# Testing the model to predict income
test$predicted.income = predict(model, newdata=test, type="response")

# Creating a confusion matrix
table(test$income, test$predicted.income > 0.5)
#        FALSE TRUE
# <=50K  6372  548
# >50K    872 1423

Acc = (6372+1423)/(6372+1423+548+872)
Acc
# [1] 0.8459034

Recall = 6732/(6372+548)
Recall
# [1] 0.9728324

Precision = 6732/(6372+872)
Precision
# [1] 0.9293208



