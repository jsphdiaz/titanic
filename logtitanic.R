


library(Amelia)
library(ggplot2)



train <- read.csv("titanic_train.csv")

test <- read.csv("titanic_test.csv")

head(train)
head(test)

str(train)

#visualizing missing map

missmap(train, main = 'Missing Map', col = c('yellow', 'black'))


#visualization

ggplot(train, aes(Survived)) + geom_bar()

ggplot(train, aes(Survived)) + 
  geom_bar(aes(fill = factor(Pclass)))

ggplot(train, aes(Survived)) + 
  geom_bar(aes(fill = factor(Sex)))


# Dealing with missing values

ggplot(train, aes(x = Pclass, y = Age)) +
  geom_boxplot((aes(group = Pclass, fill = factor(Pclass))))

####imputation of Age based on class mean

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


####

fixed.ages <- impute_age(train$Age,train$Pclass)

train$Age <- fixed.ages

missmap(train, main = 'Missing Map', col = c('yellow', 'black'))

#####

library(dplyr)

train<- select(train, -PassengerId, -Name, -Ticket, -Cabin)
head(train)



train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)
train$Parch <- factor(train$Parch)
train$SibSp <- factor(train$SibSp)



str(train)
#######

log.model <- glm(Survived ~. , family = binomial(link = 'logit'), data = train)

summary(log.model)



library(caTools)

set.seed(101)
split <- sample.split(train$Survived, SplitRatio = 0.7)
final.train <- subset(train, split == TRUE)
final.test <- subset(train, split == FALSE)


final.log.model <- glm(Survived ~., family = binomial(link = 'logit'),data = final.train)

summary(final.log.model)


fitted.probabilities <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilities>0.5,1,0)
misClassError <- mean(fitted.results != final.test$Survived)
1-misClassError

#Accuracy of .7985

#confusion Matrix

table(final.test$Survived, fitted.probabilities>0.5)
