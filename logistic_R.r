# Titanic
df.train <- read.csv("titanic_train.csv")
print(head(df.train))
print('      ')
str(df.train)
## Check how much data is missing("NA")
library("Amelia")
missmap(df.train, main = "Missing Map", col = c("yellow","black"), legend = FALSE)
library(ggplot2)
library(ggthemes)
ggplot(df.train, aes(Survived)) + geom_bar()
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))
ggplot(df.train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))
ggplot(df.train, aes(Age)) + 
        geom_histogram(bins = 20, alpha = 0.5, fill = "blue")
ggplot(df.train, aes(SibSp)) + geom_bar()
ggplot(df.train, aes(Fare)) + 
        geom_histogram(fill = 'green', color = 'black', alpha = 0.5)
pl <- ggplot(df.train, aes(Pclass, Age))
pl <- pl + geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.4))
pl <- pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))
pl + theme_bw()
## Imputation of Age based on Class
impute_age <- function(age, class){
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
                        out[i] <- age[i]
                }
        }
        return(out)
}
fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages
missmap(df.train, main = "Imputation Check", col = c("yellow", "black"), legend = FALSE)
## Feature engineering
library(dplyr)
df.train <- select(df.train, -PassengerId, -Name, -Ticket, -Cabin)
head(df.train,3)

###str(df.train)
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

## log model
log.model <- glm(Survived ~., family = binomial(link = 'logit'), data = df.train)
summary(log.model)

## Prediction
df.test <- read.csv("titanic_test.csv")
df.test$Age <- impute_age(df.test$Age, df.test$Pclass)
#missmap(df.test, main = "Imputation Check", col = c("yellow", "black"), legend = FALSE)
df.test$Pclass <- factor(df.test$Pclass)
df.test$Parch <- factor(df.test$Parch)
df.test$SibSp <- factor(df.test$SibSp)
df.test <- select(df.test, -PassengerId, -Name, -Ticket, -Cabin)
df.test <- df.test[df.test$Parch != 9,]
fitted.prob <- predict(log.model, df.test, type = "response")
fitted.results <- ifelse(fitted.prob > 0.5, 1, 0)
