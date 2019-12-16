df <- read.csv("student-mat.csv", sep = ";")
# Explore the data
any(is.na(df)) # Check if there are any na value in the dataset
###summary(df)
###str(df)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
### Num only
num.cols <- sapply(df, is.numeric)
cor.cols <- cor(df[,num.cols])
print(cor.cols)
corrplot(cor.cols, method = 'color')
corrgram(df)
corrgram(df,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
ggplot(df, aes(x = G3)) + 
        geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# Run model
## Split data into train set and test set
library(caTools)
### set a seed
set.seed(101)
# Split up sample
eg <- sample.split(df$G3, SplitRatio = 0.7)
## 70% of data --> Train
train <- subset(df, eg == TRUE)
## 30% of data --> Test
test <- subset(df, eg == FALSE)

## Train and build model
model <- lm(G3 ~., data = train)
print(summary(model))
### Plot Residual
res <- residuals(model)
res <- as.data.frame(res)
ggplot(res, aes(res)) + 
        geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
###plot(model)
## Predictions
G3.predictions <- predict(model, test)
results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted', 'actual')
results <- as.data.frame(results)

## Take care of neg values
to_zero <- function(x){
        if(x <0){
                return(0)
        }else{
                return(x)
        }
}

### apply zero function
results$predicted <- sapply(results$predicted, to_zero)
## Mean Square Error
mse <- mean( (results$predicted - results$actual)^2 )
print('MSE')
mse
###RMSE
print("Squared Root of MSE")
mse^0.5

## Errors
SSE <- sum((results$predicted-results$actual)^2)
SST <- sum((mean(df$G3)-results$actual)^2)

R2 <- 1-SSE/SST
R2