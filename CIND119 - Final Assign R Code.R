library(tidyverse)
library(dplyr) 
library(ggplot2)
library(e1071)



bank<-read.csv("Desktop/bank.csv", 
               stringsAsFactors = FALSE, header = T)
bank$job_unk <- ifelse(bank$job == "unknown", 1, 0)
bank$edu_unk <- ifelse(bank$education == "unknown", 1, 0)
bank$cont_unk <- ifelse(bank$contact == "unknown", 1, 0)
bank$pout_unk <- ifelse(bank$poutcome == "unknown", 1, 0)
bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default<- ifelse(bank$default == "yes", 1, 0)
bank$housing <- ifelse(bank$housing== "yes", 1, 0)
bank$loan<- ifelse(bank$loan== "yes", 1, 0)
bank$month <- as.numeric(as.factor(bank$month))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))
bank$y <- as.factor(bank$y)

bank_info <- bank %>% 
  mutate(job = as.factor(job),
         marital = as.factor(marital),
         education = as.factor(education),
         default = as.factor(default),
         housing = as.factor(housing),
         loan = as.factor(loan),
         contact = as.factor(contact),
         month = as.factor(month),
         poutcome = as.factor(poutcome),
         subscribe = as.factor(y)) %>% 
  select(-c(y))

str(bank)
colSums(is.na(bank))
bank_set <- subset(bank_info, select = -c(duration))

install.packages("GGally")
numericCols <- unlist(lapply(bank_set, is.numeric))
show_plot(inspect_num(bank[,numericCols]))

show_plot(inspect_cor(subset(bank, select = -c(y))))


summary.data.frame(bank_set)

table(bank$y)

install.packages("klaR")
library(caret)

set.seed(888)
TrainIndex <- createDataPartition(bank$y, p = .75,
                                     list = FALSE)
train <- bank[ TrainIndex,]
test <- bank[-TrainIndex,]


prop.table(table(train$y))

nrow(train)

prop.table(table(test$y))
nrow(test)

NBModel <- train(train[,-17], train$y, 
                 method = "nb",trControl= trainControl(method = "cv", number = 10))
NBModel
NBPredictions <-predict(NBModel, test)
confusionMatrix(NBPredictions, test$y)

