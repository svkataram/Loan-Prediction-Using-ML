library(randomForest)
library(caret)
library(e1071)

loan_train <- read.csv("C:/Users/svkat/Downloads/Loan_dataset.csv", header = TRUE)
loan_train$Credit_History_f <- as.factor(loan_train$Credit_History_f)
loan_train$Loan_Status <- as.factor(loan_train$Loan_Status)  # Ensure Loan_Status is a factor

set.seed(1234)
pd <- sample(2, nrow(loan_train), replace = TRUE, prob = c(0.8, 0.2))
train <- loan_train[pd == 1, ]
validate <- loan_train[pd == 2, ]

rownames(train) <- train[, 1]
train[, 1] <- NULL
rownames(validate) <- validate[, 1]
validate[, 1] <- NULL

set.seed(222)
rf <- randomForest(Loan_Status ~ ., data = train, ntree = 145, mtry = 5, importance = TRUE, proximity = TRUE)
print(rf)
plot(rf)

p1 <- predict(rf, train)
p2 <- predict(rf, validate)
confusionMatrix(p2, validate$Loan_Status)

tuneRF(x = subset(train, select = -Loan_Status), y = train$Loan_Status, stepFactor = 0.5, plot = TRUE, ntreeTry = 100, trace = TRUE, improve = 0.05)

hist(treesize(rf), main = 'No. Of nodes for the trees', col = 'Red')
varImpPlot(rf)
importance(rf)
varUsed(rf)