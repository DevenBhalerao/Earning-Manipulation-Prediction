library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)

data <- read.csv(file = "F:\\SCIT\\Sem2\\PA\\case2_MCA_beneish\\IMB579-XLS-ENG (1)_CompleteData.csv")
data = subset(data, select = -c(Manipulater, Company.ID))

ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

train <- SMOTE(CMANIPULATOR ~., train, perc.over = 100 )
prop.table(table(balanced.data$CMANIPULATOR))

test <- SMOTE(CMANIPULATOR ~., test, perc.over = 100 )
prop.table(table(balanced.data$CMANIPULATOR))

trainm <- sparse.model.matrix(CMANIPULATOR ~ .-1, data = train)
train_label <- train[,"CMANIPULATOR"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(CMANIPULATOR~.-1, data = test)
test_label <- test[,"CMANIPULATOR"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

nc <- length(unique(train_label))
xgb_params <- list("objective" = "binary:logistic",
                   "eval_metric" = "error"
                   )
watchlist <- list(train = train_matrix, test = test_matrix)

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       eta = 0.01
                      )

e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_error, col = 'blue')
lines(e$iter, e$test_error, col = 'red')

min(e$test_error)

imp <- xgb.importance(colnames(train_matrix), model = bst_model)
imp
xgb.plot.importance(imp)

p <- predict(bst_model, newdata = test_matrix)
p <- ifelse (p > 0.5,1,0)
library(caret)
confusionMatrix (as.factor(p), as.factor(test_label))
