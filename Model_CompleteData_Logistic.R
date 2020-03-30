data <- read.csv(file = "F:\\SCIT\\Sem2\\PA\\case2_MCA_beneish\\IMB579-XLS-ENG (1)_CompleteData.csv")

data$CMANIPULATOR <- factor(data$CMANIPULATOR)

data = subset(data, select = -c(Manipulater, Company.ID))

library(caTools)

split <- sample.split(data$CMANIPULATOR, SplitRatio = 0.75)

train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

library(DMwR)

balanced.data <- SMOTE(CMANIPULATOR ~., train_data, perc.over = 100 )
prop.table(table(balanced.data$CMANIPULATOR))

library(caret)

model <- glm (CMANIPULATOR~., data=balanced.data, family = binomial)
summary(model)

predict <- predict(model, test_data, type = 'response')
table(test_data$CMANIPULATOR, predict > 0.5)

library(ROCR)
ROCRpred <- prediction(predict, test_data$CMANIPULATOR)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))



