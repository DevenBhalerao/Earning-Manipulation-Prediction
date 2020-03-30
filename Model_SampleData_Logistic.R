data <- read.csv(file = "F:\\SCIT\\Sem2\\PA\\case2_MCA_beneish\\IMB579-XLS-ENG (1)_ModelData.csv")


data$C.MANIPULATOR <- factor(data$C.MANIPULATOR)

data = subset(data, select = -c(Manipulator, Company.ID))

library(caTools)

split <- sample.split(data$C.MANIPULATOR, SplitRatio = 0.75)

train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

library(DMwR)

balanced.data <- SMOTE(C.MANIPULATOR ~., train_data, perc.over = 100 )
prop.table(table(balanced.data$C.MANIPULATOR))

library(caret)

model <- glm (C.MANIPULATOR~., data=balanced.data, family = binomial)
summary(model)

predict <- predict(model, test_data, type = 'response')
table(test_data$C.MANIPULATOR, predict > 0.5)

library(ROCR)
ROCRpred <- prediction(predict, test_data$C.MANIPULATOR)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))



