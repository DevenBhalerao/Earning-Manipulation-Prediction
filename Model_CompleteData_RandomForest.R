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
library(randomForest)
library(e1071)
rf = randomForest(CMANIPULATOR~.,  
                  ntree = 100,
                  data = balanced.data)
plot(rf)
varImp(rf)
varImpPlot(rf,  
           sort = T,
           n.var=25,
           main="Variable Importance")

predicted.response <- predict(rf, test_data)

confusionMatrix(data=predicted.response,  
                reference=test_data$CMANIPULATOR)
