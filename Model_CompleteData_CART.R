data <- read.csv(file = "F:\\SCIT\\Sem2\\PA\\case2_MCA_beneish\\IMB579-XLS-ENG (1)_CompleteData.csv")

set.seed(1234)
data$CMANIPULATOR <- factor(data$CMANIPULATOR)
str(data)

data = subset(data, select = -c(Manipulater, Company.ID))
str(data)

#install.packages("caTools")
library(caTools)

split <- sample.split(data$CMANIPULATOR, SplitRatio = 0.75)

train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

#install.packages("DMwR")
library(DMwR)

balanced.data <- SMOTE(CMANIPULATOR ~., train_data, perc.over = 100 )
prop.table(table(balanced.data$CMANIPULATOR))

#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree <- rpart(CMANIPULATOR ~ ., data = balanced.data, control = rpart.control(cp = 0.0001))
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)


t_pred = predict(tree.pruned,test_data,type="class")
confMat <- table(test_data$CMANIPULATOR,t_pred)
confMat

library(rattle)

fancyRpartPlot(tree.pruned)
