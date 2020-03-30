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

library(e1071) 

SVMformula <- svm(CMANIPULATOR ~., data = train_data)
classifier = svm(formula = CMANIPULATOR ~ ., 
                 data = train_data, 
                 type = 'C-classification', 
                 kernel = 'linear') 


summary(classifier)
predicted = predict(classifier, newdata = test_data[-9]) 
cm = table(test_data$CMANIPULATOR, predicted)
cm

