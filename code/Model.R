library(utils)
library(dplyr)
library(knitr)
library(randomForest)
library(caret)
library(party)

dataFileCsv <- "../data/CaseStudy2data.csv"
hr_employee_attrition <- read.csv(dataFileCsv)
attach(hr_employee_attrition)
nzv <- nearZeroVar(hr_employee_attrition)
uniq <- paste(shQuote(names(hr_employee_attrition)[nzv], type="cmd"), collapse=", ")
cat( sprintf("Remove near zero value columns: %s\n", uniq) )
hr_reduced <- hr_employee_attrition[,-nzv]
detach(hr_employee_attrition)
names(hr_reduced)[1] <- "Age"
set.seed(402)
ind <- sample(2, nrow(hr_reduced), replace = TRUE, prob = c(0.7, 0.3))
train <- hr_reduced[ind==1,]
test <- hr_reduced[ind==2,]
samplesize <- (nrow(hr_reduced))*.70
datasize <- nrow(hr_reduced)
features <- as.numeric(ncol(hr_reduced)-1)

hr_reduced.rf <- randomForest(Attrition ~ . , data = hr_reduced , subset = train)
hr_reduced.rf

#PFigure 1
plot(hr_reduced.rf, main = "RandomForest - Error vs Trees Chart")  
set.seed(222)
rf <- randomForest(Attrition~., data=train,
                   ntree = 300,
                   mtry = 10,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)
attributes(rf)

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$Attrition)

#  Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$Attrition)

#Error Rate of Random Forest
plot(rf)

# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "Blue")
# Tune mtry
t <- tuneRF(train[,-2], train[,2],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)

# Partial Dependence Plot
partialPlot(rf, train, Attrition, "Yes")

# Extract Single Tree
getTree(rf, 150, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$Attrition)

#Decision tree with party
library(party)
mytree <- ctree(Attrition~., hr_reduced, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(mytree)
plot(mytree,type="simple")

#Misclassification error
tab<-table(predict(mytree), mydata$Attrition)
print(tab)
1-sum(diag(tab))/sum(tab)