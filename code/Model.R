library(utils)
library(dplyr)
library(knitr)
library(randomForest)
library(caret)
dataFileCsv <- "../data/CaseStudy2data.csv"
hr_employee_attrition <- read.csv(dataFileCsv)
attach(hr_employee_attrition)
nzv <- nearZeroVar(hr_employee_attrition)
uniq <- paste(shQuote(names(hr_employee_attrition)[nzv], type="cmd"), collapse=", ")
cat( sprintf("Remove near zero value columns: %s\n", uniq) )
hr_reduced <- hr_employee_attrition[,-nzv]
detach(hr_employee_attrition)
names(hr_reduced)[1] <- "Age"
attach(hr_reduced)
samplesize <- (nrow(hr_reduced))*.70
datasize <- nrow(hr_reduced)
train=sample(1:datasize,samplesize)
features <- as.numeric(ncol(hr_reduced)-1)

hr_reduced.rf <- randomForest(Attrition ~ . , data = hr_reduced , subset = train)
hr_reduced.rf


plot(hr_reduced.rf, main = "RandomForest - Error vs Trees Chart")    
oob.err=double(as.numeric(features))
test.err = double(as.numeric(features))

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:as.numeric(features))
{
  rf=randomForest(Attrition ~ . , data = hr_reduced , subset = train,mtry=mtry,ntree=500) 
  oob.err[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,hr_reduced[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(hr_reduced[-train,], mean( (Attrition - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
}
test.err
oob.err
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
hr_reduced.lm <- glm(relevel(as.factor(Attrition),ref="Yes") ~ relevel(as.factor(JobRole),ref="Research Scientist") + relevel(as.factor(OverTime),ref="Yes") + DistanceFromHome, data = hr_reduced, family =binomial())

summary(hr_reduced.lm)

df.res <- resid(hr_reduced.lm)
df.res
plot(df.res, ylab = "Residuals", main = "Residuals - Age ~ JobRole + OverTime + DistanceFromHome")


detach(hr_reduced)
detach(hr_reduced.rf)