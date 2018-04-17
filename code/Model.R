#Clean data set used in RandomForest (Top 3 Factors)
hr_reduced <- hr_employee_attrition[,-nzv]
attach(hr_reduced)
#Age Class was removed and orginal Age column added back in. There was an issue with AgeClass list in rf prediction.
names(hr_reduced)[1] <- "Age"

# set the record counts
samplesize <- (nrow(hr_reduced))*.75
datasize <- nrow(hr_reduced)
train=sample(1:nrow(hr_reduced),samplesize)

#WAll the Predictors in the dataset are utilized here.
hr_reduced.rf <- randomForest(JobSatisfaction ~ . , data = hr_reduced , subset = train)
hr_reduced.rf

#The above Mean Squared Error and Variance explained are calculated using Out of Bag Error Estimation.In this 
samplesize# records of Training data is used for training and the remaining 
datasize - samplesize #is used to Validate the Trees. Also, the number of variables randomly selected at each split is 10.

plot(hr_reduced.rf)     

#This plot shows the Error and the Number of Trees.We can easily notice that how the Error is dropping as we keep on adding more and more trees and average them.


oob.err=double(31)
test.err=double(31)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:31) 
{
  rf=randomForest(JobSatisfaction ~ . , data = hr_reduced, subset = train,mtry=mtry,ntree=500) 
  oob.err[mtry] = rf$mse[500] #Error of all Trees fitted
  
  pred<-predict(rf,hr_reduced[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(hr_reduced[-train,], mean( (JobSatisfaction - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

test.err
oob.err


matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

