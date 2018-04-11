# Remove the fields that have ZERO Variability
# That is, all rows contain the same exact value
nzv <- nearZeroVar(hr_employee_attrition)
uniq <- paste(shQuote(nzv, type="cmd"), collapse=", ")
cat( sprintf("Remove near zero value columns: %s\n", uniq) )
hr_reduced <- hr_employee_attrition[,-nzv]


# Remove EmployeeNumber - this has nothing to do with attrition, and not
# a measure of anything about the employee
cat( sprintf("Remove EmployeeNumber\n") )
hr_reduced[c("EmployeeNumber")] <- list(NULL)



cat( sprintf("Create new Variable AgeClass from Age by range of Age\n\n") )
hr_reduced$AgeClass <- lapply(hr_reduced$Age, ageClassification)
# Now remove the Age column
hr_reduced[c("Age")] <- list(NULL)

cat( sprintf("Create new Variable DailyRateClass from DailyRate by range of DailyRate\n\n") )
hr_reduced$DailyRateClass <- arbitraryClassification(hr_reduced$DailyRate, c(300,500,700,900,1100,1300,1500))
hr_reduced[c("DailyRate")] <- list(NULL)

cat( sprintf("Create new Variable HourlyRateClass from HourlyRate by range of HourlyRate\n\n") )
hr_reduced$HourlyRateClass <- arbitraryClassification(hr_reduced$HourlyRate, c(40,50,60,70,80,90,101))
hr_reduced[c("HourlyRate")] <- list(NULL)

cat( sprintf("Create new Variable MonthlyIncomeClass from MonthlyIncome by range of MonthlyIncome\n\n") )
hr_reduced$MonthlyIncomeClass <- arbitraryClassification(hr_reduced$MonthlyIncome, seq(2000,20000,2000))
hr_reduced[c("MonthlyIncome")] <- list(NULL)

cat( sprintf("Create new Variable MonthlyRateClass from MonthlyRate by range of MonthlyRate\n\n") )
hr_reduced$MonthlyRateClass <- arbitraryClassification(hr_reduced$MonthlyRate, seq(3000,27000,3000))
hr_reduced[c("MonthlyRate")] <- list(NULL)

cat( sprintf("Create new Variable TotalWorkingYearsClass from TotalWorkingYears by range of TotalWorkingYears\n\n") )
hr_reduced$TotalWorkingYearsClass <- arbitraryClassification(hr_reduced$TotalWorkingYears, c(10,20,30,41))
hr_reduced[c("TotalWorkingYears")] <- list(NULL)

cat( sprintf("Create new Variable YearsAtCompanyClass from YearsAtCompany by range of YearsAtCompany\n\n") )
hr_reduced$YearsAtCompanyClass <- arbitraryClassification(hr_reduced$YearsAtCompany, c(10,20,30,41))
hr_reduced[c("YearsAtCompany")] <- list(NULL)



cat( length(colnames(hr_reduced)) )

k <- min(table(hr_reduced[,1]))/2

# mtry default value is best with:  ntree=200, replace=FALSE, importance=TRUE, sampsize=c(k,k)
# replace default value with: ntree=200, importantance = TRUE, sampsize=c(k,k)
#       No gets better, Yes gets worse
clf <- randomForest(hr_reduced[,-1], hr_reduced[,1], ntree=500, replace=FALSE, importantance = TRUE, mtry=15, sampsize=c(k,k))




rfanaly <- varyNTree(hr_reduced, 1)
