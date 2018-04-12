# Check all columns for N/A's
sapply(hr_employee_attrition, function(x) sum(is.na(x)))

# Get an idea for the number of unique values in each column:
# #1) unique count > 32
# #2) unique count < 2
#
toolarge <- c()
tlcount <- c()
lessthan2 <- c()
nmax <- length(names(hr_employee_attrition))
for (colidx in 1:nmax) {
    colname <- names(hr_employee_attrition)[colidx]
    ucount <- length( unique(hr_employee_attrition[,colidx]) )
    
    if (ucount > 32) {
       toolarge <- c(toolarge, colidx)
       tlcount <- c(tlcount, ucount)
    }
    if (ucount < 2) {
      lessthan2 <- c(lessthan2, colidx)
    }
}

nmax <- length(toolarge)
for (idx in 1:nmax) {
    colidx <- toolarge[idx]
    colname <- names(hr_employee_attrition)[colidx]
    cat( sprintf("Column index %d - %s has %d unique values\n", colidx, colname, tlcount[idx]) )
}

cat( sprintf("\n\n" ) )

nmax <- length(lessthan2)
for (idx in 1:nmax) {
  colidx <- lessthan2[idx]
  colname <- names(hr_employee_attrition)[colidx]
  cat( sprintf("Column index %d - %s had 1 unique value\n", colidx, colname) )
}


# Investigate: DailyRate, MonthlyRate, MonthlyIncome, MonthlyRate

#hr_reduced[c("DailyRate")] <- list(NULL)
#hr_reduced[c("HourlyRate")] <- list(NULL)
#hr_reduced[c("MonthlyIncome")] <- list(NULL)
#hr_reduced[c("MonthlyRate")] <- list(NULL)

# http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf
# https://www.quora.com/How-should-I-handle-unbalanced-data-while-using-randomForest-in-R
