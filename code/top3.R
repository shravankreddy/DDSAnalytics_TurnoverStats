
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


# Remove the fields that have ZERO Variability
# That is, all rows contain the same exact value
nzv <- nearZeroVar(hr_employee_attrition)
hr_reduced <- hr_employee_attrition[,-nzv]


# Remove EmployeeNumber - this has nothing to do with attrition, and not
# a measure of anything about the employee
hr_reduced[c("EmployeeNumber")] <- list(NULL)


# Break up the Age Variable into AgeClass
# Teen Twenties
ageClassification <- function(x) {
  if (x < 24) {
    ageClass <- 1
  } else if (x < 30) {
    ageClass <- 2
  } else if (x < 36) {
    ageClass <- 3
  } else if (x < 42) {
    ageClass <- 4
  } else if (x < 48) {
    ageClass <- 5
  } else if (x < 55) {
    ageClass <- 6
  } else {
    ageClass <- 7
  }
  return (ageClass)
}
hr_reduced$AgeClass <- lapply(hr_reduced$Age, ageClassification)
# Now remove the Age column
hr_reduced[c("Age")] <- list(NULL)

# Investigate: DailyRate, MonthlyRate, MonthlyIncome, MonthlyRate

#hr_reduced[c("DailyRate")] <- list(NULL)
#hr_reduced[c("HourlyRate")] <- list(NULL)
#hr_reduced[c("MonthlyIncome")] <- list(NULL)
#hr_reduced[c("MonthlyRate")] <- list(NULL)

# http://statistics.berkeley.edu/sites/default/files/tech-reports/666.pdf
# https://www.quora.com/How-should-I-handle-unbalanced-data-while-using-randomForest-in-R

cat( length(colnames(hr_reduced)) )

k <- min(table(hr_reduced[,1]))/2
# mtry default value is best with:  ntree=200, replace=FALSE, importance=TRUE, sampsize=c(k,k)
# replace default value with: ntree=200, importantance = TRUE, sampsize=c(k,k)
#       No gets better, Yes gets worse
clf <- randomForest(hr_reduced[,-1], hr_reduced[,1], ntree=500, replace=FALSE, importantance = TRUE, mtry=15, sampsize=c(k,k))

varyNTree <- function(df, labelCol) {
  L <- df[,labelCol]
  M <- df[,-labelCol]
  K <- min(table(hr_reduced[,1]))/2
  
  newdf <- data.frame(matrix(ncol=7, nrow=0))
  colnames(newdf) <- c("mtry", "ntree", "oob", "no", "yes", "errno", "erryes")
  
  mtryvals <- seq(2,15)
  ntreevals <- seq(20,600, by=20)
  
  for (z1 in mtryvals) {
    for (z2 in ntreevals) {
      clf <- randomForest(M, L, ntree=z2, replace=FALSE, importantance = TRUE, mtry=z1, sampsize=c(K,K))
      mean_oob <- mean(clf$err.rate[,1])
      mean_no <- mean(clf$err.rate[,2])
      mean_yes <- mean(clf$err.rate[,3])
      newdf[nrow(newdf)+1,] <- c(z1, z2, mean_oob, mean_no, mean_yes, clf$confusion[1,3], clf$confusion[2,3])
    }
  }
  
  return (newdf)
}

rfanaly <- varyNTree(hr_reduced, 1)
