# Functions for common Use


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



# Given the vector of scalar values, and a vector of 
# non-inclusive range values, replace the actual value with the index 
# of the range value it is less than
#
arbitraryClassification <- function(values, ranges) {
    rng <- sort(ranges)
    
    result <- c()
    
    for (v in values) {
        rcnt <- 1
        for (r in rng) {
            if (v < r) {
                result <- c(result, rcnt)
                break
            }
            rcnt <- rcnt + 1
        }
    }
    
    return (result)
}


getColumnNumber <- function(df, colname){
    colnames <- names(df)
    found <- -1
    
    ndx <- 1
    for (name in colnames) {
        if (colname == name) {
            found <- ndx
            break
        }
        ndx <- ndx + 1
    }
    
    return (found)
}

printImportance <- function(df, max = 999999999){
    thestring <- cat(sprintf("%35s  %s\n", names(df)[1], names(df)[2]))
    
    count <- 0
    for (row in 1:nrow(df)) {
      
        vname <- df[row, 1]
        val <- df[row, 2]
        
        str1 <- cat( sprintf("%35s  %s\n", vname, val) )
        thestring <- paste0(thestring, str1)
        count <- count + 1
        if (count >= max) {
          break
        }
    }
    
    return (thestring)
}

sortImportance <- function(clf) {
    thenames <- attributes(clf$importance)$dimnames[[1]]
    newdf <- data.frame(matrix(ncol=2, nrow=0))
    colnames(newdf) <- c("VariableName", "MeanDecreaseGini")
    
    imps <- sort(clf$importance, decreasing = TRUE)
    
    for (i in imps) {
        idx <- 1
        thename <- ''
        
        for (i2 in clf$importance) {
            if (i2 == i) {
                thename <- thenames[idx]
                break
            } else {
                idx <- idx + 1
            }
        }
        
        newdf[nrow(newdf)+1,] <- c(thename, i)
    }
    
    return (newdf)
}

# vary the following:
#   mtry - the number of variables to use at each tree
#   ntree - the maximum number of trees
# Collect:
#   mtry value
#   ntree value
#   oob mean error rate
#   no mean error rate
#   yes mean error rate
#   no error rate
#   yes error rate
varyNTree <- function(df, labelCol, useDefaults = FALSE) {
  L <- df[,labelCol]
  M <- df[,-labelCol]
  K <- min(table(hr_reduced[,1]))/2
  
  newdf <- data.frame(matrix(ncol=7, nrow=0))
  colnames(newdf) <- c("mtry", "ntree", "oob", "no", "yes", "errno", "erryes")
  
  mtryvals <- seq(2,15)
  ntreevals <- seq(20,600, by=20)
  
  for (z1 in mtryvals) {
    for (z2 in ntreevals) {
      if (useDefaults) {
        clf <- randomForest(M, L, ntree=z2, mtry=z1, sampsize=c(K,K))
      } else {
        clf <- randomForest(M, L, ntree=z2, replace=FALSE, importantance = TRUE, mtry=z1, sampsize=c(K,K))
      }
      
      mean_oob <- mean(clf$err.rate[,1])
      mean_no <- mean(clf$err.rate[,2])
      mean_yes <- mean(clf$err.rate[,3])
      newdf[nrow(newdf)+1,] <- c(z1, z2, mean_oob, mean_no, mean_yes, clf$confusion[1,3], clf$confusion[2,3])
    }
  }
  
  return (newdf)
}
