
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

nmax <- length(lessthan2)
for (idx in 1:nmax) {
  colidx <- lessthan2[idx]
  colname <- names(hr_employee_attrition)[colidx]
  cat( sprintf("Column index %d - %s had less than 2 unique values\n", colidx, colname) )
}