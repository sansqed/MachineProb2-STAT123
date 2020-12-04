### ALWYN RETULLA DY
### STAT 123 D1 MACHINE PROBLEM #2 | Problem #5
### 2020 NOVEMBER 17

setwd("D:/2. Yupee/1.1/STAT 123/Assignment/Machine Problem 2/5")



nicePackage <- function(k){
  
  set.seed(6626068)

  recievedObj <- c() # records new object
  daysPassed <- c() # vector for storing elapsed days until receiving new object
  elapsed <- 0 # elapsed days counter
  
  while (length(recievedObj)<k) { # loops until a complete set is reached
    obj <- ceiling(runif(1, min=0, max=k))
    
    flag <- 0 # determines if obj is already received
    for (j in recievedObj){
      if (j == obj){
        flag <- 1
        break
      } 
    }
    if (flag == 0){ # if received object is new,  
      recievedObj <- c(recievedObj, obj) # add to receivedObj,
      if (elapsed!=0) daysPassed <- c(daysPassed, elapsed) # records elapsed days
      elapsed <- 0 # reset counter
    }
    
    elapsed <- elapsed + 1
  }

  # print(allObjects)
  # print(daysPassed)
  # print(length(allObjects))
  
  # plot(daysPassed, type="l", xlab = "Object", ylab = "Days Passed")
  
  # returns the mean number of elapsed days and total days to have a full set
  return (c(mean(daysPassed), sum(daysPassed+1)))
  
}

manyNicePackages <- function(k, n) {
  set.seed(981)
  
  totalDays <- c()
  
  for (i in 1:n)
    totalDays <- c(totalDays, nicePackage(k)[2])
  
  #print(totalDays)
  return (mean(totalDays))
}


nicePackage(101)
nicePackage(13)
manyNicePackages(101, 50)
manyNicePackages(13, 50)


######################################################################

manyManyNicePackage <- function(k,n){
  recievedObj <- c()
  daysPassed <- c()
  elapsed <- 0
  completeSet <- 0
  
  while (completeSet<=n) {
    obj <- ceiling(runif(1, min=0, max=k))
    
    flag < 0
    for (j in recievedObj){
      if (j == obj){
        flag <- 1
        break
      } 
    }
    if (flag == 0){
      recievedObj <- c(recievedObj, obj)
      if (elapsed!=0) daysPassed <- c(daysPassed, elapsed)
      elapsed <- 0
    }
    
    elapsed <- elapsed + 1
    if (length(recievedObj)==k) {
      completeSet <- completeSet + 1
      recievedObj <- c()
    }
    
  }
  
  # print(allObjects)
  # print(daysPassed)
  # print(length(allObjects))
  return (c(mean(daysPassed), sum(daysPassed+1)))
  
}

manyManyNicePackage(100,10)


