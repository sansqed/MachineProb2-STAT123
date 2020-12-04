
library("distrEx")


YTruncation <- function(x, a, b){
  for (i in 1:length(x)){
    if (x[i] < a)        x[i] <- a
    else if (x[i] > b)   x[i] <- b
  }
  return (x)
}

ZTruncation <- function (x,b){
  for (i in 1:length(x)){
    if (abs(x[i]) > b)  x[i] <- 0
  }
  return (x)
}

findFreq <- function (x,a,b){
  ctr <- 0
  for (i in 1:length(x)){
    if (x[i]  >= a && x[i] <= b)
      ctr <- ctr+1
  }
  return (ctr)
}


#######################################################

# create discrete random variable X
# x <- runif(1000,min = -20, max=20)
set.seed(141421356)
x <- rbinom(1000,size = 100, prob=0.23)
hist(x, main="Normal")
mean(x)

y <- YTruncation(x, 15,25)
hist(y, main="Truncated using Y with a=15, b=25")

y <- YTruncation(x, 22,25)
hist(y, main="Truncated using Y with a=22, b=25")

y <- YTruncation(x, 20,30)
hist(y, main="Truncated using Y with a=20, b=30")

y <- YTruncation(x, 15,35)
hist(y, main="Truncated using Y with a=15, b=35")

y <- YTruncation(x, 10,40)
hist(y, main="Truncated using Y with a=10, b=40")

y <- YTruncation(x, 0,50)
hist(y, main="Truncated using Y with a=0, b=50")



z <- ZTruncation(x,25)
hist(z, main="Truncated using Z with b=25")

z <- ZTruncation(x,30)
hist(z, main="Truncated using Z with b=30")

z <- ZTruncation(x,35)
hist(z, main="Truncated using Z with b=35")

z <- ZTruncation(x,40)
hist(z, main="Truncated using Z with b=40")

z <- ZTruncation(x,50)
hist(z, main="Truncated using Z with b=50")

findFreq(x,4,6)
findFreq(z,4,6)

?hist()
