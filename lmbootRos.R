# lmbootRos.R is a function that starts with an ROS object, performs a bootstrap
# of the underlying lm for the ROS, and calculates the univariate statistic speficied
# default will be mean.

lmbootRos <- function(rosObj, R.model=10, R.boot=1999, p=c(0.025,0.975)){

require(NADA)
require(simpleboot)
stopifnot(c("ros", "lm") %in% class(rosObj))
stopifnot(is.numeric(R.model))
stopifnot(is.numeric(R.boot))
stopifnot(is.numeric(p))
stopifnot(length(p)==2)





# identify censored values
cenValues <- rosObj$modeled[rosObj$censored]
ppValues <- rosObj$pp[rosObj$censored]
sampleSize <- length(rosObj$modeled)

lboot <- lm.boot(rosObj, R=R.model, new.xpts=data.frame(pp.nq=qnorm(ppValues)))

# generate imputed values
 impDF <- ldply(lboot$boot.list, function(lx) data.frame(lx$fitted), .progress="text")

# downweight the observations

impDF$weight <- length(ppValues)/length(impDF$lx.fitted)
impDF$lx.fitted <- exp(impDF$lx.fitted)

# extract the observed data
obs <- rosObj$modeled[!rosObj$censored]

#upweight the observed data
weight <- rep(1, length(obs))

sampleData <- c(obs, impDF$lx.fitted)
sampleWeight <- c(weight, impDF$weight) # weight should sum to original sample size

dataMatrix <- matrix(sample(sampleData, prob=sampleWeight/sampleSize, size=sampleSize * R.boot, replace=TRUE), ncol=sampleSize)

result <- apply(dataMatrix, MARGIN=1, FUN=mean)

ci <- quantile(result, p)
return(ci)
}


<<<<<<< HEAD
lmbootRos_MI <- function(rosObj, R.model=10, R.boot=1999, M=100, p=c(0.025,0.975)){
=======
lmbootRos4 <- function(rosObj, R.model=10, R.boot=1999, M=100, p=c(0.025,0.975)){
>>>>>>> ab889ed163992a6038dfef7cd259707699d20c13
  
  require(NADA)
  require(simpleboot)
  stopifnot(c("ros", "lm") %in% class(rosObj))
  stopifnot(is.numeric(R.model))
  stopifnot(is.numeric(R.boot))
  stopifnot(is.numeric(p))
  stopifnot(length(p)==2)
  
  
  
  
  
  # identify censored values
  cenValues <- rosObj$modeled[rosObj$censored]
  ppValues <- rosObj$pp[rosObj$censored]
  sampleSize <- length(rosObj$modeled)
  
  lboot <- lm.boot(rosObj, R=R.model, new.xpts=data.frame(pp.nq=qnorm(ppValues)))
  
  # generate imputed values
  impDF <- ldply(lboot$boot.list, function(lx) data.frame(lx$fitted))
  
  # downweight the observations
  
  impDF$weight <- length(ppValues)/length(impDF$lx.fitted)
  impDF$lx.fitted <- exp(impDF$lx.fitted)
  
  # extract the observed data
  obs <- rosObj$modeled[!rosObj$censored]
  
  #upweight the observed data
  weight <- rep(1, length(obs))
  
  sampleData <- c(obs, impDF$lx.fitted)
  sampleWeight <- c(weight, impDF$weight) # weight should sum to original sample size
  
  myboot <- one.boot(sampleData, mean, R= R.boot, w=sampleWeight, student=TRUE, M=M)
  bootci <- boot.ci(myboot, t0=mean(rosObj), 
                    t=myboot$t[,1], var.t0=sd(rosObj)^2, var.t=myboot$t[,2])
  bootCI <- data.frame(matrix(c(bootci$normal[2:3], bootci$basic[4:5],
                                bootci$perc[4:5], bootci$bca[4:5],
                                bootci$student[4:5]), nrow=1))
  names(bootCI) <- c("ros.normal.LCL", "ros.normal.UCL", "ros.basic.LCL", "ros.basic.UCL",
                     "ros.percentile.LCL", "ros.percentile.UCL", "ros.BCa.LCL", "ros.BCa.UCL",
                     "ros.studentized.LCL", "ros.studentized.UCL")
  
  return(bootCI)

}



# lmbootRos.R is a function that starts with an ROS object, performs a bootstrap
# of the underlying lm for the ROS, and calculates the univariate statistic speficied
# default will be mean.

lmbootRos2 <- function(rosObj, R.model=100, R.boot=1999, p=c(0.025,0.975)){

require(NADA)
require(simpleboot)
stopifnot(c("ros", "lm") %in% class(rosObj))
stopifnot(is.numeric(R.model))
stopifnot(is.numeric(R.boot))
stopifnot(is.numeric(p))
stopifnot(length(p)==2)





# identify censored values
cenValues <- rosObj$modeled[rosObj$censored]
ppValues <- rosObj$pp[rosObj$censored]
sampleSize <- length(rosObj$modeled)

lboot <- lm.boot(rosObj, R=R.model, new.xpts=data.frame(pp.nq=qnorm(rosObj$pp)))

# generate imputed values
impDF <- ldply(lboot$boot.list, function(lx) data.frame(bootStat=mean(exp(lx$fitted))))


ci <- quantile(impDF$bootStat, p)
return(ci)
}

lmbootRos3 <- function(rosObj, R.model=100, R.boot=1999, p=c(0.025,0.975)){

require(NADA)
require(boot)
stopifnot(c("ros", "lm") %in% class(rosObj))
stopifnot(is.numeric(R.model))
stopifnot(is.numeric(R.boot))
stopifnot(is.numeric(p))
stopifnot(length(p)==2)

dfx <- data.frame(rosObj)

rosBoot <- function(dfx,i) {

dfx <- dfx[i,]

safeROS <- failwith(NA, ros)
mean(safeROS(dfx$obs, dfx$censored))

}

bootOut <- boot(dfx, R=R.boot, rosBoot)
bootci <- boot.ci(bootOut, type=c("norm", "basic", "perc", "bca"))
bootCI <- data.frame(matrix(c(bootci$normal[2:3], bootci$basic[4:5],
                                bootci$perc[4:5], bootci$bca[4:5]), nrow=1))
# browser()
  names(bootCI) <- c("ros.normal.LCL", "ros.normal.UCL", "ros.basic.LCL", "ros.basic.UCL",
                     "ros.percentile.LCL", "ros.percentile.UCL", "ros.BCa.LCL", "ros.BCa.UCL"
                     )

  return(bootCI)

}



# identify censored values
cenValues <- rosObj$modeled[rosObj$censored]
ppValues <- rosObj$pp[rosObj$censored]
sampleSize <- length(rosObj$modeled)

lboot <- lm.boot(rosObj, R=R.model, new.xpts=data.frame(pp.nq=qnorm(rosObj$pp)))

# generate imputed values
impDF <- ldply(lboot$boot.list, function(lx) data.frame(bootStat=mean(exp(lx$fitted))))


ci <- quantile(impDF$bootStat, p)
return(ci)
}


lmbootRos2 <- function(rosObj, R.model=100, R.boot=1999, p=c(0.025,0.975)){

require(NADA)
require(simpleboot)
stopifnot(c("ros", "lm") %in% class(rosObj))
stopifnot(is.numeric(R.model))
stopifnot(is.numeric(R.boot))
stopifnot(is.numeric(p))
stopifnot(length(p)==2)




source("utilityFunctions.R")

set.seed(1)


mytestrun <- data.frame(
  distribution = rep("Log-Normal",1), censQ1=rep(0.5,1), censQ2 = rep(0.0,1), censQ3 = rep(0.0,1),
  obs1=25, obs2=0, obs3=0, simNum = rep(1000,1), meanlog=rep(0,1), sdlog=rep(1,1))

dataSize <- sum(apply(mytestrun[grep("obs", names(mytestrun))], MARGIN=1, FUN=sum) * mytestrun$simNum)
tesit <- matrix(rep(NA, dataSize * 21), ncol=21)
tesit <- mdply(mytestrun, createInput, .progress='text')

rosSet <- dlply(tesit, .(sampleNum, obs1, obs2,obs3), function(dfx) ros(dfx$obs, dfx$cen), .progress="text")


testSet <- rosSet[[1]]

confInts <-  ldply(rosSet, lmbootRos)
confInts4 <- ldply(rosSet, lmbootRos4, .progress="text")

confInts2 <-  ldply(rosSet, lmbootRos2, .progress="text")
summary(confInts2$"97.5%" >= exp(0.5))


confInts2 <-  ldply(rosSet, lmbootRos3, .progress="text")
summary(confInts2$ros.percentile.UCL >= exp(0.5))

