bootRosMI <- function(rosObj, R.model=10, R.boot=1999, M=100, p=c(0.025,0.975)){
  
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

