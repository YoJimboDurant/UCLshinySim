getDetLim <- function(input, groupno="censQ1"){
  
  if(input$distribution =="Log-Normal"){
    dist="qlnorm"
    qargs = list(meanlog=input$meanlog, sdlog=input$sdlog)
  }else{
    if(input$distribution=="Gamma"){
      dist="qgamma"
      qargs = list(shape=input$shape, scale=input$scale)
    }else{
      if(input$distribution=="Normal"){
        dist="qnorm"
        qargs = list(input$mu, sd =input$sd)
        
      }
    }
  }
  input1 <- c(input$censQ1, input$censQ2, input$censQ3)
  qargs$p <- input1[c("censQ1", "censQ2", "censQ3") %in% groupno]
  x <- do.call(dist, qargs)
  return(x)
}


createData <- function(input) {
  if(input$distribution =="Log-Normal"){
    dist="lnorm"
    qargs = list(meanlog=input$meanlog, sdlog=input$sdlog)
    pop.mean <- exp(input$meanlog + 0.5 *input$sdlog ^2)
    pop.var <- (exp(input$sdlog^2)-1) * exp(2 *input$meanlog +input$sdlog ^2)
    
  }else{
    if(input$distribution=="Gamma"){
      dist="gamma"
      qargs = list(shape=input$shape, scale=input$scale)
      pop.mean <- input$shape * input$scale
      pop.var <- input$shape * input$scale^2
      
    }else{
      if(input$distribution=="Normal"){
        dist="norm"
        qargs = list(input$mu, sd =input$sd)
        pop.mean <- input$mu
        pop.var <- input$sd^2
        
        
      }
    }
  }
  
  totalObs <- input$obs1 + input$obs2 +input$obs3
  rdist <- paste("r", dist, sep="")
  qdist <- paste("q", dist, sep="")
  
  qargs$p <- c(input$censQ1, input$censQ2, input$censQ3)
  cenValues <- do.call(qdist, qargs)
  cenValues <- rep(cenValues, times = c(input$obs1, input$obs2, input$obs3))
  
  cenValue <- sample(cenValues, totalObs, replace=FALSE)
  cenValue <- rep(cenValue, times = input$simNum)
  
  # create sample from distribution
  qargs$p <- NULL
  qargs$n <- totalObs * input$simNum
  dist <- do.call(rdist, qargs)
  cen = cenValue > dist
  
  
  dfx <- data.frame(value=dist, cen=cenValue>dist, cenValue=cenValue)
  dfx$obs <- dfx$value
  dfx$obs[dfx$cen] <- cenValue[dfx$cen]
  dfx$obs_zero <- dfx$value
  dfx$obs_zero[dfx$cen] <- 0
  dfx$obs_half <-dfx$value
  dfx$obs_half[dfx$cen] <- 0.5 * cenValue[dfx$cen]
  dfx$obs_full <- dfx$value
  dfx$obs_full[dfx$cen] <- cenValue[dfx$cen]
  dfx$obs_sqrt2 <- dfx$value
  dfx$obs_sqrt2[dfx$cen] <- cenValue[dfx$cen]/sqrt(2)
  dfx$run <- input$myValue
  dfx$sampleNum <- rep(1:input$simNum, each=totalObs)
  return(list(dfx = dfx, pop.mean = pop.mean, pop.var = pop.var))
}

createInput <- function(distribution, censQ1, censQ2, censQ3, obs1, 
                        obs2, obs3, simNum=100,...){
  
  input <-list(distribution=distribution, censQ1=censQ1, censQ2=censQ2,
               censQ3=censQ3, obs1=obs1, obs2=obs2, obs3=obs3, simNum=simNum,...)
  
  dataObj <- createData(input)
  dfx <- dataObj$dfx
  dfx$pop.mean <- dataObj$pop.mean
  dfx$pop.var <- dataObj$pop.var
  
  return(dfx)
  
}
  
#create test dataframe to generate data



mean.boot <- function(x,ind)
{
  return(c(mean(x[ind]),var(x[ind])/length(ind)))
}

rosSimpleBoot <- function(dfx, conf = 0.95, R=1999, M=50){
  require(NADA)
  require(boot)

  myros <- cenros(dfx$obs, dfx$cen)
  mydfx <- data.frame(myros)
  myboot <- boot(mydfx$modeled, mean.boot, R=R)
  bootci <- boot.ci(myboot)
  
  bootCI <- data.frame(matrix(c(bootci$normal[2:3], bootci$basic[4:5], 
                                bootci$perc[4:5], bootci$bca[4:5], 
                                bootci$student[4:5]), nrow=1))
  names(bootCI) <- c("ros.normal.LCL", "ros.normal.UCL", "ros.basic.LCL", "ros.basic.UCL", 
                     "ros.percentile.LCL", "ros.percentile.UCL", "ros.BCa.LCL", "ros.BCa.UCL",
                     "ros.studentized.LCL", "ros.studentized.UCL")
  
  return(bootCI)
  
}

library(ggplot2)
library(NADA)
library(reshape2)
library(plyr)


mytestrun <- data.frame(
  distribution = rep("Log-Normal",9), censQ1=rep(0.1,9), censQ2 = rep(0.0,9), censQ3 = rep(0.0,9), 
  obs1=c(20,30,40,50,60,70,80,90,100), obs2=rep(0,9), obs3=rep(0,9), simNum = rep(1000,9), meanlog=rep(0,9), sdlog=rep(1,9))



tesit <- mdply(mytestrun, createInput)

bootciout <- ddply(tesit, .(sampleNum, obs1), rosSimpleBoot, .progress="text")

mydf <- melt(bootciout, id.var=c("sampleNum", "obs1"))
exrate <- ddply(mydf[grep("UCL", mydf$variable),], .(variable, obs1), summarise, exceedrate = sum(value >= exp(0.5))/length(value))

ggplot(exrate, aes(x=obs1, y=exceedrate, colour=variable))+geom_point()+geom_smooth()

