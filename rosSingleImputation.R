# Attempt to pull functions outside of shiny as they are taking
# to long to run as it stands.

library(shiny)
library(ggplot2)
library(NADA)
library(reshape2)
library(plyr)
source("utilityFunctions.R")
source("about.R")

set.seed(1)


mytestrun <- data.frame(
  distribution = rep("Log-Normal",9), censQ1=rep(0.5,9), censQ2 = rep(0.0,9), censQ3 = rep(0.0,9),
  obs1=c(20,30,40,50,60,70,80,90,100), obs2=rep(0,9), obs3=rep(0,9), simNum = rep(1000,9), meanlog=rep(0,9), sdlog=rep(1,9))

dataSize <- sum(apply(mytestrun[grep("obs", names(mytestrun))], MARGIN=1, FUN=sum) * mytestrun$simNum)
tesit <- matrix(rep(NA, dataSize * 21), ncol=21)
tesit <- mdply(mytestrun, createInput, .progress='text')

rosSet <- dlply(tesit, .(sampleNum, obs1, obs2,obs3), function(dfx) ros(dfx$obs, dfx$cen), .progress="text")

#boot confidence intervals 
bootciout <- ldply(rosSet, rosSimpleBoot, .progress="text")

bootciout$N <- apply(bootciout[c("obs1","obs2", "obs3")], MARGIN=1, FUN=sum)

UCLs <- grep("[.]UCL", names(bootciout), value=TRUE)
UCLs <- c("N", UCLs)

bootUCL <- melt(bootciout[UCLs], id.var="N")
bootUCL$varaible <- factor(bootUCL$variable)
                           
                levels(bootUCL$variable)  <- c("Normal UCL", "Basic UCL", 
                                                     "Percentile UCL", "BCa UCL", "Studentized UCL")

# plot of single imputation coverage with bootstraps
x11()
bootplot1 <- ggplot(bootUCL, aes(x=factor(N), y=value, colour=factor(variable))) +geom_boxplot() + scale_y_log10() + geom_abline(intercept=tesit$pop.mean[1])+ xlab("Sample Size") + ylab("UCL")
(bootplot1 <- bootplot1 + geom_abline(intercept=log10(tesit$pop.mean[1]), slope=0)+ggtitle("Boxplots 97.5% UCL by Sample Size \n LN(0,1) 50% Censored")+ scale_colour_brewer(name="Bootstrap\nMethod", palette="Set1"))
#coverages

bootUCL$pop.mean <- tesit$pop.mean[1]
coverageUCLboot <- ddply(bootUCL, .(variable,N), summarise, coverage=sum(value >=pop.mean)/length(pop.mean), coverage5percent= sum(value >=(0.95 * pop.mean))/length(pop.mean), coverage10percent= sum(value >=(0.90 * pop.mean))/length(pop.mean), .progress="text")

(bootplot2 <- ggplot(coverageUCLboot, aes(x=factor(N), y=coverage, colour=variable, group=variable)) + geom_point() + geom_line()+ xlab("Sample Size") + ylab("UCL Coverage of Mean")+ggtitle("UCL Coverage of Mean"))
(bootplot3 <- ggplot(coverageUCLboot, aes(x=factor(N), y=coverage5percent, colour=variable, group=variable)) + geom_point() + geom_line()+ xlab("Sample Size") + ylab("UCL Coverage within 5% of Mean")+ ggtitle("UCL Coverage of within 5% of Mean")) 
(bootplot4 <- ggplot(coverageUCLboot, aes(x=factor(N), y=coverage10percent, colour=variable, group=variable)) + geom_point() + geom_line()+ xlab("Sample Size") + ylab("UCL Coverage within 10% of Mean")+ ggtitle("UCL Coverage of within 10% of Mean"))

