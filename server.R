# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  # First we create a reactive function that is reactive to either distribution changes or pressing the action button
  datasetInput <- reactive({
      isolate({thedata <- createData(input)
               dfx <- thedata$dfx
               pop.mean <- thedata$pop.mean
               pop.var <- thedata$pop.var





    # calculate means and SD by method
    sample.results <-ddply(dfx, .(sampleNum), summarise, sample.mean=mean(value), sample.sd = sd(value))
    ros.results <- dlply(dfx, .(sampleNum), function(dfx) ros(dfx$obs,dfx$cen))
    ros.mean <- ldply(ros.results, mean)
    ros.sd <-ldply(ros.results, sd)

    km.results <- dlply(dfx, .(sampleNum), function(dfx) cenfit(dfx$obs, dfx$cen))
    km.mean <- ldply(km.results, mean)
    km.sd <-ldply(km.results, sd)

    mle.results <- dlply(dfx, .(sampleNum), function(dfx) cenmle(dfx$obs, dfx$cen))
    mle.mean <- ldply(mle.results, mean)
    mle.sd <-ldply(mle.results, sd)

    # calculate substituted means and SD
    zero.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_zero), sample.sd = sd(obs_zero))
    half.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_half), sample.sd = sd(obs_half))
    full.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_full), sample.sd = sd(obs_full))
    sqrt2.results <- ddply(dfx, .(sampleNum), summarise, sample.mean=mean(obs_sqrt2), sample.sd = sd(obs_sqrt2))

    #because I want to use censummary later, I return a list packed with the data frame of results and the initial data
      if(input$distribution == "Log-Normal") dist <- paste(input$distribution, "(",input$meanlog,",",input$sdlog,")")
      if(input$distribution == "Gamma") dist <- paste(input$distribution, "(",input$shape,",",input$scale,")")
      if(input$distribution == "Normal") dist <- paste(input$distribution, "(",input$mean,",",input$sd,")")

    results = list(results = data.frame(
      sample.mean =sample.results$sample.mean,
      sample.sd = sample.results$sample.sd,
      ros.mean=ros.mean$V1,
      ros.sd=ros.sd$V1,
      km.mean=km.mean$mean,
      km.sd=km.sd$V1,
      mle.mean=mle.mean$mean,
      mle.sd=mle.sd$V1,
      sub0.mean = zero.results$sample.mean,
      sub0.sd = zero.results$sample.sd,
      subHalf.mean = half.results$sample.mean,
      subHalf.sd = half.results$sample.sd,
      subFull.mean = full.results$sample.mean,
      subFull.sd = full.results$sample.sd,
      #  ,subSqrt2.mean = sqrt2.results$sample.mean,
      # subSqrt2.sd = sqrt2.results$sample.sd
      pop.mean = rep(pop.mean, input$simNum),
      pop.var = rep(pop.var, input$simNum)
    ),
    data = dfx,
    distribution = dist
    )

#      assign("downResults", results, envir=.GlobalEnv)
           })

  myvalue <- input$myValue1
return(results)

  })

  # this serves the results to the UI using output
  output$Meanview <- renderTable({
    results <- datasetInput()
    results <- results$results
    summary(results[grep("mean", names(results))])
  })

  output$SDview <- renderTable({
    results <- datasetInput()
    results <- results$results
    summary(results[grep("sd", names(results))])
  })

  output$Meangraph <- renderPlot({
    results <- datasetInput()
    results <- results$results
    results <- results[grep("mean", names(results))]
# browser()
    boxplot(results[!grepl("pop.", names(results))],
            col="light blue",
            ylab="Mean",      # y axis label
            xlab="Censoring Method",    # x axis label
            main="Simulated Means",    # graphic title
            las=1,                # controls the orientation of the axis labels (1=horizontal)
            par(list(cex=0.8)),
            axes=FALSE)
    abline(h=results$pop.mean[1], lty=2)
    axis(1, at=1:7, labels=c(
                    "Uncensored Data",
                    "ROS",
                    "KM",
                    "MLE",
                    "ND = 0",
                    "ND = 1/2 x DL",
                    "ND = DL"))
    axis(2)
    text(0.4, results$pop.mean[1], "Pop.\n Mean", pos=3)

  })

  output$SDgraph <- renderPlot({
    results <- datasetInput()
    results <- results$results
    boxplot(results[grep("sd", names(results))],
            ylab="Std. Dev.",
            xlab="Censoring Method",
            main="Simulated Std. Dev.",
            las=1,
            par(list(cex=0.8)),
            col="light blue",
            axes=FALSE)
    abline(h=sqrt(results$pop.var[1]), lty=2)

    axis(1, at=1:7, labels=c(
                    "Uncensored Data",
                    "ROS",
                    "KM",
                    "MLE",
                    "ND = 0",
                    "ND = 1/2 x DL",
                    "ND = DL"))
    axis(2)
    text(0.4, sqrt(results$pop.var[1]), "Pop.\n SD", pos=3)

  })

  output$distGraph <-renderPlot({
    if(input$distribution=='Log-Normal'){
      .x <- seq(qlnorm(0.01, input$meanlog, input$sdlog), qlnorm(0.99, input$meanlog, input$sdlog), length.out=100)
      plot(.x, dlnorm(.x, meanlog=input$meanlog, sdlog=input$sdlog), xlab="x", ylab="Density",
           type="l")
      abline(h=0, col="gray")
    }

    if(input$distribution=='Gamma'){
      .x <- seq(qgamma(0.01, shape=input$shape, scale=input$scale),qgamma(0.99, shape=input$shape, scale=input$scale) , length.out=100)
      plot(.x, dgamma(.x, shape=input$shape, scale=input$scale), xlab="x", ylab="Density",
           type="l")
      abline(h=0, col="gray")
    }

    if(input$distribution=='Normal'){
      .x <- seq(qnorm(0.01, mean=input$mu, sd=input$sd),qnorm(0.99, mean=input$mu, sd=input$sd), length.out=100)
      plot(.x, dnorm(.x, mean=input$mu, sd=input$sd), xlab="x", ylab="Density",
           type="l")
      abline(h=0, col="gray")
    }



  })


  output$RMSEplot.mean <- renderPlot({
    results <- datasetInput()
    results <- results$results
    sampleRMSE <- sqrt(sum(((results$sample.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    rosRMSE <- sqrt(sum(((results$ros.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    kmRMSE <- sqrt(sum(((results$km.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    mleRMSE <- sqrt(sum(((results$mle.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    sub0RMSE <- sqrt(sum(((results$sub0.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    subHalfRMSE <- sqrt(sum(((results$subHalf.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)
    subFullRMSE <- sqrt(sum(((results$subFull.mean - results$pop.mean)/results$pop.mean)^2)/input$simNum)

    barplot(c(sampleRMSE, rosRMSE, kmRMSE, mleRMSE, sub0RMSE, subHalfRMSE, subFullRMSE),
            names.arg=c("Sample", "ROS", "KM", "MLE", "ND = 0", "ND = 1/2 x DL", "ND = DL"),
            main="Mean RMSE", ylab="Mean RMSE", col="light blue")
    abline(h=0, col="light grey", lty=2)
  })

  output$RMSEplot.sd <- renderPlot({
    results <- datasetInput()
    results <- results$results
    results$pop.sd <-sqrt(results$pop.var)
    samplesdRMSE <- sqrt(sum(((results$sample.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    rossdRMSE <- sqrt(sum(((results$ros.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    kmsdRMSE <- sqrt(sum(((results$km.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    mlesdRMSE <- sqrt(sum(((results$mle.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    sub0sdRMSE <- sqrt(sum(((results$sub0.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    subsdHalfRMSE <- sqrt(sum(((results$subHalf.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)
    subsdFullRMSE <- sqrt(sum(((results$subFull.sd - results$pop.sd)/results$pop.sd)^2)/input$simNum)

    barplot(c(samplesdRMSE, rossdRMSE, kmsdRMSE, mlesdRMSE, sub0sdRMSE, subsdHalfRMSE, subsdFullRMSE),
            names.arg=c("Sample", "ROS", "KM", "MLE", "ND = 0", "ND = 1/2 x DL", "ND = DL"),
            main="Std. Dev. RMSE", ylab="Std. Dev. RMSE", col="light blue")

    abline(h=0, col="light grey", lty=2)
  })

  output$Bplot.mean <- renderPlot({
    results <- datasetInput()
    results <- results$results
    sampleBias <- sum(((results$sample.mean - results$pop.mean)/results$pop.mean))/input$simNum
    rosBias <- sum(((results$ros.mean - results$pop.mean)/results$pop.mean))/input$simNum
    kmBias <- sum(((results$km.mean - results$pop.mean)/results$pop.mean))/input$simNum
    mleBias <- sum(((results$mle.mean - results$pop.mean)/results$pop.mean))/input$simNum
    sub0Bias <- sum(((results$sub0.mean - results$pop.mean)/results$pop.mean))/input$simNum
    subHalfBias <- sum(((results$subHalf.mean - results$pop.mean)/results$pop.mean))/input$simNum
    subFullBias <- sum(((results$subFull.mean - results$pop.mean)/results$pop.mean))/input$simNum

    barplot(c(sampleBias *100, rosBias * 100, kmBias * 100, mleBias * 100, sub0Bias * 100, subHalfBias * 100, subFullBias * 100),
            names.arg=c("Sample", "ROS", "KM", "MLE", "ND = 0", "ND = 1/2 x DL", "ND = DL"),
            main="Mean Percent Bias", ylab="Mean Bias (%)", col="light blue")
    abline(h=0, col="light grey", lty=2)
  })

  output$Bplot.sd <- renderPlot({
    results <- datasetInput()
    results <- results$results
    results$pop.sd <-sqrt(results$pop.var)
    samplesdBias <- sum(((results$sample.sd - results$pop.sd)/results$pop.sd))/input$simNum
    rossddBias <- sum(((results$ros.sd - results$pop.sd)/results$pop.sd))/input$simNum
    kmsdBias <- sum(((results$km.sd - results$pop.sd)/results$pop.sd))/input$simNum
    mlesdBias <- sum(((results$mle.sd - results$pop.sd)/results$pop.sd))/input$simNum
    sub0sdBias <- sum(((results$sub0.sd - results$pop.sd)/results$pop.sd))/input$simNum
    subsdHalfBias <- sum(((results$subHalf.sd - results$pop.sd)/results$pop.sd))/input$simNum
    subsdFullBias <- sum(((results$subFull.sd - results$pop.sd)/results$pop.sd))/input$simNum

    barplot(c(samplesdBias * 100, rossddBias * 100, kmsdBias * 100, mlesdBias * 100, sub0sdBias * 100, subsdHalfBias * 100, subsdFullBias * 100),
            names.arg=c("Sample", "ROS", "KM", "MLE", "ND = 0", "ND = 1/2 x DL", "ND = DL"),
            main="Std. Dev. Percent Bias", ylab="Std. Dev. Bias (%)", col="light blue")
    abline(h=0, col="light grey", lty=2)
  })

  output$censum.all <- renderTable ({
    results <- datasetInput()
    dfx <- results$data
    allsum <-t(data.frame(censummary(dfx$obs, dfx$cen)$all))
    rownames(allsum) <- "Summary Statistics"
    return(allsum)
  })


  output$censum.everything <- renderTable ({
    results <- datasetInput()
    dfx <- results$data
    allsum <-censummary(dfx$obs, dfx$cen, dfx$sampleNum)
    allsum <- ldply(allsum, function(x) t(data.frame(x$all)))
    names(allsum) <- c("Sample Number", "n", "n.cen", "pct.cen", "min", "max")
    return(allsum)
  })

  output$limits.all <- renderTable ({
    results <- datasetInput()
    dfx <- results$data
    allLim <-(data.frame(censummary(dfx$obs, dfx$cen)$limits))
    return(allLim)
  })


  output$limits.everything <- renderTable ({
    results <- datasetInput()
    dfx <- results$data
    allsum <-censummary(dfx$obs, dfx$cen, dfx$sampleNum)
    allsum <- ldply(allsum, function(x) (data.frame(x$limits)))
    names(allsum) <- c( "Sample Number", "limit", "n", "uncen", "pexceed")
    return(allsum)

  })

  output$totalObs <- renderText({

    x <- input$obs1 + input$obs2 +input$obs3
    return(x)
  })

   output$exCen <- renderText({

    n <- input$obs1 + input$obs2 +input$obs3
    o <- input$obs1 * input$censQ1 + input$obs2 * input$censQ2 + input$obs3 * input$censQ3

    x <- o/n
    return(x)
  })

  output$GSD <- renderText({
    x <- exp(input$sdlog)
    return(x)
  })

  output$GM <- renderText({
    x <- exp(input$meanlog)
    return(x)
  })


  output$DL1 <- renderText({

    x <- getDetLim(input, "censQ1")
    return(x)
  })

  output$DL2 <- renderText({

    x <- getDetLim(input, "censQ2")
    return(x)
  })


  output$DL3 <- renderText({

    x <- getDetLim(input, "censQ3")
    return(x)
  })

  output$downloadData <- isolate({downloadHandler(

      filename = function() { paste('dfx', '.csv', sep='') },
      content = function(file) {
                                        #browser()
                                        results <- get("downResults", .GlobalEnv)
                                        data <- results$data
                                        data$pop.mean <- results$pop.mean
                                        data$pop.var <- results$pop.var
                                        data$D_obs <- as.numeric(!data$cen)
                                        data$dist <- results$distribution
        write.csv(data, file)
    }
      )
})

  output$downloadResults <- isolate({downloadHandler(

        filename = function() { paste('results', '.csv', sep='') },
        content = function(file) {

            results <- get("downResults", .GlobalEnv)
            data <- results$results
            data$pop.mean <- results$pop.mean
            data$pop.var <- results$pop.var
            data$dist <- results$distribution
            write.csv(data, file)
      }
      )
                                 })





})
