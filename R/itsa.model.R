#' Run Interrupted Time Series Analyses
#'
#' Sets up an Interrupted Time Series Analysis (ITSA) for analysing short time series data.
#'
#' @param data define data.
#' @param time define time variable, must either be numeric (such as a year) or of class date.
#' @param depvar define dependent variable, must be continuous.
#' @param interrupt_var define interruption treatment/condition variable, must be a factor.
#' @param covariates specify a covariate, or vector of covariates, default is NULL.
#' @param alpha desired alpha (p-value boundary of null hypothesis rejection), default is 0.05.
#' @param no.plots logical, specify that function should not return the ITSA plot, default is FALSE.
#' @param bootstrap logical, specify where function should run bootstrap estimations of F-values and return table of results, default is TRUE.
#' @param Reps define number of replications for bootstrapping, default is 1000.
#' @param print logical, specify whether the main model result should be printed to the console.
#' @return returns a large list which can be assigned to the global environment, containing results and necessary information for running post-estimation itsa.postest function. It also contains the Time Series Interruption plot (itsa.plot) and bootstrap results, if applied.
#' @export itsa.model
#'
#' @examples
#'
#' # Build variables
#'
#' year <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
#' 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
#' depv <- c(8.22, 8.19, 8.23, 8.28, 8.32, 8.39, 8.02,
#' 7.92, 7.62, 7.23, 7.1, 6.95, 7.36, 7.51, 7.78, 7.92)
#' interruption <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
#' cov1 <- c(3.1, 3.3, 5.1, 5.2, 5.4, 4.5, 4.7, 4.9, 5.3,
#' 5.6, 5.8, 4.8, 5.2, 4.5, 4.6, 5.1)
#'
#' x <- as.data.frame(cbind(year, depv, interruption, cov1))
#'
#' # First example model
#' itsa.model(data=x, time="year", depvar="depv", interrupt_var = "interruption",
#' alpha=0.05, bootstrap=TRUE, Reps = 250)
#'
#' # Add a covariate, change alpha to 0.1
#' itsa.model(data=x, time="year", depvar="depv", interrupt_var = "interruption",
#' covariates = "cov1", alpha=0.1, bootstrap=TRUE, Reps = 250)
#'
#' # Force no plots and turn off bootstrap
#' itsa.model(data=x, time="year", depvar="depv", interrupt_var = "interruption",
#' covariates = "cov1", alpha=0.1, no.plots=TRUE, bootstrap=FALSE)
#'
#' # Example of no significant result
#' itsa.model(data=x, time="year", depvar="cov1", interrupt_var = "interruption",
#'  alpha=0.05, bootstrap=FALSE)
#'
#'
#' @details This function provides a front door for the aov function in R's stats package, setting it up for running Interrupted Time Series Analysis (ITSA).
#'
#' Using the inputted variables, a Type-2 Sum Squares ANCOVA Lagged Dependent Variable model is fitted which estimates the difference in means between interrupted and non-interrupted time periods, while accounting for the lag of the dependent variable and any further specified covariates.
#'
#' The function includes a bootstrap model by default, which runs 1000 replications of the main model with randomly drawn samples. A trimmed mean (10% removed) F-value is reported and a bootstrapped p-value derived from it. Users may turn off the bootstrapping model, or the number of replications for the bootstrap model to pass through can be altered using the Reps argument.
#'
#' Variable names must be defined using quotation marks. Any number of covariates can be passed into the covariates argument as a list of vectors (both must be within the same data as the dependent and independent variables).
#'
#' Also returned as warning messages are the results of ANOVA and residual autocorrelation assumptions check ran in the background, if any are potentially violated. These tests and further post-estimation can be done through the itsa.postest function.
#'
#' Returns to console the results from analysis of variance test, results of the F-value bootstrap model, and a summary of the result (relative to user defined alpha) and any assumption violation warnings.
#'
#' Also returns a plot to the plot window graphing the dependent variable time series and interruption points. As this is a ggplot2 generated object, users can call the plot and make further customisations to it as an output.
#'
#' Assigning to an object will return a list of all of the above, plus further tests including a Tukey Honest Significant Differences test, the data and arguments set by the user, a table of time-series group means,, the full bootstrap model results and results of assumptions tests, and the full list of residual and fitted values.
#'
#' If any of data, depvar, interrupt_var, or time are undefined, the process will stop and an error message will appear.


itsa.model <- function(data = NULL, time = NULL, depvar = NULL, interrupt_var = NULL,
                       covariates = NULL, alpha = 0.05, no.plots=FALSE, bootstrap=TRUE, Reps=1000, print=TRUE) {

  ## Save global options and set new ones
  user.ops <- options()

  options(show.signif.stars = FALSE, contrasts = c("contr.sum","contr.poly"))

  on.exit(options(user.ops))

  #par.user <- graphics::par(no.readonly = TRUE)

  #on.exit(graphics::par(par.user), add=TRUE)



  ## Check variable specifications

  if(missing(data)){
    stop("Error: data not defined", call.=TRUE)
  }

  if(missing(depvar)){
    stop("Error: dependent variable not defined", call.=TRUE)
  }

  if(missing(interrupt_var)){
    stop("Error: interruption variable not defined", call.=TRUE)
  }

  if(missing(time)){
    stop("Error: time variable not defined", call.=TRUE)
  }

  ## Assign values

  if(missing(covariates)){

    x <- data.frame(depvar=data[,depvar],
                    interrupt_var=as.factor(data[,interrupt_var]))

  }

  else {
    x <- data.frame(depvar=data[,depvar],
                    interrupt_var=as.factor(data[,interrupt_var]))

    for(i in covariates){
      x[,i] <- data[,i]
    }
  }


  ## Build object for means (point-estimates)

  ITSMeanValues <- plyr::ddply(x, ~interrupt_var,
                               plyr::summarise,count=length(depvar),
                               mean=mean(depvar, na.rm=TRUE),s.d.=stats::sd(depvar, na.rm=TRUE))



  ## Build and save plot

  iv_lagged <- c(NA, x$interrupt_var[1:(length(x$interrupt_var)-1)])

  periods <- stats::na.omit(as.numeric(data[,time][as.numeric(x$interrupt_var) - (iv_lagged) != 0]))

  if(no.plots==FALSE) {

    graphics::plot.new()

    time <- data[,time]

    temp <- as.data.frame(cbind(x, time))

    g <- ggplot2::ggplot(data=temp, ggplot2::aes(y=depvar, x=time))

    graph <- g + ggplot2::geom_line() +
      ggplot2::xlab("") +  ggplot2::ylab("Response Variable Levels") +  ggplot2::theme_bw()

    for(i in periods){
    graph <- graph +  ggplot2::geom_segment(x=i, y=min(temp$depvar), xend=i, yend=max(temp$depvar), color="red", linetype=8)
    }

    graphics::plot(graph)

    itsa.plot <- grDevices::recordPlot()

  }

  else {
   itsa.plot <- "No plot forced"
  }



  ## Construct lag and shorten data

  depvar_lagged <- c(NA, x$depvar[1:(length(x$depvar)-1)])

  x$lag_depvar <- depvar_lagged

  x <- subset(x, stats::complete.cases(x))



  ## Build ANCOVA summary objects

  adjr_sq <- round(summary(stats::lm(data = x, depvar ~ .))$adj.r.squared, digits=4)

  model <- stats::aov(data = x, depvar ~ .)

  ITSModResult <- round(car::Anova(model, type=2), digits=4)

  ITSModTukey <- stats::TukeyHSD(stats::aov(x$depvar ~ x$interrupt_var))

  result <- ifelse(ITSModResult$`Pr(>F)`[1] < alpha,
                   "Significant variation between time periods with chosen alpha",
                   "No significant variation between time periods with chosen alpha")



  ## Bootstrap F-values

  if(bootstrap==TRUE){

    set.seed(12345)

    f.stat <- function(formula, data, indices) {
      d <- data[indices,]
      fit <- car::Anova(stats::aov(formula, data=d))
      return(fit$`F value`)
    }

    f.stat.boot.results <- boot::boot(data=x, statistic=f.stat, R=Reps, formula=depvar ~ .)


    booted.ints <- matrix(nrow=(ncol(f.stat.boot.results$t)-1), ncol=4)

    for(i in 1:(ncol(f.stat.boot.results$t)-1)){

      boots <- boot::boot.ci(f.stat.boot.results, type="perc", index=i)
      booted.ints[i,2] <- boots$percent[,4]
      booted.ints[i,3] <- mean(f.stat.boot.results$t[,i], na.rm=TRUE, trim = 0.1)
      booted.ints[i,4] <- boots$percent[,5]

    }

    booted.ints <- as.data.frame(booted.ints)
    names <- colnames(x)
    names <- names[2:length(names)]
    booted.ints$V1 <- names
    names(booted.ints) <- c("Parameter", "Lower CI", "Mean F-value", "Upper CI")

    booted.ints$`P-value` <- NA

    for(i in 1:nrow(booted.ints)){
      booted.ints[i,5] <- stats::pf(booted.ints[i,3], df1= ITSModResult[i,2], df2=ITSModResult["Residuals",2], lower.tail = FALSE)
    }


  }

  else {

    f.stat.boot.results <- "No bootstrapping"

  }



  ## Build assumption test summary objects

  stest <- stats::shapiro.test(model$residuals)

  stest_r <- round(stest[["p.value"]], digits=4)

  ltest <- car::leveneTest(x$depvar ~ x$interrupt_var)

  ltest_r <- round((ltest[1,3]), digits=4)

  acf.test <- forecast::taperedacf(model$residuals, lag.max = 5, plot=FALSE)




  acf.data <- cbind(acf.test$z, acf.test$upper, acf.test$lower)

  acf.results <- as.matrix(ifelse((acf.data[,1] < acf.data[,3] | acf.data[,1] > acf.data[,2]), "FAIL", "PASS"))

  acf.out <- ifelse("FAIL" %in% acf.results[1:3,1], "Evidence of autocorrelation", "No autocorrelation evidence")



  post_sums <- ifelse((stest_r < 0.2 | ltest_r < 0.15 | acf.out == "Evidence of autocorrelation"),
                      "Warning: ANCOVA Result may be biased. Please check post-estimation.",
                      "")

  post_sums_s <- ifelse((stest_r < 0.2 | ltest_r < 0.15 | acf.out == "Evidence of autocorrelation"),
                        "Post-Est Warning",
                        "No Post-Est Warning")



  ## Build object for summary

  itsa.fit <- as.list("ITSA Model Fit")
  itsa.fit$aov.result <- ITSModResult
  itsa.fit$tukey.result <- ITSModTukey
  itsa.fit$data <- x
  itsa.fit$alpha <- alpha
  itsa.fit$itsa.result <- result
  itsa.fit$group.means <- ITSMeanValues
  itsa.fit$dependent <- x$depvar
  itsa.fit$interrupt_var <- x$interrupt_var
  itsa.fit$residuals <- model$residuals
  itsa.fit$fitted.values <- model$fitted.values
  itsa.fit$shapiro.test <- stest_r
  itsa.fit$levenes.test <- ltest_r
  itsa.fit$autcorr <- acf.out
  itsa.fit$post_sums <- post_sums_s
  itsa.fit$adjr_sq <- adjr_sq
  itsa.fit$fstat.bootstrap <- f.stat.boot.results
  itsa.fit$itsa.plot <- itsa.plot

  if(bootstrap==TRUE){
    itsa.fit$booted.ints <- booted.ints
  }


  ## Return model result objects
  if(print == TRUE){
      print(ITSModResult)
      cat(paste('', '\n'))

      if(bootstrap==TRUE){
        cat(paste('Bootstrapped F-values:', '\n'))
        print(itsa.fit$booted.ints[1,c(2:5)], digits=5, row.names=FALSE)

      }

      message(cat(paste('', '\n', 'Result: ', result, ' (<', alpha,')', '\n', sep ="")))

  }

  if(itsa.fit$post_sums=="Post-Est Warning"){
   warning("ANCOVA Result may be biased. Please check post-estimation.", call.=FALSE)
  }

  return(itsa.fit)
}
