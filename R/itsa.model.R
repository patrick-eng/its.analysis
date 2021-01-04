#' Run Interrupted Time Series Analyses
#'
#' Sets up an Interrupted Time Series Analysis (ITSA) for analysing short time series data.
#'
#' @param data define data. Must be a data frame or object coercible into a data frame.
#' @param time define time variable, must either be numeric (such as a year) or of class date.
#' @param depvar define dependent variable, must be continuous.
#' @param interrupt_var define interruption treatment/condition variable, must be a factor.
#' @param covariates specify a covariate, or vector of covariates, default is NULL.
#' @param alpha desired alpha (p-value boundary of null hypothesis rejection), default is 0.05.
#' @param no.plots logical, specify that function should not return the ITSA plot, default is FALSE.
#' @param bootstrap logical, specify where function should run bootstrap estimations of F-values and return table of results, default is TRUE.
#' @param Reps numeric, define number of replications for bootstrapping, default is 1000.
#' @param parr character, define if parallelisation should be used for bootstrapper, options are inherited from boot package (the default of "no", or "multicore" and "snow").
#' @param print logical, specify whether the main model result should be printed to the console.
#' @return Returns a large list which can be assigned to the global environment, containing results and necessary information for running post-estimation itsa.postest function. It also contains the Time Series Interruption plot (itsa.plot) and bootstrap results, if applied.
#' @export itsa.model
#'
#' @examples
#'
#' # Build variables
#'
#' year <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
#' 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)
#' depv <- c(8.22, 8.19, 8.23, 8.28, 8.32, 8.39, 8.02,
#' 7.92, 7.62, 7.23, 7.1, 7.11, 6.95, 7.36, 7.51, 7.78, 7.92, 7.81)
#' interruption <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
#' cov1 <- c(3.1, 3.3, 5.1, 5.2, 5.4, 4.5, 4.7, 4.9, 5.3,
#' 5.6, 5.8, 6.0, 4.8, 5.2, 4.5, 4.6, 5.1, 4.7)
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
#' @details This function provides an Interrupted Time Series Analysis (ITSA) model using a variance-centric approach to estimating change in a dependent variable over time.
#'
#' Using the defined variables, a Type-2 Sum Squares ANCOVA Lagged Dependent Variable model is fitted which estimates the difference in means between interrupted and non-interrupted time periods, while accounting for the lag of the dependent variable and any further specified covariates.
#'
#' The function includes a bootstrap model, which runs by default. This repeats 1000 replications of the main model with randomly drawn samples. A trimmed median (10 percent removed) F-value is reported and a bootstrapped p-value derived from it. Users may turn off the bootstrapping model, or the number of replications for the bootstrap model to pass through can be altered using the Reps argument.
#'
#' Variable names must be defined using quotation marks, or their column index number within the data. Any number of covariates can be passed into the covariates argument as a list of vectors (both must be within the same data as the dependent and independent variables).
#'
#' Also returned as warning messages are the results of ANOVA and residual autocorrelation assumptions check ran in the background, if any are potentially violated. These tests and further post-estimation can be done through the itsa.postest function.
#'
#' Returns to console the results from analysis of variance test, results of the F-value bootstrap model, and a summary of the result (relative to user defined alpha) and any assumption violation warnings. Users may force this return off by declaring print=FALSE in the model arguments.
#'
#' Further returns a plot to the plot window graphing the dependent variable time series and interruption points. As this is a ggplot2 generated object, users can call the plot and make further customisations to it as an output.
#'
#' Assigning to an object will return a list of all of the above, plus further tests including a Tukey Honest Significant Differences test, the data and arguments set by the user, a table of time-series group means, the full bootstrap model results and results of assumptions tests, and the full list of residual and fitted values.
#'
#' If any of data, depvar, interrupt_var, or time are undefined, the process will stop and an error message will appear.


itsa.model <- function(data = NULL, time = NULL, depvar = NULL, interrupt_var = NULL,
                       covariates = NULL, alpha = 0.05, no.plots=FALSE,
                       bootstrap=TRUE, Reps=1000, parr="no", print=TRUE) {

  ## Save global options and set new ones
  user.ops <- options()

  options(show.signif.stars = FALSE, contrasts = c("contr.sum","contr.poly"))

  on.exit(options(user.ops))



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


  ## Coerce data to data frame
  x <- as.data.frame(x)


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

    graph <- g + ggplot2::geom_line(group=1) +
      ggplot2::xlab("") +  ggplot2::ylab("Response Variable Levels") +  ggplot2::theme_bw()

    for(i in periods){
    graph <- graph +  ggplot2::geom_segment(x=i-1, y=min(temp$depvar),
                                            xend=i-1, yend=max(temp$depvar), color="red", linetype=8) +
      ggplot2::labs(title="Interrupted series plot", x="Time", y="Dependent series")
    }

    graphics::plot(graph)

    itsa.plot <- grDevices::recordPlot()

  }

  else {
   itsa.plot <- "No plot specified"
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
      mod <- stats::aov(formula, data=d)
      fit <- car::Anova(mod, type=2)
      return(fit$`F value`)
    }

    f.stat.boot.results <- boot::boot(data=x, statistic=f.stat, R=Reps, formula=depvar ~ ., parallel = parr)


    booted.ints <- matrix(nrow=(ncol(f.stat.boot.results$t)-1), ncol=4)

    for(i in 1:(ncol(f.stat.boot.results$t)-1)){

      boots <- boot::boot.ci(f.stat.boot.results, conf = 0.9, type="perc", index=i)
      booted.ints[i,2] <- boots$percent[,4]
      booted.ints[i,3] <- stats::median(f.stat.boot.results$t[,i], na.rm=TRUE, trim = 0.1)
      booted.ints[i,4] <- boots$percent[,5]

    }

    booted.ints <- as.data.frame(booted.ints)
    names <- colnames(x)
    names <- names[2:length(names)]
    booted.ints$V1 <- names
    names(booted.ints) <- c("Parameter", "Lower CI", "Median F-value", "Upper CI")


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
        print(itsa.fit$booted.ints[1,c(2:4)], digits=5, row.names=FALSE)

      }

      message(cat(paste('', '\n', 'Result: ', result, ' (<', alpha,')', '\n', sep ="")))

  }

  if(itsa.fit$post_sums=="Post-Est Warning"){
   warning("ANCOVA Result may be biased. Please check post-estimation.", call.=FALSE)
  }

  return(itsa.fit)
}
