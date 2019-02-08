#' Run Interrupted Time Series Analyses
#'
#' Sets up an Interrupted Time Series Analysis (ITSA) for analysing short time series data.
#'
#' @param data define data
#' @param time define time variable, must either be numeric (such as a year) or of class date
#' @param depvar define dependent variable, must be continuous
#' @param interrupt_var define interruption treatment/condition variable, must be a factor
#' @param covariates specify a covariate, or vector of covariates, default is NULL
#' @param alpha desired alpha (p-value boundary of null hypothesis rejection), default is 0.05.
#' @param no.plots logical, specify whether function should return the ITSA plot, default is FALSE
#' @return Summary object is created in the global environment named itsa.fit which contains results and necessary information for running post-estimation itsa.postest function. It also contains the Time Series Interruption plot (itsa.plot).
#' @export itsa.model
#'
#' @keywords time series, interrupted time series, analysis of variance
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
#' itsa.model(data=x, time="year", depvar="depv", interrupt_var = "interruption", alpha=0.01)
#'
#' # Add a covariate
#' itsa.model(data=x, time="year", depvar="depv", interrupt_var = "interruption",
#' covariates = "cov1", alpha=0.1)
#'
#' # Example no significant result
#' itsa.model(data=x, time="year", depvar="cov1", interrupt_var = "interruption", alpha=0.05)
#'
#'
#' @details This function provides a front door for the aov function in R's stats package, setting it up for running Interrupted Time Series Analysis (ITSA).
#'
#' Using the inputted variables, a Type-2 SS ANCOVA model is fitted which estimates the difference in means between interrupted and non-interrupted time periods, while accounting for the lag of the dependent variable and any further specified covariates.
#'
#' Variable names must be defined using quotation marks. Any further covariates can be passed into the covariates argument, either as a single vector or list of vectors (both must be within the same data as the dependent and independent variables).
#'
#' Returns tables of time period group means (including counts and standard deviations), results from analysis of variance (including R-squared), and a summary of the result (relative to user defined alpha).
#'
#' If there is suggestion of ANOVA assumption violation in the model, a warning message will appear. These tests and further post-estimation can be done through the itsa.postest function.
#'
#' If any of data, depvar, interrupt_var, or time are undefined, the function will stop and an error message will appear.


itsa.model <- function(data = NULL, time = NULL, depvar = NULL, interrupt_var = NULL,
                       covariates = NULL, alpha = 0.05, no.plots=FALSE) {

  ## Save global options and set new ones
  default_ops <- options()
  options(show.signif.stars = FALSE, contrasts = c("contr.sum","contr.poly"))


  ## Check variable specifications

  if(missing(data)){
    stop("Error: data not defined", call.=TRUE)
  }

  if(missing(depvar)){
    stop("Error: dependent variable not defined", call.=TRUE)
  }

  if(missing(interrupt_var)){
    stop("Error: independent variable not defined", call.=TRUE)
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


  ## Build object for means

  ITSMeanValues <- plyr::ddply(x, ~interrupt_var,
                               plyr::summarise,count=length(depvar),
                               mean=mean(depvar, na.rm=TRUE),s.d.=stats::sd(depvar, na.rm=TRUE))



  ## Build and save plot

  iv_lagged <- c(NA, x$interrupt_var[1:(length(x$interrupt_var)-1)])

  periods <- stats::na.omit(as.numeric(data[,time][as.numeric(x$interrupt_var) - (iv_lagged) != 0]))

  if(no.plots==FALSE) {

    graphics::plot.new()
    l <- graphics::legend(graphics::par('usr')[2], graphics::par('usr')[4], bty='n', xpd=NA,
                          c("Response var", "Interruption"), col=c("black","dark grey"), lty=c(1,1), cex=0.5)
    w <- graphics::grconvertX(l$rect$w, to='ndc') - graphics::grconvertX(0, to='ndc')
    graphics::par(mar=c(5, 4, 4, 5), xpd=TRUE, omd=c(0, 1-w, 0, 1))
    graphics::plot(data[,time], x$depvar, type = "l",
                   xlab ="", ylab = "Response Variable Levels", col="black", main="Time Series Interruption Plot")

    for(i in periods){
      graphics::segments(x0=i, y0=min(x$depvar), y1=max(x$depvar), col="red", lty=5)
    }

    graphics::legend(graphics::par('usr')[2], graphics::par('usr')[4], bty='n', xpd=NA,
                     c("Response var", "Interruption"), col=c("black","red"), lty=c(1,5), cex=0.9)
    itsa.plot <- grDevices::recordPlot()

  }

  else {
    print('No plot forced')
  }

  ## Construct lag and shorten data

  depvar_lagged <- c(NA, x$depvar[1:(length(x$depvar)-1)])

  x$lag_depvar <- depvar_lagged

  x <- subset(x, stats::complete.cases(x))

  ## Build ANCOVA summary objects

  adjr_sq <- round(summary(stats::lm(data = x, depvar ~ .))$adj.r.squared, digits=4)

  model <- stats::aov(data = x, depvar ~ .)

  ITSModResult <- car::Anova(model, type=2)

  stest <- stats::shapiro.test(model$residuals)
  stest_r <- round(stest[["p.value"]], digits=4)

  ltest <- car::leveneTest(x$depvar ~ x$interrupt_var)
  ltest_r <- round((ltest[1,3]), digits=4)

  result <- ifelse(ITSModResult$`Pr(>F)`[1] < alpha,
                   "Significant variation between time periods with chosen alpha",
                   "No significant variation between time periods with chosen alpha")

  post_sums <- ifelse((stest_r < 0.2 | ltest_r < 0.15),
                      "Warning: ANCOVA Result may be biased by abnormality in residuals or heterogeneous variances.
                      Please check post-estimation.",
                      "")

  post_sums_s <- ifelse((stest_r < 0.2 | ltest_r < 0.15),
                        "Post-Est Warning",
                        "No Post-Est Warning")

  ## Build object for summary

  itsa.fit <<- as.list("ITSA Model Fit")
  itsa.fit$aov.result <<- ITSModResult
  itsa.fit$alpha <<- alpha
  itsa.fit$itsa.result <<- result
  itsa.fit$group.means <<- ITSMeanValues
  itsa.fit$dependent <<- x$depvar
  itsa.fit$interrupt_var <<- x$interrupt_var
  itsa.fit$residuals <<- model$residuals
  itsa.fit$fitted.values <<- model$fitted.values
  itsa.fit$shapiro.test <<- stest_r
  itsa.fit$levenes.test <<- ltest_r
  itsa.fit$post_sums <<- post_sums_s
  itsa.fit$adjr_sq <<- adjr_sq

  if(no.plots==FALSE) {

    itsa.fit$itsa.plot <<- itsa.plot

  }



  ## Return objects
  cat(paste('', '\n'))
  cat(paste('Mean Values of Dependent Variable Between Time Periods:', '\n'))
  print(ITSMeanValues)
  cat(paste('', '\n', '\n'))
  cat(paste('Analysis of Variances:', '\n'))
  print(ITSModResult)
  cat(paste('', '\n'))
  cat(paste('Model Adjusted R-Squared:', adjr_sq, '\n'))
  cat(paste('', '\n'))
  cat(paste('Result:', result, '( <',alpha,')', '\n'))
  cat(paste('', '\n'))
  cat(post_sums)
  cat(paste('', '\n'))

  ## reset global options
  options(default_ops)
  graphics::par(mar=c(5.1, 4.1, 4.1, 2.1), omd=c(0,1,0,1), xpd=FALSE)

}
