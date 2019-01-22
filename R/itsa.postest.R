#' Post-estimating ITSA models
#'
#' Function to produce post-estimation plots from the itsa.model function.
#'
#' @param model define ITSA model results object, default is output from itsa.model function
#' @param no.plots logical, specify whether function should present post-estimation plots (default is FALSE)
#' @param alpha define alpha level for test statistics, default is 0.05
#' @keywords time series, interrupted time series, analysis of variance, post-estimation
#' @export itsa.postest
#'
#' @details This function runs and reports post-estimation tests on fits from the itsa.model function.
#'
#' Tests whether AN(C)OVA assumptions are met when modelling for time series with Interrupted Time Series Analysis (ITSA) framework.
#'
#' Default is to generate plots and summary table, but plots may be overriden using no.plots argument.
#'
#' Default alpha value for post-estimation statistics is 0.05.
#'
#' See 'itsa.model' documentation for further information.

itsa.postest <- function(model = NULL, no.plots = FALSE, alpha = 0.05) {

  if(missing(model)) {
    model <- itsa.fit
  }

  else {
    model <- model
  }


  if(no.plots==FALSE) {

    ## Plots

    stest_ob <- paste("Shapiro-Wilk Test of Residual Normality: p =", model$shapiro.test)
    ltest_ob <- paste("Levene's Test of Homogenous Variances: p =", model$levenes.test)


    graphics::plot(model$fitted.values, model$residuals,
                   main="ITSA Residuals v Fitted Plot",
                   xlab="ITSA Fitted Values",
                   ylab="ITSA Residuals",
                   graphics::mtext(ltest_ob, side=3));graphics::abline(0,0, col="red")


    stats::qqnorm(model$residuals,
                  main="ITSA QQ-Norm Plot");stats::qqline(model$residuals, col="red")
    graphics::mtext(stest_ob, side=3)

    ## Summary Table

    stest <- model$shapiro.test
    ltest <- model$levenes.test

    stest_r <- ifelse(stest < alpha, "Non-normality present", "Non-normality not present")
    ltest_r <- ifelse(ltest < alpha, "Heterogenous variance present", "Heterogenous variance not present")

    x <- c("Shapiro-Wilk", "Levene's")
    p <- c(stest, ltest)
    y <- c(stest_r, ltest_r)

    z <- as.data.frame(cbind(x, p, y))

    colnames(z) <- c("Test", "P-value", "Result")

    cat(paste("Residual Post-Estimation Test Results", '\n', '\n'))
    z

  }

  else {

    ## Summary Table

    stest <- model$shapiro.test
    ltest <- model$levenes.test

    stest_r <- ifelse(stest < alpha, "Non-normality present", "Non-normality not present")
    ltest_r <- ifelse(ltest < alpha, "Heterogenous variance present", "Heterogenous variance not present")

    x <- c("Shapiro-Wilk", "Levene's")
    p <- c(stest, ltest)
    y <- c(stest_r, ltest_r)

    z <- as.data.frame(cbind(x, p, y))

    colnames(z) <- c("Test", "P-value", "Result")

    cat(paste("Residual Post-Estimation Test Results", '\n', '\n'))
    z

  }

}
