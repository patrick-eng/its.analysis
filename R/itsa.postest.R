#' Post-estimating ITSA models
#'
#' Function to produce post-estimation plots from the itsa.model function.
#'
#' @param model define ITSA model results object, default is output from itsa.model function
#' @param no.plots logical, specify whether function should present post-estimation plots (default is FALSE)
#' @param alpha define alpha level for test statistics, default is 0.05
#' @return if no.plots is not forced, will return a table to the console and two plots to the plot tab
#' @keywords time series, interrupted time series, analysis of variance, post-estimation
#' @export itsa.postest
#'
#' @details This function runs and reports post-estimation tests on fits from the itsa.model function, and generates four plots.
#'
#' Main tests are whether two key ANCOVA assumptions are met, and an additional autocorrelation test for the time series framework.
#'
#' The Shaprio-Wilks test examines the residuals from the fitted model for normality. A p-value less than alpha indicates abnormal residuals.
#'
#' The Levene's Test makes sure that there are equal variances between the treated groups. A p-value less than alpha indicates hetrogenous variances.
#'
#' A QQ-Norm and Boxplot are generated with the test results overlaid (respectively), with a Residual v Fitted and Autocorrelation Function Plot also generated.
#'
#' Default is to generate plots and summary table, but plots may be overriden using no.plots argument.
#'
#' Default alpha value for post-estimation statistics is 0.05, test results will suggest potential presence of residual problems at 0.2, but user discretion is needed (examined in tandam with the Residuals v Fitted plot).
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

    ### Plots


    # Result text objects
    stest_ob <- paste("Shapiro-Wilk Test of Residual Normality: p =", model$shapiro.test)
    ltest_ob <- paste("Levene's Test of Homogenous Variances: p =", model$levenes.test)

    # Residual v Fitted
    graphics::plot(model$fitted.values, model$residuals,
                   main="ITSA Residuals v Fitted Plot",
                   xlab="ITSA Fitted Values",
                   ylab="ITSA Residuals");graphics::abline(0,0, col="red")

    # ACF plot
    forecast::Acf(model$residuals, main="ITSA Autocorrelation Plot")

    # Residual v Factor Boxplot
    graphics::boxplot(model$residuals ~ model$interrupt_var,
            main="ITSA Residual Variances",
            xlab="Time Period",
            ylab="ITSA Residuals")
    graphics::mtext(ltest_ob, side=3)


    # QQ-Norm plot
    stats::qqnorm(model$residuals,
                  main="ITSA QQ-Norm Plot");stats::qqline(model$residuals, col="red")
    graphics::mtext(stest_ob, side=3)



    ## Summary Table

    stest <- model$shapiro.test
    ltest <- model$levenes.test

    if(stest < alpha){
      stest_r <- "Non-normality present"
      }
    else{
      if(stest < alpha * 4){
        stest_r <- "Non-normality could be present"
    }

    else{
      stest_r <- "Non-normality not present"
    }
    }


    if(ltest < alpha){
      ltest_r <- "Heterogenous variances present"
    }
    else{
      if(ltest < alpha * 4){
        ltest_r <- "Heterogenous variances could be present"
      }

      else{
        ltest_r <- "Heterogenous variances not present"
      }
    }


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
