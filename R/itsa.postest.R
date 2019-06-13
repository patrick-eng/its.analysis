#' Post-estimating ITSA models
#'
#' Function to produce post-estimation plots from the itsa.model function.
#'
#' @param model define ITSA model results object.
#' @param no.plots logical, specify whether function should present post-estimation plots (default is FALSE).
#' @param alpha define alpha level for test statistics, default is 0.05.
#' @param bootstrap logical, declare if itsa.model was bootstrapped for plot of these results.
#' @param print logical, specify whether the main model result should be printed to the console.
#' @return returns a list containing post-estimation test results and four plots (plus one additional if bootstrapping applied) which are also sent directly to the plot tab.
#' @export itsa.postest
#'
#' @details This function runs and reports post-estimation tests on fits from the itsa.model function, and generates four plots.
#'
#' Main tests are whether two key ANCOVA assumptions are met, and an additional autocorrelation test for the time series framework.
#'
#' The Shaprio-Wilks test examines the residuals from the fitted model for abnormality. A p-value less than alpha indicates abnormal residuals.
#'
#' The Levene's Test makes sure that there are equal variances between the treated groups. A p-value less than alpha indicates heterogeneous variances.
#'
#' A QQ-Norm and Boxplot are generated with the test results overlaid (respectively), with a Residual v Fitted and Autocorrelation Function Plot also generated.
#'
#' The results of bootstrap estimations in itsa.model will be plotted, unless argument is switched to FALSE.
#'
#' Default is to generate plots and summary table, but plots may be overriden using no.plots argument.
#'
#' Default alpha value for post-estimation statistics is 0.05, test results will suggest potential presence of problems at higher values (and also at higher levels relative to a user-inputted alpha), but user discretion is needed (examined in tandam with the Residuals v Fitted plot).
#'
#' See 'itsa.model' documentation for further information.

itsa.postest <- function(model = NULL, no.plots = FALSE, alpha = 0.05, bootstrap = TRUE, print = TRUE) {

  if(missing(model)) {
    stop("Error: model object not defined", call.=TRUE)
  }


  user.ops <- options()

  options(show.signif.stars = FALSE, contrasts = c("contr.sum","contr.poly"))

  on.exit(options(user.ops))


  if(no.plots==FALSE) {

    ### Plots
    graphics::plot.new()

    ## Bootstrap plot
    if(bootstrap==TRUE){

      graphics::plot(model$fstat.bootstrap, index=1)
      bootstrap.plot <- grDevices::recordPlot()

      }


    # Result text objects
    stest_ob <- paste("Shapiro-Wilk Test of Abnormality: p =", model$shapiro.test)
    ltest_ob <- paste("Levene's Test of Heterogeneous Variances: p =", model$levenes.test)


    # Residual v Fitted
    graphics::plot(model$fitted.values, model$residuals,
                   main="ITSA Residuals v Fitted Plot",
                   xlab="ITSA Fitted Values",
                   ylab="ITSA Residuals");graphics::abline(0,0, col="red")
    residual.plot <- grDevices::recordPlot()


    # ACF plot
    forecast::Acf(model$residuals, main="ITSA Autocorrelation Plot")
    acf.plot <- grDevices::recordPlot()


    # DV v Factor Boxplot
    graphics::boxplot(model$dependent ~ model$interrupt_var,
                      main="Group Variances",
                      xlab="Time Period",
                      ylab="Dependent Variable");graphics::mtext(ltest_ob, side=3)
    variance.plot <- grDevices::recordPlot()


    # QQ-Norm plot
    stats::qqnorm(model$residuals,
                  main="ITSA QQ-Norm Plot");stats::qqline(model$residuals, col="red")
    graphics::mtext(stest_ob, side=3)
    qqnorm.plot <- grDevices::recordPlot()



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
      ltest_r <- "Heterogeneous variances present"
    }
    else{
      if(ltest < alpha * 2){
        ltest_r <- "Heterogeneous variances could be present"
      }
      else{
        ltest_r <- "Heterogeneous variances not present"
      }
    }



    acftest <- model$autcorr
    acftest_r <- ifelse(acftest=="Evidence of autocorrelation", "Autocorrelation may be present", "Autocorrelation not present")



    x <- c("Shapiro-Wilk", "Levene's", "Autocorrelation")
    p <- c(stest, ltest, "NA")
    y <- c(stest_r, ltest_r, acftest_r)

    z <- as.data.frame(cbind(x, p, y))

    colnames(z) <- c("Test", "P-value", "Result")

    itsa.postest.results <- as.list("ITSA Post-Estimation")
    itsa.postest.results$test_results <- z
    if(bootstrap==TRUE){
      itsa.postest.results$bootstrap_plot <- bootstrap.plot
    }
    itsa.postest.results$residual_plot <- residual.plot
    itsa.postest.results$acf_plot <- acf.plot
    itsa.postest.results$variance_plot <- variance.plot
    itsa.postest.results$qqnorm_plot <- qqnorm.plot

    if(print==TRUE){
      z
    }

    return(itsa.postest.results)

  }




  else {

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
      ltest_r <- "Heterogeneous variances present"
    }
    else{
      if(ltest < alpha * 2){
        ltest_r <- "Heterogeneous variances could be present"
      }

      else{
        ltest_r <- "Heterogeneous variances not present"
      }
    }

    acftest <- model$autcorr
    acftest_r <- ifelse(acftest=="Evidence of autocorrelation", "Autocorrelation may be present", "Autocorrelation not present")

    x <- c("Shapiro-Wilk", "Levene's", "Autocorrelation")
    p <- c(stest, ltest, "NA")
    y <- c(stest_r, ltest_r, acftest_r)

    z <- as.data.frame(cbind(x, p, y))

    colnames(z) <- c("Test", "P-value", "Result")

    itsa.postest.results <- as.list("ITSA Post-Estimation")
    itsa.postest.results$test_results <- z
    itsa.postest.results$plots <- "No plots forced"

    if(print==TRUE){
      z
    }

    return(itsa.postest.results)

  }

}
