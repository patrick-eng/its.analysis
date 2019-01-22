#' @details Interrupted Time Series Analysis (ITSA) serves as a replacement for traditional time series modelling techniques when sample sizes are too small for autocorrelation to be effectively estimated and then reduced without overfitting model parameters.
#'
#' The ITSA offered in this package is an AN(C)OVA model with a range of additional bolt-ons which provide further detailed information and test assumptions relative to time series analysis. See referenced paper for information on setting up data for ITSA.
#'
#' It is incumbent on the researcher to sensibly define interruption time periods, and to use caution when fitting covariates to shorter data series. A limit of two covariates is enforced by the model as the ability to reliably fit a third covariate would indicate a data series long enough to run more conventional time series models with.
#'
#' @keywords internal
#'
#' @references English, P. (2019) 'Interrupted Time Series Analysis as a Solution to Short Time Series Data', SSRN.
#'
"_PACKAGE"
#> [1] "_PACKAGE"
