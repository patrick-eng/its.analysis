# its.analysis

## BETA package for running time series analysis on small samples using the 'interruption treatment' approach.

Patrick English - University of Exeter

<p.english@exeter.ac.uk>

This package contains two functions for running interrupted time series analysis (ITSA). This 'quasi-experimental' approach is used frequently in the biomedical and biological sciences, but not so much in the social sciences. ITSA involves considering a that naturally-progressing dependent time series can potentially be 'interrupted' by an exogenous, independent factors which cause significant impacts on the movement of the dependent series. As well as critical events or changes, we can also consider threshold, trend-alteration, or acceleration moments in independent variable time series to be 'interruptions' which then have lasting and quantifiable impacts on dependent series. Time in this sense can be split into factors according to different moments of pre, during, and post interruption.

ITSA can get us around the vexing and common issue of time series data which is too short to effectively model in ARIMA or OLS frameworks, as the it can be modelled using far less taxing (in terms of statistical power and error freedom) methods. By making use of a model similar to a repeated-measures ANCOVA (but with additional time-series specific components), this package allows researchers to investigate whether or not substantial changes in the trajectories, levels, or thresholds of an independent time series have had a significant impact on a dependent series without running into such problems (see referenced paper for full discussion). 

The model contained in the itsa.model() function takes as its input a dataframe, specified temporal, dependent, independent, and covariate vectors (within the data frame), and offers users the ability to change the alpha value against which the results of the test are measured (and turn off the automatically generated plot. The function returns tables of group means, analysis of variances, an R-squared statistic, a summary result, a summary object to the global environment, and an interruption plot to the plot window.

The dependent variable must be a continuous vector, and the independent variable must be a factor variable which identify substantively different periods of time relative to the original independent variable series. For example, a suddent step-change in the independent variable series, or the passing of a threshold, a quick and sustained change in a previous trend, or another quantifiable and qualifiable movement in the development of the independent variable time series. Covariates may be fit using the covariates argument in the model function, which both increases the power of the test (by accounting for more variance) and also adjusts the variance explained by the factoral independent variable (controlling for the competing variance of the covariate).

Various post-estimation proceedures can be ran using the itsa.postest() function. These include: a Shapiro-Wilks test for residual normality (overlaid on a QQ-Norm plot), a Levene's Test of heterogenous variances (overlain on a boxplot), a resdiual v fitted plot, and an autocorrelation function plot. 

Included in this repository is some test data for analysis. 

Country_Data.csv is a fake database from three made-up countries, with 30 annual reports of subjective economic well-being, GDP per capita, and unemployment rates. The examples demonstrate how the ITSA model will handle and then post-estimate different pastterns of time series trends and interruptions. Note, data can only be handled one-strata (country, region, etc) at a time, so it will be neccessary to subset this test data and any other cross-sectional data before running the functions.

The prepared data contains the following variables: 
country - 
year -
subjective_econ - aggregate annual perceptions of economic well-being
gdp_pc - GDP per capita (original independent variable series)
crisis_treatment - gdp_pc recoded as a factor, covering non-crisis (0), crisis (1), and post-crisis (2) periods
unemp_perc - a covariate, unemployment percentage
left_gov - another covariate, this time a dummy for the presence of a left-wing government

I am incredibly grateful for any and all suggestions and feedback on the model and its performance. It would be fantastic to hear about users' experiences applying this to their own data/other data they have access to.
