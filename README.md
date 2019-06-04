# its.analysis

## A package for running time series analysis on small samples using the 'interruption treatment' approach.

Patrick English - University of Exeter

<p.english@exeter.ac.uk>

The its.analysis R  package\footnote{The package is available for download via the CRAN repository.} contains two functions for running interrupted time series analysis (ITSA), using an ANCOVA based design. This 'quasi-experimental' approach is used frequently in the biomedical and biological sciences, but not so much in the social sciences. ITSA considers a that naturally-progressing dependent time series can potentially be 'interrupted' by an exogenous, independent factor which causes significant change in its variation over time. In this sense, we can view a dependent time series as being 'treated' by different, exogenous conditions (of an independent variable), and hence specify the 'quasi-experimental' model approach.

As well as critical events or major sudden changes (to legislation, for example), we can consider moments of threshold reaching, sudden trend-alteration, or rapid acceleration in independent variable time series to be 'interruptions' which then have lasting and quantifiable impacts on dependent series. Time in this sense can be split into periods according to different moments of 'pre', 'during', and 'post interruption'. Equally, time could be split into two factors ('pre- and post- interruption'), which would be most common perhaps in instances of (non-reversed) policy changes. Whichever approach is used, the design offers a different way of inspecting the association between two temporally related variables using statistical methods than classical models, and - crucially - an approach which is much more flexible and yet robust when working with small samples.

ITSA can get us around the vexing and common issue of time series data which is too short to effectively model in ARIMA or OLS frameworks, as the it can be modelled using far less taxing (in terms of statistical power and error freedom) methods. By making use of a model similar to a repeated-measures ANCOVA (but with additional time-series specific components), this package allows researchers to investigate whether or not substantial changes in the trajectories, levels, or thresholds of an independent time series have had a significant impact on a dependent series without running into such problems (see referenced paper for full discussion). 

The model contained in the itsa.model() function is designed to provide users with an easy, all-inclusive model for estimating the impact of shocks, changes, and crises on dependent time series. It delivers a wide range of outputs by default, including point estimates of means, analysis of variance adjusted for temporal dependency, and bootstrapped F-values. It takes as its input a dataframe, specified temporal, dependent, independent, and covariate vectors (within the data frame). It offers users the ability to change the alpha value against which the results of the test are measured, turn off the automatically generated plot, turn off the automatic bootstrapping, or change the number of replicates that the bootstrap model produces (1,000 by default). The function returns to the console a tables of group means, a table of analysis of variance (Type II Sum Squares) between the time periods, an R-squared statistic, a summary result of the model and assumption tests, and send a graph to the plot window. 

Assigning the function will create a summary object in the global environment which contains all of the above as objects within a list, as well as a Tukey's 'Honest Significant Difference test result', the bootstrapped confidence intervals for all model parameters, the full length of F-values produced by the bootstrap model, summaries of each individual assumption test, the data used in the main model, and the residual and fitted values.

For the function to work correctly, the dependent variable must be a continuous vector, and the independent variable must be a factor variable which identify substantively different periods of time relative to the original independent variable series. For example, a sudden step-change in the independent variable series, or the passing of a threshold, a quick and sustained change in a previous trend, or another quantifiable and quantifiable movement in the development of the independent variable time series. Covariates may be fit using the covariates argument in the model function, which both increases the power of the test (by accounting for more variance) and also adjusts the variance explained by the factorial independent variable (controlling for the competing variance of the covariate). Users should be sensible in the number of covariates fitted, and keep in mind normal assumptions regarding multicollinearity and interaction between covariates. 

Various post-estimation procedures can be ran using the itsa.postest() function. These include: a plot of the bootstrapped F-values, a Shapiro-Wilks test for residual abnormality (overlaid on a QQ-Norm plot), a Levene's Test of heterogeneous variances (overlain on a boxplot), a residual v fitted plot, and an autocorrelation function plot. These are designed to test typical AN(C)OVA and time series model assumptions. The model name must be defined as the object assigned to the global environment.

The its.analysis ITSA model deploys Type II Sum Squares (T2SS) in order to account for covariance. T2SS, sometimes dubbed 'random effects' ANOVAs  \citep{Stahle1989}, account for the variance of all other parameters included in the model before estimating a particular parameter's variance itself. This means we can fit covariate controls which will effectively isolate exogenous effects from temporal periods. Type II models are also most recommended for imbalanced sample designs \citep{Langsrud2003}, which will be naturally more common in time series modelling where periods of normality will most likely be far longer than periods of abnormality (or interruption)\footnote{See however \cite{Shaw1993} for discussion on how means-level focused AN(C)OVA models may not be so readily susceptible to problems induced by imbalanced samples.}.

Included in this repository is some test data for analysis. 

Country_Data.csv is a fake database from three made-up countries, with 30 annual reports of subjective economic well-being, GDP per capita, and unemployment rates. The examples demonstrate how the ITSA model will handle and then post-estimate different pastterns of time series trends and interruptions. Note, data can only be handled one-strata (country, region, etc) at a time, so it will be neccessary to subset this test data and any other cross-sectional data before running the functions.

The prepared data contains the following variables: 
<br /> country - 
<br /> year -
<br /> subjective_econ - aggregate annual perceptions of economic well-being
<br /> gdp_pc - GDP per capita (original independent variable series)
<br /> crisis_treatment - gdp_pc recoded as a factor, covering non-crisis (0), crisis (1), and post-crisis (2) periods
<br /> unemp_perc - a covariate, unemployment percentage
<br /> left_gov - another covariate, this time a dummy for the presence of a left-wing government

Monthly_Data.csv contains further fake data covering monthly public support for interventionist economic policies during a period of high unemployment, in which the mass media took three very distinct positions on government policy over the course of 28 months. The research question here is: does media coverage drive public opinion on government economic intervention? This particular dataset demonstrates a cautious positive result, with significant difference detected at alpha of 0.1. This compares to an OLS model with the continious version of the data (significant relationship at p < 0.01 for simple relationships, at p < 0.05 including lagged DV and trend variable). 

The five variables are:
<br /> date - 
<br /> public_opinion - aggregate public preferences in favour of the policy
<br /> media_position - an index of media favourability/hostility toward the policy
<br /> media_period - a factorised version of media_poisition for the its.analysis model
<br /> unemployment - unemployment as % of the workforce

I am incredibly grateful for any and all suggestions and feedback on the model and its performance. It would be fantastic to hear about users' experiences applying this to their own data/other data they have access to and any obvious Type I or Type II errors encountered.
