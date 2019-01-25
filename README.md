# its.analysis
BETA package for running time series analysis on small samples using the 'interruption treatment' approach

Included is some test data for analysis. 

Country_Data.csv is a fake database from three made-up countries, with 30 annual reports of subjective economic well-being, GDP per capita, and unemployment rates. The examples demonstrate how the ITSA model will handle and then post-estimate different pastterns of time series trends and interruptions. Note, data can only be handled one-strata (country, region, etc) at a time, so it will be neccessary to subset this test data and any other cross-sectional data before running the functions.
