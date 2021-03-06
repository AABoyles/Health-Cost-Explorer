---
title: "Health Cost Explorer"
output:
  html_document:
    toc: true
    toc_float: true
---

*Driving across town could save you thousands.*

About the Data
--------------

### Financial

The only Financial column we're currently using is Average Total Payments.  Annoyingly, the Inpatient and Outpatient datasets define this field slightly differently.

From the Outpatient Data Codebook:

> Average Estimated Submitted Charges : The provider's average estimated submitted charge for services covered by Medicare for the APC. These will vary from hospital to hospital because of differences in hospital charge structures.

> Average Total Payments : The average of total payments to the provider for the APC including the Medicare APC amount. Also included in Total Payments are co-payment and deductible amounts that the patient is responsible for.

From the Inpatient Data Codebook:

> Average Covered Charges: The provider's average charge for services covered by Medicare for all discharges in the MS-DRG. These will vary from hospital to hospital because of differences in hospital charge structures.

> Average Total Payments: The average total payments to all providers for the MS-DRG including the MSDRG amount, teaching, disproportionate share, capital, and outlier payments for all cases. Also included in average total payments are co-payment and deductible amounts that the patient is responsible for and any additional payments by third parties for coordination of benefits.

> Average Medicare Payments: The average amount that Medicare pays to the provider for Medicare's share of the MS-DRG. Average Medicare payment amounts include the MS-DRG amount, teaching, disproportionate share, capital, and outlier payments for all cases. Medicare payments DO NOT include beneficiary co-payments and deductible amounts nor any additional payments from third parties for coordination of benefits. Note: In general, Medicare FFS claims with dates-of-service or dates-of-discharge on or after April 1, 2013, incurred a 2 percent reduction in Medicare payment. This is in response to mandatory across-the-board reductions in Federal spending, also known as sequestration. For additional information, visit http://www.cms.gov/Outreach-andEducation/Outreach/FFSProvPartProg/Downloads/2013-03-08-standalone.pdf

In other words, it shows the average total amount a Medical Service Provider should have recieved for a procedure in a given year.  Note that this is distinct from what they charged Medicare, what they charged the patient's insurance (if any), or what the patient's responsibility was.

### Locations

The Medicare Data Contains the Address, City, State, and zip codes. These were assembled into a continuous address string (as can be seen in build.r) and then passed to [the batch geocoder at FindLatitudeAndLongitude.com](http://www.findlatitudeandlongitude.com/batch-geocode/).

### Raw Data

All Raw Data was downloaded from [CMS](http://cms.gov).

Outpatient Data | Inpatient Data
----------------|---------------
[2011](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Outpatient2011.html) | [2011](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient2011.html)
[2012](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Outpatient2012.html) | [2012](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient2012.html)
[2013](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Outpatient2013.html) | [2013](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient2013.html)
[2014](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Outpatient2014.html) | [2014](http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient2014.html)

Under the Hood
--------------

This is an open-source application. The code can be cloned from http://github.com/aaboyles/Health-Cost-Explorer

 * app.R - Contains the code for the Shiny App
 * build.R - adds the paragraphs and html markups of the doc and lays out the interface
 * data/geoCoded.csv - Contains address, lat and long for each Medical Provider
 * data/StateCentroids.csv - Contains the latitudes and longitudes for the approximate geographic centroids of each US State (+DC). Used to center the map when the user selects a new State.
 * data/Medicare_Data.Rda - Compressed Medicare data merged with geoCoded.csv to provide a mapping between Provider, Procedure, and Location. Is output by running build.R.
