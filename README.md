# Residential mobility in England and Wales
Project Status: [completed]

## Project Description
Residential mobility - people moving where they live between local areas in the UK - is one factor shaping health in local areas. People who are young and in good health tend to move to areas where many other young and healthy people live. This has the potential to reinforce existing geographic inequalities in health.

This analysis aims to describe residential mobility in England and Wales, according to self-reported health, age and economic activity. We also aimed to identify different types of residential mobility at the local authority level, and compare the prevalence of overweight/obesity and common mental health disorders between local authorities of each residential mobility type. 

## Outputs
The findings have been published on the [Health Foundation website](add URL).

## Data source
We used open, area-level [residential mobility data](https://www.nomisweb.co.uk/sources/census_2011_ukmigration) from the 2011 Census. (Detailed migration data were not available for the 2021 Census at the time of analysis.) These data are based on self-reported address changes in the year up to the Census date of 27th March 2011. 

We also used data from Fingertips on the prevalence of [adult overweight/obesity](https://fingertips.phe.org.uk/profile/national-child-measurement-programme/data#page/4/gid/1938133368/ati/301/iid/93088/age/168/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1) and [common mental disorders](https://fingertips.phe.org.uk/search/common%20mental%20health#page/3/gid/1/pat/6/par/E12000001/ati/201/are/E06000047/iid/93495/age/164/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1). 

## How does it work? 
All data used in this analysis are open data. We have indicated in the main analysis scripts links where the data can be downloaded. This code can be used to replicate our analysis at the local authority (district/unitary) level. 

- Script 1 includes the code for analysis of residential mobility by self-reported health. 
- Script 2 includes the code for analysis of residential mobility by age.
- Script 3 includes the code for analysis of residential mobility by economic activity. 
- Script 4 includes the code for the cluster analysis, which we used to classify local authorities in 4 residential mobility types. 

These analyses were conducted within Amazon SW3, using S3 buckets. 

The exploratory_analyses folder includes scripts used for exploratory analyses, including analyses at the output area and middle-super output area levels, and supplementary analyses at the local authority level which were not included in the publication. 

### Requirements
These scripts were written in R. The following R packages (available on CRAN) are needed:
- TBC

## The Health Foundation project team
* Francesca Cavallaro
* Anne Alarilla
* Jay Hughes
* Mai Stafford
* Charles Tallack

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/domcare_hospital_LBBD/blob/master/LICENSE).
