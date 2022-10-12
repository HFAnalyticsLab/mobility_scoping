## Comparing life expectancy between residential mobility clusters

# clear R environment
rm(list = ls())

# load packages
library(plyr)
library(dplyr)
library(tidyverse)
library(factoextra)
library(cluster)
library(aws.s3)
library(rio)
library(data.table)
library(devtools)
#install_github('vqv/ggbiplot')
library(ggbiplot)
library(stringr)
library(janitor)


data_bucket <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp'


# load data on residential mobility clusters (created in script 4)

data <- s3read_using(FUN = fread,
                     object = 'Francesca/mobility_scoping/data/clean/mobility_clusters_LA_v2.csv',
                     bucket = data_bucket)




#Merge with life expectancy data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig006
le_dta <- s3read_using(import, 
                       object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/life_expectancy_LA.xlsx') # File to open 

dim(le_dta)      #55 variables, 16842 observations

# tidy variables names
le_dta<-le_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions


le_dta <- le_dta %>%
  dplyr::filter(x4 == "00-01")

le_dta <- le_dta %>%
  dplyr::select("title", "life_expectancy_by_local_authority", x3, le_2010_12 = x32, le_2017_19 = x53)

le_dta$le_2010_12 <- as.numeric(le_dta$le_2010_12)
le_dta$le_2017_19 <- as.numeric(le_dta$le_2017_19)

le_dta <- le_dta %>%
  pivot_wider(names_from = x3, values_from = c(le_2010_12, le_2017_19))



# Drop Scotland and Northern Ireland
le_dta <- le_dta %>%
  subset(str_detect(life_expectancy_by_local_authority, '^E') | str_detect(life_expectancy_by_local_authority, '^W'))

le_dta <- left_join(le_dta, data, by = c("title" = "geography"))

tabyl(le_dta$cluster)
# 13% missing data due to change in LA boundaries between 2020 and 2011




# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(le_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'mobility_clusters_le_LA.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




#Merge with 2012 life expectancy data to minimise missing values
## data were downloaded from: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/lifeexpectancyatbirthandatage65bylocalareasintheunitedkingdomtable1ukandlocalareasinenglandandwales
le2012_dta <- s3read_using(import, 
                           object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/life_expectancy_LA_2012_prep.xlsx') # File to open 

dim(le2012_dta)      #7 variables, 449 observations

# tidy variables names
le2012_dta<-le2012_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions

# drop rows with missing values (formatting)
le2012_dta <- le2012_dta %>%
  drop_na(female_2009_2011) %>%
  dplyr::select(-area_name) 

le2012_dta$x3 <- gsub(" UA", "" , le2012_dta$x3)


le2012_dta<- le2012_dta %>%
  mutate(male_2009_2011 = round(as.numeric(male_2009_2011), 1), 
         male_2010_2012 = round(as.numeric(male_2010_2012), 1),
         female_2009_2011 = round(as.numeric(female_2009_2011), 1),
         female_2010_2012 = round(as.numeric(female_2010_2012), 1))

tabyl(le2012_dta$male_2009_2011)
# only 2 LAs with missing data


# merge onto data df
data <- left_join(data, le2012_dta, by = c("geography" = "x3"))
tabyl(data$male_2009_2011)


# remove weight variables and re-merge original net migration variables
data <- data %>%
  dplyr::select(V1:female_2010_2012)


# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(data # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'mobility_clusters_le2012_LA.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

