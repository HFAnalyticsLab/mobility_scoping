## Preparing area-level mobility data - Output areas

# Housekeeping
# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

# list.of.packages.cran <- c(
#   "arm", "car", "corrplot", "FRK", "gghighlight",
#   "ggplot2", "ggmap", "GISTools", "gridExtra", "gstat",
#   "jtools", "kableExtra", "knitr", "lme4", "lmtest",
#   "lubridate", "MASS", "merTools", "plyr", "RColorBrewer",
#   "rgdal", "sf", "sjPlot", "sp", "spgwr",
#   "spatialreg", "spacetime", "stargazer", "tidyverse", "tmap",
#   "viridis", "tufte"
# )
# 
# new.packages.cran <- list.of.packages.cran[!(list.of.packages.cran %in% installed.packages()[,"Package"])]
# if(length(new.packages.cran)) install.packages(new.packages.cran)
# 
# for(i in 1:length(list.of.packages.cran)) {
#   library(list.of.packages.cran[i], character.only = T)
# }



#load packages
library(data.table)
library(aws.s3)

pacman::p_load(haven, 
               dplyr, 
               survey, 
               janitor,
               questionr, 
               epiDisplay, 
               rio, 
               ggplot2, 
               apyramid,
               magrittr, 
               stringr, 
               here,
               readr)


# set working directory (local/fixed pathway)
  #NOTE: obsolete since project sets wd and using "here"
  #setwd("M:/Analytics/Francesca/Mobility_data")
here()


# import all data
  ## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008

## load files to the environment directly from s3 bucket
eastdta <- s3read_using(import # Which function are we using to read
             , object = 'censusmig_OA_East.csv' # File to open
             , bucket = buck_data) # Bucket name defined above

emdta <- s3read_using(import # Which function are we using to read
                        , object = 'censusmig_OA_EastMidlands.csv' # File to open
                        , bucket = buck_data) # Bucket name defined above

londondta <- s3read_using(import # Which function are we using to read
                      , object = 'censusmig_OA_London.csv' # File to open
                      , bucket = buck_data) # Bucket name defined above

nedta <- s3read_using(import # Which function are we using to read
                          , object = 'censusmig_OA_NorthEast.csv' # File to open
                          , bucket = buck_data) # Bucket name defined above

nwdta <- s3read_using(import # Which function are we using to read
                      , object = 'censusmig_OA_NorthWest.csv' # File to open
                      , bucket = buck_data) # Bucket name defined above

sedta <- s3read_using(import # Which function are we using to read
                      , object = 'censusmig_OA_SouthEast.csv' # File to open
                      , bucket = buck_data) # Bucket name defined above

swdta <- s3read_using(import # Which function are we using to read
                      , object = 'censusmig_OA_SouthWest.csv' # File to open
                      , bucket = buck_data) # Bucket name defined above

wmdta <- s3read_using(import # Which function are we using to read
                      , object = 'censusmig_OA_WestMidlands.csv' # File to open
                      , bucket = buck_data) # Bucket name defined above

yhdta <- s3read_using(import # Which function are we using to read
                      , object = 'censusmig_OA_YorkshireHumber.csv' # File to open
                      , bucket = buck_data) # Bucket name defined above


# append all datasets together
eng_dta <- rbind(eastdta, emdta, londondta, nedta, nwdta, sedta, swdta, wmdta, yhdta)

dim(eng_dta)      #24 variables, 171372 observations
table(eng_dta$date)

# tidy variables names
names(eng_dta)<-str_replace_all(names(eng_dta), c(" " = "." , "," = "" , ";" = "" , "/" = "" , ":" = ""))
  #remove spaces so that we can refer to column names in functions

# eng_dta<-eng_dta %>%
#   clean_names() %>% 
#   select(1:10) 
# 
# names(eng_dta)[4:10]<-c('n_usualres','n_samead', 'n_totalmig','n_migwithin','n_miguk','n_migfor','n_outmig')

eng_dta <- rename(eng_dta, n_samead = Migrationethnic.group.Lived.at.same.address.one.year.ago.measures.Value)
eng_dta <- rename(eng_dta, n_usualres11 = Migrationethnic.group.All.usual.residents.measures.Value)
eng_dta <- rename(eng_dta, n_movedwithin = Migrationethnic.group.Migrants.Moved.within.the.area.measures.Value)
eng_dta <- rename(eng_dta, n_inmiguk = Migrationethnic.group.Migrants.Moved.into.the.area.from.within.the.UK.measures.Value)
eng_dta <- rename(eng_dta, n_inmigfor = Migrationethnic.group.Migrants.Moved.into.the.area.from.outside.the.UK.measures.Value)
eng_dta <- rename(eng_dta, n_outmig = Migrationethnic.group.Moved.out.of.the.area.measures.Value)

# usual residents one year before census
eng_dta <- eng_dta %>%
  mutate(n_usualres10 = n_samead + n_outmig + n_movedwithin)

# usual residents - midyear
eng_dta <- eng_dta %>% 
  mutate(midyrpop = (n_usualres10 + n_usualres11)/2)

# total inmigrants
eng_dta <- eng_dta %>%
  mutate(n_inmig = n_inmiguk + n_inmigfor)

# recalculate net migration below as (n_inmig - n_outmig) / midyrpop



#drop columns don't need
eng_dta <- eng_dta %>% 
  dplyr::select(1:3, contains(c("n_")), midyrpop) 

#alternative code - ignore, done in command lines 50/51
# p_load(data.table)
# eng_dta <- as.data.table(eng_dta)
# eng_dta <- eng_dta[, 1:10] #first arg = rows, second = cols, third = group by

head(eng_dta)

class(eng_dta$n_samead)

#trying to get loops to work - tabulate a list of variable 
for (i in c(3:10)) {
  i
  print(tabyl(eng_dta[, i]))
}

# or with variable names
varlist <- c("n_outmig", "n_usualres11", "n_inmig")
for (i in varlist) {
  print(tabyl(eng_dta[, i]))
}


# or with lapply
varlist <- c("n_outmig", "n_usualres11", "n_inmig")
lapply(eng_dta[varlist], tabyl)

  #Can also use tableone instead of tabyl for tabulations



# Describe size of OAs
summary(eng_dta$n_usualres11)
  # median = 303 residents, max = 4140 - exclude these? 


# calculate proportion of current residents who lived at the same address one year ago
eng_dta <- eng_dta %>% 
  mutate(prop_samead_1y = as.numeric(n_samead/n_usualres11*100))
summary(eng_dta$prop_samead_1y)
  # median = 90% of people in OA lived at same address a year ago

# check how many rows have very small values 
sum(eng_dta$prop_samead_1y < 50)
sum(!is.na(eng_dta$prop_samead_1y))
1477/171372
  # should these be removed from analysis? e.g. student halls


# Identify areas of net outmigration
  # these are areas where total migrants is smaller than outmigrants
eng_dta$net_outmig <- ifelse(eng_dta$n_outmig > eng_dta$n_inmig, 1, 0)

# Recreate Brown 2010 classification
# Identify areas of 10% + net increase (using mid year population as denominator)
eng_dta <- eng_dta %>% 
  mutate(net_migration = (n_inmig - n_outmig)/(midyrpop)*100)
summary(eng_dta$net_migration)  
  #median net migration = 0.41% (seems low compared with 16% turnover in previous year in Brown study for Scotland, but what area level did they use?)

sum(eng_dta$net_migration <= -10) #1730
sum(eng_dta$net_migration >= 10) #9220
9220/171372 #5%
1730/171372 #1% - not a very big category, reclassify to +/- 5% 

sum(eng_dta$net_migration <= -5) #13732
13732/171372 #8% 
  # use 5% cut-off for high in/out-migration areas (see other paper by Brown mentioned by Anne)


# Calculate turnover as total inmigrants + outmigrants divided by current residents
  # this includes people moving in from elsewhere in UK and abroad (but not within the area)
  # (churn would additionally include migration within)
  # Brown et al. exclude people who'd moved from abroad in one of their papers, do we want to do the same?
  

eng_dta <- eng_dta %>% 
  mutate(turnover = (n_inmig + n_outmig)/n_usualres11*100)
summary(eng_dta$turnover)
  #median turnover = 19% (similar to 16% turnover in previous year in Brown study for Scotland)


# Create variable with mobility categories
  # below does not work
eng_dta$mob_cat <- as.factor(ifelse(eng_dta$net_migration <= -5, 'Decreasing', 
                            ifelse(eng_dta$net_migration >= 5, 'Increasing',
                            ifelse(eng_dta$turnover >= 19, 'Stable, high turnover',
                            ifelse(eng_dta$turnover <19, 'Stable, low turnover', 'Error' )))))       
  
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= -5 ~ "Decreasing",
    net_migration >= 5 ~ "Increasing",
    net_migration > -5 & net_migration < 5 & turnover >= 19 ~ "Stable, high turnover",
    net_migration > -5 & net_migration < 5 & turnover <19 ~ "Stable, low turnover"
  ))
  
#Advice for recoding: 
  # stay away from factors, use as.character instead 
  # if want to use it as factor, transform it at the very end 
  # only use factor for ordered categorical, does some weird things for graphs etc. 
  # can write levels within as.factor so R knows the order in which categories should be displayed
  # if unordered categorical (e.g. region/religion), use character unless want them displayed in specific order


# Tab variable
tabyl(eng_dta$mob_cat)
  # Stable, low turnover accounts for 44% of all OAs in England for period 2010-11


#Save dataset 
s3write_using(eng_dta # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'eng_dta_OA.RDS' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above
