## Preparing area-level mobility data - MSOAs

# Housekeeping
# clear R environment
rm(list = ls())

#load packages
pacman::p_load(haven, 
               dplyr, 
               survey, 
               janitor,
               questionr, 
               epiDisplay, 
               epirhandbook,
               rio, 
               ggplot2, 
               apyramid,
               magrittr, 
               stringr, 
               here)

# set working directory (local/fixed pathway)
#NOTE: obsolete since project sets wd and using "here"
#setwd("M:/Analytics/Francesca/Mobility_data")
here()

# import all data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008
eng_dta <- s3read_using(import, 
                        object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/censusmig_MSOA_England.csv') # File to open 

dim(eng_dta)      #24 variables, 7201 observations
table(eng_dta$date)

# tidy variables names
names(eng_dta)<-str_replace_all(names(eng_dta), c(" " = "." , "," = "" , ";" = "" , "/" = "" , ":" = ""))
  #remove spaces so that we can refer to column names in functions

eng_dta <- rename(eng_dta, n_samead = Migrationethnic.group.Lived.at.same.address.one.year.ago.measures.Value)
eng_dta <- rename(eng_dta, n_usualres = Migrationethnic.group.All.usual.residents.measures.Value)
eng_dta <- rename(eng_dta, n_totalmig = Migrationethnic.group.Migrants.Total.measures.Value)
eng_dta <- rename(eng_dta, n_migwithin = Migrationethnic.group.Migrants.Moved.within.the.area.measures.Value)
eng_dta <- rename(eng_dta, n_miguk = Migrationethnic.group.Migrants.Moved.into.the.area.from.within.the.UK.measures.Value)
eng_dta <- rename(eng_dta, n_migfor = Migrationethnic.group.Migrants.Moved.into.the.area.from.outside.the.UK.measures.Value)
eng_dta <- rename(eng_dta, n_outmig = Migrationethnic.group.Moved.out.of.the.area.measures.Value)


#drop columns don't need
eng_dta <- eng_dta %>% 
  dplyr::select(1:10) 


# Describe size of MSOAs
summary(eng_dta$n_usualres)
  # median = 7616 residents, max = 16342

# calculate proportion of current residents who lived at the same address one year ago
eng_dta <- eng_dta %>% 
  mutate(prop_samead_1y = as.numeric(n_samead/n_usualres*100))
summary(eng_dta$prop_samead_1y)
  # median = 90% of people in MSOA lived at same address a year ago

# check how many rows have very small values 
sum(eng_dta$prop_samead_1y < 50)
sum(!is.na(eng_dta$prop_samead_1y))
32/7201
# should these be removed from analysis? e.g. student halls


# Identify areas of net outmigration
# these are areas where total migrants is smaller than outmigrants
eng_dta$net_outmig <- ifelse(eng_dta$n_outmig > eng_dta$n_totalmig, 1, 0)

# Recreate Brown 2010 classification
# Identify areas of 10% + net increase (using previous year's residents as denominator)
eng_dta <- eng_dta %>% 
  mutate(net_migration = (n_totalmig - n_outmig)/(n_usualres + n_outmig - n_totalmig)*100)
summary(eng_dta$net_migration)  
  #median net migration = 2.1%

sum(eng_dta$net_migration <= -10) #1
sum(eng_dta$net_migration >= 10) #254
  #small numbers of MSOAs

sum(eng_dta$net_migration <= -5) #3
3/7201 #0.04% 

sum(eng_dta$net_migration <= -1) #83
83/7201 #1.1% 

sum(eng_dta$net_migration <= -0.5) #198
198/7201 #2.7% 

sum(eng_dta$net_migration <= -0) #510
510/7201 #7.1% 
  # need to use different cut-off for declining areas for this area size

sum(eng_dta$net_migration >5) #989
989/7201 #13.7% MSOAs with >5% population growth in one year



# Calculate turnover as % of current residents who lived outside the area one year ago
  # this includes people moving in from elsewhere in UK and abroad (but not within the area)
  # Brown et al. exclude people who'd moved from abroad in one of their papers, do we want to do the same? 

eng_dta <- eng_dta %>% 
  mutate(turnover = (n_miguk + n_migfor)/n_usualres*100)
summary(eng_dta$turnover)
#median turnover = 8.3% 


# Create variable with mobility categories
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= 0 ~ "Decreasing",
    net_migration > 5 ~ "Increasing >5%",
    net_migration > 0 & net_migration <= 5 & turnover >= 8.3 ~ "Stable, high turnover",
    net_migration > -5 & net_migration <= 5 & turnover <8.3 ~ "Stable, low turnover"
  ))

tabyl(eng_dta$mob_cat)
# Stable, low turnover accounts for 42% of all MSOAs in England for period 2010-11


