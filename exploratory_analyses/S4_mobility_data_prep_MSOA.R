## Preparing area-level mobility data - MSOAs

# Housekeeping
# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

#load packages
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
               here)

# set working directory (local/fixed pathway)
#NOTE: obsolete since project sets wd and using "here"
#setwd("M:/Analytics/Francesca/Mobility_data")
here()

# import all data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008
eng_dta <- s3read_using(import, 
                        object = 'censusmig_MSOA_England.csv', 
                        bucket = buck_data) # File to open 

dim(eng_dta)      #24 variables, 7201 observations
table(eng_dta$date)

# tidy variables names
names(eng_dta)<-str_replace_all(names(eng_dta), c(" " = "." , "," = "" , ";" = "" , "/" = "" , ":" = ""))
  #remove spaces so that we can refer to column names in functions]

# eng_dta<-eng_dta %>%
#   clean_names() %>% 
#   select(1:10) 

 # names(eng_dta)[4:10]<-c('n_usualres11','n_samead', 'n_totalmig','n_migwithin','n_inmiguk','n_inmigfor','n_outmig')

eng_dta <- dplyr::rename(eng_dta, n_samead = Migrationethnic.group.Lived.at.same.address.one.year.ago.measures.Value)
eng_dta <- dplyr::rename(eng_dta, n_usualres11 = Migrationethnic.group.All.usual.residents.measures.Value)
eng_dta <- dplyr::rename(eng_dta, n_movedwithin = Migrationethnic.group.Migrants.Moved.within.the.area.measures.Value)
eng_dta <- dplyr::rename(eng_dta, n_inmiguk = Migrationethnic.group.Migrants.Moved.into.the.area.from.within.the.UK.measures.Value)
eng_dta <- dplyr::rename(eng_dta, n_inmigfor = Migrationethnic.group.Migrants.Moved.into.the.area.from.outside.the.UK.measures.Value)
eng_dta <- dplyr::rename(eng_dta, n_outmig = Migrationethnic.group.Moved.out.of.the.area.measures.Value)


#drop columns don't need
eng_dta <- eng_dta %>% 
  dplyr::select(1:10) 

# usual residents one year before census
eng_dta <- eng_dta %>%
  mutate(n_usualres10 = n_samead + n_outmig + n_movedwithin)

# usual residents - midyear
eng_dta <- eng_dta %>% 
  mutate(midyrpop = (n_usualres10 + n_usualres11)/2)

# total inmigrants
eng_dta <- eng_dta %>%
  mutate(n_inmig = n_inmiguk + n_inmigfor)


# Describe size of MSOAs
summary(eng_dta$n_usualres11)
  # median = 7616 residents, max = 16342

# calculate proportion of current residents who lived at the same address one year ago
eng_dta <- eng_dta %>% 
  mutate(prop_samead_1y = as.numeric(n_samead/n_usualres11*100))
summary(eng_dta$prop_samead_1y)
  # median = 90% of people in MSOA lived at same address a year ago

# check how many rows have very small values 
sum(eng_dta$prop_samead_1y < 50)
sum(!is.na(eng_dta$prop_samead_1y))
32/7201
# should these be removed from analysis? e.g. student halls


# Identify areas of net outmigration
# these are areas where total migrants is smaller than outmigrants
eng_dta$net_outmig <- ifelse(eng_dta$n_outmig > eng_dta$n_inmig, 1, 0)

# Recreate Brown 2010 classification
# Identify areas of 10% + net increase (using previous year's residents as denominator)
eng_dta <- eng_dta %>% 
  mutate(net_migration = (n_inmig - n_outmig)/(midyrpop)*100)
summary(eng_dta$net_migration)  
#median net migration = 0.44%

sum(eng_dta$net_migration <= -10) #1
sum(eng_dta$net_migration >= 10) #106
  #small numbers of MSOAs

sum(eng_dta$net_migration <= -5) #9
9/7201 #0.12% 

sum(eng_dta$net_migration <= -1) #593
593/7201 #8.2% 

  # need to use different cut-off for declining areas for this area size

sum(eng_dta$net_migration >5) #399
399/7201 #5.5% MSOAs with >5% population growth in one year



# Calculate turnover as total inmigrants + outmigrants divided by current residents
# this includes people moving in from elsewhere in UK and abroad (but not within the area)
# (churn would additionally include migration within)
# Brown et al. exclude people who'd moved from abroad in one of their papers, do we want to do the same?

eng_dta <- eng_dta %>% 
  mutate(turnover = (n_inmig + n_outmig)/n_usualres11*100)
summary(eng_dta$turnover)
#median turnover = 16.3%


# Create variable with mobility categories
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= -1 ~ "Decreasing <-1%",
    net_migration > 5 ~ "Increasing >5%",
    net_migration > -1 & net_migration <= 5 & turnover >= 16.3 ~ "Stable, high turnover",
    net_migration > -1 & net_migration <= 5 & turnover <16.3 ~ "Stable, low turnover"
  ))

tabyl(eng_dta$mob_cat, show_missing_levels = T)
# Stable, low turnover accounts for 44% of all MSOAs in England for period 2010-11
sum(is.na(eng_dta$mob_cat))

#Save dataset 
s3write_using(eng_dta # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'eng_dta_MSOA.RDS' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above

