## Preparing area-level mobility data, breakdown by age - MSOAs

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
               here, 
               aws.s3,
               readr)

# set working directory (local/fixed pathway)
#NOTE: obsolete since project sets wd and using "here"
#setwd("M:/Analytics/Francesca/Mobility_data")
here()

# import all data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008
age_dta <- s3read_using(import, 
                        object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/migration_age_sex.csv') # File to open 

dim(age_dta)      #303 variables, 7201 observations

# tidy variables names
names(age_dta)<-str_replace_all(names(age_dta), c(" " = "." , "," = "" , ";" = "" , "/" = "" , ":" = ""))
#remove spaces so that we can refer to column names in functions]


  # naming second column x until figured out what it  means
names(age_dta)[4:10]<-c('n_samead', 'x' , 'n_totalmig','n_migwithin','n_miguk','n_migfor','n_outmig')
names(age_dta)[14:20]<-c('n_samead_0_4', 'x_0_4' , 'n_totalmig_0_4','n_migwithin_0_4','n_miguk_0_4','n_migfor_0_4','n_outmig_0_4')
names(age_dta)[24:30]<-c('n_samead_5_15', 'x_5_15' , 'n_totalmig_5_15','n_migwithin_5_15','n_miguk_5_15','n_migfor_5_15','n_outmig_5_15')
names(age_dta)[34:40]<-c('n_samead_16_19', 'x_16_19' , 'n_totalmig_16_19','n_migwithin_16_19','n_miguk_16_19','n_migfor_16_19','n_outmig_16_19')
names(age_dta)[44:50]<-c('n_samead_20_24', 'x_20_24' , 'n_totalmig_20_24','n_migwithin_20_24','n_miguk_20_24','n_migfor_20_24','n_outmig_20_24')
names(age_dta)[54:60]<-c('n_samead_25_34', 'x_25_34' , 'n_totalmig_25_34','n_migwithin_25_34','n_miguk_25_34','n_migfor_25_34','n_outmig_25_34')
names(age_dta)[64:70]<-c('n_samead_35_49', 'x_35_49' , 'n_totalmig_35_49','n_migwithin_35_49','n_miguk_35_49','n_migfor_35_49','n_outmig_35_49')
names(age_dta)[74:80]<-c('n_samead_50_64', 'x_50_64' , 'n_totalmig_50_64','n_migwithin_50_64','n_miguk_50_64','n_migfor_50_64','n_outmig_50_64')
names(age_dta)[84:90]<-c('n_samead_65_74', 'x_65_74' , 'n_totalmig_65_74','n_migwithin_65_74','n_miguk_65_74','n_migfor_65_74','n_outmig_65_74')
names(age_dta)[94:100]<-c('n_samead_75p', 'x_75p' , 'n_totalmig_75p','n_migwithin_75p','n_miguk_75p','n_migfor_75p','n_outmig_75p')


# Create variables for mean age of those living in area a year ago, inmigrants and outmigrants
age_dta <- age_dta %>%
  mutate(meanage_samead = (n_samead_0_4*2.5 + n_samead_5_15*10.5 + n_samead_16_19*18 + n_samead_20_24*22.5 + n_samead_25_34*30 + n_samead_35_49*42.5 + n_samead_50_64*57.5 + n_samead_65_74*70 + n_samead_75p*85)/ n_samead)

age_dta <- age_dta %>%
  mutate(meanage_totalmig = (n_totalmig_0_4*2.5 + n_totalmig_5_15*10.5 + n_totalmig_16_19*18 + n_totalmig_20_24*22.5 + n_totalmig_25_34*30 + n_totalmig_35_49*42.5 + n_totalmig_50_64*57.5 + n_totalmig_65_74*70 + n_totalmig_75p*85)/ n_totalmig)

age_dta <- age_dta %>%
  mutate(meanage_outmig = (n_outmig_0_4*2.5 + n_outmig_5_15*10.5 + n_outmig_16_19*18 + n_outmig_20_24*22.5 + n_outmig_25_34*30 + n_outmig_35_49*42.5 + n_outmig_50_64*57.5 + n_outmig_65_74*70 + n_outmig_75p*85)/ n_outmig)

age_dta <- age_dta %>%
  mutate(meanage_diff = meanage_totalmig-meanage_outmig)




# tab data
age_dta %>%
  summarise(min(meanage_samead), mean(meanage_samead), max(meanage_samead))

age_dta %>%
  summarise(min(meanage_totalmig), mean(meanage_totalmig), max(meanage_totalmig))

age_dta %>%
  summarise(min(meanage_outmig), mean(meanage_outmig), max(meanage_outmig))

age_dta %>%
  summarise(min(meanage_diff), mean(meanage_diff), max(meanage_diff))


#Save dataset 

buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(age_dta # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'eng_age_dta_MSOA.RDS' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

