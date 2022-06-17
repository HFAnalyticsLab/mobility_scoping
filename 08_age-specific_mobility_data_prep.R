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
               readr,
               matrixStats)

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
age_dta<-age_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions


  # naming second column x until figured out what it  means
names(age_dta)[4:10]<-c('n_samead', 'n_movedwithin' , 'n_inmig','x','y','z','n_outmig')
names(age_dta)[14:20]<-c('n_samead_0_4', 'n_movedwithin_0_4' , 'n_inmig_0_4','x_0_4', 'y_0_4','z_0_4','n_outmig_0_4')
names(age_dta)[24:30]<-c('n_samead_5_15', 'n_movedwithin_5_15' , 'n_inmig_5_15','x_5_15', 'y_5_15','z_5_15','n_outmig_5_15')
names(age_dta)[34:40]<-c('n_samead_16_19', 'n_movedwithin_16_19' , 'n_inmig_16_19','x_16_19','y_16_19', 'z_16_19','n_outmig_16_19')
names(age_dta)[44:50]<-c('n_samead_20_24', 'n_movedwithin_20_24' , 'n_inmig_20_24','x_20_24','y_20_24','z_20_24','n_outmig_20_24')
names(age_dta)[54:60]<-c('n_samead_25_34', 'n_movedwithin_25_34' , 'n_inmig_25_34','x_25_34', 'y_25_34','z_25_34','n_outmig_25_34')
names(age_dta)[64:70]<-c('n_samead_35_49', 'n_movedwithin_35_49' , 'n_inmig_35_49','x_35_49','y_35_49','z_35_49','n_outmig_35_49')
names(age_dta)[74:80]<-c('n_samead_50_64', 'n_movedwithin_50_64' , 'n_inmig_50_64','x_50_64','y_50_64','z_50_64','n_outmig_50_64')
names(age_dta)[84:90]<-c('n_samead_65_74', 'n_movedwithin_65_74' , 'n_inmig_65_74','x_65_74','y_65_74','z_65_74','n_outmig_65_74')
names(age_dta)[94:100]<-c('n_samead_75p', 'n_movedwithin_75p' , 'n_inmig_75p','x_75p','y_75p','z_75p','n_outmig_75p')

# Create variables for usual residents in 2010 and 2011
ages <- c("0_4", "5_15", "16_19", "20_24", "25_34", "35_49", "50_64", "65_74", "75p")

for (age in ages){
  age_dta <- age_dta %>%
    mutate( !!paste0("n_usualres10_", age) := !!as.name(paste0("n_samead_", age)) + !!as.name(paste0("n_outmig_", age)) + !!as.name(paste0("n_movedwithin_", age)))

  age_dta <- age_dta %>%
    mutate( !!paste0("n_usualres11_", age) := !!as.name(paste0("n_samead_", age)) + !!as.name(paste0("n_inmig_", age)) + !!as.name(paste0("n_movedwithin_", age)))
  
}  

age_dta <- age_dta %>%
  mutate(n_usualres10 = n_samead + n_outmig + n_movedwithin,
         n_usualres11 = n_samead + n_inmig + n_movedwithin)




# Create variables for mean age of those living in area a year ago, inmigrants and outmigrants
  # do samead + within in first category

age_dta <- age_dta %>%
  mutate(meanage_samead = (n_samead_0_4*2.5 + n_samead_5_15*10.5 + n_samead_16_19*18 + n_samead_20_24*22.5 + n_samead_25_34*30 + n_samead_35_49*42.5 + n_samead_50_64*57.5 + n_samead_65_74*70 + n_samead_75p*85)/ n_samead)

age_dta <- age_dta %>%
  mutate(meanage_inmig = (n_inmig_0_4*2.5 + n_inmig_5_15*10.5 + n_inmig_16_19*18 + n_inmig_20_24*22.5 + n_inmig_25_34*30 + n_inmig_35_49*42.5 + n_inmig_50_64*57.5 + n_inmig_65_74*70 + n_inmig_75p*85)/ n_inmig)

age_dta <- age_dta %>%
  mutate(meanage_outmig = (n_outmig_0_4*2.5 + n_outmig_5_15*10.5 + n_outmig_16_19*18 + n_outmig_20_24*22.5 + n_outmig_25_34*30 + n_outmig_35_49*42.5 + n_outmig_50_64*57.5 + n_outmig_65_74*70 + n_outmig_75p*85)/ n_outmig)

age_dta <- age_dta %>%
  mutate(meanage_diff = meanage_inmig-meanage_outmig)

age_dta <- age_dta %>%
  mutate(meanage_usualres11 = (n_usualres11_0_4*2.5 + n_usualres11_5_15*10.5 + n_usualres11_16_19*18 + n_usualres11_20_24*22.5 + n_usualres11_25_34*30 + n_usualres11_35_49*42.5 + n_usualres11_50_64*57.5 + n_usualres11_65_74*70 + n_usualres11_75p*85)/ n_usualres11)

age_dta <- age_dta %>%
  mutate(meanage_usualres10 = (n_usualres10_0_4*2.5 + n_usualres10_5_15*10.5 + n_usualres10_16_19*18 + n_usualres10_20_24*22.5 + n_usualres10_25_34*30 + n_usualres10_35_49*42.5 + n_usualres10_50_64*57.5 + n_usualres10_65_74*70 + n_usualres10_75p*85)/ n_usualres10)

age_dta <- age_dta %>%
  mutate(diff_2010_11 = meanage_usualres11-meanage_usualres10)

age_dta <- age_dta %>%
  mutate(diff_2010_11_cat = case_when(
    diff_2010_11 < -0.5 ~ "< -0.5",
    diff_2010_11 >= -0.5 & diff_2010_11 < 0.5 ~ "No change",
    diff_2010_11 > 0.5 ~ ">0.5"
    ))
tabyl(age_dta$diff_2010_11_cat, show_na = T)


# tab data
age_dta %>%
  summarise(min(meanage_samead), mean(meanage_samead), max(meanage_samead))

age_dta %>%
  summarise(min(meanage_inmig), mean(meanage_inmig), max(meanage_inmig))

age_dta %>%
  summarise(min(meanage_outmig), mean(meanage_outmig), max(meanage_outmig))

age_dta %>%
  summarise(min(meanage_diff), mean(meanage_diff), max(meanage_diff))

age_dta %>%
  summarise(min(meanage_usualres11), mean(meanage_usualres11), max(meanage_usualres11))

age_dta %>%
  summarise(min(meanage_usualres10), mean(meanage_usualres10), max(meanage_usualres10))

age_dta %>%
  summarise(min(diff_2010_11), mean(diff_2010_11), max(diff_2010_11))



# calculate medians
# create empty variable
age_dta <- age_dta %>% 
  mutate(median_cat="")

#calculate cutoff for median
  age_dta <- age_dta %>%
    mutate(cutoff = n_samead/2)
  
# create counter variable (0 to start with)
  age_dta <- age_dta %>%
    mutate(counter = 0)


ages <- c("0_4", "5_15", "16_19", "20_24", "25_34", "35_49", "50_64", "65_74", "75p")

for (age in ages){
  age_dta <- age_dta %>%
    mutate(counter = counter +  !!as.name(paste0("n_samead_", age)))
  age_dta <- age_dta %>%
    mutate(median_cat = replace(median_cat, cutoff <= counter & median_cat == "", age)) 
  }

tabyl(age_dta$median_cat)

age_dta %>% dplyr::select(contains("n_samead"), cutoff, median_cat)





#Save dataset 

buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(age_dta # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'eng_age_dta_MSOA.RDS' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

