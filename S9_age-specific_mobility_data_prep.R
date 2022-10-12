## Preparing area-level mobility data, breakdown by age - MSOAs

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
               here, 
               aws.s3,
               readr,
               matrixStats)

here()

# import all data
    ## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008
age_dta <- s3read_using(import, 
                        object = 'migration_age_sex.csv', 
                        bucket = buck_data)  

dim(age_dta)      #303 variables, 7201 observations

# tidy variables names
age_dta<-age_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions


  # naming unneeded colums x, y, z
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

# samead
# create empty variable
age_dta <- age_dta %>% 
  mutate(median_samead="")

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
    mutate(median_samead = replace(median_samead, cutoff <= counter & median_samead == "", age)) 
  }

tabyl(age_dta$median_samead)

#view results
age_dta %>% dplyr::select(contains("n_samead"), cutoff, median_samead)


# inmig
# create empty variable
age_dta <- age_dta %>% 
  mutate(median_inmig="")

#calculate cutoff for median
age_dta <- age_dta %>%
  mutate(cutoff = n_inmig/2)

# create counter variable (0 to start with)
age_dta <- age_dta %>%
  mutate(counter = 0)


ages <- c("0_4", "5_15", "16_19", "20_24", "25_34", "35_49", "50_64", "65_74", "75p")

for (age in ages){
  age_dta <- age_dta %>%
    mutate(counter = counter +  !!as.name(paste0("n_inmig_", age)))
  age_dta <- age_dta %>%
    mutate(median_inmig = replace(median_inmig, cutoff <= counter & median_inmig == "", age)) 
}

tabyl(age_dta$median_inmig)


# outmig
# create empty variable
age_dta <- age_dta %>% 
  mutate(median_outmig="")

#calculate cutoff for median
age_dta <- age_dta %>%
  mutate(cutoff = n_outmig/2)

# create counter variable (0 to start with)
age_dta <- age_dta %>%
  mutate(counter = 0)


ages <- c("0_4", "5_15", "16_19", "20_24", "25_34", "35_49", "50_64", "65_74", "75p")

for (age in ages){
  age_dta <- age_dta %>%
    mutate(counter = counter +  !!as.name(paste0("n_outmig_", age)))
  age_dta <- age_dta %>%
    mutate(median_outmig = replace(median_outmig, cutoff <= counter & median_outmig == "", age)) 
}

tabyl(age_dta$median_outmig)


# median_diff
age_dta <- age_dta %>%
  mutate(median_diff = case_when(median_inmig == median_outmig ~ "same median age",
                                 median_inmig == '16_19' & (median_outmig == '20_24' | median_outmig == '25_34') ~ "lower median inmig age",
                                 median_inmig == '20_24' & median_outmig == '25_34' ~ "lower median inmig age",
                                 median_inmig == '25_34' & median_outmig == '35_49' ~ "lower median inmig age",
                                 median_outmig == '16_19' & (median_inmig == '20_24' | median_inmig == '25_34') ~ "higher median inmig age",
                                 median_outmig == '20_24' & (median_inmig == '25_34' | median_inmig == '35_49') ~ "higher median inmig age",
                                 median_outmig == '25_34' & (median_inmig == '35_49' | median_inmig == '50_64') ~ "higher median inmig age",
                                 median_outmig == '35_49' & median_inmig == '50_64' ~ "higher median inmig age"
                                 )
  )
tabyl(age_dta$median_diff, show_na = T)
#view results
age_dta %>% dplyr::select(median_inmig, median_outmig, median_diff)



# usual residents 2011
# create empty variable
age_dta <- age_dta %>% 
  mutate(median_usualres11="")

#calculate cutoff for median
age_dta <- age_dta %>%
  mutate(cutoff = n_usualres11/2)

# create counter variable (0 to start with)
age_dta <- age_dta %>%
  mutate(counter = 0)


ages <- c("0_4", "5_15", "16_19", "20_24", "25_34", "35_49", "50_64", "65_74", "75p")

for (age in ages){
  age_dta <- age_dta %>%
    mutate(counter = counter +  !!as.name(paste0("n_usualres11_", age)))
  age_dta <- age_dta %>%
    mutate(median_usualres11 = replace(median_usualres11, cutoff <= counter & median_usualres11 == "", age)) 
}

tabyl(age_dta$median_usualres11)


# usual residents 2010
# create empty variable
age_dta <- age_dta %>% 
  mutate(median_usualres10="")

#calculate cutoff for median
age_dta <- age_dta %>%
  mutate(cutoff = n_usualres10/2)

# create counter variable (0 to start with)
age_dta <- age_dta %>%
  mutate(counter = 0)


ages <- c("0_4", "5_15", "16_19", "20_24", "25_34", "35_49", "50_64", "65_74", "75p")

for (age in ages){
  age_dta <- age_dta %>%
    mutate(counter = counter +  !!as.name(paste0("n_usualres10_", age)))
  age_dta <- age_dta %>%
    mutate(median_usualres10 = replace(median_usualres10, cutoff <= counter & median_usualres10 == "", age)) 
}

tabyl(age_dta$median_usualres10)

tabyl(age_dta, median_usualres11, median_usualres10)


# median_diff_1011
age_dta <- age_dta %>%
  mutate(median_diff_1011 = case_when(median_usualres10 == median_usualres11 ~ "same median age",
                                      median_usualres10 == '16_19' & (median_usualres11 == '20_24' | median_usualres11 == '25_34') ~ "lower median 2010 age",
                                      median_usualres10 == '20_24' & median_usualres11 == '25_34' ~ "lower median 2010 age",
                                      median_usualres10 == '25_34' & median_usualres11 == '35_49' ~ "lower median 2010 age", 
                                      median_usualres10 == '35_49' & median_usualres11 == '50_64' ~ "lower median 2010 age",
                                      median_usualres11 == '16_19' & (median_usualres10 == '20_24' | median_usualres10 == '25_34') ~ "higher median 2010 age",
                                      median_usualres11 == '20_24' & (median_usualres10 == '25_34' | median_usualres10 == '35_49') ~ "higher median 2010 age",
                                      median_usualres11 == '25_34' & (median_usualres10 == '35_49' | median_usualres10 == '50_64') ~ "higher median 2010 age",
                                      median_usualres11 == '35_49' & median_usualres10 == '50_64' ~ "higher median 2010 age"
  )
  )
tabyl(age_dta$median_diff_1011, show_na = T)
#view results
age_dta %>% dplyr::select(median_usualres10, median_usualres11, median_diff_1011)



#Save dataset 
s3write_using(age_dta # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'eng_age_dta_MSOA.RDS' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above

