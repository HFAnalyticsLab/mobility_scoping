## Preliminary maps - economic mobility data prep - LAD level

  #NOTE: FC made this one based on AA's script 16 using the LA-level data download directly
              # (rather than lookup table to aggregate MSOAs into LAs)
              # this was to ensure comparability with age and health re: 5 LAs which had problems merging
              # net migration estimates are identical to those obtained in script 16 for the merged LAs


# Housekeeping
# clear R environment
rm(list = ls())


#load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle,
               viridis, 
               wesanderson,
               aws.s3,
               tidyverse,
               rio,
               janitor,
               ggmap)


# import all data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig006
EA_dta <- s3read_using(import, 
                       object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/censusmig_economicactivity_LAD.csv') # File to open 

dim(EA_dta)      #183 variables, 406 observations

# tidy variables names
EA_dta<-EA_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions

colnames(EA_dta)

clean_dta<-EA_dta %>% 
  dplyr::select(!contains (c("and_full_time_student", "including_full_time_students", 
                             "all_categories", "total_migration"))) %>% 
  dplyr::select(contains(c("date", "geography", "geography_code", "same_address", "inflow_total_measures","outflow_total_measures", "within_same_area_measures"))) 

grps<-c("ea_full_time_employment", "ea_part_time_eployment", "ea_unemployed", "ei_retired", "ei_looking_after", "ei_lt_sick_disabled", "ei_other")


names(clean_dta)[4:10]<-paste0(grps,"_n_samead")
names(clean_dta)[11:17]<-paste0(grps,"_n_inmig")
names(clean_dta)[18:24]<-paste0(grps,"_n_outmig")
names(clean_dta)[25:31]<-paste0(grps,"_n_movedwithin")

clean_dta<-clean_dta %>% 
  mutate(ea_n_samead=(ea_full_time_employment_n_samead+ea_part_time_eployment_n_samead+ea_unemployed_n_samead),
         ei_n_samead=(ei_retired_n_samead+ei_looking_after_n_samead+ei_lt_sick_disabled_n_samead+ei_other_n_samead),
         ea_n_inmig=(ea_full_time_employment_n_inmig+ea_part_time_eployment_n_inmig+ea_unemployed_n_inmig),
         ei_n_inmig=(ei_retired_n_inmig+ei_looking_after_n_inmig+ei_lt_sick_disabled_n_inmig+ei_other_n_inmig),
         ea_n_outmig=(ea_full_time_employment_n_outmig+ea_part_time_eployment_n_outmig+ea_unemployed_n_outmig),
         ei_n_outmig=(ei_retired_n_outmig+ei_looking_after_n_outmig+ei_lt_sick_disabled_n_outmig+ei_other_n_outmig),
         ea_n_movedwithin=(ea_full_time_employment_n_movedwithin+ea_part_time_eployment_n_movedwithin+ea_unemployed_n_movedwithin),
         ei_n_movedwithin=(ei_retired_n_movedwithin+ei_looking_after_n_movedwithin+ei_lt_sick_disabled_n_movedwithin+ei_other_n_movedwithin)) %>% 
  dplyr::select(c("date", "geography", "geography_code",ea_n_samead:ei_n_movedwithin))

cats <- c("ea_", "ei_")

for (cat in cats){
  clean_dta <- clean_dta %>%
    mutate( !!paste0(cat,"n_usualres10") := !!as.name(paste0(cat, "n_samead")) + !!as.name(paste0(cat,"n_outmig")) + !!as.name(paste0(cat,"n_movedwithin"))) %>%
    mutate( !!paste0(cat,"n_usualres11") := !!as.name(paste0(cat,"n_samead")) + !!as.name(paste0(cat,"n_inmig")) + !!as.name(paste0(cat,"n_movedwithin"))) %>% 
    mutate( !!paste0(cat,"n_midyrpop") := (!!as.name(paste0(cat,"n_usualres11")) + !!as.name(paste0(cat,"n_usualres10"))) /2 ) %>%
    mutate( !!paste0(cat,"netmigration") := (!!as.name(paste0(cat,"n_inmig")) - !!as.name(paste0(cat,"n_outmig"))) / !!as.name(paste0(cat,"n_midyrpop")) * 100)
  
} 

students_dta<-EA_dta %>% 
  dplyr::select(contains (c("date", "geography", "geography_code", "and_full_time_student", "including_full_time_students"))) %>% 
  dplyr::select(contains(c("date", "geography", "geography_code", "same_address", "inflow_total_measures","outflow_total_measures", "within_same_area_measures"))) 

grps<-c("ea_full_time_employment_student", "ea_part_time_employment_student", "ea_unemployed_student", "ei_student")


names(students_dta)[4:7]<-paste0(grps,"_n_samead")
names(students_dta)[8:11]<-paste0(grps,"_n_inmig")
names(students_dta)[12:15]<-paste0(grps,"_n_outmig")
names(students_dta)[16:19]<-paste0(grps,"_n_movedwithin")

students_dta<-students_dta %>% 
  mutate(ea_student_n_samead=ea_full_time_employment_student_n_samead+
           ea_part_time_employment_student_n_samead+ea_unemployed_student_n_samead,
         ea_student_n_inmig=ea_full_time_employment_student_n_inmig+
           ea_part_time_employment_student_n_inmig+ea_unemployed_student_n_inmig,
         ea_student_n_outmig=ea_full_time_employment_student_n_outmig+
           ea_part_time_employment_student_n_outmig+ea_unemployed_student_n_outmig,
         ea_student_n_movedwithin=ea_full_time_employment_student_n_movedwithin+
           ea_part_time_employment_student_n_movedwithin+ea_unemployed_student_n_movedwithin)

cats <- c("ea_student_", "ei_student_")

for (cat in cats){
  students_dta <- students_dta %>%
    mutate( !!paste0(cat,"n_usualres10") := !!as.name(paste0(cat, "n_samead")) + !!as.name(paste0(cat,"n_outmig")) + !!as.name(paste0(cat,"n_movedwithin"))) %>%
    mutate( !!paste0(cat,"n_usualres11") := !!as.name(paste0(cat,"n_samead")) + !!as.name(paste0(cat,"n_inmig")) + !!as.name(paste0(cat,"n_movedwithin"))) %>% 
    mutate( !!paste0(cat,"n_midyrpop") := (!!as.name(paste0(cat,"n_usualres11")) + !!as.name(paste0(cat,"n_usualres10"))) /2 ) %>%
    mutate( !!paste0(cat,"netmigration") := (!!as.name(paste0(cat,"n_inmig")) - !!as.name(paste0(cat,"n_outmig"))) / !!as.name(paste0(cat,"n_midyrpop")) * 100)
  
} 

students_dta<-students_dta %>% 
  mutate(student_n_samead=ei_student_n_samead+ea_student_n_samead,
         student_n_inmig=ei_student_n_inmig+ea_student_n_inmig,
         student_n_outmig=ei_student_n_outmig+ea_student_n_outmig,
         student_n_movedwithin=ei_student_n_movedwithin+ea_student_n_movedwithin,
         student_n_usualres10=student_n_samead+student_n_movedwithin+student_n_outmig,
         student_n_usualres11=student_n_samead+student_n_inmig+student_n_movedwithin,
         student_n_midyearpop=(student_n_usualres10+student_n_usualres11)/2,
         student_netmigration=((student_n_inmig-student_n_outmig)/student_n_midyearpop)*100)


ea_dta<-clean_dta %>% 
  left_join(students_dta, by=c("date", "geography", "geography_code"))

ea_dta<-ea_dta %>% 
  dplyr::select(contains (c("geography", "geography_code", "netmigration")))

# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(ea_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'ea_with_students_net_migration_LAD_v2.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above



