## Preliminary maps - economic mobility data prep - by Local Authority District

  #NOTE: script S14 does the same analysis using MSOA-level data aggregated into LAs
              # 5 LAs had issues merging 
              # net migration estimates are identical to those obtained in script S14 for the merged LAs


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


# data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig006
        # download > local authorities: district / unitary (prior to April 2015)
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


# renaming columns for same address (n_samead), moved within the local authority (n_movedwithin), inmigrants (n_inmig) and outmigrants (n_outmig)
    # in each economic activity group
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


# Drop Scotland and Northern Ireland
ea_dta <- ea_dta %>%
  subset(str_detect(geography_code, '^E') | str_detect(geography_code, '^W'))

summary(ea_dta$ea_netmigration) #med = 1.016
summary(ea_dta$ei_netmigration) # med = 0.575
summary(ea_dta$ea_student_netmigration) # med = -6.958
summary(ea_dta$ei_student_netmigration) # med = -7.804
summary(ea_dta$student_netmigration) #-7.243


  # NOTE: changed cut-offs since including Wales gave us slightly different medians

ea_dta <- ea_dta %>%
  mutate(EA_mig = case_when(
    ea_netmigration <=1.016 & ei_netmigration <=0.575 & student_netmigration <=-7.243 ~ "Below median net migration - all groups",
    ea_netmigration >1.016 & ei_netmigration >0.575 &student_netmigration> -7.243 ~ "Above median net migration - all groups",
    ea_netmigration >1.016 & ei_netmigration <=0.575 &student_netmigration<= -7.243~ "Above median net migration - Economically active",
    ea_netmigration <=1.016 & ei_netmigration >0.575 &student_netmigration<= -7.243 ~ "Above median net migration - Economically inactive", 
    ea_netmigration <=1.016 & ei_netmigration <=0.575 &student_netmigration> -7.243 ~ "Above median net migration - Students",
    ea_netmigration <=1.016 & ei_netmigration >0.575 &student_netmigration> -7.243 ~ "Below median net migration - Economically active only",
    ea_netmigration >1.016 & ei_netmigration <=0.575 &student_netmigration> -7.243 ~ "Below median net migration - Economically inactive only",
    ea_netmigration >1.016 & ei_netmigration >0.575 &student_netmigration<= -7.243 ~ "Below median net migration - Students only"))

tabyl(ea_dta$EA_mig)



# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(ea_dta # R object to be saved
              , FUN = write.csv # R function used to save
              , object = 'ea_with_students_net_migration_LAD_v2.csv' # Name of file to save 
              , bucket = buck) # Bucket name defined above



# Calculate net migration by Levelling Up priority category
    #note: this analysis was not included in the final piece
# Import Levelling Up data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping' ## my bucket name

#data were downloaded from https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjyroCDndj6AhWGUcAKHVAtCNsQFnoECAsQAQ&url=https%3A%2F%2Fassets.publishing.service.gov.uk%2Fgovernment%2Fuploads%2Fsystem%2Fuploads%2Fattachment_data%2Ffile%2F966137%2FLevelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx&usg=AOvVaw3NIQ1EBTwu0jLNSisnn3XT 
levup <-s3read_using(import # Which function are we using to read
                     , object = 'data/Levelling_Up_priority_areas/Levelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx' # File to open
                     , bucket = buck) # Bucket name defined above


dim(levup)
# 368 LAs in file

# tidy column names
names(levup)<-str_replace_all(names(levup), c(" " = "." ))

tabyl(levup$Priority.category, show_missing_levels = T)



# merge data
ea_dta <- left_join(ea_dta, levup, by = c("geography" = "Name"))


# calculate net migration by age and by Levelling Up category
summary(subset(ea_dta, Priority.category == 1 & Country == 'England')$ea_netmigration)
summary(subset(ea_dta, Priority.category == 2 & Country == 'England')$ea_netmigration)
summary(subset(ea_dta, Priority.category == 3 & Country == 'England')$ea_netmigration)

summary(subset(ea_dta, Priority.category == 1 & Country == 'England')$ei_netmigration)
summary(subset(ea_dta, Priority.category == 2 & Country == 'England')$ei_netmigration)
summary(subset(ea_dta, Priority.category == 3 & Country == 'England')$ei_netmigration)

summary(subset(ea_dta, Priority.category == 1 & Country == 'England')$student_netmigration)
summary(subset(ea_dta, Priority.category == 2 & Country == 'England')$student_netmigration)
summary(subset(ea_dta, Priority.category == 3 & Country == 'England')$student_netmigration)


# export data by Levelling Up category
tibble_ea <- ea_dta %>%
  dplyr::filter(Country == 'England') %>%
  group_by(levelling_up_priority = Priority.category) %>%
  summarise(med_netmigration_ea = median(ea_netmigration),
            q1_ea = quantile(ea_netmigration, 0.25),
            q3_ea = quantile(ea_netmigration, 0.75),
            med_netmigration_ei = median(ei_netmigration), 
            q1_ei = quantile(ei_netmigration, 0.25),
            q3_ei = quantile(ei_netmigration, 0.75),
            med_netmigration_student = median(student_netmigration),
            q1_student = quantile(student_netmigration, 0.25),
            q3_student = quantile(student_netmigration, 0.75))
tibble_ea

s3write_using(tibble_ea # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'outputs/table_netmigration_ea_levellingup.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


# Calculate net migration by IMD 
    #note: this analysis was not included in the final piece
# Import IMD data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping' ## my bucket name

#data were downloaded from https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
imd <-s3read_using(import # Which function are we using to read
                   , object = 'data/IMD_LAD.xlsx' # File to open
                   , bucket = buck) # Bucket name defined above
# Source: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019


dim(imd)
# 317 LAs in file

# tidy column names
imd<-imd %>% 
  clean_names() 

imd <- imd %>%
  dplyr::select("local_authority_district_name_2019", "imd_rank_of_average_rank")

tabyl(imd$imd_rank_of_average_rank, show_missing_levels = T)

# Make a variable to identify top and bottom quintile of LAs
imd <- imd %>%
  mutate(quintile = cut(imd_rank_of_average_rank,
                        breaks = quantile(imd_rank_of_average_rank, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1, NA)),
                        labels = 1:5, na.rm = TRUE))

# merge data
ea_dta <- left_join(ea_dta, imd, by = c("geography" = "local_authority_district_name_2019"))



# calculate net migration by age for top and bottom IMD quintiles
summary(subset(ea_dta, quintile == 1 & Country == 'England')$ea_netmigration)
summary(subset(ea_dta, quintile == 5 & Country == 'England')$ea_netmigration)

summary(subset(ea_dta, quintile == 1 & Country == 'England')$ei_netmigration)
summary(subset(ea_dta, quintile == 5 & Country == 'England')$ei_netmigration)

summary(subset(ea_dta, quintile == 1 & Country == 'England')$student_netmigration)
summary(subset(ea_dta, quintile == 5 & Country == 'England')$student_netmigration)


# Export data by IMD quintile
tibble_ea2 <- ea_dta %>%
  dplyr::filter(str_detect(geography_code, '^E') ) %>%
  group_by(quintile = quintile) %>%
  summarise(med_netmigration_ea = median(ea_netmigration),
            q1_ea = quantile(ea_netmigration, 0.25),
            q3_ea = quantile(ea_netmigration, 0.75),
            med_netmigration_ei = median(ei_netmigration), 
            q1_ei = quantile(ei_netmigration, 0.25),
            q3_ei = quantile(ei_netmigration, 0.75),
            med_netmigration_student = median(student_netmigration),
            q1_student = quantile(student_netmigration, 0.25),
            q3_student = quantile(student_netmigration, 0.75))
tibble_ea2

s3write_using(tibble_ea2 # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'outputs/table_netmigration_ea_imd.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




# load packages
pacman::p_load(sf,
               XML,
               tmap,
               devtools, 
               viridis)

# import shp data
    #data were downloaded from https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(BDY_LAD%2CDEC_2011)
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shp',
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.cpg',
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.dbf',
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.prj',
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shx',
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.xml',
            file = here::here("shapefiles", "eng.xml"))

# read LAD boundaries
lad_shp <- st_read(here::here("shapefiles", "eng.shp"))

str(lad_shp)

# Drop Scotland and Northern Ireland
lad_shp <- lad_shp %>%
  subset(str_detect(lad11cd, '^E') | str_detect(lad11cd, '^W'))

# replace name for selected local authorities so they merge correctly
ea_dta <- ea_dta %>%
  mutate(geography = replace(geography, geography == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))
ea_dta <- ea_dta %>%
  mutate(geography = replace(geography, geography == "Folkestone and Hythe", "Shepway"))
ea_dta <- ea_dta %>%
  mutate(geography = replace(geography, geography == "Vale of Glamorgan", "The Vale of Glamorgan"))

# Join spatial data
lad_shp <- left_join(lad_shp, ea_dta, by = c("lad11nm" = "geography"))


# Map of economic activity residential mobility classification
map3_1 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "EA_mig", style = "cat", palette =  c('#53a9cd', '#2a7979', '#F39214', '#744284',  '#dd0031', '#ee9b90', '#0c402b', '#a6d7d3'), title = "") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map3_1
