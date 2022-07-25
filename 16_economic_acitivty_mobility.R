## Preliminary maps - economic mobility data prep - Middle Layer Super Output Area level
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
                        object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/msoa_migration_by_economic_activity.csv') # File to open 

dim(EA_dta)      #183 variables, 7201 observations


#load shp files 
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp',
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.cpg',
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.dbf',
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.prj',
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx',
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.xml',
            file = here::here("shapefiles", "eng.xml"))

# load shp data
msoa_shp <- st_read(here::here("shapefiles", "eng.shp"))



# tidy variables names
EA_dta<-EA_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions

colnames(EA_dta)

economic_groups<-c("economically_inactive","economically_active")

clean_dta<-EA_dta %>% 
  dplyr::select(c("date", "geography", "geography_code", contains(paste0(economic_groups,"_total_migration")))) %>% 
  dplyr::select(contains(c("date", "geography", "geography_code", "same_address", "inflow_total_measures","outflow_total_measures", "within_same_area_measures"))) 

names(clean_dta)[c(4,6,8,10)]<-paste0("economically_inactive_",c('n_samead','n_inmig','n_outmig', 'n_movedwithin'))
names(clean_dta)[c(5,7,9,11)]<-paste0("economically_active_",c('n_samead','n_inmig','n_outmig', 'n_movedwithin'))

clean_dta<-clean_dta %>%
   mutate(economically_inactive_usualres11=(economically_inactive_n_samead+
                                                          economically_inactive_n_movedwithin+economically_inactive_n_inmig),
          economically_active_usualres11=(economically_active_n_samead+
                                                  economically_active_n_movedwithin+economically_active_n_inmig)) %>% 
   mutate(economically_inactive_usualres10=(economically_inactive_n_samead+
                                                 economically_inactive_n_movedwithin+economically_inactive_n_outmig),
         economically_active_usualres10=(economically_active_n_samead+
                                                       economically_active_n_movedwithin+ economically_active_n_outmig)) %>% 
             mutate(economically_inactive_midyearpop=((economically_inactive_usualres10+economically_inactive_usualres11)/2),
                    economically_active_midyearpop=((economically_active_usualres10+economically_active_usualres11)/2)) %>% 
  mutate(economically_inactive_netmigration=(economically_inactive_n_inmig-economically_inactive_n_outmig)/(economically_inactive_midyearpop)*100,
         economically_active_netmigration=(economically_active_n_inmig-economically_active_n_outmig)/(economically_active_midyearpop)*100)

buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(clean_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'economic_net_migration.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

clean_dta<-clean_dta %>% 
  dplyr::select(c("date", "geography", "geography_code",contains(c("netmigration"))))


ea_dta_plot<-clean_dta %>% 
  dplyr::select(contains("netmigration")) %>% 
  pivot_longer(everything(), names_to="metric", values_to="net_migration")

t<-boxplot(net_migration~metric,data=ea_dta_plot, main="Net Migration by Economic activity",
           xlab="economic activity", ylab="Net Migration")

#  a lot are close to 0 so should do the 1% again

clean_dta<-clean_dta %>% 
  mutate(economically_inactive_netmigration_lab=case_when(economically_inactive_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                                          economically_inactive_netmigration > 1 ~ "Positive (greater than 1% of people moving in)", 
                                             TRUE ~ "stable migration (~1% of population moving in and out)"),
         economically_active_netmigration_lab=case_when(economically_active_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                                          economically_active_netmigration > 1 ~ "Positive (greater than 1% of people moving in)", 
                                                          TRUE ~ "stable migration (~1% of population moving in and out)"))



# Join spatial data
msoa_shp <- left_join(msoa_shp,clean_dta , by = c("MSOA11CD" = "geography_code"))
# geography.code is the MSOA code in the eng_dta df and MSOA11CD the code in the shapefile data


# id for country name initial
# creates a variable with E for England, W for Wales etc. 
msoa_shp$ctry_nm <- substr(msoa_shp$MSOA11CD, 1, 1)
msoa_shp$ctry_nm <- as.factor(msoa_shp$ctry_nm)


msoa_shp<-msoa_shp %>% 
  filter(ctry_nm=="E")

# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does

#For saving maps 
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/outputs' ## my bucket name



map13_1 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "economically_inactive_netmigration_lab", palette = "viridis", title = "Economically inactive Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
# +
#   tm_text(text="MSOA11NM")

map13_1

filepath<- here::here('outputs', "map13_1.png")

tmap_save(map13_1,filepath)

map13_2 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "economically_active_netmigration_lab", palette = "viridis", title = "Economically active Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
# +
#   tm_text(text="MSOA11NM")

map13_2

filepath<- here::here('outputs', "map13_2.png")

tmap_save(map13_2,filepath)

ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 


map13_3<- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "economically_inactive_netmigration_lab", palette = "viridis", title = "Economically inactive Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
# +
#   tm_text(text="MSOA11NM")

map13_3

filepath<- here::here('outputs', "map13_3.png")

tmap_save(map13_3,filepath)

map13_4 <- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "economically_active_netmigration_lab", palette = "viridis", title = "Economically active Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)


map13_4

filepath<- here::here('outputs', "map13_4.png")

tmap_save(map13_4,filepath)


# Excluded students -------------------------------------------------------

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
  dplyr::select(c("date", "geography", "geography_code", contains(c("ei_student", "ea_student"))))


ea_dta<-clean_dta %>% 
    left_join(students_dta, by=c("date", "geography", "geography_code"))


buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(ea_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'ea_with_students_net_migration.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


ea_dta_plot<-ea_dta %>% 
  dplyr::select(contains("netmigration")) %>% 
  pivot_longer(everything(), names_to="metric", values_to="net_migration")

t<-boxplot(net_migration~metric,data=ea_dta_plot, main="Net Migration by economic activity",
           xlab="Economic activity", ylab="Net Migration")

#a lot less movement if you exclude students

# recode those <=1% as stable
ea_dta<-ea_dta %>% 
  mutate(netmigration_ea_lab=case_when(ea_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                       ea_netmigration > 1 ~ "Positive (greater than 1% of people moving in)", 
                                           TRUE ~ "stable migration (~1% of population moving in and out)"),
         netmigration_ea_student_lab=case_when(ea_student_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                               ea_student_netmigration> 1 ~ "Positive (greater than 1% of people moving in)", 
                                           TRUE ~ "stable migration (~1% of population moving in and out)"),
         netmigration_ei_lab=case_when(ei_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                       ei_netmigration > 1 ~ "Positive (greather than 1% of people moving in)",
                                           TRUE ~ "stable migration (~1% of population moving in and out)"),
         netmigration_ei_student_lab=case_when(ei_student_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                               ei_student_netmigration > 1 ~ "Positive (greather than 1% of people moving in)",
                                       TRUE ~ "stable migration (~1% of population moving in and out)"))




# Join spatial data
msoa_shp <- left_join(msoa_shp,ea_dta , by = c("MSOA11CD" = "geography_code"))
# geography.code is the MSOA code in the eng_dta df and MSOA11CD the code in the shapefile data


#For saving maps 
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/outputs' ## my bucket name


map13_1 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ea_lab", palette = "viridis", title = "Economically active (excl. students) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_1

map13_2 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ea_student_lab", palette = "viridis", title = "Economically active (students only) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_2

map13_3 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ei_lab", palette = "viridis", title = "Economically inactive (excl. students) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_3

map13_4 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ei_student_lab", palette = "viridis", title = "Economically inactive (students only) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_4


# London maps
ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 


map13_5 <- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ea_lab", palette = "viridis", title = "Economically active (excl. students) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_5

map13_6 <- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ea_student_lab", palette = "viridis", title = "Economically active (students only) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_6


map13_7 <- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ei_lab", palette = "viridis", title = "Economically inactive (excl. students) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_7

map13_8 <- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_ei_student_lab", palette = "viridis", title = "Economically inactive (students only) Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_8



# Local authority ---------------------------------------------------------

# Prepare lookup table data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping' ## my bucket name

lookup <- s3read_using(import # Which function are we using to read
                       , object = 'data/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv' # File to open
                       , bucket = buck) # Bucket name defined above

# drop columns don't need
lookup <- lookup %>% 
  dplyr::select(MSOA11CD, LAD11CD) %>% 
  distinct()

eng_dta <- left_join(ea_dta, lookup, by = c("geography_code"="MSOA11CD"))

eng_dta<-eng_dta %>% 
  group_by(LAD11CD) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  dplyr::select(-date)

#Check it doesn't have Northern Ireland or Scotland
eng_dta %>% 
  filter(str_detect(LAD11CD, "N")| str_detect(LAD11CD, "S"))

cats <- c("ea_student_", "ei_student_", "ei_", "ea_")

for (cat in cats){
  eng_dta <- eng_dta %>%
     mutate( !!paste0(cat,"netmigration") := (!!as.name(paste0(cat,"n_inmig")) - !!as.name(paste0(cat,"n_outmig"))) / !!as.name(paste0(cat,"n_midyrpop")) * 100)
  
} 


buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(eng_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'ea_with_students_net_migration_LAD.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above



# New definition - based on over/under median net migration across LAs
eng_dta %>% 
  select(contains("netmigration")) %>% 
  tbl_summary(digits=everything()~3) #default is median (25%,75%) 

summ(eng_dta$ea_netmigration)
summ(eng_dta$ei_netmigration)
summ(eng_dta$ea_student_netmigration)
summ(eng_dta$ei_student_netmigration)

##the median of ei student and ea student is pretty much the same 
##the spread looks most similar for students so will just combine theme

eng_dta<-eng_dta %>% 
  mutate(student_n_samead=ei_student_n_samead+ea_student_n_samead,
         student_n_inmig=ei_student_n_inmig+ea_student_n_inmig,
         student_n_outmig=ei_student_n_outmig+ea_student_n_outmig,
         student_n_movedwithin=ei_student_n_movedwithin+ea_student_n_movedwithin,
         student_n_usualres10=student_n_samead+student_n_movedwithin+student_n_outmig,
         student_n_usualres11=student_n_samead+student_n_inmig+student_n_movedwithin,
         student_n_midyearpop=(student_n_usualres10+student_n_usualres11)/2,
         student_netmigration=((student_n_inmig-student_n_outmig)/student_n_midyearpop)*100)


eng_dta %>% 
  select(contains("netmigration")) %>% 
  tbl_summary(digits=everything()~3) #default is median (25%,75%) 

summ(eng_dta$ea_netmigration)
summ(eng_dta$ei_netmigration)
summ(eng_dta$ea_student_netmigration)
summ(eng_dta$ei_student_netmigration)
summ(eng_dta$student_netmigration)


eng_dta <- eng_dta %>%
  mutate(EA_mig = case_when(
    ea_netmigration <=1.016 & ei_netmigration <=0.574 & student_netmigration <=-7.243 ~ "Below median net migration - all groups",
    ea_netmigration >1.016 & ei_netmigration >0.574 &student_netmigration> -7.243 ~ "Above median net migration - all groups",
    ea_netmigration >1.016 & ei_netmigration <=0.574 &student_netmigration<= -7.243~ "Above median net migration - Economically active",
    ea_netmigration <=1.016 & ei_netmigration >0.574 &student_netmigration<= -7.243 ~ "Above median net migration - Economically inactive", 
    ea_netmigration <=1.016 & ei_netmigration <=0.574 &student_netmigration> -7.243 ~ "Above median net migration - Students",
    ea_netmigration <=1.016 & ei_netmigration >0.574 &student_netmigration> -7.243 ~ "Below median net migration - Economically active only",
    ea_netmigration >1.016 & ei_netmigration <=0.574 &student_netmigration> -7.243 ~ "Below median net migration - Economically inactive only",
    ea_netmigration >1.016 & ei_netmigration >0.574 &student_netmigration<= -7.243 ~ "Below median net migration - Students only"))

tabyl(eng_dta$EA_mig)


write.csv(eng_dta, "eng_dta.csv")


eng_dta2 <- eng_dta %>%
  mutate(EA_mig = case_when(
    ea_netmigration <=1.016 & ei_netmigration >0.574 ~ "Above median net migration for Economically inactive but below Economically active", 
    ea_netmigration>1.016 & ei_netmigration <=0.574 ~ "Above median net migration for Economically active but below Economically inactive",
    student_netmigration> -7.243 & ea_netmigration<1.016 ~ "Above median net migration students but below for economically active",
    student_netmigration<= -7.243&ea_netmigration>1.016  ~ "Below median net migration students and economically active"))

tabyl(eng_dta2$EA_mig)

write.csv(eng_dta2, "eng_dta2.csv")

# Maps for LAD level ------------------------------------------------------

# import shp data
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
  subset(str_detect(lad11cd, 'E') | str_detect(lad11cd, 'W'))

# Join spatial data
lad_shp <- left_join(lad_shp, eng_dta, by = c("lad11cd" = "LAD11CD"))


