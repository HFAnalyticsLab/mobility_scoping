## Preliminary maps - age-specific mobility - Middle Layer Super Output Area level

# Housekeeping
# clear R environment
rm(list = ls())

# load packages
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

#load mobility MSOA data
# buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name
# 
# age_dta<-s3read_using(readRDS # Which function are we using to read
#                       , object = 'eng_age_dta_MSOA.RDS' # File to open
#                       , bucket = buck) # Bucket name defined above
# 


## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008
age_dta <- s3read_using(import, 
                        object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/migration_age_sex.csv') # File to open 

dim(age_dta)      #303 variables, 7201 observations

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


str(msoa_shp)


##Clean age data

age_dta<-age_dta %>% 
  clean_names() 

colnames(age_dta)

under_34<-c("0_to_4", "5_to_15", "16_to_19","20_to_24", "25_to_34")
x35_to_64<-c("35_to_49","50_to_64")
x65plus<-c("65_to_64","75_and_over")

age_dta_clean<-age_dta %>% 
  select(c("date", "geography", "geography_code", contains(c("same_address", "inflow_total_measures","outflow_total_measures")))) %>% 
  # select(contains(c("inflow_total_measures"))) %>%
  rowwise() %>% 
  mutate(under_34_samead=sum(c_across(contains(paste0("all_persons_age_age_", under_34,"_migration_", "lived_at_same_address")))),
         x35_to_64_samead=sum(c_across(contains(paste0("all_persons_age_age_", x35_to_64,"_migration_", "lived_at_same_address")))),
         x65plus_samead=sum(c_across(contains(paste0("all_persons_age_age_", x65plus,"_migration_", "lived_at_same_address"))))) %>% 
  mutate(under_34_inmig=sum(c_across(contains(paste0("all_persons_age_age_", under_34,"_migration_", "inflow_total_measures")))),
         x35_to_64_inmig=sum(c_across(contains(paste0("all_persons_age_age_", x35_to_64,"_migration_", "inflow_total_measures")))),
         x65plus_inmig=sum(c_across(contains(paste0("all_persons_age_age_", x65plus,"_migration_", "inflow_total_measures"))))) %>% 
  mutate(under_34_outmig=sum(c_across(contains(paste0("all_persons_age_age_", under_34,"_migration_", "outflow_total_measures")))),
         x35_to_64_outmig=sum(c_across(contains(paste0("all_persons_age_age_", x35_to_64,"_migration_", "outflow_total_measures")))),
         x65plus_outmig=sum(c_across(contains(paste0("all_persons_age_age_", x65plus,"_migration_", "outflow_total_measures"))))) %>% 
  mutate(under_34_usualres=sum(c_across(c("under_34_samead","under_34_inmig"))),
         x35_to_64_usualres=sum(c_across(c("x35_to_64_samead","x35_to_64_inmig"))),
         x65plus_usualres=sum(c_across(c("x65plus_samead","x65plus_inmig")))) %>% 
  select(c("date", "geography", "geography_code",contains(c("under_34","x35_to_64","x65plus"))))

#work out net migration 
age_dta_clean<-age_dta_clean %>% 
  mutate(under_34_netmigration=(under_34_inmig-under_34_outmig)/(under_34_usualres+under_34_outmig-under_34_inmig)*1000,
         x35_to_64_netmigration=(x35_to_64_inmig-x35_to_64_outmig)/(x35_to_64_usualres+x35_to_64_outmig-x35_to_64_inmig)*1000,
         x65plus_netmigration=(x65plus_inmig-x65plus_outmig)/(x65plus_usualres+x65plus_outmig-x65plus_inmig)*1000)

#label net migration 
age_dta_clean<-age_dta_clean %>% 
  mutate(under_34_netmigration_lab=case_when(under_34_netmigration==0 ~ "0", 
                                             under_34_netmigration<0 ~ "negative",
                                             under_34_netmigration >0 ~ "positive"),
         x35_to_64_netmigration_lab=case_when(x35_to_64_netmigration==0 ~ "0", 
                                              x35_to_64_netmigration<0 ~ "negative",
                                              x35_to_64_netmigration >0 ~ "positive"),
         x65plus_netmigration_lab=case_when(x65plus_netmigration==0 ~ "0", 
                                            x65plus_netmigration<0 ~ "negative",
                                            x65plus_netmigration >0 ~ "positive"))

age_dta_shp<-age_dta_clean %>% 
  select(c("date", "geography", "geography_code",contains(c("netmigration"))))

# Join spatial data
msoa_shp <- left_join(msoa_shp, age_dta_shp, by = c("MSOA11CD" = "geography_code"))
# geography.code is the MSOA code in the eng_dta df and MSOA11CD the code in the shapefile data



# id for country name initial
# creates a variable with E for England, W for Wales etc. 
msoa_shp$ctry_nm <- substr(msoa_shp$MSOA11CD, 1, 1)
msoa_shp$ctry_nm <- as.factor(msoa_shp$ctry_nm)



# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does


# Map 9.1 - map of net migration rate by age
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name


map9_1 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "under_34_netmigration_lab", palette = "viridis", title = "Under 34 Net migration rate per 1,000 people") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
# +
#   tm_text(text="MSOA11NM")

map9_1


map9_2<-tm_shape(msoa_shp) +
  tm_fill(col = "x35_to_64_netmigration_lab", palette = "viridis", title = "35-64 Net migration rate per 1,000 people") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map9_2 

map9_3<-tm_shape(msoa_shp) +
  tm_fill(col = "x65plus_netmigration_lab", style = "cat", palette = "viridis", title = "65+ Net migration rate per 1,000 people") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map9_3 


map9<-tmap_arrange(map9_1, map9_2, map9_3)

map9


s3write_using(map9 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'map9.png' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above



# tmap_save(map3, here("outputs", "map3_mobility_MSOAs.tiff"))
# # Note: need to figure out how to export maps with sw3 commands
# # + scale_fill_manual to set category colours manually
# 

# Make maps for selected metro areas - London
msoa_shp %>% 
  filter(substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) %>%
  tabyl(MSOA11NM)

ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 


# M map of - London
map10_1 <- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "under_34_netmigration_lab", palette = "viridis", title = "Under 34 Net migration rate in London per 1,000 people") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map10_1

map10_2<-tm_shape(ldn_msoa_shp) +
  tm_fill(col = "x35_to_64_netmigration_lab", palette = "viridis", title = "35-64 Net migration rate in London per 1,000 people") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map10_2 

map10_3<-tm_shape(ldn_msoa_shp) +
  tm_fill(col = "x65plus_netmigration_lab", style = "cat", palette = "viridis", title = "65+ Net migration rate in London per 1,000 people") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map10_3 


map10<-tmap_arrange(map10_1, map10_2, map10_3)

map10


s3write_using(map10 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'map10.png' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




