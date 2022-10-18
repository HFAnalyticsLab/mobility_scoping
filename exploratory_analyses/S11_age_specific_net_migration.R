## Preliminary maps - age-specific mobility - Middle Layer Super Output Area level

# Housekeeping
# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

# load packages
pacman::p_load(sf,
               XML,
               tmap,
               viridis, 
               wesanderson,
               aws.s3,
               tidyverse,
               rio,
               janitor,
               ggmap)

#load mobility MSOA data
# age_dta<-s3read_using(readRDS # Which function are we using to read
#                       , object = 'eng_age_dta_MSOA.RDS' # File to open
#                       , bucket = buck_clean) # Bucket name defined above
# 


## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig001
age_dta <- s3read_using(import, 
                        object = 'migration_age_sex.csv', 
                        bucket = buck_data) 

dim(age_dta)      #303 variables, 7201 observations

#load shp files 
    # shapefiles were downloaded from https://geoportal.statistics.gov.uk/datasets/ons::middle-layer-super-output-areas-december-2011-boundaries-super-generalised-clipped-bsc-ew-v3
save_object(object = 'MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp',
            file = here::here("shapefiles", "eng.shp"), 
            bucket = buck_data)
save_object(object = 'MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.cpg',
            file = here::here("shapefiles", "eng.cpg"), 
            bucket = buck_data)
save_object(object = 'MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.dbf',
            file = here::here("shapefiles", "eng.dbf"), 
            bucket = buck_data)
save_object(object = 'MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.prj',
            file = here::here("shapefiles", "eng.prj"),
            bucket = buck_data)
save_object(object = 'MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx',
            file = here::here("shapefiles", "eng.shx"), 
            bucket = buck_data)
save_object(object = 'MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.xml',
            file = here::here("shapefiles", "eng.xml"), 
            bucket = buck_data)

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
      # Note: mistake in the column name of raw dataset (65 to 64 instead of 74)

age_dta_clean<-age_dta %>% 
  dplyr::select(c("date", "geography", "geography_code", contains(c("same_address", "inflow_total_measures","outflow_total_measures", "within_same_area_measures")))) %>% 
  # select(contains(c("inflow_total_measures"))) %>%
  rowwise() %>% 
  mutate(under_34_samead=sum(c_across(contains(paste0("all_persons_age_age_", under_34,"_migration_", "lived_at_same_address")))),
         x35_to_64_samead=sum(c_across(contains(paste0("all_persons_age_age_", x35_to_64,"_migration_", "lived_at_same_address")))),
         x65plus_samead=sum(c_across(contains(paste0("all_persons_age_age_", x65plus,"_migration_", "lived_at_same_address"))))) %>% 
  mutate(under_34_inmig=sum(c_across(contains(paste0("all_persons_age_age_", under_34,"_migration_", "inflow_total_measures")))),
         x35_to_64_inmig=sum(c_across(contains(paste0("all_persons_age_age_", x35_to_64,"_migration_", "inflow_total_measures")))),
         x65plus_inmig=sum(c_across(contains(paste0("all_persons_age_age_", x65plus,"_migration_", "inflow_total_measures"))))) %>% 
  mutate(under_34_n_movedwithin=sum(c_across(contains(paste0("all_persons_age_age_", under_34,"_migration_lived_elsewhere_one_year_ago_within_same_area_measures")))),
         x35_to_64_n_movedwithin=sum(c_across(contains(paste0("all_persons_age_age_", x35_to_64,"_migration_lived_elsewhere_one_year_ago_within_same_area_measures")))),
         x65plus_n_movedwithin=sum(c_across(contains(paste0("all_persons_age_age_", x65plus,"_migration_lived_elsewhere_one_year_ago_within_same_area_measures"))))) %>%
  mutate(under_34_outmig=sum(c_across(contains(paste0("all_persons_age_age_", under_34,"_migration_", "outflow_total_measures")))),
         x35_to_64_outmig=sum(c_across(contains(paste0("all_persons_age_age_", x35_to_64,"_migration_", "outflow_total_measures")))),
         x65plus_outmig=sum(c_across(contains(paste0("all_persons_age_age_", x65plus,"_migration_", "outflow_total_measures"))))) %>% 
  mutate(under_34_usualres11=sum(c_across(c("under_34_samead","under_34_n_movedwithin", "under_34_inmig"))),
         x35_to_64_usualres11=sum(c_across(c("x35_to_64_samead","x35_to_64_n_movedwithin", "x35_to_64_inmig"))),
         x65plus_usualres11=sum(c_across(c("x65plus_samead","x65plus_n_movedwithin", "x65plus_inmig")))) %>% 
  mutate(under_34_usualres10=sum(c_across(c("under_34_samead","under_34_n_movedwithin", "under_34_outmig"))),
         x35_to_64_usualres10=sum(c_across(c("x35_to_64_samead","x35_to_64_n_movedwithin", "x35_to_64_outmig"))),
         x65plus_usualres10=sum(c_across(c("x65plus_samead","x65plus_n_movedwithin", "x65plus_outmig")))) %>% 
  mutate(under_34_midyearpop=sum((under_34_usualres10+under_34_usualres11)/2),
         x35_to_64_midyearpop=sum((x35_to_64_usualres10+x35_to_64_usualres11)/2),
         x65plus_midyearpop=sum((x65plus_usualres10+x65plus_usualres11)/2)) %>% 
  dplyr::select(c("date", "geography", "geography_code",contains(c("under_34","x35_to_64","x65plus"))))



#n_movedwithin= within_same_area
#n_usualres10= n_movedwithin+ n_samead+ n_outmig 
#mid_yearpop= (n_usualres10+n_usualres11)/2

#work out net migration 
#net_migration= (n_inmig-n_outmig)/(mid_yearpop)*1000


age_dta_clean<-age_dta_clean %>% 
  mutate(under_34_netmigration=(under_34_inmig-under_34_outmig)/(under_34_midyearpop)*100,
         x35_to_64_netmigration=(x35_to_64_inmig-x35_to_64_outmig)/(x35_to_64_midyearpop)*100,
         x65plus_netmigration=(x65plus_inmig-x65plus_outmig)/(x65plus_midyearpop)*100)


age_dta_shp<-age_dta_clean %>% 
  dplyr::select(c("date", "geography", "geography_code",contains(c("netmigration"))))

#save data
s3write_using(age_dta_shp # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'age_net_migration.csv' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above


summary(age_dta_shp)


age_dta_shp_plot<-age_dta_shp %>% 
  dplyr::select(under_34_netmigration:x65plus_netmigration) %>% 
  pivot_longer(everything(), names_to="age", values_to="net_migration")
  
t<-boxplot(net_migration~age,data=age_dta_shp_plot, main="Net Migration by Age",
        xlab="Age", ylab="Net Migration")


#label net migration 
age_dta_shp<-age_dta_shp %>% 
  mutate(under_34_netmigration_lab=case_when(under_34_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                             under_34_netmigration > 1 ~ "Positive (greater than 1% of people moving in)", 
                                             TRUE ~ "stable migration (~1% of population moving in and out)"),
         x35_to_64_netmigration_lab=case_when(x35_to_64_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                              x35_to_64_netmigration > 1 ~ "Positive (greater than 1% of people moving in)",
                                              TRUE ~ "stable migration (~1% of population moving in and out)"),
         x65plus_netmigration_lab=case_when(x65plus_netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                            x65plus_netmigration > 1 ~ "Positive (greater than 1% of people moving in)",
                                            TRUE ~ "stable migration (~1% of population moving in and out)"))




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

map10_1 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "under_34_netmigration_lab", palette = "viridis", title = "Under 34 Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
# +
#   tm_text(text="MSOA11NM")

map10_1



map10_2<-tm_shape(msoa_shp) +
  tm_fill(col = "x35_to_64_netmigration_lab", palette = "viridis", title = "35-64 Net migration (%)") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map10_2 


map10_3<-tm_shape(msoa_shp) +
  tm_fill(col = "x65plus_netmigration_lab", style = "cat", palette = "viridis", title = "65+ Net migration (%)") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map10_3 


# map9<-tmap_arrange(map9_1, map9_2, map9_3)
# 
# map9
# 
# 
# 

# Make maps for selected metro areas - London
msoa_shp %>% 
  filter(substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) %>%
  tabyl(MSOA11NM)

ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 


# M map of - London
map10_4 <- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "under_34_netmigration_lab", palette = "viridis", title = "Under 34 Net migration rate in London (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map10_4


map10_5<-tm_shape(ldn_msoa_shp) +
  tm_fill(col = "x35_to_64_netmigration_lab", palette = "viridis", title = "35-64 Net migration rate in London (%)") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map10_5

map10_6<-tm_shape(ldn_msoa_shp) +
  tm_fill(col = "x65plus_netmigration_lab", style = "cat", palette = "viridis", title = "65+ Net migration rate in London (%)") +
  tm_borders(alpha=0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map10_6



