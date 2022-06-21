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
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008
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


