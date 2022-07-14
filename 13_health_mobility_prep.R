## Preparing area-level mobility data, breakdown by health status - MSOAs

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
               matrixStats,
               tidyverse, 
               viridis, 
               viridisLite)

# import all data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig005 
health_dta <- s3read_using(import, 
                        object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/censusmig_health_MSOA.csv') # File to open 

dim(health_dta)      #43 variables, 7201 observations


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
health_dta<-health_dta %>% 
  clean_names() 

# naming unneeded colums x, y, z
names(health_dta)[4:10]<-c('n_samead', 'n_movedwithin' , 'n_inmig','x','y','z','n_outmig')
names(health_dta)[14:20]<-c('n_samead_limlot', 'n_movedwithin_limlot' , 'n_inmig_limlot','x_limlot', 'y_limlot','z_limlot','n_outmig_limlot')
names(health_dta)[24:30]<-c('n_samead_limlit', 'n_movedwithin_limlit' , 'n_inmig_limlit','x_limlit', 'y_limlit','z_limlit','n_outmig_limlit')
names(health_dta)[34:40]<-c('n_samead_notlim', 'n_movedwithin_notlim' , 'n_inmig_notlim','x_notlim', 'y_notlim','z_notlim','n_outmig_notlim')


# Create variables for usual residents in 2010 and 2011
cats <- c("limlot", "limlit", "notlim")

for (cat in cats){
  health_dta <- health_dta %>%
    mutate( !!paste0("n_usualres10_", cat) := !!as.name(paste0("n_samead_", cat)) + !!as.name(paste0("n_outmig_", cat)) + !!as.name(paste0("n_movedwithin_", cat))) %>%
    mutate( !!paste0("n_usualres11_", cat) := !!as.name(paste0("n_samead_", cat)) + !!as.name(paste0("n_inmig_", cat)) + !!as.name(paste0("n_movedwithin_", cat))) %>% 
    mutate( !!paste0("n_midyrpop_", cat) := (!!as.name(paste0("n_usualres11_", cat)) + !!as.name(paste0("n_usualres10_", cat))) /2 ) %>%
    mutate( !!paste0("netmigration_", cat) := (!!as.name(paste0("n_inmig_", cat)) - !!as.name(paste0("n_outmig_", cat))) / !!as.name(paste0("n_midyrpop_", cat)) * 100)
   
}  

health_dta <- health_dta %>%
  mutate(n_usualres10 = n_samead + n_outmig + n_movedwithin,
         n_usualres11 = n_samead + n_inmig + n_movedwithin, 
         n_midyrpop = (n_usualres10 + n_usualres11) /2)

clean_dta<-health_dta %>% 
  dplyr::select(c("date", "geography", "geography_code",contains(c("netmigration"))))

#examine types of areas
# 7201 MSOAs
sum(clean_dta$netmigration_limlot <0 & clean_dta$netmigration_limlit <0 & clean_dta$netmigration_notlim <0 , na.rm=TRUE)
# 905 have outmigration in all health groups
sum(clean_dta$netmigration_limlot >0 & clean_dta$netmigration_limlit >0 & clean_dta$netmigration_notlim >0 , na.rm=TRUE)
# 1604 have outmigration in all health groups
sum(clean_dta$netmigration_limlot <0 & clean_dta$netmigration_limlit <0 & clean_dta$netmigration_notlim >0 , na.rm=TRUE)
# 1034 have inmig healthy but outmig for both other groups
sum(clean_dta$netmigration_limlot >0 & clean_dta$netmigration_notlim <0 , na.rm=TRUE)
# 1034 have inmig unhealthy but outmig for healthy (regardless of what limlit does)
sum(clean_dta$netmigration_limlot <0 & clean_dta$netmigration_limlit >0  & clean_dta$netmigration_notlim >0 , na.rm=TRUE)
# 1036 have inmig healthy/limlit but outmig for limlot
sum(clean_dta$netmigration_limlot >0 & clean_dta$netmigration_limlit <0  & clean_dta$netmigration_notlim >0 , na.rm=TRUE)
# 663 


# classification
clean_dta <- clean_dta %>%
  mutate(health_mig = case_when(
              netmigration_limlot <=0 & netmigration_notlim <=0 ~ "General outmigration",
              netmigration_limlot >0 & netmigration_notlim >0 ~ "General inmigration",
              netmigration_limlot >0 & netmigration_notlim <=0 ~ "Inmigration - less healthy",
              netmigration_limlot <=0 & netmigration_notlim >0 ~ "Inmigration - more healthy"))

tabyl(clean_dta$health_mig)

buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(clean_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'health_net_migration.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


ea_dta_plot<-clean_dta %>% 
  dplyr::select(contains("netmigration")) %>% 
  pivot_longer(everything(), names_to="metric", values_to="net_migration")

t<-boxplot(net_migration~metric,data=ea_dta_plot, main="Net Migration by Health Condition",
           xlab="Limited in daily activities", ylab="Net Migration")

summ(clean_dta$netmigration_limlot)
summ(clean_dta$netmigration_limlit)
summ(clean_dta$netmigration_notlim)


# recode those <=1% as stable
clean_dta<-clean_dta %>% 
  mutate(netmigration_limlot_lab=case_when(netmigration_limlot < -1 ~ "Negative (greater than 1% of people leaving)",
                                           netmigration_limlot > 1 ~ "Positive (greater than 1% of people moving in)", 
                                                          TRUE ~ "stable migration (~1% of population moving in and out)"),
         netmigration_limlit_lab=case_when(netmigration_limlit < -1 ~ "Negative (greater than 1% of people leaving)",
                                           netmigration_limlit> 1 ~ "Positive (greater than 1% of people moving in)", 
                                                        TRUE ~ "stable migration (~1% of population moving in and out)"),
         netmigration_notlim_lab=case_when(netmigration_notlim < -1 ~ "Negative (greater than 1% of people leaving)",
                                           netmigration_notlim > 1 ~ "Positive (greather than 1% of people moving in)",
                                                        TRUE ~ "stable migration (~1% of population moving in and out)"))


# Join spatial data
msoa_shp <- left_join(msoa_shp,clean_dta , by = c("MSOA11CD" = "geography_code"))
# geography.code is the MSOA code in the eng_dta df and MSOA11CD the code in the shapefile data


#For saving maps 
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/outputs' ## my bucket name



map13_1 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_limlot_lab", palette = "viridis", title = "Limited a lot in daily activities Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_1



map13_2 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_limlit_lab", palette = "viridis", title = "Limited a little in daily activities Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_2



map13_3 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_notlim_lab", palette = "viridis", title = "Not limited in daily activities Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_3


map13_4 <- tm_shape(msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "health_mig",  style = "cat", palette = c('#dd0031', '#53a9cd',  '#744284',  '#ffd412'), title = "Typology of areas - health status of migrants") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map13_4



# London maps
ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 


map13_4<- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_limlot_lab", palette = "viridis", title = "Limited a lot in daily activities Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map13_4



map13_5<- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_limlit_lab", palette = "viridis", title = "Limited a little in daily activities Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map13_5




map13_6<- tm_shape(ldn_msoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "netmigration_notlim_lab", palette = "viridis", title = "Not limited in daily activities Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map13_6

