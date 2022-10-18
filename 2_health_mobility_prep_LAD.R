## Preparing area-level mobility data, breakdown by health status - by Local Authority District

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

#load packages
pacman::p_load(haven, 
               dplyr, 
               janitor,
               epiDisplay, 
               rio, 
               stringr, 
               here, 
               ggplot2, 
               aws.s3, 
               tidyverse)


# import all data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig005 
    # download > local authorities: district / unitary (prior to April 2015)
health_dta <- s3read_using(import, 
                        object = 'censusmig_health_LAD.csv',
                        bucket = buck_data) # File to open 

dim(health_dta)      #43 variables, 406 observations

# import shp data
    #data were downloaded from https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(BDY_LAD%2CDEC_2011)
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shp',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.cpg',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.dbf',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.prj',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shx',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.xml',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.xml"))

# read LAD boundaries
lad_shp <- st_read(here::here("shapefiles", "eng.shp"))

# read LAD boundaries
lad_shp <- sf::st_read(here::here("shapefiles", "eng.shp"))

str(lad_shp)

# Drop Scotland and Northern Ireland in shp data
lad_shp <- lad_shp %>%
  subset(str_detect(lad11cd, '^E') | str_detect(lad11cd, '^W'))



# tidy variables names
health_dta<-health_dta %>% 
  clean_names() 

# Drop Scotland and Northern Ireland in mobility data
health_dta <- health_dta %>%
  subset(str_detect(geography_code, '^E') | str_detect(geography_code, '^W'))

dim(health_dta)      #43 variables, 348 observations

# replace name for selected local authorities so they merge correctly
health_dta <- health_dta %>%
  mutate(geography = replace(geography, geography == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))
health_dta <- health_dta %>%
  mutate(geography = replace(geography, geography == "Folkestone and Hythe", "Shepway"))
health_dta <- health_dta %>%
  mutate(geography = replace(geography, geography == "Vale of Glamorgan", "The Vale of Glamorgan"))


# renaming columns for same address (n_samead), moved within the local authority (n_movedwithin), inmigrants (n_inmig) and outmigrants (n_outmig)
    # in each health status group
names(health_dta)[4:10]<-c('n_samead', 'n_movedwithin' , 'n_inmig','x','y','z','n_outmig')
names(health_dta)[14:20]<-c('n_samead_limlot', 'n_movedwithin_limlot' , 'n_inmig_limlot','x_limlot', 'y_limlot','z_limlot','n_outmig_limlot')
names(health_dta)[24:30]<-c('n_samead_limlit', 'n_movedwithin_limlit' , 'n_inmig_limlit','x_limlit', 'y_limlit','z_limlit','n_outmig_limlit')
names(health_dta)[34:40]<-c('n_samead_notlim', 'n_movedwithin_notlim' , 'n_inmig_notlim','x_notlim', 'y_notlim','z_notlim','n_outmig_notlim')


# Create variables for usual residents in 2010 and 2011, and net migration
    #n_usualres11 = n_samead + n_movedwithin + n_inmig
    #n_usualres10 = n_movedwithin + n_samead + n_outmig 
    #mid_yearpop = (n_usualres10 + n_usualres11)/2
    #net_migration= (n_inmig-n_outmig)/(mid_yearpop)*100

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


# Classify local authorities based on over/under median net migration in each health status group across LAs --------

summ(clean_dta$netmigration_limlot) # median net migration 0.232
summ(clean_dta$netmigration_limlit) # median net migration 0.298
summ(clean_dta$netmigration_notlim) # median net migration 0.755

a<-median(clean_dta$netmigration_limlot)
b<-median(clean_dta$netmigration_limlit)
c<-median(clean_dta$netmigration_notlim)

#examine types of areas
# 348 LAs
sum(clean_dta$netmigration_limlot <a & clean_dta$netmigration_limlit <b & clean_dta$netmigration_notlim <c , na.rm=TRUE)
# 70 have net migration below median in all health groups
sum(clean_dta$netmigration_limlot >a & clean_dta$netmigration_limlit >b & clean_dta$netmigration_notlim >c , na.rm=TRUE)
# 68 have net migration above median in all health groups
sum(clean_dta$netmigration_limlot <a & clean_dta$netmigration_limlit <b & clean_dta$netmigration_notlim >c , na.rm=TRUE)
# 44 have net migration above median for healthy but net migration below median for both other groups
sum(clean_dta$netmigration_limlot >a & clean_dta$netmigration_notlim <c , na.rm=TRUE)
# 90 have net migration above median among unhealthy but below median for healthy (regardless of what limlit does)
sum(clean_dta$netmigration_limlot <a & clean_dta$netmigration_limlit >b  & clean_dta$netmigration_notlim >c , na.rm=TRUE)
# 46 have net migration above median for healthy/limlit but below median for limlot
sum(clean_dta$netmigration_limlot >a & clean_dta$netmigration_limlit <b  & clean_dta$netmigration_notlim >c , na.rm=TRUE)
# 16



# binary variables for above/below median
clean_dta <- clean_dta %>%
  mutate(above_median_limlot = case_when(netmigration_limlot<=a ~ 0,
                                         TRUE ~ 1), 
         above_median_limlit = case_when(netmigration_limlit<=b ~ 0,
                                         TRUE ~1),
         above_median_notlim = case_when(netmigration_notlim<=c ~ 0,
                                         TRUE ~ 1))

# classification
clean_dta <- clean_dta %>%
  mutate(health_mig = case_when(
    netmigration_limlot <=a& netmigration_notlim <=c ~ "Net migration below median for all health groups",
    netmigration_limlot >a & netmigration_notlim >c ~ "Net migration above median for all health groups",
    netmigration_limlot >a & netmigration_notlim <=c ~ "Net migration above median - less healthy",
    netmigration_limlot <=a & netmigration_notlim >c ~ "Net migration above median - more healthy"))

tabyl(clean_dta$health_mig)

s3write_using(clean_dta # R object to save
              , FUN = write.csv # R function used to save
              , object = 'health_net_migration_LA.csv' # Name of file to save 
              , bucket = buck_clean) # Bucket name 


# Box plot of net migration by health status
ea_dta_plot<-clean_dta %>% 
  dplyr::select(contains("netmigration")) %>% 
  pivot_longer(everything(), names_to="metric", values_to="net_migration")

t<-boxplot(net_migration~metric,data=ea_dta_plot, main="Net Migration by Health Condition",
           xlab="Limited in daily activities", ylab="Net Migration")



# Calculate net migration by Levelling Up priority category ---------------
    #note: this analysis was not included in the final piece
# Import Levelling Up data
    #data were downloaded from https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjyroCDndj6AhWGUcAKHVAtCNsQFnoECAsQAQ&url=https%3A%2F%2Fassets.publishing.service.gov.uk%2Fgovernment%2Fuploads%2Fsystem%2Fuploads%2Fattachment_data%2Ffile%2F966137%2FLevelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx&usg=AOvVaw3NIQ1EBTwu0jLNSisnn3XT 
levup <-s3read_using(import # Which function are we using to read
                     , object = 'data/Levelling_Up_priority_areas/Levelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx' # File to open
                     , bucket = buck_main) # Bucket name 


dim(levup)
# 368 LAs in file

# tidy column names
names(levup)<-str_replace_all(names(levup), c(" " = "." ))

tabyl(levup$Priority.category, show_missing_levels = T)



# merge data
clean_dta <- left_join(clean_dta, levup, by = c("geography" = "Name"))


# calculate net migration by age and by Levelling Up category
summary(subset(clean_dta, Priority.category == 1 & Country == 'England')$netmigration_limlot)
summary(subset(clean_dta, Priority.category == 2 & Country == 'England')$netmigration_limlot)
summary(subset(clean_dta, Priority.category == 3 & Country == 'England')$netmigration_limlot)

summary(subset(clean_dta, Priority.category == 1 & Country == 'England')$netmigration_limlit)
summary(subset(clean_dta, Priority.category == 2 & Country == 'England')$netmigration_limlit)
summary(subset(clean_dta, Priority.category == 3 & Country == 'England')$netmigration_limlit)

summary(subset(clean_dta, Priority.category == 1 & Country == 'England')$netmigration_notlim)
summary(subset(clean_dta, Priority.category == 2 & Country == 'England')$netmigration_notlim)
summary(subset(clean_dta, Priority.category == 3 & Country == 'England')$netmigration_notlim)


# export data by Levelling Up category
tibble_health <- clean_dta %>%
  dplyr::filter(Country == 'England') %>%
  group_by(levelling_up_priority = Priority.category) %>%
  summarise(med_netmigration_limlot = median(netmigration_limlot),
            q1_limlot = quantile(netmigration_limlot, 0.25),
            q3_limlot = quantile(netmigration_limlot, 0.75),
            med_netmigration_limlit = median(netmigration_limlit), 
            q1_limlit = quantile(netmigration_limlit, 0.25),
            q3_limlit = quantile(netmigration_limlit, 0.75),
            med_netmigration_notlim = median(netmigration_notlim),
            q1_notlim = quantile(netmigration_notlim, 0.25),
            q3_notlim = quantile(netmigration_notlim, 0.75))
tibble_health

s3write_using(tibble_health # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'outputs/table_netmigration_health_levellingup.csv' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name 


# Calculate net migration by IMD ------------------------------------------

    #note: this analysis was not included in the final piece
# Import IMD data
    #data were downloaded from https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
imd <-s3read_using(import # Which function are we using to read
                   , object = 'data/IMD_LAD.xlsx' # File to open
                   , bucket = buck_main) # Bucket name 

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
clean_dta <- left_join(clean_dta, imd, by = c("geography" = "local_authority_district_name_2019"))



# calculate net migration by age for top and bottom IMD quintiles
summary(subset(clean_dta, quintile == 1 & Country == 'England')$netmigration_limlot)
summary(subset(clean_dta, quintile == 5 & Country == 'England')$netmigration_limlot)

summary(subset(clean_dta, quintile == 1 & Country == 'England')$netmigration_limlit)
summary(subset(clean_dta, quintile == 5 & Country == 'England')$netmigration_limlit)

summary(subset(clean_dta, quintile == 1 & Country == 'England')$netmigration_notlim)
summary(subset(clean_dta, quintile == 5 & Country == 'England')$netmigration_notlim)


# Export data by IMD quintile
tibble_health2 <- clean_dta %>%
  dplyr::filter(Country == 'England') %>%
  group_by(quintile = quintile) %>%
  summarise(med_netmigration_limlot = median(netmigration_limlot),
            q1_limlot = quantile(netmigration_limlot, 0.25),
            q3_limlot = quantile(netmigration_limlot, 0.75),
            med_netmigration_limlit = median(netmigration_limlit), 
            q1_limlit = quantile(netmigration_limlit, 0.25),
            q3_limlit = quantile(netmigration_limlit, 0.75),
            med_netmigration_notlim = median(netmigration_notlim),
            q1_notlim = quantile(netmigration_notlim, 0.25),
            q3_notlim = quantile(netmigration_notlim, 0.75))
tibble_health2

s3write_using(tibble_health2 # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'outputs/table_netmigration_health_imd.csv' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name 



# Make maps ---------------------------------------------------------------
# Join spatial data
lad_shp <- left_join(lad_shp,clean_dta , by = c("lad11nm" = "geography"))
# geography.code is the LAD code in the eng_dta df and lad11cd the code in the shapefile data

# load packages
pacman::p_load(sf,
               XML,
               tmap,
               devtools, 
               viridis)


# Map 1 - LAs above/below median net migration for "limited a lot"
map2_1 <- tm_shape(lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "above_median_limlot",  style = "cat", palette = c('#53a9cd', '#dd0031'), title = "Net migration above/below median - limited a lot",
          labels = c("Below median", "Above median"))   +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 
map2_1


# Map 2 - above/below median net migration for "limited a little"
map2_2 <- tm_shape(lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "above_median_limlit",  style = "cat", palette = c('#53a9cd', '#dd0031'), title = "Net migration above/below median - limited a little",
          labels = c("Below median", "Above median"))   +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 
map2_2


# Map 3 - above/below median for "not limited"
map2_3 <-  tm_shape(lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "above_median_notlim",  style = "cat", palette = c('#53a9cd', '#dd0031'), title = "Net migration above/below median - not limited",
          labels = c("Below median", "Above median"))   +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 
map2_3


# Map 4 - classification of LAs by residential mobility by health status
map2_4 <- tm_shape(lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "health_mig",  style = "cat", palette = c('#F39214', '#744284', '#53a9cd', '#dd0031'), title = "Mobility by health status") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map2_4



# London maps
ldn_lad_shp <- lad_shp %>% 
  dplyr::filter(, substring(lad11cd, 1, 3) == 'E09' & str_detect(lad11cd, 'E')) 


# Map 5 - above/below median for "limited a lot" - London
map2_5<- tm_shape(ldn_lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "above_median_limlot",  style = "cat", palette = c('#53a9cd', '#dd0031'), title = "Net migration above/below median - limited a lot",
          labels = c("Below median", "Above median"))   +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 
map2_5


# Map 6 - above/below median for "limited a little" - London
map2_6 <- tm_shape(ldn_lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "above_median_limlit",  style = "cat", palette = c('#53a9cd', '#dd0031'), title = "Net migration above/below median - limited a little",
          labels = c("Below median", "Above median"))   +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 
map2_6



# Map 7 - above/below median for "not limited" - London
map2_7<- tm_shape(ldn_lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "above_median_notlim",  style = "cat", palette = c('#53a9cd', '#dd0031'), title = "Net migration above/below median - not limited",
          labels = c("Below median", "Above median"))   +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 

map2_7


# Map 8 - classification of LAs by residential mobility by health status - London
map2_8<- tm_shape(ldn_lad_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "health_mig", palette = "viridis", title = "Not limited in daily activities Net migration (%)") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

map2_8

