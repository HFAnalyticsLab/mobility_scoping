## Preliminary maps - Lower Super Output Area level
# Run this script after 01_mobility_data_prep


# Housekeeping
# clear R environment
rm(list = ls())

#load packages
library(data.table)
library(aws.s3)

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
               readr)


  # NOTE: Census migration data not available at LSOA level
    # we can reconstruct it using lookup tables
  
# Prepare lookup table data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping' ## my bucket name

lookup <- s3read_using(import # Which function are we using to read
                        , object = 'data/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv' # File to open
                        , bucket = buck) # Bucket name defined above


# drop columns don't need
lookup <- lookup %>% 
  dplyr::select(OA11CD, LSOA11CD) 

# merge on to eng_dta (created in script 1)
eng_dta <- s3read_using(import # Which function are we using to read
                        , object = 'data/clean/eng_dta_OA.RDS' # File to open
                        , bucket = buck) # Bucket name defined above

eng_dta <- left_join(lookup, eng_dta, by = c("OA11CD" = "geography"))

# keep only unique rows
dim(eng_dta)
eng_dta <- eng_dta %>% 
  distinct() 

# generate columns for LSOA level
eng_dta <- eng_dta %>%
  group_by(LSOA11CD) %>%
  mutate(n_samead_lsoa = sum(n_samead),
         n_usualres_lsoa11 = sum(n_usualres11),
         n_movedwithin_lsoa = sum(n_movedwithin),
         n_inmiguk_lsoa = sum(n_inmiguk),
         n_inmigfor_lsoa = sum(n_inmigfor),
         n_outmig_lsoa = sum(n_outmig), 
         n_usualres_lsoa10 = sum(n_usualres10),
         midyrpop_lsoa = sum(midyrpop),
         n_inmig_lsoa = sum(n_inmig))

# drop variables related to OAs and deduplicate rows
eng_dta <- eng_dta  %>% 
  dplyr::select(LSOA11CD, ends_with("_lsoa"), ends_with("_lsoa11"), ends_with("_lsoa10"))
eng_dta <- eng_dta %>% 
  distinct() 

# drop Wales rows
eng_dta <- eng_dta %>%
  subset(str_detect(LSOA11CD, 'E'))

dim(eng_dta)


# recreate variables needed for maps
# net migration
eng_dta <- eng_dta %>% 
  mutate(net_migration = (n_inmig_lsoa - n_outmig_lsoa)/(midyrpop_lsoa)*100)
summary(eng_dta$net_migration)  #median net migration = 0.4% (similar to OAs)

sum(!is.na(eng_dta$net_migration)) #32844
sum(eng_dta$net_migration <= -5, na.rm = TRUE) #179
179/32844 #0.5% 
sum(eng_dta$net_migration < -2, na.rm = TRUE) #3431
3431/32844 #10.4% 

# turnover
eng_dta <- eng_dta %>% 
  mutate(turnover = (n_inmig_lsoa + n_outmig_lsoa)/n_usualres_lsoa11*100)
summary(eng_dta$turnover)
#median turnover = 19.0% 

# Create variable with mobility categories
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= -2 ~ "Decreasing <-2%",
    net_migration > 5 ~ "Increasing >5%",
    net_migration > -2 & net_migration <= 5 & turnover >= 19 ~ "Stable, high turnover",
    net_migration > -2 & net_migration <= 5 & turnover <19 ~ "Stable, low turnover"
  ))

tabyl(eng_dta$mob_cat)
sum(is.na(eng_dta$mob_cat))



# load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle)

# import shapefile data
# import shp data
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp',
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.cpg',
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.dbf',
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.prj',
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shx',
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.xml',
            file = here::here("shapefiles", "eng.xml"))


# read LSOA boundaries
lsoa_shp <- st_read(here("shapefiles", "eng.shp"))

str(lsoa_shp)


# Join spatial data
  #NOTE: need to download census mobility data at LSOA level
  
lsoa_shp <- left_join(lsoa_shp, eng_dta, by = c("LSOA11CD" = "LSOA11CD"))
# geography is the LSOA code in the eng_dta df and OA11CD the code in the shapefile data
# merged the indicators onto the shapefile data


# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does



# Map 3 - map of mobility categories 
map3_1 <- tm_shape(lsoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map3_1
s3write_using(map3_1 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map3_1_mobility_LSOA.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above



# Make maps for selected metro areas - London
lsoa_shp %>% 
  filter(substring(LSOA11CD, 2) < '01004681' & str_detect(LSOA11CD, 'E')) %>%
 tabyl(LSOA11NM)
  

ldn_lsoa_shp <- lsoa_shp %>% 
  dplyr::filter(, substring(LSOA11CD, 2) < '01004681' & str_detect(LSOA11CD, 'E')) 



# Map 2 - map of mobility categories - London
map3_2 <- tm_shape(ldn_lsoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map3_2
s3write_using(map3_2 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map3_2_mobility_London.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above


