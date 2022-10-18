## Preliminary maps - Output Area level
# Run this script after 01_mobility_data_prep

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

# load packages
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
               readr, 
               BiocManager,
               sf,
               XML,
               tmap,
               THFstyle,
               dplyr)


# import shp data
      # shapefiles were downloaded from: https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(BDY_OA%2CDEC_2011)
save_object(object = 'OA_shapefile_data/Output_Areas__December_2011__Boundaries_EW_BFC.shp',
            file = here::here("shapefiles", "eng.shp"), 
            bucket = buck_data)
save_object(object = 'OA_shapefile_data/Output_Areas__December_2011__Boundaries_EW_BFC.cpg',
            file = here::here("shapefiles", "eng.cpg"), 
            bucket = buck_data)
save_object(object = 'OA_shapefile_data/Output_Areas__December_2011__Boundaries_EW_BFC.dbf',
            file = here::here("shapefiles", "eng.dbf"), 
            bucket = buck_data)
save_object(object = 'OA_shapefile_data/Output_Areas__December_2011__Boundaries_EW_BFC.prj',
            file = here::here("shapefiles", "eng.prj"),
            bucket = buck_data)
save_object(object = 'OA_shapefile_data/Output_Areas__December_2011__Boundaries_EW_BFC.shx',
            file = here::here("shapefiles", "eng.shx"), 
            bucket = buck_data)
save_object(object = 'OA_shapefile_data/Output_Areas__December_2011__Boundaries_EW_BFC.xml',
            file = here::here("shapefiles", "eng.xml"), 
            bucket = buck_data)


# load shp data
oa_shp <- st_read(here::here("shapefiles", "eng.shp"))

str(oa_shp)




# Join spatial data with eng_dta (created in script 1)
eng_dta <- s3read_using(import # Which function are we using to read
                        , object = 'eng_dta_OA.RDS' # File to open
                        , bucket = buck_clean) # Bucket name defined above

oa_shp <- left_join(oa_shp, eng_dta, by = c("OA11CD" = "geography"))
  # geography is the OA code in the eng_dta df and OA11CD the code in the shapefile data
  # This command merges the indicators onto the shapefile data


# id for country name initial
# creates a variable with E for England, W for Wales etc. 
oa_shp$ctry_nm <- substr(oa_shp$OA11CD, 1, 1)
oa_shp$ctry_nm <- as.factor(oa_shp$ctry_nm)

# simplify boundaries
oa_shp_simple <- st_simplify(oa_shp, 
                             preserveTopology =T,
                             dTolerance = 1000) # 1km

# ensure geometry is valid
oa_shp_simple <- sf::st_make_valid(oa_shp_simple)
  # not sure how this command changes boundaries in practice


# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)


# These maps take a long time to run, and are hard to read. 
  # not spending more time on these in favour of LSOA/MSOA maps

# Map 1 - Simple map of England and Wales
tm_shape(oa_shp_simple) +
  tm_fill(col = "ctry_nm", style = "cat", palette = pal_THF, title = "Country") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
  # OA level doesn't produce very readable maps even after removing boundaries
  # and takes much longer to produce OA level maps than LA level maps

# remove polygon borders
map2_1 <- tm_shape(oa_shp_simple) +
  tm_borders(, alpha=0) +
  tm_fill(col = "ctry_nm", style = "cat", palette = pal_THF, title = "Country") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 
map2_1



# Map 2 - map of mobility categories
map2_2 <- tm_shape(oa_shp_simple) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map2_2 


# Repeat Map 2 - map of mobility categories - without simplified boundaries
map2_2 <- tm_shape(oa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map2_2 
s3write_using(map2_2 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map2_mobility_OAs.tiff' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name defined above


