## Preliminary maps - Middle Layer Super Output Area level
# Run this script after S4_mobility_data_prep_MSOA

# Housekeeping
# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

# load packages
pacman::p_load(sf,
               XML,
               tmap)

# read OA boundaries
    # Option 1: manually download all data to "shapefiles" folder in R workspace
    # then run command below to load data
# msoa_shp <- st_read(here("shapefiles", "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp"))

# Option 2: trying command above with save_object to save directly into R workspace without manually downloading
  # Need to download all 6 files in folder for .shp to load correctly
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


#load mobility MSOA data
eng_dta<-s3read_using(readRDS # Which function are we using to read
             , object = 'eng_dta_MSOA.RDS' # File to open
             , bucket = buck_clean) # Bucket name defined above



# Join spatial data
msoa_shp <- left_join(msoa_shp, eng_dta, by = c("MSOA11CD" = "geography.code"))
# geography.code is the MSOA code in the eng_dta df and MSOA11CD the code in the shapefile data
  tabyl(msoa_shp$mob_cat)


# id for country name initial
# creates a variable with E for England, W for Wales etc. 
msoa_shp$ctry_nm <- substr(msoa_shp$MSOA11CD, 1, 1)
msoa_shp$ctry_nm <- as.factor(msoa_shp$ctry_nm)

# simplify boundaries
msoa_shp_simple <- st_simplify(msoa_shp, 
                             preserveTopology =T,
                             dTolerance = 1000) # 1km

# ensure geometry is valid
msoa_shp_simple <- sf::st_make_valid(msoa_shp_simple)
  # not sure what this command does (from the geographic data science practical)


# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does


# Map 1 - map of mobility categories - without simplified boundaries
map5_1 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map5_1
s3write_using(map5_1 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map5_1_mobility_MSOA.tiff' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands
  # + scale_fill_manual to set category colours manually


# Make maps for selected metro areas - London
msoa_shp %>% 
  filter(substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, '^E')) %>%
  tabyl(MSOA11NM)

ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, '^E')) 



# Map 2 - map of mobility categories - London
map5_2 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map5_2
s3write_using(map5_2 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map5_2_mobility_MSOA.tiff' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands



# Looking at single local authority e.g. Hackney
test <- msoa_shp %>% 
  dplyr::filter(, str_detect(MSOA11NM, 'Hackney')) 
tm_shape(test) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)

