## Preliminary maps - Middle Layer Super Output Area level
# Run this script after 04_mobility_data_prep_MSOA

# load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle)

# read OA boundaries
    # Option 1: manually download all data to "shapefiles" folder in R workspace
# msoa_shp <- st_read(here("shapefiles", "Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp"))

# Option 2: trying command above with save_object to save directly into R workspace without manually downloading
  # Need to download all 6 files in folder for .shp to load correctly
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shp',
            file = here("shapefiles", "eng.shp"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.cpg',
            file = here("shapefiles", "eng.cpg"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.dbf',
            file = here("shapefiles", "eng.dbf"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.prj',
            file = here("shapefiles", "eng.prj"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.shx',
            file = here("shapefiles", "eng.shx"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/MSOA_shapefile_data/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Super_Generalised_Clipped_(BSC)_EW_V3.xml',
            file = here("shapefiles", "eng.xml"))

# load shp data
msoa_shp <- st_read(here("shapefiles", "eng.shp"))

str(msoa_shp)

# Join spatial data
msoa_shp <- left_join(msoa_shp, eng_dta, by = c("MSOA11CD" = "geography.code"))
  # geography.code is the MSOA code in the eng_dta df and MSOA11CD the code in the shapefile data
  


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


# Map 3 - map of mobility categories - without simplified boundaries
map3 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map3
tmap_save(map3, here("outputs", "map3_mobility_MSOAs.tiff"))
  # Note: need to figure out how to export maps with sw3 commands


# Make maps for selected metro areas - London
msoa_shp %>% 
  filter(substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) %>%
  tabyl(MSOA11NM)

ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 



# Map 4 - map of mobility categories - London
map4 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map4
tmap_save(map4, here("outputs", "map4_mobility_MSOAs_London.tiff"))



# Closer look at Hackney out of curiosity
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

