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
               wesanderson)

# Download and load shapefile data
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


#load mobility MSOA data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

age_dta<-s3read_using(readRDS # Which function are we using to read
                      , object = 'eng_age_dta_MSOA.RDS' # File to open
                      , bucket = buck) # Bucket name defined above

# Join spatial data
msoa_shp <- left_join(msoa_shp, age_dta, by = c("MSOA11CD" = "geography.code"))
# geography.code is the MSOA code in the eng_dta df and MSOA11CD the code in the shapefile data



# id for country name initial
# creates a variable with E for England, W for Wales etc. 
msoa_shp$ctry_nm <- substr(msoa_shp$MSOA11CD, 1, 1)
msoa_shp$ctry_nm <- as.factor(msoa_shp$ctry_nm)



# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does


# Map 9.1 - map of mean age among residents living at same address one year ago
map9_1 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_samead", style = "cont", palette = "viridis", title = "Mean age non-movers") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_1


# Map 9.2 - map of mean age among in-migrants
map9_2 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_totalmig", style = "cont", palette = "viridis", title = "Mean age in-migrants") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_2


# Map 9.3 - map of mean age among outmigrants
map9_3 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_outmig", style = "cont", palette = "viridis", title = "Mean age out-migrants") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_3


# Map 9.4 - map of difference in meant age of inmigrants and outmigrants
pal <- wes_palette("Zissou1", 100, type = "continuous")
map9_4 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_diff", style = "cont", palette = pal, title = "Mean age difference") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_4


# Make maps for selected metro areas - London
msoa_shp %>% 
  filter(substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) %>%
  tabyl(MSOA11NM)

ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 



# Map 9.5 - age of non-movers - London
map9_5 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_samead", style = "cont", palette = "viridis", title = "Mean age non-movers") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_5


# Map 9.6 - age of inmigrants - London
map9_6 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_totalmig", style = "cont", palette = "viridis", title = "Mean age inmigrants") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_6


# Map 9.7 - age of outmigrants - London
map9_7 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_outmig", style = "cont", palette = "viridis", title = "Mean age outmigrants") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_7



# Map 9.8 - difference in mean age - London
pal <- wes_palette("Zissou1", 100, type = "continuous")
map9_8 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_diff", style = "cont", palette = pal, title = "Difference mean age") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_8
