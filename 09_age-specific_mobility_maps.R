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
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping' ## my bucket name

age_dta<-s3read_using(readRDS # Which function are we using to read
                      , object = 'data/clean/eng_age_dta_MSOA.RDS' # File to open
                      , bucket = buck) # Bucket name defined above

# Join spatial data
msoa_shp <- left_join(msoa_shp, age_dta, by = c("MSOA11CD" = "geography_code"))
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
s3write_using(map9_1 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_1_meanage_nonmovers.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands



# Map 9.2 - map of mean age among in-migrants
map9_2 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_inmig", style = "cont", palette = "viridis", title = "Mean age in-migrants") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_2
s3write_using(map9_2 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_2_meanage_inmigs.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands



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
s3write_using(map9_3 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_3_meanage_outmigs.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


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
s3write_using(map9_4 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_4_meanage_diffinoutmig.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


# Map 9.5 - map of mean age of usual residents in 2010
map9_5 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_usualres10", style = "cont", palette = "viridis", title = "Mean age - usual residents in 2010") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_5
s3write_using(map9_5 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_5_meanage_usualres10.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


# Map 9.6 - map of mean age of usual residents in 2011
map9_6 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_usualres11", style = "cont", palette = "viridis", title = "Mean age - usual residents in 2011") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_6
s3write_using(map9_6 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_6_meanage_usualres11.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


# Map 9.7 - map of difference in mean age 2010-11
map9_7 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "diff_2010_11", style = "cont", palette = pal, title = "Mean age diff. - 2010/11") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_7
s3write_using(map9_7 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_7_meanage_diff2010-11.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


# Map 9.8 - map of difference in mean age - categorical 2010/11
map9_8 <- tm_shape(msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "diff_2010_11_cat", style = "cont", palette = "viridis" , title = "Mean age diff. - 2010/11") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_8
s3write_using(map9_8 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_8_meanage_diff2010-11_cat.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands





# Make maps for selected metro areas - London
msoa_shp %>% 
  filter(substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) %>%
  tabyl(MSOA11NM)

ldn_msoa_shp <- msoa_shp %>% 
  dplyr::filter(, substring(MSOA11CD, 2) < '02000983' & str_detect(MSOA11CD, 'E')) 



# Map 9.9 - age of non-movers - London
map9_9 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_samead", style = "cont", palette = "viridis", title = "Mean age non-movers") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_9


# Map 9.10 - age of inmigrants - London
map9_10 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_inmig", style = "cont", palette = "viridis", title = "Mean age inmigrants") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_10


# Map 9.11 - age of outmigrants - London
map9_11 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_outmig", style = "cont", palette = "viridis", title = "Mean age outmigrants") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_11



# Map 9.12 - difference in mean age - London
map9_12 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_diff", style = "cont", palette = pal, title = "Difference mean age") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","bottom"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_12



# Map 9.13 - map of mean age of usual residents in 2010
map9_13 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_usualres10", style = "cont", palette = "viridis", title = "Mean age - usual residents in 2010") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_13
s3write_using(map9_13 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_13_meanage_usualres10_ldn.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


# Map 9.14 - map of mean age of usual residents in 2011
map9_14 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "meanage_usualres11", style = "cont", palette = "viridis", title = "Mean age - usual residents in 2011") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_14
s3write_using(map9_14 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_14_meanage_usualres11_ldn.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


# Map 9.15 - map of difference in mean age 2010-11
map9_15 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "diff_2010_11", style = "cont", palette = pal, title = "Mean age diff. - 2010/11") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_15
s3write_using(map9_15 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_15_meanage_diff2010-11_ldn.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands


# Map 9.16 - map of difference in mean age - categorical 2010/11
map9_16 <- tm_shape(ldn_msoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "diff_2010_11_cat", style = "cont", palette = "viridis" , title = "Mean age diff. - 2010/11") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map9_16
s3write_using(map9_16 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map9_16_meanage_diff2010-11_cat_ldn.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands

