## Mapping Levelling Up priority areas


# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

# Load packages
pacman::p_load(haven, 
               dplyr, 
               survey, 
               janitor,
               questionr, 
               epiDisplay, 
               rio, 
               ggplot2, 
               apyramid,
               magrittr, 
               stringr, 
               here)

# Import Levelling Up data
    #data were downloaded from https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjyroCDndj6AhWGUcAKHVAtCNsQFnoECAsQAQ&url=https%3A%2F%2Fassets.publishing.service.gov.uk%2Fgovernment%2Fuploads%2Fsystem%2Fuploads%2Fattachment_data%2Ffile%2F966137%2FLevelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx&usg=AOvVaw3NIQ1EBTwu0jLNSisnn3XT 
levup <-s3read_using(import # Which function are we using to read
                      , object = 'Levelling_Up_priority_areas/Levelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx' # File to open
                      , bucket = buck_data) # Bucket name defined above


dim(levup)
  # 368 LAs in file

# tidy column names
names(levup)<-str_replace_all(names(levup), c(" " = "." ))

tabyl(levup$Priority.category, show_missing_levels = T)


# replace name for Rhondda so it merges correctly
levup <- levup %>%
  mutate(Name = replace(Name, Name == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))


# load packages
pacman::p_load(sf,
               XML,
               tmap)

# read LAD boundaries from 2021 (to match Levelling Up boundaries)
# import shp data
      # Note: the shapefiles for May 2021 do not seem to be available anymore
      # Dec 2021 boundaries can be downloaded here: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2021-uk-bfe/explore
save_object(object = 'LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.shp',
            file = here::here("shapefiles", "eng.shp"),
            bucket = buck_data)
save_object(object = 'LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.cpg',
            file = here::here("shapefiles", "eng.cpg"),
            bucket = buck_data)
save_object(object = 'LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.dbf',
            file = here::here("shapefiles", "eng.dbf"),
            bucket = buck_data)
save_object(object = 'LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.prj',
            file = here::here("shapefiles", "eng.prj"),
            bucket = buck_data)
save_object(object = 'LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.shx',
            file = here::here("shapefiles", "eng.shx"),
            bucket = buck_data)
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(May_2021)_UK_BFE_V3.xml',
            file = here::here("shapefiles", "eng.xml"),
            bucket = buck_data)

lad_shp21 <- st_read(here::here("shapefiles", "eng.shp"))

str(lad_shp21)
  #374 LAs

# Join spatial data
lad_shp21 <- left_join(lad_shp21, levup, by = c("LAD21NM" = "Name"))
# Name is LA in the levup df and LAD21NM in the shapefile data

dim(lad_shp21)
  #374 LAs

# Drop Scotland and Northern Ireland
lad_shp21 <- lad_shp21 %>%
  subset(str_detect(LAD21CD, '^E') | str_detect(LAD21CD, '^W'))


tabyl(lad_shp21$Priority.category)
  # 2 with missing data
  
lad_shp21 %>%
  filter(is.na(Priority.category)) %>%
    tabyl(LAD21NM)
  # two remaining are North Northamptonshire and West Northamptonshire - collectively Northamptonshire 
      # Corby, East Northamptonshire, Kettering and Wellingborough (in levup) merged into North Northamptonshire (lad_shp)
      # Daventry, Northampton and South Northamptonshire merged into West Northamptonshire
      # difficult because the smaller LAs had different priority levels so unsure what the new value is - think about this
      # either use earlier LA boundaries, or assign a value to these
          # using mode for both larger areas - 1 for North Northamponshire and 2 for West Northamptonshire


# Impute priority category for North and West Northamptonshire
lad_shp21 <- lad_shp21 %>%
  mutate(Priority.category = replace(Priority.category, LAD21NM == "North Northamptonshire", "1"))
lad_shp21 <- lad_shp21 %>%
  mutate(Priority.category = replace( Priority.category, LAD21NM == "West Northamptonshire", "2"))
                                    


# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does


pal_THF_cont <- c('#dd0031', '#ee7375', '#f2a0a2')
grDevices::palette(pal_THF_cont)



# Map 6_1 - map of Levelling Up priority areas
map6_1 <- tm_shape(lad_shp21) +
  tm_borders(, alpha=0) +
  tm_fill(col = "Priority.category", style = "cat", palette = pal_THF_cont, title = "Levelling Up priority category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map6_1
s3write_using(map6_1 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map6_1_LevUp_priorityareas.tiff' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name defined above  



# Make maps for selected metro areas - London
ldn_lad_shp21 <- lad_shp21 %>% 
  dplyr::filter(, substring(LAD21CD, 1, 3) == 'E09' & str_detect(LAD21CD, 'E')) 



# Map 6_2 - map of mobility categories - London
map6_2 <- tm_shape(ldn_lad_shp21) +
  tm_borders(lwd=0) +
  tm_fill(col = "Priority.category", style = "cat", palette = pal_THF_cont, title = "Levelling Up priority category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map6_2
s3write_using(map6_2 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map6_2_LevUp_priorityareas_London.tiff' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name defined above  

