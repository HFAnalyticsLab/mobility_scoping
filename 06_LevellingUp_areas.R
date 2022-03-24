## Mapping Levelling Up priority areas

# Load packages
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
               here)

# Import data
levup <- import(here("data", "Levelling_Up_priority_areas", "Levelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx"))

dim(levup)
  # 368 LAs in file

# tidy column names
names(levup)<-str_replace_all(names(levup), c(" " = "." ))

tabyl(levup$Priority.category)


# replace name for Rhondda so it merges correctly
levup <- levup %>%
  mutate(Name = replace(Name, Name == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))


# load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle)

# read OA boundaries
lad_shp21 <- st_read(here("data", "LAD_shapefile_data", "LAD_MAY_2021_UK_BFE_V2.shp"))

str(lad_shp21)
  #374 LAs

# Join spatial data
lad_shp21 <- left_join(lad_shp21, levup, by = c("LAD21NM" = "Name"))
# Name is LA in the levup df and LAD21NM in the shapefile data

dim(lad_shp21)
  #374 LAs

# Drop Scotland and Northern Ireland
lad_shp21 <- lad_shp21 %>%
  subset(str_detect(LAD21CD, 'E') | str_detect(LAD21CD, 'W'))


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



# Map 5 - map of Levelling Up priority areas
map5 <- tm_shape(lad_shp21) +
  tm_borders(, alpha=0) +
  tm_fill(col = "Priority.category", style = "cat", palette = pal_THF_cont, title = "Levelling Up priority category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map5
tmap_save(map5, here("outputs", "map5_LevUp_priorityareas.tiff"))




# Make maps for selected metro areas - London
ldn_lad_shp21 <- lad_shp21 %>% 
  dplyr::filter(, substring(LAD21CD, 1, 3) == 'E09' & str_detect(LAD21CD, 'E')) 



# Map 4 - map of mobility categories - London
map5b <- tm_shape(ldn_lad_shp21) +
  tm_borders(lwd=0) +
  tm_fill(col = "Priority.category", style = "cat", palette = pal_THF_cont, title = "Levelling Up priority category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map5b
tmap_save(map5b, here("outputs", "map4_LevUp_priorityareas_London.tiff"))

