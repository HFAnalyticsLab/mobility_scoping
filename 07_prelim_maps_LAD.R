## Preliminary maps - Local Authority level
# Run this script after 01_mobility_data_prep and 06_LevellingUp_areas

# install HatchedPolygons for overlaid map
devtools::install_github("statnmap/HatchedPolygons")


# NOTE: Census migration data not available at LSOA level
# we can reconstruct it using lookup tables

# Prepare lookup table data
lookup <- import(here("data", "PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv"))

# drop columns don't need
lookup <- lookup %>% 
  dplyr::select(OA11CD, LAD11CD) 

# merge on to eng_dta
eng_dta <- left_join(lookup, eng_dta, by = c("OA11CD" = "geography"))

# keep only unique rows
dim(eng_dta)
eng_dta <- eng_dta %>% 
  distinct() 

# generate columns for LSOA level
eng_dta <- eng_dta %>%
  group_by(LAD11CD) %>%
  mutate(n_usualres_lad = sum(n_usualres),
         n_totalmig_lad = sum(n_totalmig),
         n_outmig_lad = sum(n_outmig),
         n_miguk_lad = sum(n_miguk),
         n_migfor_lad = sum(n_migfor))

# drop variables related to OAs and deduplicate rows
eng_dta <- eng_dta  %>% 
  dplyr::select(LAD11CD, n_usualres_lad, n_totalmig_lad, n_outmig_lad, n_miguk_lad, n_migfor_lad)
eng_dta <- eng_dta %>% 
  distinct() 

# drop Wales rows
eng_dta <- eng_dta %>%
  subset(str_detect(LAD11CD, 'E'))

dim(eng_dta)


# recreate variables needed for maps
# net migration
eng_dta <- eng_dta %>% 
  mutate(net_migration = (n_totalmig_lad - n_outmig_lad)/(n_usualres_lad + n_outmig_lad - n_totalmig_lad)*100)
summary(eng_dta$net_migration)  
#median net migration = 0.9% (a little higher than OAs)

sum(!is.na(eng_dta$net_migration)) #326
sum(eng_dta$net_migration <= 0, na.rm = TRUE) #15
15/326 #0.5% 
sum(eng_dta$net_migration < -1, na.rm = TRUE) #0

sum(eng_dta$net_migration > 5, na.rm = TRUE) #6
sum(eng_dta$net_migration > 2, na.rm = TRUE) #6


# turnover
eng_dta <- eng_dta %>% 
  mutate(turnover = (n_miguk_lad + n_migfor_lad)/n_usualres_lad*100)
summary(eng_dta$turnover)
#median turnover = 10.4% 

# Create variable with mobility categories
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= 0 ~ "Decreasing <0%",
    net_migration > 2 ~ "Increasing >2%",
    net_migration > 0 & net_migration <= 2 & turnover >= 10.4 ~ "Stable, high turnover",
    net_migration > 0 & net_migration <= 2 & turnover <10.4 ~ "Stable, low turnover"
  ))

tabyl(eng_dta$mob_cat)



# load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle,
               HatchedPolygons, 
               rmapshaper)

# read LAD boundaries
lad_shp <- st_read(here("data", "LAD_shapefile_data", "Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shp"))

str(lad_shp)

# Drop Scotland and Northern Ireland
lad_shp <- lad_shp %>%
  subset(str_detect(lad11cd, 'E') | str_detect(lad11cd, 'W'))


# Join spatial data
#NOTE: need to download census mobility data at LSOA level

lad_shp <- left_join(lad_shp, eng_dta, by = c("lad11cd" = "LAD11CD"))


# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does



# Map 6 - map of mobility categories 
map6 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map6
tmap_save(map6, here("outputs", "map6_mobility_LADs.tiff"))




# Make maps for selected metro areas - London
ldn_lad_shp <- lad_shp %>% 
  dplyr::filter(, substring(lad11cd, 1, 3) == 'E09' & str_detect(lad11cd, 'E')) 



# Map 4 - map of mobility categories - London
map6b <- tm_shape(ldn_lad_shp) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map6b
tmap_save(map6b, here("outputs", "map4_mobility_LADs_London.tiff"))



# Overlay Levelling Up priority areas

# overlay map with two sets of colours 
  # Levelling up category semi-transparent (alpha = 0.5)

map7 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) +
  tm_shape(lad_shp21) +
  tm_borders(, alpha=0) +
  tm_fill(col = "Priority.category", style = "cat", palette = pal_THF_cont, alpha = 0.5, title = "Priority category")
map7


# Use HatchedPolygons to get different texture
lad_shp21_islands = ms_filter_islands(lad_shp21, min_area = 10000000000)
lad21.hatch<-hatched.SpatialPolygons(lad_shp21_islands,density= c(40,60), angle=c(45, 135))
proj4string(lad21.hatch)<-proj4string(lad_shp21_islands)


map7 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) +
  tm_shape(lad21.hatch) +
  tm_fill(col = "Priority.category", style = "cat", palette = pal_THF_cont, alpha = 0.5, title = "Priority category")
map7


# Cross tab residential mobility by Levelling Up priority area
lad_shp <- left_join(lad_shp, levup, by = c("lad11nm" = "Name"))

dim(lad_shp)

lad_shp %>%
  tabyl(Priority.category, mob_cat)
#Not working, adding geometry within priority category

lad_shp %>%
  filter(Priority.category == 1) %>%
  tabyl(mob_cat)


lad_shp %>%
  filter(Priority.category == 2) %>%
  tabyl(mob_cat)

lad_shp %>%
  filter(Priority.category == 3) %>%
  tabyl(mob_cat)
