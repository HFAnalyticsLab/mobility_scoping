## Preliminary maps - Lower Super Output Area level
# Run this script after 01_mobility_data_prep


  # NOTE: Census migration data not available at LSOA level
    # we can reconstruct it using lookup tables
  
# Prepare lookup table data
lookup <- import(here("data", "PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv"))

# drop columns don't need
lookup <- lookup %>% 
  dplyr::select(OA11CD, LSOA11CD) 

# merge on to eng_dta
eng_dta <- left_join(lookup, eng_dta, by = c("OA11CD" = "geography"))

# keep only unique rows
dim(eng_dta)
eng_dta <- eng_dta %>% 
  distinct() 

# generate columns for LSOA level
eng_dta <- eng_dta %>%
  group_by(LSOA11CD) %>%
  mutate(n_usualres_lsoa = sum(n_usualres),
         n_totalmig_lsoa = sum(n_totalmig),
         n_outmig_lsoa = sum(n_outmig),
         n_miguk_lsoa = sum(n_miguk),
         n_migfor_lsoa = sum(n_migfor))

# drop variables related to OAs and deduplicate rows
eng_dta <- eng_dta  %>% 
  dplyr::select(LSOA11CD, n_usualres_lsoa, n_totalmig_lsoa, n_outmig_lsoa, n_miguk_lsoa, n_migfor_lsoa)
eng_dta <- eng_dta %>% 
  distinct() 

# drop Wales rows
eng_dta <- eng_dta %>%
  subset(str_detect(LSOA11CD, 'E'))

dim(eng_dta)


# recreate variables needed for maps
# net migration
eng_dta <- eng_dta %>% 
  mutate(net_migration = (n_totalmig_lsoa - n_outmig_lsoa)/(n_usualres_lsoa + n_outmig_lsoa - n_totalmig_lsoa)*100)
summary(eng_dta$net_migration)  
#median net migration = 0.5% (similar to OAs)

sum(!is.na(eng_dta$net_migration)) #32844
sum(eng_dta$net_migration <= -5, na.rm = TRUE) #138
138/32844 #0.4% 
sum(eng_dta$net_migration < -2, na.rm = TRUE) #2974
2974/32844 #9.1% 

# turnover
eng_dta <- eng_dta %>% 
  mutate(turnover = (n_miguk_lsoa + n_migfor_lsoa)/n_usualres_lsoa*100)
summary(eng_dta$turnover)
#median turnover = 9.6% 

# Create variable with mobility categories
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= -2 ~ "Decreasing",
    net_migration > 5 ~ "Increasing >5%",
    net_migration > -2 & net_migration <= 5 & turnover >= 9.6 ~ "Stable, high turnover",
    net_migration > -5 & net_migration <= 5 & turnover <9.6 ~ "Stable, low turnover"
  ))

tabyl(eng_dta$mob_cat)



# load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle)

# read OA boundaries
lsoa_shp <- st_read(here("data", "LSOA_shapefile_data", "Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp"))

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
map3 <- tm_shape(lsoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map3
tmap_save(map3, here("outputs", "map3_mobility_LSOAs.tiff"))




# Make maps for selected metro areas - London
lsoa_shp %>% 
  filter(substring(LSOA11CD, 2) < '01004681' & str_detect(LSOA11CD, 'E')) %>%
 tabyl(LSOA11NM)
  

ldn_lsoa_shp <- lsoa_shp %>% 
  dplyr::filter(, substring(LSOA11CD, 2) < '01004681' & str_detect(LSOA11CD, 'E')) 



# Map 4 - map of mobility categories - London
map4 <- tm_shape(ldn_lsoa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map4
tmap_save(map4, here("outputs", "map4_mobility_LSOAs_London.tiff"))


