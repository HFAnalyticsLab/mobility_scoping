## Preliminary maps - Output Area level
# Run this script after 01_mobility_data_prep

# load packages
install.packages('BiocManager')
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle)

# read OA boundaries
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp' ## my bucket name

# attempt to import shp data
  ## command does not work
  ## error message: Error in parse_aws_s3_response(r, Sig, verbose = verbose) : 
  ## Forbidden (HTTP 403).

oa_shp <- s3read_using(st_read # Which function are we using to read
                        , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/OA_shapefile_data/Output_Areas__December_2011__Boundaries_EW_BFC.shp') # File to open
                      
                       
# original command in ODE
oa_shp <- st_read(here("data", "OA_shapefile_data", "Output_Areas__December_2011__Boundaries_EW_BFC.shp"))


str(oa_shp)

# Join spatial data
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
  # not sure what this command does (from the geographic data science practical)


# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
  # not sure what the palette command does


# Map 1 - Simple map of England and Wales
tm_shape(oa_shp_simple) +
  tm_fill(col = "ctry_nm", style = "cat", palette = pal_THF, title = "Country") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
  # OA level doesn't produce the most readable maps - unless manage to remove boundaries between polygons entirely
  # and takes much longer to produce OA level maps than LA level maps

# remove polygon borders
map1 <- tm_shape(oa_shp_simple) +
  tm_borders(, alpha=0) +
  tm_fill(col = "ctry_nm", style = "cat", palette = pal_THF, title = "Country") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1) 
map1
  # seems to have removed borders but can't figure out what the white areas are 

tmap_save(map1, here("outputs", "map1_EW_OAs.tiff"))


# Map 2 - map of mobility categories
map2 <- tm_shape(oa_shp_simple) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map2 


# Repeat Map 2 - map of mobility categories - without simplified boundaries
map2 <- tm_shape(oa_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "mob_cat", style = "cat", palette = pal_THF, title = "Mobility category") +
  tm_borders(lwd = 0)  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map2 
tmap_save(map2, here("outputs", "map2_mobility_OAs.tiff"))

  # should we remove Wales from the shp file? 
  # need to figure out how to make maps for selected metro areas

