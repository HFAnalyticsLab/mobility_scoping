#save script in bucket 
## save this script to that folder in the bucket
# 
# put_object(
#   file = '08_CRDC_data_prep.R',
#   object = '08_CDRC_data_prep.R',
#   bucket = buck_main
# )

#tidy lib 
rm(list=ls())

# run script with bucket names
source("0_file_pathways.R") 

pacman::p_load(tidyverse,
               sf,
               XML,
               tmap,
               viridis, 
               aws.s3,
               rio,
               janitor)

#Load LSOA shape file 

# Need to download all 6 files in folder for .shp to load correctly
save_object(object = 'LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp',
            file = here::here("shapefiles", "eng.shp"), 
            bucket = buck_data)
save_object(object = 'LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.cpg',
            file = here::here("shapefiles", "eng.cpg"), 
            bucket = buck_data)
save_object(object = 'LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.dbf',
            file = here::here("shapefiles", "eng.dbf"), 
            bucket = buck_data)
save_object(object = 'LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.prj',
            file = here::here("shapefiles", "eng.prj"), 
            bucket = buck_data)
save_object(object = 'LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shx',
            file = here::here("shapefiles", "eng.shx"), 
            bucket = buck_data)
save_object(object = 'LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.xml',
            file = here::here("shapefiles", "eng.xml"), 
            bucket = buck_data)

# load shp data
lsoa_shp <- st_read(here::here("shapefiles", "eng.shp"))

str(lsoa_shp)

#remove Wales
lsoa_shp <- lsoa_shp %>%
  subset(str_detect(LSOA11CD, '^E'))




#Prep CDRC data (only have LSOA or LAD)
#data source: https://data.cdrc.ac.uk/dataset/cdrc-residential-mobility-index
   #>cdrc residential mobility indext lsoa 1997-2020

cdrc_lsoa<-s3read_using(read_csv # Which function are we using to read
                        , object = 'CDRC_residential_mobility_index_LSOA.csv' # File to open
                        , bucket = buck_data) # Bucket name defined above

#have churn of every year from 1997-2019 compared to 2020
summary(cdrc_lsoa)


#drop 2020 (no data) and only include LSOA code

cdrc_lsoa_clean<-cdrc_lsoa %>% 
dplyr::select(-chn2020) %>% 
  filter(., grepl("E010", area))


#join dfs 

lsoa_shp <- left_join(lsoa_shp, cdrc_lsoa_clean, by = c("LSOA11CD" = "area"))

summary(lsoa_shp)
#Save dataset 

s3write_using(lsoa_shp # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'lsoa_cdrc.RDS' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above



# Prepare THF colour scheme
pal_THF <- c('#dd0031', '#53a9cd',  '#744284',  '#ffd412',   '#2a7979', '#ee9b90', '#0c402b', '#a6d7d3', '#005078', '#f39214', '#2ca365')
grDevices::palette(pal_THF)
# not sure what the palette command does


# Map 9.1 - map of net migration rate by age
map9_1 <- tm_shape(lsoa_shp) +
  tm_borders(,alpha=0) +
  tm_fill(col = "chn2011", style="cont", palette = "viridis", title = "Churn index 2011") +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
# +
#   tm_text(text="MSOA11NM")

map9_1



par(mfrow=c(2,2))

loop.vector<-11:33
vars<-list()
lsoa_shp_cdrc<-lsoa_shp[,11:33]

for (j in seq_along(loop.vector)) {
  vars[[j]]<-as.name(colnames(lsoa_shp_cdrc)[j])
}


map_churn<- function(data=lsoa_shp,var=col) {
tm_shape(data) +
    tm_borders(,alpha=0) +
    tm_fill(col = var, style="cont", palette = "viridis", title = paste0("Churn index", substr(var,4,7))) +
    tm_layout(legend.title.size = 0.8,
              legend.text.size = 0.6,
              legend.position = c("left","top"),
              legend.bg.color = "white",
              legend.bg.alpha = 1) 

}

g<-lapply(as.character(vars[1:length(vars)]),map_churn,data=lsoa_shp)


# Make maps for selected metro areas - London
lsoa_shp %>% 
  filter(substring(LSOA11CD, 2) < '01004681' & str_detect(LSOA11CD, 'E')) %>%
  tabyl(LSOA11NM)


# ldn_lsoa_shp <- lsoa_shp %>% 
#   dplyr::filter(, substring(LSOA11CD, 2) < '01004681' & str_detect(LSOA11CD, 'E')) 
# 
# 
# 
# # Map 4 - map of mobility categories - London
# map4 <- tm_shape(ldn_lsoa_shp) +
#   tm_borders(, alpha=0) +
#   tm_fill(col = "chn2011", style = "cont", palette = pal_THF, title = "Churn Index 2011") +
#   tm_borders(lwd = 0)  +
#   tm_layout(legend.title.size = 1,
#             legend.text.size = 0.6,
#             legend.position = c("right","top"),
#             legend.bg.color = "white",
#             legend.bg.alpha = 1)
# map4


##Work out 2010-2011 change 

cdrc_lsoa_clean %>% 
  select(chn2010,chn2011) %>% 
  mutate(diff_2010_2011=chn)



