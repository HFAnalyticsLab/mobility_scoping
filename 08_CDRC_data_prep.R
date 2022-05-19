#save script in bucket 
## save this script to that folder in the bucket
# buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/' ## my bucket name
# 
# put_object(
#   file = '08_CRDC_data_prep.R',
#   object = '08_CDRC_data_prep.R',
#   bucket = buck
# )


#Load LSOA shape file 


# Need to download all 6 files in folder for .shp to load correctly
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp',
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.cpg',
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.dbf',
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.prj',
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shx',
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LSOA_shapefile_data/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.xml',
            file = here::here("shapefiles", "eng.xml"))

# load shp data
lsoa_shp <- st_read(here::here("shapefiles", "eng.shp"))

str(lsoa_shp)


#Prep CDRC data (only have LSOA or LAD)

buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data' ## my bucket name


cdrc_lsoa<-s3read_using(read_csv # Which function are we using to read
                        , object = 'CDRC_residential_mobility_index_LSOA.csv' # File to open
                        , bucket = buck) # Bucket name defined above

#have churn of every year from 1997-2019 compared to 2020
summary(cdrc_lsoa)






