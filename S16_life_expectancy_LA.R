# Map of life expectancy by LA 

# Housekeeping
# clear R environment
rm(list = ls())


#load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle,
               viridis, 
               wesanderson,
               aws.s3,
               tidyverse,
               rio,
               janitor,
               ggmap)


# import all data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig006
le_dta <- s3read_using(import, 
                       object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/life_expectancy_LA.xlsx') # File to open 

dim(EA_dta)      #55 variables, 16842 observations

# tidy variables names
le_dta<-le_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions

le_dta <- le_dta %>%
  dplyr::filter(x4 == "00-01" & x3 == "Female")

le_dta <- le_dta %>%
  dplyr::select("title", "life_expectancy_by_local_authority", x53)

le_dta$x53 <- as.numeric(le_dta$x53)

le_dta <- le_dta %>%
  mutate(quintile = cut(x53,
                        breaks = quantile(x53, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1, NA)),
                        labels = 1:5, na.rm = TRUE))




# load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle,
               devtools, 
               viridis, 
               wesanderson)

# import shp data
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.shp',
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.cpg',
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.dbf',
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.prj',
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/LAD_MAY_2021_UK_BFE_V2.shx',
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(May_2021)_UK_BFE_V3.xml',
            file = here::here("shapefiles", "eng.xml"))


# read LAD boundaries
lad_shp <- st_read(here::here("shapefiles", "eng.shp"))

str(lad_shp)

# Drop Scotland and Northern Ireland
lad_shp <- lad_shp %>%
  subset(str_detect(LAD21CD, 'E') | str_detect(LAD21CD, 'W'))

# replace name for Rhondda so it merges correctly
#rename first column
le_dta <- le_dta %>%
  mutate(title = replace(title, title == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))
le_dta <- le_dta %>%
  mutate(title = replace(title, title == "Folkestone and Hythe", "Shepway"))
le_dta <- le_dta %>%
  mutate(title = replace(title, title == "Vale of Glamorgan", "The Vale of Glamorgan"))


# Join spatial data
lad_shp <- left_join(lad_shp, le_dta, by = c("LAD21NM" = "title"))

tabyl(lad_shp$x53)



# Map of age migration classification
adjustcolor("#53a9cd", alpha.f = 0.5) #53A9CD80
adjustcolor("#e84245", alpha.f = 0.5) #E8424580
adjustcolor("#e84245", alpha.f = 0.75) #E84245BF


pal <- c('#53a9cd', '#53A9CD80', '#E8424580' ,'#E84245BF', '#dd0031')
map18_1 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "quintile", style = "cat", palette = pal, title = "Life expectancy quintile") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map18_1


# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(lad_shp # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'life_expectancy_for_Flourish.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

