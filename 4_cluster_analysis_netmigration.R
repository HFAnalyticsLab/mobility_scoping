## Cluster analysis of residential mobility factors

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

# load packages
pacman::p_load(tidyverse, # general data manipulation & cluster analysis
               factoextra,
               cluster,
               aws.s3,
               rio,
               data.table,
               devtools,
               ggbiplot,
               janitor,
               sf, # mapping and vis
               XML,
               tmap,
               devtools)
               
#install_github('vqv/ggbiplot') # only available on GitHub I believe

# load data on net migration by age (created in script 1)
age <- s3read_using(FUN = fread,
                    object = 'age_net_migration_LA.csv',
                    bucket = buck_clean)[, .(geography,
                                              geography_code,
                                              under_34_netmigration,
                                              x35_to_64_netmigration,
                                              x65plus_netmigration)]

# load data on net migration health status (created in script 2)
health <- s3read_using(FUN = fread,
                       object = 'health_net_migration_LA.csv',
                       bucket = buck_clean)[, .(geography,
                                                 geography_code,
                                                 netmigration_limlot,
                                                 netmigration_limlit,
                                                 netmigration_notlim)]

# load data on net migration by economic activity (created in script 3)
ea <- s3read_using(FUN = fread,
                   object = 'ea_with_students_net_migration_LAD_v2.csv',
                   bucket = buck_clean)[, .(geography,
                                             geography_code,
                                             ea_netmigration,
                                             ei_netmigration,
                                             student_netmigration)]

#load data on overall net migration in local authorities (created in script S17)
netmig <- s3read_using(FUN = fread,
                       object = 'net_migration_LAD.csv',
                       bucket = buck_clean)[, .(geography,
                                                 geography_code,
                                                 netmigration)]

# Create data frame with columns used in cluster analysis
data <- age[health, on = 'geography_code'][ea, on = 'geography_code'][netmig, on = 'geography_code'][
  !is.na(geography), .(geography,
                       under_34_netmigration,
                       x35_to_64_netmigration,
                       x65plus_netmigration,
                       netmigration_limlot,
                       netmigration_limlit,
                       netmigration_notlim,
                       ea_netmigration,
                       ei_netmigration,
                       student_netmigration,
                       netmigration)] %>%
  data.frame()

# Move local authority name to row names
row.names(data) <- data$geography
data$geography <- NULL

# scale net migration rates in each group
data <- scale(data) %>% data.frame()
names(data) <- c('younger',
                 'middle aged',
                 'retirement age',
                 'poorer health',
                 'reasonable health',
                 'healthy',
                 'economically active',
                 'economically inactive',
                 'students', 
                 'netmigration')

#set random seed (to obtain reproducible results)
set.seed(6524531)
# perform k means analysis, setting 4 clusters
fit <- kmeans(data, 4, nstart = 25)

# Merge clusters on to main data frame
data$cluster <- fit$cluster

# Describe highest and lowest scaled net migration rates in each of the four clusters
aggregate(data, by=list(cluster=fit$cluster), min)
aggregate(data, by=list(cluster=fit$cluster), max)

fit$size
fit$centers

# Plot clusters
fviz_cluster(fit, data) +
  theme_minimal() + theme(legend.position = 'none') +
  ggtitle('Cluster analyis at LA level - population mobility factors') +
  xlab('Dim 1: - EI and older group migration (36.1% var)') +
  ylab('Dim 2: - Healthy and younger group migration (32.8% var)')

## function to check within-groups sum of squares 
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(data) # check 'elbow' around 4 clusters


## Principal Components Analysis
## sense check and understand which factors are influencing the cluster creation
pca <- prcomp(data, center = TRUE)

ggbiplot(pca, groups = as.character(fit$cluster)) + 
  theme_minimal() +
  scale_color_manual(values=c('#dd0031', '#53a9cd', '#744284', '#F39214')) +
  theme(legend.position = 'none') +
  ggtitle('PCA analyis at LA level - population mobility factors') +
  xlab('PC1 - EI and older group migration') +
  ylab('PC2 - Healthy and younger group migration')

## y axis is about healthy migration and young migration (PC2 - 32.8% var) reversed
# x axis about economically inactive and older migration (PC1 - 36.1% var)

# Map cluster classification
  
# import shp data
#data were downloaded from https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(BDY_LAD%2CDEC_2011)
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shp',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.cpg',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.dbf',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.prj',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shx',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 'LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.xml',
            bucket = buck_data,
            file = here::here("shapefiles", "eng.xml"))

# read LAD boundaries
lad_shp <- st_read(here::here("shapefiles", "eng.shp"))

str(lad_shp)

# Drop Scotland and Northern Ireland
lad_shp <- lad_shp %>%
  subset(str_detect(lad11cd, '^E') | str_detect(lad11cd, '^W'))

# replace name for Rhondda so it merges correctly
#rename first column
data$geography <- row.names(data)

data <- data %>%
  mutate(geography = replace(geography, geography == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))
data <- data %>%
  mutate(geography = replace(geography, geography == "Folkestone and Hythe", "Shepway"))
data <- data %>%
  mutate(geography = replace(geography, geography == "Vale of Glamorgan", "The Vale of Glamorgan"))

# Edit cluster variable
data <- data %>% 
  mutate(cluster_lab = case_when(
    cluster ==2 ~ "Outliers",
    cluster == 4 ~ "Not younger/healthier/students",
    cluster == 1 ~ "General low net migration", 
    cluster == 3 ~ "Younger/healthier/students"
  ))

tabyl(data$cluster)


# Save data
s3write_using(data # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'mobility_clusters_LA_v2.csv' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above

# Join spatial data
lad_shp <- left_join(lad_shp, data, by = c("lad11nm" = "geography"))

tabyl(lad_shp$cluster)

# Map of residential mobility classification
map4_1 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "cluster_lab", style = "cat", palette =  c('#744284',  '#53a9cd' , '#dd0031', '#F39214'), title = "Mobility clusters") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map4_1
