## Cluster analysis of residential mobility factors
## FC 10/08/2021

# clear R environment
rm(list = ls())

library(plyr)
library(dplyr)
library(tidyverse)
library(factoextra)
library(cluster)
library(aws.s3)
library(rio)
library(data.table)
library(devtools)
#install_github('vqv/ggbiplot')
library(ggbiplot)
library(stringr)
library(janitor)


data_bucket <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp'

age <- s3read_using(FUN = fread,
                    object = 'Francesca/mobility_scoping/data/clean/age_net_migration_LA.csv',
                    bucket = data_bucket)[, .(geography,
                                              geography_code,
                                              under_34_netmigration,
                                              x35_to_64_netmigration,
                                              x65plus_netmigration)]

health <- s3read_using(FUN = fread,
                       object = 'Francesca/mobility_scoping/data/clean/health_net_migration_LA.csv',
                       bucket = data_bucket)[, .(geography,
                                                 geography_code,
                                                 netmigration_limlot,
                                                 netmigration_limlit,
                                                 netmigration_notlim)]

ea <- s3read_using(FUN = fread,
                   object = 'Francesca/mobility_scoping/data/clean/ea_with_students_net_migration_LAD_v2.csv',
                   bucket = data_bucket)[, .(geography,
                                             geography_code,
                                             ea_netmigration,
                                             ei_netmigration,
                                             student_netmigration)]

netmig <- s3read_using(FUN = fread,
                       object = 'Francesca/mobility_scoping/data/clean/net_migration_LAD.csv',
                       bucket = data_bucket)[, .(geography,
                                                 geography_code,
                                                 netmigration)]

setdiff(health$geography_code, age$geography_code)
setdiff(age$geography_code, health$geography_code)

setdiff(health$geography_code, ea$geography_code)
setdiff(ea$geography_code, health$geography_code)

setdiff(ea$geography_code, age$geography_code)
setdiff(age$geography_code, ea$geography_code)

setdiff(netmig$geography_code, age$geography_code)
setdiff(netmig$geography_code, ea$geography_code)
setdiff(netmig$geography_code, health$geography_code)

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

row.names(data) <- data$geography
data$geography <- NULL

str(data)

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

set.seed(6524531)
fit <- kmeans(data, 4, nstart = 25)

data$cluster <- fit$cluster

clusplot(data, fit$cluster, color=TRUE, shade=TRUE, main = 'LSOA Cluster Analysis',
         labels=2, lines=0)


aggregate(data, by=list(cluster=fit$cluster), min)
aggregate(data, by=list(cluster=fit$cluster), max)

fit$size
fit$centers

fviz_cluster(fit, data) +
  theme_minimal() + theme(legend.position = 'none') +
  ggtitle('Cluster analyis at LA level - population mobility factors') +
  xlab('Dim 1: - EI and older group migration (36.1% var)') +
  ylab('Dim 2: - Healthy and younger group migration (32.8% var)')


fviz_nbclust(data, FUNcluster = kmeans)

wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(data)


## PCA
pca <- prcomp(data, center = TRUE)
summary(pca)
str(pca)

ggbiplot(pca)
ggbiplot(pca, labels = rownames(data))
ggbiplot(pca, groups = as.character(fit$cluster)) + 
  theme_minimal() +
  scale_color_manual(values=c('#dd0031', '#53a9cd', '#744284', '#F39214')) +
  theme(legend.position = 'none') +
  ggtitle('PCA analyis at LA level - population mobility factors') +
  xlab('PC1 - EI and older group migration') +
  ylab('PC2 - Healthy and younger group migration')
ggbiplot(pca, groups = as.character(fit$cluster), labels = rownames(data)) + 
  theme_minimal() +
  theme(legend.position = 'none') +
  ggtitle('PCA analyis at LA level - population mobility factors') +
  xlab('PC1 - ei and older group migration') +
  ylab('PC2 - healthy and younger group migration')

## y axis is about healthy migration and young migration (PC2 - 32.8% var) reversed
# x axis about economically inactive and older migration (PC1 - 36.1% var)

ggbiplot(pca, choices = c(3, 4))
ggbiplot(pca, labels = rownames(data), choices = c(3, 4))
ggbiplot(pca, groups = as.character(fit$cluster), choices = c(3, 4)) + 
  theme_minimal() +
  theme(legend.position = 'none') +
  ggtitle('PCA analyis at LA level - population mobility factors') +
  xlab('PC3 - student vs EA migration') +
  ylab('PC4 - Middle aged vs younger group migration')
ggbiplot(pca, groups = as.character(fit$cluster), labels = rownames(data), choices = c(3, 4)) + 
  theme_minimal() +
  theme(legend.position = 'none') +
  ggtitle('PCA analyis at LA level - population mobility factors') +
  xlab('PC3 - ei and older group migration') +
  ylab('PC4 - healthy and younger group migration')

<<<<<<< HEAD
data$cluster <- fit$cluster

cluster1 <- data[data$cluster == 2, -10]
## PCA
pca_1 <- prcomp(cluster1, center = TRUE)
summary(pca_1)
str(pca_1)

ggbiplot(pca_1)
ggbiplot(pca_1, labels = rownames(cluster1))
=======
  
  
  
  # Map cluster classification
  
  # load packages
  pacman::p_load(sf,
                 XML,
                 tmap,
                 THFstyle,
                 devtools, 
                 viridis, 
                 wesanderson)

# import shp data
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shp',
            file = here::here("shapefiles", "eng.shp"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.cpg',
            file = here::here("shapefiles", "eng.cpg"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.dbf',
            file = here::here("shapefiles", "eng.dbf"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.prj',
            file = here::here("shapefiles", "eng.prj"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shx',
            file = here::here("shapefiles", "eng.shx"))
save_object(object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/LAD_shapefile_data/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.xml',
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
    cluster ==3 ~ "Outliers",
    cluster == 4 ~ "Not younger/healthier/students",
    cluster == 2 ~ "General low net migration", 
    cluster == 1 ~ "Younger/healthier/students"
  ))

tabyl(data$cluster)


# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(data # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'mobility_clusters_LA_v2.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above





# Join spatial data
lad_shp <- left_join(lad_shp, data, by = c("lad11nm" = "geography"))

tabyl(lad_shp$cluster)


# Map of age migration classification
map17_1 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "cluster_lab", style = "cat", palette =  c('#744284',  '#53a9cd' , '#dd0031', '#F39214'), title = "Mobility clusters") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map17_1



# Merge with previous classification to check differences
data_v1 <- s3read_using(import, 
                       object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean/mobility_clusters_LA.csv') # File to open 

data_v1 <- data_v1 %>%
  rename( cluster_v1 = cluster) %>%
  rename(cluster_lab_v1 = cluster_lab) %>%
  dplyr::select('geography', 'cluster_v1', 'cluster_lab_v1')


data <- left_join(data_v1, data, by = c("geography" = "geography"))

tabyl(data, cluster, cluster_v1)

#change coding so they match in both data sources
data <- data %>%
  mutate(cluster_v1 = case_when(
    cluster_v1 == 1 ~ 3,
    cluster_v1 == 2 ~ 4,
    cluster_v1 == 3 ~ 2,
    cluster_v1 == 4 ~ 1
  ))

tabyl(data, cluster, cluster_v1)
# only 7 LAs have different categories for both sets of cluster analysis

data <- data %>%
  mutate(diff = cluster != cluster_v1)

data %>%
  dplyr::filter(diff == 1) %>%
  dplyr::select('geography', starts_with('cluster')) %>%
  print()
    # changed for middlesbrough, Amber Valley, Padby and Wigston, North Kesteven, Hounslow, Wokingham, Powys



#Merge with life expectancy data
## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig006
le_dta <- s3read_using(import, 
                       object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/life_expectancy_LA.xlsx') # File to open 

dim(le_dta)      #55 variables, 16842 observations

# tidy variables names
le_dta<-le_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions


le_dta <- le_dta %>%
  dplyr::filter(x4 == "00-01")

le_dta <- le_dta %>%
  dplyr::select("title", "life_expectancy_by_local_authority", x3, le_2010_12 = x32, le_2017_19 = x53)

le_dta$le_2010_12 <- as.numeric(le_dta$le_2010_12)
le_dta$le_2017_19 <- as.numeric(le_dta$le_2017_19)

le_dta <- le_dta %>%
  pivot_wider(names_from = x3, values_from = c(le_2010_12, le_2017_19))



# Drop Scotland and Northern Ireland
le_dta <- le_dta %>%
  subset(str_detect(life_expectancy_by_local_authority, '^E') | str_detect(life_expectancy_by_local_authority, '^W'))

le_dta <- left_join(le_dta, data, by = c("title" = "geography"))

tabyl(le_dta$cluster)
# 13% missing data due to change in LA boundaries between 2020 and 2011




# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(le_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'mobility_clusters_le_LA.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




#Merge with 2012 life expectancy data to minimise missing values
## data were downloaded from: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/lifeexpectancyatbirthandatage65bylocalareasintheunitedkingdomtable1ukandlocalareasinenglandandwales
le2012_dta <- s3read_using(import, 
                           object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/life_expectancy_LA_2012_prep.xlsx') # File to open 

dim(le2012_dta)      #7 variables, 449 observations

# tidy variables names
le2012_dta<-le2012_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions

# drop rows with missing values (formatting)
le2012_dta <- le2012_dta %>%
  drop_na(female_2009_2011) %>%
  dplyr::select(-area_name) 

le2012_dta$x3 <- gsub(" UA", "" , le2012_dta$x3)


le2012_dta<- le2012_dta %>%
  mutate(male_2009_2011 = round(as.numeric(male_2009_2011), 1), 
         male_2010_2012 = round(as.numeric(male_2010_2012), 1),
         female_2009_2011 = round(as.numeric(female_2009_2011), 1),
         female_2010_2012 = round(as.numeric(female_2010_2012), 1))

tabyl(le2012_dta$male_2009_2011)
# only 2 LAs with missing data


# merge onto data df
data <- left_join(data, le2012_dta, by = c("geography" = "x3"))
tabyl(data$male_2009_2011)


# remove weight variables and re-merge original net migration variables
data <- data %>%
  dplyr::select(cluster:female_2010_2012)

data <- left_join(data, health, by = c("geography" = "geography"))
data <- left_join(data, age, by = c("geography" = "geography"))
data <- left_join(data, ea, by = c("geography" = "geography"))

data <- data %>%
  dplyr::select(-geography_code.x, -geography_code.y, -geography_code)



# Save data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/clean' ## my bucket name

s3write_using(data # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'mobility_clusters_le2012_LA.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

