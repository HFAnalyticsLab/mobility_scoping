## Cluster analysis of residential mobility factors
      # Note: this is the initial script for the cluster analysis
      # the updated script can be found under script 4

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

library(ggbiplot)
library(tidyverse)
library(factoextra)
library(cluster)
library(aws.s3)
library(rio)
library(data.table)
library(devtools)
#install_github('vqv/ggbiplot')
library(stringr)


age <- s3read_using(FUN = fread,
                    object = 'age_net_migration_LA.csv',
                    bucket = buck_clean)[, .(geography,
                                              geography_code,
                                              under_34_netmigration,
                                              x35_to_64_netmigration,
                                              x65plus_netmigration)]

health <- s3read_using(FUN = fread,
                       object = 'health_net_migration_LA.csv',
                       bucket = buck_clean)[, .(geography,
                                                 geography_code,
                                                 netmigration_limlot,
                                                 netmigration_limlit,
                                                 netmigration_notlim)]

ea <- s3read_using(FUN = fread,
                   object = 'ea_with_students_net_migration_LAD_v2.csv',
                   bucket = buck_clean)[, .(geography,
                                             geography_code,
                                             ea_netmigration,
                                             ei_netmigration,
                                             student_netmigration)]


setdiff(health$geography_code, age$geography_code)
setdiff(age$geography_code, health$geography_code)

setdiff(health$geography_code, ea$geography_code)
setdiff(ea$geography_code, health$geography_code)

setdiff(ea$geography_code, age$geography_code)
setdiff(age$geography_code, ea$geography_code)

data <- age[health, on = 'geography_code'][ea, on = 'geography_code'][
  !is.na(geography), .(geography,
                       under_34_netmigration,
                       x35_to_64_netmigration,
                       x65plus_netmigration,
                       netmigration_limlot,
                       netmigration_limlit,
                       netmigration_notlim,
                       ea_netmigration,
                       ei_netmigration,
                       student_netmigration)] %>%
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
                 'students')

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

#<<<<<<< HEAD
#data$cluster <- fit$cluster

#cluster1 <- data[data$cluster == 2, -10]
## PCA
#pca_1 <- prcomp(cluster1, center = TRUE)
#summary(pca_1)
#str(pca_1)

#ggbiplot(pca_1)
#ggbiplot(pca_1, labels = rownames(cluster1))
#=======



# Map cluster classification

# load packages
pacman::p_load(sf,
               XML,
               tmap,
               devtools, 
               viridis, 
               wesanderson)

# import shp data
    # the shapefiles were downloaded from: https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2011-boundaries-ew-bfc/explore 
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
    cluster ==1 ~ "Outliers",
    cluster == 2 ~ "2- Not younger/healthier/students",
    cluster == 3 ~ "3- General low net migration", 
    cluster == 4 ~ "1- Younger/healthier/students"
  ))

tabyl(data$cluster)


# Save data
s3write_using(data # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'mobility_clusters_LA.csv' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above





# Join spatial data
lad_shp <- left_join(lad_shp, data, by = c("lad11nm" = "geography"))

tabyl(lad_shp$cluster)


# Map of age migration classification
pal <- c('#F39214', '#53a9cd' ,'#744284', '#dd0031')
map17_1 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "cluster_lab", style = "cat", palette = pal, title = "Mobility clusters") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map17_1




