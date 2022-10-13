## Checking comparability of mobility cluster classification with previous iterations

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

# load packages
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



# load data on residential mobility clusters (created in script 4)

data <- s3read_using(FUN = fread,
                    object = 'mobility_clusters_LA_v2.csv',
                    bucket = buck_clean)


# Merge with previous classification to check differences
data_v1 <- s3read_using(import, 
                        object = 'mobility_clusters_LA.csv', 
                        bucket = buck_clean) 

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
# only 8 LAs have different categories for both sets of cluster analysis

data <- data %>%
  mutate(diff = cluster != cluster_v1)

data %>%
  dplyr::filter(diff == 1) %>%
  dplyr::select('geography', starts_with('cluster')) %>%
  print()
# changed for LAs including Middlesbrough, Amber Valley, Padby and Wigston, North Kesteven, Hounslow, Wokingham, Powys


# second iteration
rm(list = ls())


# load data on residential mobility clusters (created in script 4)

data <- s3read_using(FUN = fread,
                     object = 'mobility_clusters_LA_v2.csv',
                     bucket = buck_clean)

