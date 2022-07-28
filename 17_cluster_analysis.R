## Cluster analysis of residential mobility factors
## JH 26/07/2021

library(tidyverse)
library(factoextra)
library(cluster)
library(aws.s3)
library(rio)
library(data.table)
library(devtools)
#install_github('vqv/ggbiplot')
library(ggbiplot)


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

