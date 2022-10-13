## Map of net migration by age over IMD 

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

#load packages
pacman::p_load(haven, 
               dplyr, 
               survey, 
               janitor,
               questionr, 
               epiDisplay, 
               rio, 
               ggplot2, 
               apyramid,
               magrittr, 
               stringr, 
               here, 
               aws.s3,
               readr,
               readxl, 
               reshape2)

# Load data
age_dta <- s3read_using(import, 
                        object = 'age_net_migration.csv', 
                        bucket = buck_clean) 

  # Downloaded from https://research.mysociety.org/sites/imd2019/about/#data
  # 2019 IMD, not 2011
imd_dta <- s3read_using(import, 
                        object = 'IMD_MSOA.csv', 
                        bucket = buck_data)  

# merge on to data
age_dta <- left_join(imd_dta, age_dta, by = c("MSOAC" = "geography_code"))

# rename columns
names(age_dta)<-str_replace_all(names(age_dta), c(" " = "." , "," = "" , ";" = "" , "/" = "" , ":" = ""))


# Keep only data needed
age_dta <- age_dta %>%
  dplyr::select(MSOAC, MSOARANK, IMD19.SCORE, MSOADECILE, ends_with("_netmigration"))

dim(age_dta)
  # 6791 MSOAs


# reshape to long for ggplot
age_dta <- melt(age_dta, id.vars = c("MSOAC", "MSOARANK", "IMD19.SCORE", "MSOADECILE"))
  
dim(age_dta)
  #20,373 MSOA-agegroup combinations
6791*3 # this is correct


  
# Scatter plot - MSOA rank
graph_11_1 <- ggplot(age_dta, aes(x=MSOARANK, y=value, color = variable, group = variable)) +
  geom_point(alpha=0.5, aes(color=variable), size=0.8, shape=21) +
  theme_light() +
  ylab("Net migration") +
  xlab("MSOA IMD rank") +
  scale_color_manual(values = c('#dd0031', '#00AFBB', "gold"), name= "", labels=c("0-34", "35-64", "65")) +
  geom_smooth(method = 'lm')
graph_11_1  
ggsave("graph11_1_netmigration_age_MSOArank.tiff")
put_object(
  file = 'graph11_1_netmigration_age_MSOArank.tiff', 
  object = 'outputs/graph11_1_netmigration_age_MSOArank.tiff',
  bucket = buck_main) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands
unlink("graph11_1_netmigration_age_MSOArank.tiff")


# Scatter plot - MSOA IMD index
graph_11_2 <- ggplot(age_dta, aes(x=IMD19.SCORE, y=value, color = variable, group = variable)) +
  geom_point(alpha=0.5, aes(color=variable), size=0.8, shape=21) +
  theme_light() +
  ylab("Net migration") +
  xlab("MSOA IMD score") +
  scale_color_manual(values = c('#dd0031', '#00AFBB', "gold"), name= "", labels=c("0-34", "35-64", "65")) +
  geom_smooth(method = 'lm')
graph_11_2
ggsave("graph11_2_netmigration_age_MSOAindex.tiff")
put_object(
  file = 'graph11_2_netmigration_age_MSOAindex.tiff', 
  object = 'outputs/graph11_2_netmigration_age_MSOAindex.tiff',
  bucket = buck_main) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands
unlink("graph11_2_netmigration_age_MSOAindex.tiff")



# Scatter plot - MSOA IMD decile
graph_11_3 <- ggplot(age_dta, aes(x=MSOADECILE, y=value, color = variable, group = variable)) +
  geom_point(alpha=0.5, aes(color=variable), size=0.8, shape=21) +
  theme_light() +
  ylab("Net migration") +
  xlab("MSOA IMD decile") +
  scale_color_manual(values = c('#dd0031', '#00AFBB', "gold"), name= "", labels=c("0-34", "35-64", "65")) +
  geom_smooth(method = 'lm')
graph_11_3
ggsave("graph11_3_netmigration_age_MSOAdecile.tiff")
put_object(
  file = 'graph11_3_netmigration_age_MSOAindex.tiff', 
  object = 'outputs/graph11_3_netmigration_age_MSOAdecile.tiff',
  bucket = buck_main) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands
unlink("graph11_3_netmigration_age_MSOAdecile.tiff")




