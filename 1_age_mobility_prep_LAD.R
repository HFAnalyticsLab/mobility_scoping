## Preparing area-level mobility data, breakdown by age - by Local Authority District

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

#load packages
pacman::p_load(haven, 
               dplyr, 
               janitor,
               epiDisplay, 
               rio, 
               stringr, 
               here, 
               aws.s3)


## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig001
      # download > local authorities: district / unitary (prior to April 2015)
age_dta <- s3read_using(import, 
                        object = 'censusmig_age_sex_LAD.csv', 
                        bucket = buck_data) # File to open 

dim(age_dta)      #303 variables, 406 observations


# tidy variable names
age_dta<-age_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions

# Drop Scotland and Northern Ireland
age_dta <- age_dta %>%
  subset(str_detect(geography_code, '^E') | str_detect(geography_code, '^W'))

dim(age_dta)      #303 variables, 348 observations


# renaming columns for same address (n_samead), moved within the local authority (n_movedwithin), inmigrants (n_inmig) and outmigrants (n_outmig)
    # in each age group
names(age_dta)[4:10]<-c('n_samead', 'n_movedwithin' , 'n_inmig','x','y','z','n_outmig')
names(age_dta)[14:20]<-c('n_samead_0_4', 'n_movedwithin_0_4' , 'n_inmig_0_4','x_0_4', 'y_0_4','z_0_4','n_outmig_0_4')
names(age_dta)[24:30]<-c('n_samead_5_15', 'n_movedwithin_5_15' , 'n_inmig_5_15','x_5_15', 'y_5_15','z_5_15','n_outmig_5_15')
names(age_dta)[34:40]<-c('n_samead_16_19', 'n_movedwithin_16_19' , 'n_inmig_16_19','x_16_19','y_16_19', 'z_16_19','n_outmig_16_19')
names(age_dta)[44:50]<-c('n_samead_20_24', 'n_movedwithin_20_24' , 'n_inmig_20_24','x_20_24','y_20_24','z_20_24','n_outmig_20_24')
names(age_dta)[54:60]<-c('n_samead_25_34', 'n_movedwithin_25_34' , 'n_inmig_25_34','x_25_34', 'y_25_34','z_25_34','n_outmig_25_34')
names(age_dta)[64:70]<-c('n_samead_35_49', 'n_movedwithin_35_49' , 'n_inmig_35_49','x_35_49','y_35_49','z_35_49','n_outmig_35_49')
names(age_dta)[74:80]<-c('n_samead_50_64', 'n_movedwithin_50_64' , 'n_inmig_50_64','x_50_64','y_50_64','z_50_64','n_outmig_50_64')
names(age_dta)[84:90]<-c('n_samead_65_74', 'n_movedwithin_65_74' , 'n_inmig_65_74','x_65_74','y_65_74','z_65_74','n_outmig_65_74')
names(age_dta)[94:100]<-c('n_samead_75p', 'n_movedwithin_75p' , 'n_inmig_75p','x_75p','y_75p','z_75p','n_outmig_75p')


# Create variables for usual residents in 2010 and 2011, and midyear population in each Local Authority
    #n_usualres11 = n_samead + n_movedwithin + n_inmig
    #n_usualres10 = n_movedwithin + n_samead + n_outmig 
    #mid_yearpop = (n_usualres10 + n_usualres11)/2


ages <- c("0_4", "5_15", "16_19", "20_24", "25_34", "35_49", "50_64", "65_74", "75p")

for (age in ages){
  age_dta <- age_dta %>%
    mutate( !!paste0("n_usualres10_", age) := !!as.name(paste0("n_samead_", age)) + !!as.name(paste0("n_outmig_", age)) + !!as.name(paste0("n_movedwithin_", age)))
  
  age_dta <- age_dta %>%
    mutate( !!paste0("n_usualres11_", age) := !!as.name(paste0("n_samead_", age)) + !!as.name(paste0("n_inmig_", age)) + !!as.name(paste0("n_movedwithin_", age)))

  age_dta <- age_dta %>%
    mutate( !!paste0("n_midyrpop_", age) := (!!as.name(paste0("n_usualres10_", age)) + !!as.name(paste0("n_usualres11_", age))) / 2)
}  

age_dta <- age_dta %>%
  mutate(n_usualres10 = n_samead + n_outmig + n_movedwithin,
         n_usualres11 = n_samead + n_inmig + n_movedwithin,
         n_midyrpop = (n_usualres10 + n_usualres11)/2)

# construct variables for three main age groups used in analysis (0-35, 35-64, 65+)

under_34<-c("0_4", "5_15", "16_19","20_24", "25_34")
x35_to_64<-c("35_49","50_64")
x65plus<-c("65_74","75p")


age_dta <- age_dta %>% 
  dplyr::select(c("geography", "geography_code", contains(c("n_samead", "inmig","outmig", "movedwithin")))) %>% 
  rowwise() %>% 
  mutate(under_34_samead=sum(c_across(contains(paste0("n_samead_", under_34)))),
         x35_to_64_samead=sum(c_across(contains(paste0("n_samead_", x35_to_64)))),
         x65plus_samead=sum(c_across(contains(paste0("n_samead_", x65plus))))) %>% 
  mutate(under_34_inmig=sum(c_across(contains(paste0("n_inmig_", under_34)))),
         x35_to_64_inmig=sum(c_across(contains(paste0("n_inmig_", x35_to_64)))),
         x65plus_inmig=sum(c_across(contains(paste0("n_inmig_", x65plus))))) %>% 
  mutate(under_34_n_movedwithin=sum(c_across(contains(paste0("n_movedwithin_", under_34)))),
         x35_to_64_n_movedwithin=sum(c_across(contains(paste0("n_movedwithin_", x35_to_64)))),
         x65plus_n_movedwithin=sum(c_across(contains(paste0("n_movedwithin_", x65plus))))) %>%
  mutate(under_34_outmig=sum(c_across(contains(paste0("n_outmig_", under_34)))),
         x35_to_64_outmig=sum(c_across(contains(paste0("n_outmig_", x35_to_64)))),
         x65plus_outmig=sum(c_across(contains(paste0("n_outmig_", x65plus))))) %>% 
  mutate(under_34_usualres11=sum(c_across(c("under_34_samead","under_34_n_movedwithin", "under_34_inmig"))),
         x35_to_64_usualres11=sum(c_across(c("x35_to_64_samead","x35_to_64_n_movedwithin", "x35_to_64_inmig"))),
         x65plus_usualres11=sum(c_across(c("x65plus_samead","x65plus_n_movedwithin", "x65plus_inmig")))) %>% 
  mutate(under_34_usualres10=sum(c_across(c("under_34_samead","under_34_n_movedwithin", "under_34_outmig"))),
         x35_to_64_usualres10=sum(c_across(c("x35_to_64_samead","x35_to_64_n_movedwithin", "x35_to_64_outmig"))),
         x65plus_usualres10=sum(c_across(c("x65plus_samead","x65plus_n_movedwithin", "x65plus_outmig")))) %>% 
  mutate(under_34_midyearpop=sum((under_34_usualres10+under_34_usualres11)/2),
         x35_to_64_midyearpop=sum((x35_to_64_usualres10+x35_to_64_usualres11)/2),
         x65plus_midyearpop=sum((x65plus_usualres10+x65plus_usualres11)/2)) %>% 
  dplyr::select(c("geography", "geography_code",contains(c("under_34","x35_to_64","x65plus"))))




# Calculate net migration for each age group
    #net_migration= (n_inmig-n_outmig)/(mid_yearpop)*100

age_dta<-age_dta %>% 
  mutate(under_34_netmigration=(under_34_inmig-under_34_outmig)/(under_34_midyearpop)*100,
         x35_to_64_netmigration=(x35_to_64_inmig-x35_to_64_outmig)/(x35_to_64_midyearpop)*100,
         x65plus_netmigration=(x65plus_inmig-x65plus_outmig)/(x65plus_midyearpop)*100)


# Classify local authorities based on over/under migration in each age group across LAs -----------


#median net migration in each age group across LAs
summ(age_dta$under_34_netmigration) # median 0.531
summ(age_dta$x35_to_64_netmigration) # median 0.671
summ(age_dta$x65plus_netmigration) # median 0.196

a<-median(age_dta$under_34_netmigration)
b<-median(age_dta$x35_to_64_netmigration)
c<-median(age_dta$x65plus_netmigration)

age_dta <- age_dta %>%
 mutate(age_mig = case_when(
              under_34_netmigration <=a & x35_to_64_netmigration <=b & x65plus_netmigration <=c ~ "Below median net migration - all age groups",
              under_34_netmigration >a & x65plus_netmigration >c ~ "Above median net migration - all age groups",
              under_34_netmigration >a & x65plus_netmigration <=c ~ "Above median net migration - younger",
              under_34_netmigration <=a & x65plus_netmigration >c ~ "Above median net migration - older", 
              under_34_netmigration <=a & x35_to_64_netmigration>b & x65plus_netmigration<c ~ "Above median net migration - middle age only"))

tabyl(age_dta$age_mig)

# Save data
clean_dta<-age_dta %>% 
  dplyr::select(c("geography", "geography_code",contains(c("netmigration")), "age_mig"))

s3write_using(age_dta # R object to save
              , FUN = write.csv # function used to save
              , object = 'age_net_migration_LA.csv' # Name of file to save 
              , bucket = buck_clean) # Bucket name



# Calculate net migration by Levelling Up priority category ---------------
#note: this analysis was not included in the final piece
# Import Levelling Up data
    #data were downloaded from https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjyroCDndj6AhWGUcAKHVAtCNsQFnoECAsQAQ&url=https%3A%2F%2Fassets.publishing.service.gov.uk%2Fgovernment%2Fuploads%2Fsystem%2Fuploads%2Fattachment_data%2Ffile%2F966137%2FLevelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx&usg=AOvVaw3NIQ1EBTwu0jLNSisnn3XT 
levup <-s3read_using(import # function used to read
                     , object = 'data/Levelling_Up_priority_areas/Levelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx' # File to open
                     , bucket = buck_main) # Bucket name 


dim(levup)
# 368 LAs in file

# tidy column names
names(levup)<-str_replace_all(names(levup), c(" " = "." ))

tabyl(levup$Priority.category, show_missing_levels = T)



# merge data
age_dta <- left_join(age_dta, levup, by = c("geography" = "Name"))


# calculate net migration by age and by Levelling Up category
summary(subset(age_dta, Priority.category == 1 & Country == 'England')$under_34_netmigration)
summary(subset(age_dta, Priority.category == 2 & Country == 'England')$under_34_netmigration)
summary(subset(age_dta, Priority.category == 3 & Country == 'England')$under_34_netmigration)

summary(subset(age_dta, Priority.category == 1 & Country == 'England')$x35_to_64_netmigration)
summary(subset(age_dta, Priority.category == 2 & Country == 'England')$x35_to_64_netmigration)
summary(subset(age_dta, Priority.category == 3 & Country == 'England')$x35_to_64_netmigration)

summary(subset(age_dta, Priority.category == 1 & Country == 'England')$x65plus_netmigration)
summary(subset(age_dta, Priority.category == 2 & Country == 'England')$x65plus_netmigration)
summary(subset(age_dta, Priority.category == 3 & Country == 'England')$x65plus_netmigration)


# export data by Levelling Up category
tibble_age1 <- age_dta %>%
  dplyr::filter(Country == 'England') %>%
  group_by(levelling_up_priority = Priority.category) %>%
  summarise(med_netmigration_under34 = median(under_34_netmigration),
            q1_under34 = quantile(under_34_netmigration, 0.25),
            q3_under34 = quantile(under_34_netmigration, 0.75),
            med_netmigration_35to64 = median(x35_to_64_netmigration), 
            q1_35to64 = quantile(x35_to_64_netmigration, 0.25),
            q3_35to64 = quantile(x35_to_64_netmigration, 0.75),
            med_netmigration_65plus = median(x65plus_netmigration),
            q1_65plus = quantile(x65plus_netmigration, 0.25),
            q3_65plus = quantile(x65plus_netmigration, 0.75))
tibble_age1

s3write_using(tibble_age1 # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'outputs/table_netmigration_age_levellingup.csv' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name 




# Calculate net migration by IMD  -----------------------------------------
    #note: this analysis was not included in the final piece
# Import IMD data
    #data were downloaded from https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
imd <-s3read_using(import # Which function are we using to read
                     , object = 'data/IMD_LAD.xlsx' # File to open
                     , bucket = buck_main) # Bucket name 

dim(imd)
# 317 LAs in file

# tidy column names
imd<-imd %>% 
  clean_names() 

imd <- imd %>%
  dplyr::select("local_authority_district_name_2019", "imd_rank_of_average_rank")

tabyl(imd$imd_rank_of_average_rank, show_missing_levels = T)

# Make a variable to identify top and bottom quintile of LAs
imd <- imd %>%
  mutate(quintile = cut(imd_rank_of_average_rank,
                        breaks = quantile(imd_rank_of_average_rank, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1, NA)),
                        labels = 1:5, na.rm = TRUE))

# merge data
age_dta <- left_join(age_dta, imd, by = c("geography" = "local_authority_district_name_2019"))


# calculate net migration by age for top and bottom IMD quintiles
summary(subset(age_dta, quintile == 1 & Country == 'England')$under_34_netmigration)
summary(subset(age_dta, quintile == 5 & Country == 'England')$under_34_netmigration)

summary(subset(age_dta, quintile == 1 & Country == 'England')$x35_to_64_netmigration)
summary(subset(age_dta, quintile == 5 & Country == 'England')$x35_to_64_netmigration)

summary(subset(age_dta, quintile == 1 & Country == 'England')$x65plus_netmigration)
summary(subset(age_dta, quintile == 5 & Country == 'England')$x65plus_netmigration)


# Export data by IMD quintile
tibble_age2 <- age_dta %>%
  dplyr::filter(Country == 'England') %>%
  group_by(quintile = quintile) %>%
  summarise(med_netmigration_under34 = median(under_34_netmigration),
            q1_under34 = quantile(under_34_netmigration, 0.25),
            q3_under34 = quantile(under_34_netmigration, 0.75),
            med_netmigration_35to64 = median(x35_to_64_netmigration), 
            q1_35to64 = quantile(x35_to_64_netmigration, 0.25),
            q3_35to64 = quantile(x35_to_64_netmigration, 0.75),
            med_netmigration_65plus = median(x65plus_netmigration),
            q1_65plus = quantile(x65plus_netmigration, 0.25),
            q3_65plus = quantile(x65plus_netmigration, 0.75))
tibble_age2

s3write_using(tibble_age2 # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'outputs/table_netmigration_age_imd.csv' # Name of the file to save to (include file type)
              , bucket = buck_main) # Bucket name 


# Make maps of net migration by age ---------------------------------------
# load packages
pacman::p_load(sf,
               XML,
               tmap,
               devtools)


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

# replace name for a few local authorities so they merge correctly
  age_dta <- age_dta %>%
    mutate(geography = replace(geography, geography == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))
  age_dta <- age_dta %>%
    mutate(geography = replace(geography, geography == "Folkestone and Hythe", "Shepway"))
  age_dta <- age_dta %>%
    mutate(geography = replace(geography, geography == "Vale of Glamorgan", "The Vale of Glamorgan"))
  
# Join spatial data
lad_shp <- left_join(lad_shp, age_dta, by = c("lad11nm" = "geography"))


# Map of age migration classification
map1_1 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "age_mig", style = "cat", palette = c('#53a9cd', '#2a7979', '#F39214', '#744284',  '#dd0031'), title = "Mobility by age") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map1_1


s3write_using(map1_1 # R object to save
              , FUN = tmap_save # R function used to save
              , object = 'outputs/map12_1_age_netmigration.tiff' # Name of file to save 
              , bucket = buck_main) # Bucket name 

