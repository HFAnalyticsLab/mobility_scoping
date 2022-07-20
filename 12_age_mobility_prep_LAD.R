## Preparing area-level mobility data, breakdown by age - LAD

# Housekeeping
# clear R environment
rm(list = ls())


#load packages
pacman::p_load(haven, 
               dplyr, 
               survey, 
               janitor,
               questionr, 
               epiDisplay, 
               epirhandbook,
               rio, 
               ggplot2, 
               apyramid,
               magrittr, 
               stringr, 
               here, 
               aws.s3,
               readr,
               matrixStats)


## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig001
age_dta <- s3read_using(import, 
                        object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping/data/censusmig_age_sex_LAD.csv') # File to open 

dim(age_dta)      #303 variables, 406 observations


# tidy variables names
age_dta<-age_dta %>% 
  clean_names() 
#remove spaces so that we can refer to column names in functions]


# naming second column x until figured out what it  means
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


# Create variables for usual residents in 2010 and 2011
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

# construct variables for three main age groups
under_34<-c("0_4", "5_15", "16_19","20_24", "25_34")
x35_to_64<-c("35_49","50_64")
x65plus<-c("65_64","75p")


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


#n_movedwithin= within_same_area
#n_usualres10= n_movedwithin+ n_samead+ n_outmig 
#mid_yearpop= (n_usualres10+n_usualres11)/2

#work out net migration 
#net_migration= (n_inmig-n_outmig)/(mid_yearpop)*1000


age_dta<-age_dta %>% 
  mutate(under_34_netmigration=(under_34_inmig-under_34_outmig)/(under_34_midyearpop)*100,
         x35_to_64_netmigration=(x35_to_64_inmig-x35_to_64_outmig)/(x35_to_64_midyearpop)*100,
         x65plus_netmigration=(x65plus_inmig-x65plus_outmig)/(x65plus_midyearpop)*100)


# attempt classification based on Jay's cluster analysis
# OLD definition
#age_dta <- age_dta %>%
 # mutate(age_mig = case_when(
#              under_34_netmigration <=0 & x35_to_64_netmigration <=0 & x65plus_netmigration <=0 ~ "General outmigration",
 #             under_34_netmigration >0 & x65plus_netmigration >0 ~ "General inmigration",
  #            under_34_netmigration >0 & x65plus_netmigration <=0 ~ "Younger inmigration",
   #           under_34_netmigration <=0 & x65plus_netmigration >0 ~ "Older inmigration",
    #          under_34_netmigration <=0 & x35_to_64_netmigration >0 ~ "Older inmigration"))


# New definition - based on over/under median net migration across LAs
summ(age_dta$under_34_netmigration) # median 0.465
summ(age_dta$x35_to_64_netmigration) # median 0.659
summ(age_dta$x65plus_netmigration) # median 0.159

age_dta <- age_dta %>%
 mutate(age_mig = case_when(
              under_34_netmigration <=0.465 & x35_to_64_netmigration <=0.659 & x65plus_netmigration <=0.159 ~ "Below median net migration - all age groups",
              under_34_netmigration >0.465 & x65plus_netmigration >0.159 ~ "Above median net migration - all age groups",
              under_34_netmigration >0.465 & x65plus_netmigration <=0.159 ~ "Above median net migration - younger",
              under_34_netmigration <=0.469 & x65plus_netmigration >0.159 ~ "Above median net migration - older", 
              under_34_netmigration <=0.469 & x35_to_64_netmigration>0.659 & x65plus_netmigration<0.159 ~ "Above median net migration - middle age only"))

tabyl(age_dta$age_mig)



# Import Levelling Up data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/mobility_scoping' ## my bucket name

levup <-s3read_using(import # Which function are we using to read
                     , object = 'data/Levelling_Up_priority_areas/Levelling_Up_Fund_list_of_local_authorities_by_priority_category.xlsx' # File to open
                     , bucket = buck) # Bucket name defined above


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


tibble <- age_dta %>%
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
tibble
options(pillar.sigfig=3) # controls number of digits displayed, couldn't figure out how to limit to 2 decimal places in csv below

s3write_using(tibble # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'outputs/table_netmigration_age_levellingup.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above



# load packages
pacman::p_load(sf,
               XML,
               tmap,
               THFstyle,
               devtools)

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
  subset(str_detect(lad11cd, 'E') | str_detect(lad11cd, 'W'))

# replace name for Rhondda so it merges correctly
  age_dta <- age_dta %>%
    mutate(geography = replace(geography, geography == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))
  age_dta <- age_dta %>%
    mutate(geography = replace(geography, geography == "Folkestone and Hythe", "Shepway"))
  age_dta <- age_dta %>%
    mutate(geography = replace(geography, geography == "Vale of Glamorgan", "The Vale of Glamorgan"))
  
# Join spatial data
lad_shp <- left_join(lad_shp, age_dta, by = c("lad11nm" = "geography"))


# Map of age migration classification
map12_1 <- tm_shape(lad_shp) +
  tm_borders(, alpha=0) +
  tm_fill(col = "age_mig", style = "cat", palette = "viridis", title = "Mobility category") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.6,
            legend.position = c("left","top"),
            legend.bg.color = "white",
            legend.bg.alpha = 1)
map12_1
s3write_using(map12_1 # What R object we are saving
              , FUN = tmap_save # Which R function we are using to save
              , object = 'outputs/map12_1_age_netmigration.tiff' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above  # Note: need to figure out how to export maps with sw3 commands

