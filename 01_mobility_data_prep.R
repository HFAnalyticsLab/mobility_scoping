## Preparing area-level mobility data - Output areas

# Housekeeping
# clear R environment
rm(list = ls())

# list.of.packages.cran <- c(
#   "arm", "car", "corrplot", "FRK", "gghighlight",
#   "ggplot2", "ggmap", "GISTools", "gridExtra", "gstat",
#   "jtools", "kableExtra", "knitr", "lme4", "lmtest",
#   "lubridate", "MASS", "merTools", "plyr", "RColorBrewer",
#   "rgdal", "sf", "sjPlot", "sp", "spgwr",
#   "spatialreg", "spacetime", "stargazer", "tidyverse", "tmap",
#   "viridis", "tufte"
# )
# 
# new.packages.cran <- list.of.packages.cran[!(list.of.packages.cran %in% installed.packages()[,"Package"])]
# if(length(new.packages.cran)) install.packages(new.packages.cran)
# 
# for(i in 1:length(list.of.packages.cran)) {
#   library(list.of.packages.cran[i], character.only = T)
# }



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
               here)


list.of.packages.cran <- c(
  "arm", "car", "corrplot", "FRK", "gghighlight",
  "ggplot2", "ggmap", "GISTools", "gridExtra", "gstat",
  "jtools", "kableExtra", "knitr", "lme4", "lmtest",
  "lubridate", "MASS", "merTools", "plyr", "RColorBrewer",
  "rgdal", "sf", "sjPlot", "sp", "spgwr",
  "spatialreg", "spacetime", "stargazer", "tidyverse", "tmap",
  "viridis", "tufte"
)

new.packages.cran <- list.of.packages.cran[!(list.of.packages.cran %in% installed.packages()[,"Package"])]
if(length(new.packages.cran)) install.packages(new.packages.cran)

for(i in 1:length(list.of.packages.cran)) {
  library(list.of.packages.cran[i], character.only = T)
}


# set working directory (local/fixed pathway)
  #NOTE: obsolete since project sets wd and using "here"
  #setwd("M:/Analytics/Francesca/Mobility_data")
here()


# import all data
  ## data were downloaded from: https://www.nomisweb.co.uk/census/2011/ukmig008
eastdta <- import(here("data", "censusmig_OA_East.csv"))
emdta <- import(here("data", "censusmig_OA_EastMidlands.csv"))
londondta <- import(here("data", "censusmig_OA_London.csv"))
nedta <- import(here("data", "censusmig_OA_NorthEast.csv"))
nwdta <- import(here("data", "censusmig_OA_NorthWest.csv"))
sedta <- import(here("data", "censusmig_OA_SouthEast.csv"))
swdta <- import(here("data", "censusmig_OA_SouthWest.csv"))
wmdta <- import(here("data", "censusmig_OA_WestMidlands.csv"))
yhdta <- import(here("data", "censusmig_OA_YorkshireHumber.csv"))

# append all datasets together
eng_dta <- rbind(eastdta, emdta, londondta, nedta, nwdta, sedta, swdta, wmdta, yhdta)

dim(eng_dta)      #24 variables, 171372 observations
table(eng_dta$date)

# tidy variables names
names(eng_dta)<-str_replace_all(names(eng_dta), c(" " = "." , "," = "" , ";" = "" , "/" = "" , ":" = ""))
  #remove spaces so that we can refer to column names in functions

eng_dta <- rename(eng_dta, n_samead = Migrationethnic.group.Lived.at.same.address.one.year.ago.measures.Value)
eng_dta <- rename(eng_dta, n_usualres = Migrationethnic.group.All.usual.residents.measures.Value)
eng_dta <- rename(eng_dta, n_totalmig = Migrationethnic.group.Migrants.Total.measures.Value)
eng_dta <- rename(eng_dta, n_migwithin = Migrationethnic.group.Migrants.Moved.within.the.area.measures.Value)
eng_dta <- rename(eng_dta, n_miguk = Migrationethnic.group.Migrants.Moved.into.the.area.from.within.the.UK.measures.Value)
eng_dta <- rename(eng_dta, n_migfor = Migrationethnic.group.Migrants.Moved.into.the.area.from.outside.the.UK.measures.Value)
eng_dta <- rename(eng_dta, n_outmig = Migrationethnic.group.Moved.out.of.the.area.measures.Value)


#drop columns don't need
eng_dta <- eng_dta %>% 
  dplyr::select(1:10) 

#alternative code - ignore, done in command lines 50/51
# p_load(data.table)
# eng_dta <- as.data.table(eng_dta)
# eng_dta <- eng_dta[, 1:10] #first arg = rows, second = cols, third = group by

head(eng_dta)

class(eng_dta$n_samead)

#trying to get loops to work - tabulate a list of variable 
for (i in c(3:10)) {
  i
  print(tabyl(eng_dta[, i]))
}

# or with variable names
varlist <- c("n_outmig", "n_usualres", "n_totalmig")
for (i in varlist) {
  print(tabyl(eng_dta[, i]))
}


# or with lapply
varlist <- c("n_outmig", "n_usualres", "n_totalmig")
lapply(eng_dta[varlist], tabyl)

  #Jay uses tableone instead of tabyl for tabulations, check out links (bookmarked)



# Describe size of OAs
summary(eng_dta$n_usualres)
  # median = 303 residents, max = 4140 - exclude these? 


# calculate proportion of current residents who lived at the same address one year ago
eng_dta <- eng_dta %>% 
  mutate(prop_samead_1y = as.numeric(n_samead/n_usualres*100))
summary(eng_dta$prop_samead_1y)
  # median = 90% of people in OA lived at same address a year ago

# check how many rows have very small values 
sum(eng_dta$prop_samead_1y < 50)
sum(!is.na(eng_dta$prop_samead_1y))
1477/171372
  # should these be removed from analysis? e.g. student halls


# Identify areas of net outmigration
  # these are areas where total migrants is smaller than outmigrants
eng_dta$net_outmig <- ifelse(eng_dta$n_outmig > eng_dta$n_totalmig, 1, 0)

# Recreate Brown 2010 classification
# Identify areas of 10% + net increase (using previous year's residents as denominator)
eng_dta <- eng_dta %>% 
  mutate(net_migration = (n_totalmig - n_outmig)/(n_usualres + n_outmig - n_totalmig)*100)
summary(eng_dta$net_migration)  
  #median net migration = 0.59% (seems low compared with 16% turnover in previous year in Brown study for Scotland)

sum(eng_dta$net_migration <= -10) #1381
sum(eng_dta$net_migration >= 10) #10630
10630/171372 #6%
1381/171372 #0.8% - not a very big category, think about reclassifying to 5%+ decline? 

sum(eng_dta$net_migration <= -5) #1381
12325/171372 #7.2% 
  # probably makes more sense to use 5% cut-off for high in/out-migration areas (see other paper by Brown mentioned by Anne)


# Calculate turnover as % of current residents who lived outside the area one year ago
  # this includes people moving in from elsewhere in UK and abroad (but not within the area)
  # Brown et al. exclude people who'd moved from abroad in one of their papers, do we want to do the same? 

eng_dta <- eng_dta %>% 
  mutate(turnover = (n_miguk + n_migfor)/n_usualres*100)
summary(eng_dta$turnover)
  #median turnover = 9.6% (lower than 16% turnover in previous year in Brown study for Scotland)


# Create variable with mobility categories
  # below does not work
eng_dta$mob_cat <- as.factor(ifelse(eng_dta$net_migration <= -5, 'Decreasing', 
                            ifelse(eng_dta$net_migration >= 5, 'Increasing',
                            ifelse(eng_dta$turnover >= 9.6, 'Stable, high turnover',
                            ifelse(eng_dta$turnover <9.6, 'Stable, low turnover', 'Error' )))))       
  
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= -5 ~ "Decreasing",
    net_migration >= 5 ~ "Increasing",
    net_migration > -5 & net_migration < 5 & turnover >= 9.6 ~ "Stable, high turnover",
    net_migration > -5 & net_migration < 5 & turnover <9.6 ~ "Stable, low turnover"
  ))
  
#Advice from Jay: 
  # stay away from factors, use as.character instead 
  # if want to use it as factor, transform it at the very end 
  # only use factor for ordered categorical, does some weird things for graphs etc. 
  # can write levels within as.factor so R knows the order in which categories should be displayed
  # if unordered categorical (e.g. region/religion), use character unless want them displayed in specific order


# Tab variable
tabyl(eng_dta$mob_cat)
  # Stable, low turnover accounts for 44% of all OAs in England for period 2010-11

