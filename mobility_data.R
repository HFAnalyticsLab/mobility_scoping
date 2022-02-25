## Preparing area-level mobility data

# Housekeeping
library(pacman)
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
               stringr)

# set working directory (local/fixed pathway)
setwd("M:/Analytics/Francesca/Mobility data")

# import all data
eastdta <- import("censusmig_OA_East.csv")
emdta <- import("censusmig_OA_EastMidlands.csv")
londondta <- import("censusmig_OA_London.csv")
nedta <- import("censusmig_OA_NorthEast.csv")
nwdta <- import("censusmig_OA_NorthWest.csv")
sedta <- import("censusmig_OA_SouthEast.csv")
swdta <- import("censusmig_OA_SouthWest.csv")
wmdta <- import("censusmig_OA_WestMidlands.csv")
yhdta <- import("censusmig_OA_YorkshireHumber.csv")

# append all datasets together
eng_dta <- rbind(eastdta, emdta, londondta, nedta, nwdta, sedta, swdta, wmdta, yhdta)

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

#alternative code
# p_load(data.table)
# eng_dta <- as.data.table(eng_dta)
# eng_dta <- eng_dta[, 1:10] #first arg = rows, second = cols, third = group by

head(eng_dta)

class(eng_dta$n_samead)

#loop 
for (i in c(1:10)) {
  tabyl(eng_dta[, i])
}

eng_dta[, i]
i
# assigned 1 to i but doesn't print the right column
# check functions sent by Jay


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


# Calculate turnover (inverse of % living at same address one year ago)
eng_dta <- eng_dta %>% 
  mutate(turnover = n_totalmig/n_usualres*100)
summary(eng_dta$turnover)
  #median turnover = 9.7% (a little lower than 16% turnover in previous year in Brown study for Scotland)


# Create variable with mobility categories
  # below does not work
eng_dta$mob_cat <- as.factor(ifelse(eng_dta$net_migration <= -10, 'Decreasing', 
                            ifelse(eng_dta$net_migration >= 10, 'Increasing',
                            ifelse(eng_dta$turnover >= 9.7, 'Stable, high turnover',
                            ifelse(eng_dta$turnover <9.7, 'Stable, low turnover', 'Error' )))))       
  
eng_dta <- eng_dta %>%
  mutate(mob_cat = case_when(
    net_migration <= -10 ~ "Decreasing",
    net_migration >= 10 ~ "Increasing",
    net_migration > -10 & net_migration < 10 & turnover >= 9.7 ~ "Stable, high turnover",
    net_migration > -10 & net_migration < 10 & turnover <9.7 ~ "Stable, low turnover"
  ))
  # stay away from factors, use as.character instead 
  # if want to use it as factor, transform it at the very end 
  # only use factor for ordered categorical, does some weird things
  # can write levels within as.factor so knows the order
  # if region/religion (unordered categorical), use character unless want them displayed in specific order


# Tab variable
tabyl(eng_dta$mob_cat)
  # Stable, low turnover accounts for 49% of all OAs

