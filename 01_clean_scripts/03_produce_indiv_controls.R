# Produce final clean data
# Created: October 10th, 2025
# Last modified: October 10th 2025. 

# Load libraries
library(tidyverse)

# Load data
dta.files <- list.files("~/Data/Immigrant_Occupations/for_analysis/")

# Load quinquenial data
dta.files.yr<- dta.files[str_detect(dta.files, "-") == TRUE & str_detect(dta.files, "JUN2025_acs") == TRUE ]
yr_tabs <- list()


# For each group of five years, produce each of the following summary statistics 

i <- 1
for(i in 1:length(dta.files.yr)){
  dta <- read_rds(paste0("~/Data/Immigrant_Occupations/for_analysis/"
                         , dta.files.yr[i]))
  tot <- sum(dta$PERWT)
  dta.mig <- dta%>%
    filter(EMPSTAT == 1) %>%
    filter(AGE>= 25 & AGE <= 54)%>%
    mutate(AGE = AGE - mean(AGE)
           , YRS_USA = YEAR-YRIMMIG
           , FEMALE = ifelse(SEX ==2, 1, 0 )
           , RACE = case_when(HISPAN == 2  ~ "Hispanic"
                              ,  RACE == 1 ~"White Non-Hispanic"
                              , RACE == 2 ~ "Black Non-Hispanic" 
                              , RACE %in% c(3,7,8,9) ~"Other or multiracial Non-Hispanic"
                              , RACE %in% c(4,5,6) ~  "Asian Non-Hispanic"
                              , TRUE ~ as.character(RACE))
           , EDUC.sh = factor(EDUC.sh, levels = c(1,2,3,4)
                              , labels = c("Less than high school", "High school"
                                           ,"Some college","College or above"))
           , SELFEMP = ifelse(CLASSWKR == 1, 1, 0)) %>%
    
    select(YEAR.5, PERWT, AGE, FEMALE, RACE, EDUC.sh,  SELFEMP, YRS_USA,
           ,IS_IMMIGRANT, YEAR.5,  INCWAGE)
 
   yr_tabs[[i]] <- dta.mig 
}

# Name list
names(yr_tabs)<- c('2000-2003','2004-2007', '2008-2011'
                   , '2012-2015', '2016-2019', '2020-2023')
# Bind rows

# We save one file per 4 year aggregate
i <- 1
vec <- c('2000-2003','2004-2007', '2008-2011','2012-2015', '2016-2019', '2020-2023' )
for(i in 1:length(vec)){
  mig <-yr_tabs[[i]]%>% filter(IS_IMMIGRANT == 1) %>% select(-IS_IMMIGRANT)
  nat <-yr_tabs[[i]]%>% filter(IS_IMMIGRANT == 0) %>% select(-c("IS_IMMIGRANT", "YRS_USA"))
  
  write_rds(mig, paste0("~/Data/Immigrant_Occupations/for_analysis/SEPT2025_MIG_worker_controls_", vec[i], ".rds"))
  write_rds(nat, paste0("~/Data/Immigrant_Occupations/for_analysis/SEPT2025_NAT_worker_controls_", vec[i], ".rds"))
}


# End file
