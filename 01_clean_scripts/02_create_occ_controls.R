# Produce occupation-level controls
# These controls are the same across all workers: female, male, native, and immigrant
# These are produced for a given year, and a given occupation. 
# Date created: June 26th, 2025
# Last modified: October 10th, 2025

# Load libraries
library(tidyverse)

# Load data.
dta.files <- list.files("~/Data/Immigrant_Occupations/for_analysis/")
# Load quinquenial data
dta.files.yr<- dta.files[str_detect(dta.files, "-") == TRUE & str_detect(dta.files, "JUN2025_acs") == TRUE ]
yr_tabs <- list()

# For each year file, produce summary stats. 
i<- 1

# These are the occupation-level controls we want to set up
# * **FEM:** percent female in that occupation.
# * **MIG:** percent migrant worker in that occupation.
# * **COL:** percent college educated in that occupation.
# * **SELFEMP:** percent self-employed in that occupation. (`CLASSWKR`)
# * **STEM:** whether occupation is classified as STEM based on 2018 crosswalk status

dta.files <- list.files("~/Data/Immigrant_Occupations/for_analysis/")

# Load quinquenial data
dta.files.yr<- dta.files[str_detect(dta.files, "-") == TRUE & str_detect(dta.files, "JUN2025_acs") == TRUE ]
yr_tabs <- list()

# Read files

# For each group of five years, produce each of the following summary statistics 
for(i in 1:length(dta.files.yr)){
  dta <- read_rds(paste0("~/Data/Immigrant_Occupations/for_analysis/"
                         , dta.files.yr[i]))
  dta$OCC2010 <- as.character(dta$OCC2010)
  tot <- sum(dta$PERWT)
  dta.mig <- dta%>%
    filter(EMPSTAT == 1) %>%
    group_by(OCC2010) %>%
    summarize(occ_share_fem = sum(PERWT[SEX==2])/(sum(PERWT))
              , occ_share_mig = sum(PERWT[IS_IMMIGRANT==1])/(sum(PERWT))
              , occ_share_col = sum(PERWT[EDUC.sh==4])/(sum(PERWT))
              , occ_share_self_emp = sum(PERWT[CLASSWKR==1])/(sum(PERWT))
              , occ_size = sum(PERWT)/tot)
  
  yr_tabs[[i]] <- dta.mig 
}

# Name list
names(yr_tabs)<- c('2000-2003','2004-2007', '2008-2011'
                   , '2012-2015', '2016-2019', '2020-2023')
# Bind rows
occ_yr_controls <-bind_rows(yr_tabs, .id = "year")
# Write file
write_rds(occ_yr_controls, "~/Data/Immigrant_Occupations/for_analysis/clean_occupations.rds")

# End script
