## Analysis 1: produce non-parametric estimates based on the memos authored by Xi
## Date: June 26th, 2025
## Last modified: January 16, 2026
## Purpose: Using ACS data, revise the size of occupations to establish wheter
# we can put together a non-parametric estimation of the decompositions.
# Copied to XS_paper folder April 10th, 2026

################################################################################
########################### Load libraries and data  ###########################
################################################################################
library(tidyverse)
library(haven)
library(Hmisc)
# Load data.
# List year-level files we'll use
dta.files <- list.files("~/Data/Immigrant_Occupations/for_analysis/")
dta.files.yr<- dta.files[str_detect(dta.files, "-") == FALSE & str_detect(dta.files, "JAN2026_acs") == TRUE ]
# Read clean occupation-level data
occ <- read_rds("~/Data/Immigrant_Occupations/for_analysis/clean_occupations.rds")

summarize<-dplyr::summarize
################################################################################
####################### Find consistently reported occs  #######################
################################################################################
# Pull the clean occupations
occ_sample <- occ[, c('year', 'OCC2010',"occ_share_mig")]%>%
  # For each occupation, find the number of years reported
  group_by(OCC2010)%>%
  summarize(years_reported = sum(occ_share_mig >0))%>%
  # Find all occupations that are reported for the maximum number of years
  filter(years_reported == max(years_reported))%>%
  pull(OCC2010)


################################################################################
########## For each year, produce summary stats for every occupation ###########
################################################################################
# This summarizes occupation-level gaps
yr_tabs <- list()

# This summarizes year-level gaps
overall_data <- list()

# For each year file, produce summary stats. 
i<- 1


for(i in 1:length(dta.files.yr)){
  dta <- read_rds(paste0("~/Data/Immigrant_Occupations/for_analysis/"
                         , dta.files.yr[i])) %>%
    mutate(INCWAGE_adj23 = as.numeric(INCWAGE_adj23))
  
  
  # Produce summary statistics for Native workers
  dta.native <- dta%>%
    filter(IS_IMMIGRANT ==0 )%>%
    group_by(OCC2010) %>%
    summarize(obs_count = n() # Number of observations
              , w_count = sum(PERWT) # Estimated number of people
              , mean_wages =wtd.mean( INCWAGE_adj23
                                     ,PERWT
                                     ,na.rm = TRUE ) # Mean wages, excluding 0s
              , sd_wages = sqrt(wtd.var(INCWAGE_adj23
                                        ,PERWT
                                        ,na.rm = TRUE)))%>% # SD of wages, excluding 0s
    dplyr::rename_with(~ifelse(.== "OCC2010", .,paste0(., "_native")))
  
  # Produce summary statistics for Immigrant workers
  dta.immigrant <- dta%>%
    filter(IS_IMMIGRANT ==1 )%>%
    group_by(OCC2010) %>%
    summarize(obs_count = n()
              , w_count = sum(PERWT)
              , mean_wages =wtd.mean(INCWAGE_adj23
                                     ,PERWT
                                     ,na.rm = TRUE ) # Mean wages, excluding 0s
              , sd_wages = sqrt(wtd.var( INCWAGE_adj23
                                        ,PERWT
                                        ,na.rm = TRUE)))%>% # SD of wages, excluding 0s
    dplyr::rename_with(~ifelse(.== "OCC2010", ., paste0(., "_migrant")))
  
  # Join them
  year_obs <- full_join(dta.native, dta.immigrant, by = "OCC2010")
  
  yr_tabs[[i]] <- year_obs
  
  # Produce yearly summary of the entire economy 
  dta.overall <- dta %>%
    summarize(n_immigrant = sum(PERWT*IS_IMMIGRANT)
              , n_native = sum(PERWT*(IS_IMMIGRANT==0))
              , immigrant_avg_wage = wtd.mean(dta[dta$IS_IMMIGRANT==1,]$INCWAGE_adj23
                                              ,dta[dta$IS_IMMIGRANT==1,]$PERWT
                                              ,na.rm = TRUE)
              , native_avg_wage = wtd.mean( dta[dta$IS_IMMIGRANT==0,]$INCWAGE_adj23
                                           ,dta[dta$IS_IMMIGRANT==0,]$PERWT
                                           ,na.rm = TRUE))
  # Save the object
  overall_data[[i]] <- dta.overall
}

# Name them with the year the data is pulled from 
names(yr_tabs) <-  c(as.character(2000:2023))
names(overall_data) <-  c(as.character(2000:2023))

# Bind them by row
yr_est <- bind_rows(yr_tabs, .id = "year")
overall_data <- bind_rows(overall_data, .id = "year")

# Final sample! 
final_sample <- yr_est[yr_est$OCC2010%in% occ_sample,  ]

################################################################################
########## For all years, produce the OCC2010 to OCC Title crosswalk ###########
################################################################################
# Load the data that has the right attributes
dta <- read_rds("~/Data/Immigrant_Occupations/for_analysis/JUN2025_acs_2001_estimates.rds")
# Manually create the crosswalk
occ_title_cw <- tibble('title' = names(attributes(dta$OCC2010)$labels)
                       , 'OCC2010' = as.character(attributes(dta$OCC2010)$labels))
# Keep the occupations we're interested in. 
occ_title_cw <- occ_title_cw %>% 
  filter(OCC2010 %in% final_sample$OCC2010)%>%
  mutate(occ_title = janitor::make_clean_names(title))

################################################################################
################################# Write files ##################################
################################################################################
final_sample <- final_sample%>%
  left_join(., occ_title_cw,by = "OCC2010")
 
overall_data <- overall_data %>%
  mutate(sh.mig = n_immigrant/(n_immigrant+n_native))

write_rds(final_sample, "~/Desktop/Occ_Wage_Gap/02_Analysis_ASA/data_output/20260122_occ_summary_nonparametric.rds")
write_rds(overall_data, "~/Desktop/Occ_Wage_Gap/02_Analysis_ASA/data_output/20260122_year_summary_nonparametric.rds")
write_rds(occ_title_cw, "~/Desktop/Occ_Wage_Gap/02_Analysis_ASA/data_output/occ_title_crosswalk.rds")

# End script. 