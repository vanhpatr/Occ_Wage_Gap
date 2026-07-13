# 2012-2019 Decomposition
# This takes year by year estimations from the ACS, and produces decomposition 
# based on the estimated wage averages and sizes just for the period of growth:
# 2012 to 2019. 
# Date created: January 16, 2026
# Last modified: July 3rd, 2026

################################################################################
########################### Load data and libraries ############################
################################################################################
library(tidyverse)

# Pull data on every occupation for every year: created ~/clean_scripts/02_Wrangle_for_Occ_Yr_Size_Dataset.R
dt <- readRDS("~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_occ_summary_nonparametric.rds")


################################################################################
##################### Estimate the decomposition, yr to yr #####################
################################################################################
# Next, manually create each of the components of the decomposition 
dt <- dt %>%
  group_by(year)%>%
  mutate(occ_size = w_count_migrant+w_count_native
         , so_mig = w_count_migrant/sum(w_count_migrant)
         , so_native = w_count_native/sum(w_count_native))%>%ungroup()
# Growth structure

years <- c(2001:2023)
size_decomp <- list()

for (i in 1:length(years)){
  y1 <- years[i]
  y2 <- years[i]+1
  
  size_decomp[[i]] <- dt[dt$year == y1 ,c("occ_title", "mean_wages_migrant", "w_count_migrant","occ_size", "so_mig")]%>%
    left_join(., dt[dt$year == y2 ,c("occ_title", "mean_wages_migrant","w_count_migrant", "occ_size", "so_mig")]
              , suffix = c("_y1", "_y2"), by = "occ_title")%>%
    mutate( C1 = (so_mig_y2 - so_mig_y1)*mean_wages_migrant_y1
            , C2 = (mean_wages_migrant_y2 - mean_wages_migrant_y1)*so_mig_y1
            , C3 = (so_mig_y2 - so_mig_y1)*(mean_wages_migrant_y2 - mean_wages_migrant_y1)
            , delta_wbar = C1+C2+C3
            , wage_growth =(mean_wages_migrant_y2 - mean_wages_migrant_y1)
            , part_growth = (so_mig_y2 - so_mig_y1))
}



names(size_decomp) <- paste0("y",years)

size_decomp <- size_decomp %>%
  bind_rows(., .id = "first_yr")


write_rds(size_decomp,"~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_w_in_nativity_decomp_migrant.rds")



years <- c(2001:2023)
size_decomp <- list()

for (i in 1:length(years)){
  y1 <- years[i]
  y2 <- years[i]+1
  
  size_decomp[[i]] <- dt[dt$year == y1 ,c("occ_title", "mean_wages_native", "w_count_native","occ_size", "so_native")]%>%
    left_join(., dt[dt$year == y2 ,c("occ_title", "mean_wages_native", "w_count_native","occ_size", "so_native")]
              , suffix = c("_y1", "_y2"), by = "occ_title")%>%
    mutate( C1 = (so_native_y2 - so_native_y1)*mean_wages_native_y1
            , C2 = (mean_wages_native_y2 - mean_wages_native_y1)*so_native_y1
            , C3 = (so_native_y2 - so_native_y1)*(mean_wages_native_y2 - mean_wages_native_y1)
            , delta_wbar = C1+C2+C3
            , wage_growth =(mean_wages_native_y2 - mean_wages_native_y1)
            , part_growth = (so_native_y2 - so_native_y1))
}

names(size_decomp) <- paste0("y",years)


size_decomp <- size_decomp %>%
  bind_rows(., .id = "first_yr")




write_rds(size_decomp,"~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_w_in_nativity_decomp_native.rds")



