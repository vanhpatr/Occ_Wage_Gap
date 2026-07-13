# Full, yearly decomposition 
# This takes year by year estimations from the ACS, and produces decomposition 
# based on the estimated wage averages and sizes for every given year. 
# Date created: January 16, 2026
# Last modified: July 3rd, 2026

################################################################################
########################### Load data and libraries ############################
################################################################################
library(tidyverse)
summarize <- dplyr::summarize

final_sample <- readRDS("~/Desktop/Occ_Wage_Gap/03_Outputs/20260122_occ_summary_nonparametric.rds")
occ_title_cw <- readRDS("~/Desktop/Occ_Wage_Gap/03_Outputs/occ_title_crosswalk.rds")

################################################################################
########## For each year, assess sample coverage for each occupation ###########
################################################################################

# Quickly check the sample coverage: it looks at the number of survey responses
# for every occupation 
final_sample %>%
  mutate(few_imm_30 = ifelse(obs_count_migrant <30, 1, 0)
         , few_imm_100 = ifelse(obs_count_migrant<100,1,0))%>%
  group_by(OCC2010) %>% 
  summarise(below30 = sum(few_imm_30)
            , below100 =sum(few_imm_100))

################################################################################
##################### Estimate the decomposition, yr to yr #####################
################################################################################
# Next, manually create each of the components of the decomposition 
dt <- final_sample %>%
  rename('mu_A'= mean_wages_native, 'mu_B'= mean_wages_migrant) %>%
  mutate(p_iB = w_count_migrant/sum(w_count_migrant, na.rm = TRUE)
         , p_iA = w_count_native/sum(w_count_native, na.rm = TRUE)
         , m_i = (p_iB+p_iA)/2
         , s_i = p_iA-p_iB
         , delta_i = mu_A-mu_B
         , mu_i = (mu_A + mu_B)/2)%>%
  group_split(OCC2010)

# Create an empty list 
decomp <- list() 
years <- as.character(c(2000:2023))

# Within an occupation, produce the decomposition. 
i <- 2

for(i in 1:417){
  an.occ <- dt[[i]] %>% arrange(year) 
  occ.code <- unique(an.occ$OCC2010)
  occ.tib <- tibble()
  j <- 1
  for( j in 1:23){
    # First component: change in occupational nativity segregation
    C1 <- ((an.occ[an.occ$year == years[j], ]$mu_i+an.occ[an.occ$year == years[j+1], ]$mu_i)/2)*
      (an.occ[an.occ$year == years[j+1], ]$s_i - an.occ[an.occ$year == years[j], ]$s_i)
    # Second component: change in within-occupation native-immigrant earning gaps
    C2 <- ((an.occ[an.occ$year == years[j], ]$m_i+an.occ[an.occ$year == years[j+1], ]$m_i)/2)*
      (an.occ[an.occ$year == years[j+1], ]$delta_i - an.occ[an.occ$year == years[j], ]$delta_i)
    # Third component: change in occupational earnings structure
    C3 <- ((an.occ[an.occ$year == years[j], ]$s_i+an.occ[an.occ$year == years[j+1], ]$s_i)/2)*
      (an.occ[an.occ$year == years[j+1], ]$mu_i - an.occ[an.occ$year == years[j], ]$mu_i)
    # Fourth component: change in occupational composition
    C4 <- ((an.occ[an.occ$year == years[j], ]$delta_i+an.occ[an.occ$year == years[j+1], ]$delta_i)/2)*
      (an.occ[an.occ$year == years[j+1], ]$m_i - an.occ[an.occ$year == years[j], ]$m_i)
    # Label the years
    j_j1 <- paste0(years[j], "-", years[j+1])
    
    new.row <- tibble('year' = j_j1 , 'C1' = C1, 'C2'= C2, 'C3'= C3, 'C4'= C4
                      , 'Delta'= C1+C2+C3+C4
                      , 'OCC2010' = occ.code )
    occ.tib <- bind_rows(occ.tib, new.row)
  }
  decomp[[i]] <- occ.tib
}

# Merge individual occupation decompositions
decomp <- bind_rows(decomp) 
decomp$OCC2010 <- as.character(decomp$OCC2010)

# Merge labels
decomp <- left_join(decomp, occ_title_cw, by = "OCC2010")


################################################################################
################################## Save files ##################################
################################################################################
write_rds(decomp,"~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_all_year_all_occ_decomposition.rds")

