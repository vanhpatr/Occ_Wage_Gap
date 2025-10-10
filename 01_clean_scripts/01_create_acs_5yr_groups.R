# Clean ACS Data
# Date: 17 June, 2025
# Last modified: 10 Oct, 2025
# Description: pull the raw data, pulled June 16, 2025 and produce 5-year aggregates

# Load libraries
library(tidyverse)
library(ipumsr)

# Load DDI and data
ddi <- read_ipums_ddi("~/Data/Immigrant_Occupations/raw_data/usa_00001.xml")
data <- read_ipums_micro(ddi)

# Select sample and recode important variables
dt <- data %>%
  # Only in labor force not in school
  filter(LABFORCE == 2 & SCHOOL == 1 & YEAR!='1990') %>%
  select(-c('LABFORCE', 'SCHOOL')) %>%
  mutate(EDUC.sh = lbl_relabel(EDUC
                               , lbl(1, "Less than high school") ~ .val %in% c(0,1,2,3,4,5)
                               , lbl(2, "High school") ~ .val %in% c(6)
                               , lbl(3, "Some college") ~ .val %in% c(7,8,9)
                               , lbl(4, "College or above") ~ .val %in% c(10,11))
         , BPL = lbl_relabel(BPL
                             , lbl(1, "NATIVE") ~ .val %in% c(1:120)
                             , lbl(2, "EUROPE, AUSTRALIA, & CANADA") ~ .val %in% c(150, 155, 199, 400, 401, 402
                                                                                   , 403, 404, 405, 410, 411, 412
                                                                                   ,413,414,419,420, 421, 422,423
                                                                                   ,424,425,426,429, 430, 431,432
                                                                                   ,433,434, 435, 436, 437,438, 439
                                                                                   , 440, 450, 499, 531, 700)
                             , lbl(3, "LATINAMERICA AND CARIBBEAN") ~ .val %in% c(200,210, 299, 300, 260,250, 160)
                             , lbl(4, "EASTERN EUROPE") ~ .val %in% c(451, 452, 453, 454, 455, 456, 457
                                                                      , 458, 459, 460, 461, 462, 463, 465)
                             , lbl(5, "EAST ASIA") ~ .val %in% c(500, 501, 502, 509, 510, 511, 512
                                                                 , 513, 514, 515, 516, 517, 518, 519
                                                                 , 599, 710)
                             , lbl(6, "SOUTH ASIA") ~ .val %in% c(520, 521,523,524,548, 549, 550 )
                             , lbl(7, "MIDDLE EAST") ~ .val %in% c(522,530, 532, 533
                                                                   , 534, 535, 536, 537, 538, 539
                                                                   , 540, 541, 542, 543, 544, 545
                                                                   ,546, 547)
                             , lbl(8, "AFRICA") ~ .val %in% c(600)
                             , lbl(9, "OTHER UNKNOWN") ~ .val %in% c(800, 900, 950, 997, 999))
         , IS_IMMIGRANT = ifelse(BPL %in% c(2,3,4,5,6,7,8) & YRIMMIG > 0, 1, 0)
         
         # Arreglar los años que por algun motivo no marcan bien el 2000. 
         , YEAR.5 = case_when(  YEAR %in% c(2000:2003) ~ '2000-2003'
                                , YEAR %in% c(2004:2007) ~ '2004-2007'
                                , YEAR %in% c(2008:2011) ~ '2008-2011'
                                , YEAR %in% c(2012:2015) ~ '2012-2015'
                                , YEAR %in% c(2016:2019) ~ '2016-2019'
                                , YEAR %in% c(2020:2023) ~ '2020-2023'))

# We are saving one file per year
i <- 2000
for(i in c(2000:2023)){
  write_rds(dt[dt$YEAR == i, ], paste0("~/Data/Immigrant_Occupations/for_analysis/JUN2025_acs_", i, "_estimates.rds"))
}


# We save one file per 4 year aggregate
i <- 1
vec <- c('2000-2003','2004-2007', '2008-2011','2012-2015', '2016-2019', '2020-2023' )
for(i in 1:length(vec)){
  write_rds(dt[dt$YEAR.5 == vec[i], ], paste0("~/Data/Immigrant_Occupations/for_analysis/JUN2025_acs_", vec[i], "_estimates.rds"))
}


# End script 
