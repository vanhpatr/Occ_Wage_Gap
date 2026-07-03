# Clean ACS Data
# Date: 17 June, 2025
# Last modified: 16 January, 2026
# Description: pull the raw data, pulled June 16, 2025 and produce yearly aggregates
# Refactored: 3rd July, 2026

# Load libraries
library(tidyverse)
library(ipumsr)
library(priceR)

# Load DDI and data
ddi <- read_ipums_ddi("~/Desktop/Occ_Wage_Gap/Paper/data_raw/usa_00001.xml")
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
         , YEAR = as.numeric(YEAR))

dt <- dt %>%
  mutate(MOD_AGE = AGE-25
       , OCC2010 = OCC2010
       , FEM = ifelse(SEX == 2, 1, 0)
       , RACE = case_when(RACE == 1 ~ "WHITE"
                          , RACE ==2 ~"BLACK"
                          , RACE ==3 ~"NATIVE AMERICAN"
                          , RACE %in% c(4,5,6) ~ "ASIAN"
                          , RACE %in% c(7,8,9) ~ "OTHER")
       , INCWAGE = zap_labels(INCWAGE)
      , RACE= case_when(HISPAN >0 ~ "HISPANIC"
                         , HISPAN==0 ~ RACE
                         , TRUE ~ NA_character_)
       , EDUC = case_when(EDUC.sh == 1 ~ "BELOW_HS"
                          , EDUC.sh ==2 ~"HS"
                          , EDUC.sh ==3 ~"SOME_COLL"
                          , EDUC.sh == 4 ~ "COLL_PLUS"
                          , EDUC.sh == 99 ~ "OTHER")
       , SELFEMP = ifelse(CLASSWKR ==1 , 1 ,0)
       , YRS.MIG = ifelse(YRIMMIG >0, 2023-YRIMMIG, -1)
       , PERWT = as.numeric(PERWT))


# We are saving one file per year
i <- 2000
for(i in c(2000:2023)){
  yr.dt <- dt[dt$YEAR == i,  ]
  yr.dt <- yr.dt %>%
    mutate(INCWAGE_adj23 = adjust_for_inflation(INCWAGE, from_date = i, country = "US", to_date = 2023))
  write_rds(yr.dt, paste0("~/Desktop/Occ_Wage_Gap/Paper/data_mod/JUL2026_acs_", i, "_estimates.rds"))
}


for(i in c(2000:2023)){
 colSums(is.na(dt[dt$YEAR == i, ]))
}
# End script 
