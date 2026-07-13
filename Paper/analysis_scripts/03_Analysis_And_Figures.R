# Purpose: produce the analysis across: 1) general time trends, 2) specific time
# periods, 3) Across decomposition components, and 4) Dimensions within decomposition
# components.
# Date created: April, 2026
# Last modified: July 3rd, 2026


################################################################################
############################ Load data and libraries ###########################
################################################################################
library(ggrepel)
library(tidyverse)

# Pull data on every occupation for every year: created ~/clean_scripts/02_Wrangle_for_Occ_Yr_Size_Dataset.R
dt <- readRDS("~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_occ_summary_nonparametric.rds")
dt_yr_trend <- readRDS("~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_year_summary_nonparametric.rds")
# Pull the decomposed data: created ~/analysis_script/01_Year_To_Year_Decomposition_ACS.R
dt_yr_decomp <- readRDS("~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_all_year_all_occ_decomposition.rds")
# Pull the occupation-level pre-post decomposition: created ~/analysis_script/~02_2012_to_2019_Decomposition.R
dt_decomp_mig <- readRDS("~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_w_in_nativity_decomp_migrant.rds")
dt_decomp_nat <- readRDS("~/Desktop/Occ_Wage_Gap/Paper/data_mod/20260703_w_in_nativity_decomp_native.rds")


# We assess whether growth is parallel or converging, and whether it is progressive or shifts at specific turning points. 
# We ask whether, for each of 416 occupations, these changes occur because of occupational reallocation or within-occupation wage gains. 
# Our findings inform an empirical framework to understand wage growth under the dual labor market theory of migration, providing granular 
# insights into how occupational hierarchy evolves over time. 

################################################################################
############### I. What are the trends in average wage by nativity #############
################################################################################

# Estimate the all year wages based on the share of people in each occupation 
# (from each job market, the native and the migrant)
occ_yr_wages <- dt%>%
 group_by(year)%>%
 mutate(pct_nat = w_count_native/sum(w_count_native)
     ,pct_mig = w_count_migrant/sum(w_count_migrant)) %>%
 dplyr::summarize(Native = sum(mean_wages_native*pct_nat)
          , `Foreign Born` = sum(mean_wages_migrant*pct_mig))%>%
 mutate(Rat = Native/`Foreign Born`) %>%
 pivot_longer(cols = 2:4, names_to = "Nativity", values_to = "est")%>%
 mutate(type = "Occupation Average Wage")

# Compare that to the worker-level averages by year produced in cleaning file, 
# the average of all workers in the labor market.

yr_trend <- dt_yr_trend |>
 mutate(Rat = native_avg_wage/immigrant_avg_wage) |>
 pivot_longer(cols = c("immigrant_avg_wage", "native_avg_wage", "Rat"), names_to = "Native", values_to = "est")|>
 mutate(type = "Worker Average"
     , Nativity = case_when(Native == "native_avg_wage"~ "Native"
                , Native == "immigrant_avg_wage"~ "Foreign Born"
                , Native == "Rat"~ "Rat"))|>
 select(year, Nativity, est,type)


############## Fig 1. Time series of average wages Native vs. FB ###############

fig.1 <- yr_trend%>%
 bind_rows(occ_yr_wages)|>
 filter(Nativity!= "Rat")|>
 ggplot(aes(x = as.numeric(year), y = est, group = interaction(Nativity,type) 
       ,color = Nativity)) + 
 geom_line()+
 scale_y_continuous(breaks = seq(30000, 80000, 2000)
           , labels = scales::dollar_format()
           , limits = c(50000, 75000))+
 scale_x_continuous(breaks = seq(2001,2023,5), limits = c(2000,2024))+
 scale_color_manual(values = c("darkslategrey", "darkorange1"))+
 labs(x = "Year"
    , y = "Average Wage"
    , title = "Trends in Average Wage"
    , color = "Nativity"
 )+
 theme(legend.text =element_text(size = 10)
    , legend.position = "bottom", legend.title = element_blank())+
 facet_wrap(~type, ncol =1)+
 theme_minimal() +
 theme(
  panel.grid.major.x = element_blank()
  , legend.position = "bottom", legend.title = element_blank()
  , text = element_text(family = "Times", size = 10)
  )

############## Fig 2. Year gap in average wages by avg wage measure ############
fig.2 <- yr_trend%>%
 bind_rows(occ_yr_wages)|>
 filter(Nativity == "Rat") |>
 ggplot(aes(x = as.numeric(year), y = est, group = type 
       ,color = type)) + 
 geom_line()+
 scale_y_continuous(breaks = seq(0.95, 1.25, .1), limits = c(0.95,1.25))+
 scale_x_continuous(breaks = seq(2001,2023,5), limits = c(2000,2024))+
 scale_color_manual(values = c("#D8B130", "#892844"))+
 geom_hline(yintercept = 1, color = "blue",linetype = 2)+
 labs(x = "Year"
    , y = "Wage Ratio"
    , title = "Ratio of Native Wages to Foreign Born Wages"
 )+
 theme(legend.text =element_text(size = 10)
    , legend.position = "bottom", legend.title = element_blank())+
 theme_minimal() +
 theme(
  panel.grid.major.x = element_blank()
  , legend.position = "bottom", legend.title = element_blank()
  , text = element_text(family = "Times", size = 10)
 )


################################################################################
################ II. Year by Year decomposition: waterfall plots ###############
################################################################################


##### Create auxiliary graph for wage decomposition #####
my_waterfall_graph <- function(a.df, a.title){
 int1 <- a.df %>%
  select(first_yr, Delta, C1:C3) %>%
  pivot_longer(C1:C3, names_to = "comp", values_to = "est")%>%
  mutate(
   yr = as.numeric(str_extract(first_yr, "\\d{4}")),
   t_int = factor(yr, levels = sort(unique(yr))),
   comp = factor(comp, levels = c("C1", "C2", "C3")))
 
 totals <- int1%>%
  group_by(t_int) %>%
  summarise(total = sum(est), .groups = "drop") %>%
  mutate(
   cum_total = cumsum(total),
   base = lag(cum_total, default = 0)
  )
 
 int2 <- int1 %>%
  left_join(totals, by = "t_int") %>%
  group_by(t_int) %>%
  arrange(comp) %>%
  mutate(
   ymin = base + cumsum(lag(est, default = 0)),
   ymax = ymin + est
  ) %>%
  ungroup()
 
 the.plot <- int2 %>%
  ggplot() +
  geom_rect(
   aes(
    xmin = yr - 0.4,
    xmax = yr + 0.4,
    ymin = ymin,
    ymax = ymax,
    fill = comp
   ),
   color = "white"
  ) +
  scale_y_continuous(breaks = seq(-4000, 12000, 2000), limits = c(-4000,12000)
            , labels = function(x){scales::dollar(x)})+
  scale_x_continuous(breaks = seq(2001,2023,2), limits = c(2000,2024))+
  scale_fill_manual(values = c("#44AA99", "#332288", "#CC6677", "gold" )) +
  labs(
   x = "Year",
   y = "Cumulative Growth In Avg Wage",
   fill = "Component", 
   title = a.title
  ) +
  geom_hline(yintercept = 0, linetype = 2, color = "blue")+
  theme_minimal() +
  theme(
   panel.grid.major.x = element_blank(), 
   text = element_text(family = "Times")
   
  )
 
 return(the.plot)
 
}


############## Fig 3. Waterfall plot of average wage growth for immigrants ############
all_yrs <- dt_decomp_mig%>%
 group_by(first_yr)%>%
 dplyr::summarise(C1 = round(sum(C1),digits =0)
          , C2 = round(sum(C2),digits =0)
          , C3 = round(sum(C3),digits =0)
          , Delta = round(sum(delta_wbar),digits =0))


fig.3 <- my_waterfall_graph(all_yrs, "Occupation Average Wage Growth Among Foreign Born")+ 
 labs( caption = "American Community Survey (2001-2023)")


all_yrs <- dt_decomp_nat%>%
 group_by(first_yr)%>%
 dplyr::summarise(C1 = round(sum(C1),digits =0)
          , C2 = round(sum(C2),digits =0)
          , C3 = round(sum(C3),digits =0)
          , Delta = round(sum(delta_wbar),digits =0))

############## Fig 4. Waterfall plot of average wage growth for natives ############

fig.4 <- my_waterfall_graph(all_yrs, "Occupation Average Wage Growth Among Natives")+ 
 labs( caption = "American Community Survey (2001-2023)")




################################################################################
############### III. Labor-market level decompositions of growth ###############
################################################################################
dt_all <- dt_yr_decomp|>
  mutate( yr = as.numeric(str_extract(year, "\\d{4}"))
          , period = case_when(between(yr, 2001, 2006) ~ "2001 - 2006"
                               ,between(yr, 2007, 2011)~ "2007 - 2011"
                               ,between(yr, 2012, 2019)~ "2012 - 2019"
                               ,between(yr, 2020, 2023)~ "2020 - 2023"))


periods <- c( "2001 - 2006", "2007 - 2011", "2012 - 2019", "2020 - 2023")
four_part_decomp_period <- lapply(as.list(periods, ), function(x){
  p1 <- dt_all[dt_all$period == x,  ]|>
   dplyr::summarize(C1 = sum(C1, na.rm = TRUE)
              , C2 = sum(C2, na.rm = TRUE)
              , C3 = sum(C3, na.rm = TRUE)
              , C4 = sum(C4, na.rm = TRUE)
              , Delta = sum(Delta, na.rm = TRUE))|>
    mutate(C1pct = C1/Delta
           , C2pct = C2/Delta
           , C3pct = C3/Delta
           , C4pct = C4/Delta)})


names(four_part_decomp_period) <- periods

four_part_decomp_period <- bind_rows(four_part_decomp_period, .id = "period")

fig.5 <- four_part_decomp_period|>
  pivot_longer(cols = C1:Delta, names_to = "comp", values_to = "est")|>
  ggplot(aes(x = period, y = est, fill = comp))+
  geom_bar(stat="identity", position = "dodge")+
  labs(x = "Period", y = "Change in wage gap by component", fill = "Component"
       , title = "Change in the wage gap across periods")+
  scale_y_continuous(breaks = seq(-350,50,50), labels = function(x){scales::dollar(x)})+
  theme_classic()+
  theme(text = element_text(family = "times"))


fig.6 <- four_part_decomp_period|>
  pivot_longer(cols = C1pct:C4pct, names_to = "comp", values_to = "est")|>
  ggplot(aes(x = period, y = est, fill = comp))+
  geom_bar(stat="identity", position = "dodge")+
  geom_hline(yintercept = 0) +
  geom_text(
    aes(label = scales::percent(est,accuracy = .1)), 
    position = position_dodge(width = 1), 
    vjust = -0.5,     
    size = 3.5        
  )+
  scale_fill_discrete(labels = c("Occupational Segregation", "Wage Inequality", "Occupational Growth", "Wage Growth")) +
  scale_y_continuous(breaks = seq(-.1, 1.1, .1), labels = function(x){scales::percent(x)}) +
  labs(x = "Period", title = "Percentage of the change in wage gap by component", fill = "Component"
       , y = "Percentage of the change")+
  theme_classic()+
  theme(text = element_text(family = "times"))



four_part_decomp_all <- dt_all|>
  dplyr::summarize(C1 = sum(C1, na.rm = TRUE)
                   , C2 = sum(C2, na.rm = TRUE)
                   , C3 = sum(C3, na.rm = TRUE)
                   , C4 = sum(C4, na.rm = TRUE)
                   , Delta = sum(Delta, na.rm = TRUE))|>
  mutate(C1pct = C1/Delta
         , C2pct = C2/Delta
         , C3pct = C3/Delta
         , C4pct = C4/Delta)

fig.7 <- four_part_decomp_all|>
  pivot_longer(cols = C1pct:C4pct, names_to = "comp", values_to = "est")|>
  ggplot(aes(x = comp, y = est))+
  geom_bar(stat="identity", position = "dodge")+
  geom_hline(yintercept = 0) +
  geom_text(
    aes(label = scales::percent(est,accuracy = .1)), 
    position = position_dodge(width = 1), 
    vjust = -0.5,     
    size = 3.5)+
  scale_x_discrete(labels = c("Occupational Segregation", "Wage Inequality", "Occupational Growth", "Wage Growth")) +
  scale_y_continuous(breaks = seq(-.1, 1.1, .1), labels = function(x){scales::percent(x)}) +
  labs(x = "Period", title = "Decomposition of decline in wage gap 2001-2023", fill = "Component"
       , y = "Percentage of the change")+
  theme_classic()+
  theme(text = element_text(family = "times"))

################################################################################
################# IV. Tables of growth trends by occ, nativity #################
################################################################################

############## Table 1. Occupational trends by nativity ############

mutate_occupation_trajectory_table <- function(a.dt){
 a.dt |>
  group_by(occ_title, type)|>
  dplyr::summarise(C1 = round(sum(C1, na.rm = TRUE))
           , C2 = round(sum(C2, na.rm = TRUE))
           , C3 = round(sum(C3, na.rm = TRUE))
           , Delta = round(sum(delta_wbar, na.rm = TRUE))
           , wage_growth =sum(wage_growth, na.rm = TRUE)
           , part_growth = sum(part_growth, na.rm = TRUE))%>%ungroup()%>%
  mutate(nice_occ = occ_title %>%str_replace_all(., "_", " ")%>%str_to_sentence())|>
  mutate(category_fill = case_when(C1 >0 & C2 >0 ~ "Entries and wage growth"
                   , C1 >0 & C2<=0 ~ "Entries, but wage decline"
                   , C1 <=0 & C2>0 ~ "Exits, but wage growth"
                   , C2<=0 & C2<=0 ~ "Exits and wage decline"
                   , TRUE ~ "TRUE")
      , category_fill = factor(category_fill, levels =c("Entries and wage growth","Exits, but wage growth"
                               , "Entries, but wage decline", "Exits and wage decline")
                  , ordered = TRUE))
 
}


dt_birth <- dt_decomp_nat|>
 mutate(type = "native") |>
 bind_rows(dt_decomp_mig)|>
 mutate( yr = as.numeric(str_extract(first_yr, "\\d{4}"))
     , type = ifelse(is.na(type), "migrant", type)
     , period = case_when(between(yr, 2001, 2006) ~ "2001 - 2006"
                ,between(yr, 2007, 2011)~ "2007 - 2011"
                ,between(yr, 2012, 2019)~ "2012 - 2019"
                ,between(yr, 2020, 2023)~ "2020 - 2023"))
 


# Create a table of the outcomes by occupation 2001 to 2023
all_years <- dt_birth |>
 mutate_occupation_trajectory_table()

 
tab.1 <- all_years|>
 select(-c(C1:part_growth)) |>
 pivot_wider(values_from = category_fill, names_from = type)|>
 janitor::tabyl(migrant, native)


############## Table 2. Occupational trends by nativity by period ##############

periods <- c( "2001 - 2006", "2007 - 2011", "2012 - 2019", "2020 - 2023")
tab.2_period_tables <- lapply(as.list(periods) , function(x){
 dt_birth |>
  filter(period == x)|>
  mutate_occupation_trajectory_table()|>
  select(-c(C1:part_growth)) |>
  pivot_wider(values_from = category_fill, names_from = type)|>
  janitor::tabyl(migrant, native)
 
})

names(tab.2_period_tables) <- periods


diagonal_estimations <- lapply(tab.2_period_tables, function(x){
 a.tb <- as.matrix(x[,-1])/417
 above.diag<- sum(a.tb[upper.tri(a.tb)])
 diagonal <- sum(diag(a.tb))
 below.diag <- sum(a.tb[lower.tri(a.tb)])
 return(list("diag" = diagonal, "abv.diag" = above.diag, "blw.diag" = below.diag))
})


################################################################################
################ V. Graphs of growth trends by component & occ ################
################################################################################

plot_C1_quad <- function(data, fill_filter = NULL, color, title, label_filters, x_var, y_var, size_var, type, filter_var) {
 
 if (!is.null(fill_filter)) {
  plot_data <- data %>% filter({{filter_var}} == fill_filter)
 } else {
  plot_data <- data
 }
 
 to_label <- plot_data %>% filter(eval(label_filters))
 
 plot_data |>
  ggplot(aes(x = {{x_var}}, y = {{y_var}}, size = {{size_var}}, colour = {{filter_var}})) +
  geom_point(alpha = 0.3) +
  geom_text_repel(
   data = to_label,
   aes(label = nice_occ),
   size = 3,
   max.overlaps = 20,
   box.padding = 0.4,
   segment.color = "gray50"
  ) +
  scale_color_manual(values = color) +
  scale_size_manual(values = cat_sizes, name = "Occupation Size") +
  geom_abline(intercept = 0, slope = 1, color = "grey50", linetype ="dotted")+
  geom_abline(intercept = 0, slope = -1, color = "grey50", linetype ="dotted")+
  geom_hline(yintercept = 0, color = "grey50")+
  geom_vline(xintercept = 0, color = "grey50")+
  theme_minimal() +
  labs(title = title,
     color = "Relative advantage",
     x = paste0(type, " Among Natives"),
     y = paste0(type," Among Foreign Born"))
}



dt_birth <- dt_decomp_nat|>
 left_join(dt_decomp_mig, by = c("first_yr", "occ_title"), suffix = c("_nat", "_mig"))|>
 mutate( yr = as.numeric(str_extract(first_yr, "\\d{4}"))
     , period = case_when(between(yr, 2001, 2006) ~ "2001 - 2006"
                ,between(yr, 2007, 2011)~ "2007 - 2011"
                ,between(yr, 2012, 2019)~ "2012 - 2019"
                ,between(yr, 2020, 2023)~ "2020 - 2023"))


dt_birth <- dt_birth|>
 mutate(C1 = C1_mig/C1_nat
     , C2 = C2_mig/C2_nat)|>
 mutate(fig1_fill = case_when(C1_nat >=0 & C1_mig>=0 ~ "Both Advantage"
                , C1_nat >0 & C1_mig<=0 ~ "C1 Advantage"
                , C1_nat <=0 & C1_mig>0 ~ "C2 Advantage"
                , C1_nat<=0 & C1_mig<=0 ~ "Both Disadvantage"
                , TRUE ~ "TRUE")
     , fig2_fill = case_when(C2_nat >=0 & C2_mig >=0 ~ "Both Advantage"
                 , C2_nat >0 & C2_mig<=0 ~ "Native Advantage"
                 , C2_nat <=0 & C2_mig>0 ~ "Migrant Advantage"
                 , C2_nat<=0 & C2_mig<=0 ~ "Both Disadvantage"
                 , TRUE ~ "TRUE")
     ,tot_size = w_count_migrant_y1+w_count_native_y1
     ,tot_size_cat = cut(tot_size, c(0, 20000, 100000,440000,1000000, max(tot_size))
               , labels = c("Under 20,000", "20,001 to 100,000", "100,001 to 440,000"
                     , "440,001 to 1,000,000", "Over 1,000,000")))|>
 mutate(nice_occ = occ_title %>%str_replace_all(., "_", " ")%>%str_to_sentence())



to_label <- dt_birth %>%
 filter( 
  C1_mig > 1e6    |  # largest bubbles
   C1_nat > 400     |  # rightmost points
   C1_mig > 1000     |  # topmost points
   C1_mig < -200       # bottommost points
 )

cat_sizes <- c(
 "Under 20,000"     = 1,
 "20,001 to 100,000"   = 3,
 "100,001 to 440,000"  = 4,
 "440,001 to 1,000,000" = 5,
 "Over 1,000,000"    = 6
)


################################################################################
################# C1 component: between occupation reallocation ################
################################################################################

period_bubble_data <- function(x, a.period){
 
 p1<- dt_birth[dt_birth$period %in% a.period,] |>
  group_by(occ_title)|>
  dplyr::summarize(C1_nat = sum(C1_nat, na.rm = TRUE)
           , C2_nat = sum(C2_nat, na.rm = TRUE)
           , C1_mig = sum(C1_mig, na.rm = TRUE)
           , C2_mig = sum(C2_mig, na.rm = TRUE)
           , delta_nat = sum(delta_wbar_nat, na.rm = TRUE)
           , delta_mig = sum(delta_wbar_mig, na.rm = TRUE)
           , tot_size = max(tot_size)
           , tot_size_cat = unique(tot_size_cat)[1])|>
  mutate( C1 = C1_mig/C1_nat
      , C2 = C2_mig/C2_nat
      , fig1_fill = case_when(C1_nat >=0 & C1_mig>=0 ~ "Native entries, Foreign born entries"
                  , C1_nat >0 & C1_mig<=0 ~ "Native entries, Foreign born exits"
                  , C1_nat <=0 & C1_mig>0 ~ "Native exits, Foreign born entries"
                  , C1_nat<=0 & C1_mig<=0 ~ "Native exits, Foreign born exits"
                  , TRUE ~ "TRUE")
      , fig2_fill = case_when(C2_nat >=0 & C2_mig >=0 ~ "Native wage gain, Foreign born wage gain"
                  , C2_nat >0 & C2_mig<=0 ~ "Native wage gain, Foreign born wage loss"
                  , C2_nat <=0 & C2_mig>0 ~ "Native wage loss, Foreign born wage gain"
                  , C2_nat<=0 & C2_mig<=0 ~ "Native wage loss, Foreign born wage loss"
                  , TRUE ~ "TRUE")
      , fig3_fill = case_when(delta_nat >=0 & delta_mig >=0 ~ "Native gain, Foreign born gain"
                  , delta_nat >0 & delta_mig<=0 ~ "Native gain, Foreign born loss"
                  , delta_nat <=0 & delta_mig>0 ~ "Native loss, Foreign born gain"
                  , delta_nat<=0 & delta_mig<=0 ~ "Native loss, Foreign born loss"
                  , TRUE ~ "TRUE"))|>
  mutate(nice_occ = occ_title %>%str_replace_all(., "_", " ")%>%str_to_sentence())
 
 # Both Advantage
 C1_quadII <- plot_C1_quad(
  data     = p1[p1$C1_nat>0 & p1$C1_mig>0, ],
  x_var     = C1_nat,
  y_var     = C1_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig1_fill,
  fill_filter = "Native entries, Foreign born entries",
  color    = c("Native entries, Foreign born entries" = "#4DD091"),
  title    = "Immigrant & C1 Advantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native entries"
     , y = "Foreign born entries"
     , title = "Dollar gains in average wage from occupational sorting component")
 # Migrant Advantage
 
 C1_quadI <- plot_C1_quad(
  data     = p1[p1$C1_nat<=0 & p1$C1_mig>0, ],
  x_var     = C1_nat,
  y_var     = C1_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig1_fill,
  fill_filter = "Native exits, Foreign born entries",
  color    = c("Native exits, Foreign born entries" = "#FFA23A"),
  title    = "C2 Advantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native exits"
     , y = "Foreign born entries"
     , title = "Dollar gains in average wage from occupational sorting component")
 
 
 # Both Disadvantage
 C1_quadIII <- plot_C1_quad(
  data     = p1[p1$C1_nat<=0 & p1$C1_mig<=0, ],
  x_var     = C1_nat,
  y_var     = C1_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig1_fill,
  fill_filter = "Native exits, Foreign born exits",
  color    = c("Native exits, Foreign born exits" = "#FF5768"),
  title    = "Native & Immigrant Disadvantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5))+
   labs(x = "Native exits"
      , y = "Foreign born exits"
      , title = "Dollar gains in average wage from occupational sorting component")
 
 
 # C1 Advantage
 C1_quadIV <- plot_C1_quad(
  data     = p1[p1$C1_nat>0 & p1$C1_mig<=0, ],
  x_var     = C1_nat,
  y_var     = C1_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig1_fill,
  fill_filter = "Native entries, Foreign born exits",
  color    = c("Native entries, Foreign born exits" = "#6C88C4"),
  title    = "Native & Immigrant Disadvantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native entries"
     , y = "Foreign born exits"
     , title = "Dollar gains in average wage from occupational sorting component")
 
 
 C1_all_quads <- plot_C1_quad(
  data     = p1,
  x_var     = C1_nat,
  y_var     = C1_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig1_fill,
  color     = c("Native entries, Foreign born entries"  = "#4DD091",
           "Native exits, Foreign born entries" = "#FFA23A",
           "Native exits, Foreign born exits" = "#FF5768",
           "Native entries, Foreign born exits" = "#6C88C4"),
  title     = "C1 Component by Nativity",
  label_filters = quote(tot_size > 1e6 | C1 > 10 | C2 > 10 | C1 < -10 | C2 < -10)
 )+
  labs(x = "Dollar gains from native entries or exits"
     , y = "Dollar gains from foreign-born entries or exits"
     , title = "Dollar gains in average wage from occupational sorting component")
 
 
 
 # Both Advantage
 C2_quadII <- plot_C1_quad(
  data     = p1[p1$C2_nat>0 & p1$C2_mig>0, ],
  x_var     = C2_nat,
  y_var     = C2_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig2_fill,
  fill_filter = "Native wage gain, Foreign born wage gain",
  color    = c("Native wage gain, Foreign born wage gain" = "#4DD091"),
  title    = "Immigrant & C1 Advantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native wage gains"
     , y = "Foreign born wage gains"
     , title = "Dollar gains in average wage from wage component")
 # Migrant Advantage
 
 C2_quadI <- plot_C1_quad(
  data     = p1[p1$C2_nat<=0 & p1$C2_mig>0, ],
  x_var     = C2_nat,
  y_var     = C2_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig2_fill,
  fill_filter = "Native wage loss, Foreign born wage gain",
  color    = c("Native wage loss, Foreign born wage gain" = "#FFA23A"),
  title    = "C2 Advantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native wage losses"
     , y = "Foreign born wage gains"
     , title = "Dollar gains in average wage from wage component")
 
 # Both Disadvantage
 C2_quadIII <- plot_C1_quad(
  data     = p1[p1$C2_nat<=0 & p1$C2_mig<=0, ],
  x_var     = C2_nat,
  y_var     = C2_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig2_fill,
  fill_filter = "Native wage loss, Foreign born wage loss",
  color    = c( "Native wage loss, Foreign born wage loss" = "#FF5768"),
  title    = "Native & Immigrant Disadvantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native wage losses"
     , y = "Foreign born wage losses"
     , title = "Dollar gains in average wage from wage component")
 
 # C1 Advantage
 C2_quadIV <- plot_C1_quad(
  data     = p1[p1$C2_nat>0 & p1$C2_mig<=0, ],
  x_var     = C2_nat,
  y_var     = C2_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig2_fill,
  fill_filter = "Native wage gain, Foreign born wage loss",
  color    = c("Native wage gain, Foreign born wage loss" = "#6C88C4"),
  title    = "Native & Immigrant Disadvantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native wage gains"
     , y = "Foreign born wage losses"
     , title = "Dollar gains in average wage from wage component")
 
 
 C2_all_quads <- plot_C1_quad(
  data     = p1,
  x_var     = C2_nat,
  y_var     = C2_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig2_fill,
  color     = c("Native wage gain, Foreign born wage gain"  = "#4DD091",
           "Native wage loss, Foreign born wage gain" = "#FFA23A",
           "Native wage loss, Foreign born wage loss" = "#FF5768",
           "Native wage gain, Foreign born wage loss" = "#6C88C4"),
  title     = "C1 Component by Nativity",
  label_filters = quote(tot_size > 1e6 | C1 > 10 | C2 > 10 | C1 < -10 | C2 < -10))+
  labs(x = "Dollar gains from native wage gains or losses"
     , y = "Dollar gains from foreign-born wage gains or losses"
     , title = "Dollar gains in average wage from wage component")
 
 
 # Migrant Advantage
 # Both Advantage
 delta_quadII <- plot_C1_quad(
  data     = p1[p1$delta_nat>0 & p1$delta_mig>0, ],
  x_var     = delta_nat,
  y_var     = delta_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig3_fill,
  fill_filter = "Native gain, Foreign born gain",
  color    = c("Native gain, Foreign born gain" = "#4DD091"),
  title    = "Immigrant & C1 Advantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native gains"
     , y = "Foreign born gains"
     , title = "Dollar gains in average from component")
 delta_quadI <- plot_C1_quad(
  data     = p1[p1$delta_nat<=0 & p1$delta_mig>0, ],
  x_var     = delta_nat,
  y_var     = delta_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig3_fill,
  fill_filter = "Native loss, Foreign born gain",
  color    = c("Native loss, Foreign born gain" = "#FFA23A"),
  title    = "delta Advantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native losses"
     , y = "Foreign born gains"
     , title = "Dollar gains in average from component")
 
 # Both Disadvantage
 delta_quadIII <- plot_C1_quad(
  data     = p1[p1$delta_nat<=0 & p1$delta_mig<=0, ],
  x_var     = delta_nat,
  y_var     = delta_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig3_fill,
  fill_filter = "Native loss, Foreign born loss",
  color    = c( "Native loss, Foreign born loss" = "#FF5768"),
  title    = "Native & Immigrant Disadvantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native losses"
     , y = "Foreign born losses"
     , title = "Dollar gains in average from component")
 
 # C1 Advantage
 delta_quadIV <- plot_C1_quad(
  data     = p1[p1$delta_nat>0 & p1$delta_mig<=0, ],
  x_var     = delta_nat,
  y_var     = delta_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig3_fill,
  fill_filter = "Native gain, Foreign born loss",
  color    = c("Native gain, Foreign born loss" = "#6C88C4"),
  title    = "Native & Immigrant Disadvantage Across C1 Component",
  label_filters = quote(tot_size > 1e6 | C1 > 5 | C2 > 5 | C1 < -5 | C2 < -5)
 )+
  labs(x = "Native gains"
     , y = "Foreign born losses"
     , title = "Delta")
 
 
 delta_all_quads <- plot_C1_quad(
  data     = p1,
  x_var     = delta_nat,
  y_var     = delta_mig,
  size_var   = tot_size_cat,
  type = "C1",
  filter_var = fig3_fill,
  color     = c("Native gain, Foreign born gain"  = "#4DD091",
           "Native loss, Foreign born gain" = "#FFA23A",
           "Native loss, Foreign born loss" = "#FF5768",
           "Native gain, Foreign born loss" = "#6C88C4"),
  title     = "C1 Component by Nativity",
  label_filters = quote(tot_size > 1e6 | C1 > 10 | C2 > 10 | C1 < -10 | C2 < -10))+
  labs(x = "Dollar gains or losses for natives"
     , y = "Dollar gains or losses for foreign-born"
     , title = "Dollar gains in average wage from wage component")
 return(list("C1_q1" = C1_quadI ,"C1_q2" = C1_quadII ,"C1_q3" = C1_quadIII ,"C1_q4" = C1_quadIV , "C1_all" = C1_all_quads,
       "C2_q1" = C2_quadI ,"C2_q2" = C2_quadII ,"C2_q3" = C2_quadIII ,"C2_q4" = C2_quadIV, "C2_all" = C2_all_quads, 
       "delta_q1" = delta_quadI ,"delta_q2" = delta_quadII ,"delta_q3" = delta_quadIII ,"delta_q4" = delta_quadIV, "delta_all" = delta_all_quads, 
       "p1" = p1))
 
}



period_1_bubble_graphs <- period_bubble_data(dt_birth, "2001 - 2006")
period_2_bubble_graphs <- period_bubble_data(dt_birth, "2007 - 2011")
period_3_bubble_graphs <- period_bubble_data(dt_birth, "2012 - 2019")
period_4_bubble_graphs <- period_bubble_data(dt_birth, "2020 - 2023")
period_5_bubble_graphs <- period_bubble_data(dt_birth, c("2001 - 2006","2007 - 2011", "2012 - 2019", "2020 - 2023"))



#
period_1_bubble_graphs$C1_all+
 scale_y_continuous(limits = c(-600, 600))+
 scale_x_continuous(limits = c(-600, 600))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2001 - 2007 Dollar gains in average wage from occupational sorting component")
 
 
period_2_bubble_graphs$C1_all+
 scale_y_continuous(limits = c(-600, 600))+
 scale_x_continuous(limits = c(-600, 600))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+

 labs(title = "2008 - 2011 Dollar gains in average wage from occupational sorting component")



period_3_bubble_graphs$C1_all+
 scale_y_continuous(limits = c(-600, 600))+
 scale_x_continuous(limits = c(-600, 600))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2012 - 2019 Dollar gains in average wage from occupational sorting component")



period_4_bubble_graphs$C1_all+
 scale_y_continuous(limits = c(-500, 500))+
 scale_x_continuous(limits = c(-500, 500))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2020 - 2023 Dollar gains in average wage from occupational sorting component")




period_5_bubble_graphs$C1_all+
 scale_y_continuous(limits = c(-1200, 2800))+
 scale_x_continuous(limits = c(-1200, 2800))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2001 - 2023 Dollar gains in average wage from occupational sorting component")





period_1_bubble_graphs$delta_all+
 scale_y_continuous(limits = c(-600, 600))+
 scale_x_continuous(limits = c(-600, 600))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2001 - 2007 Overall dollar gains in average wage by nativity status")


period_2_bubble_graphs$delta_all+
 scale_y_continuous(limits = c(-600, 600))+
 scale_x_continuous(limits = c(-600, 600))+
theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2008 - 2011 Overall dollar gains in average wage by nativity status")



period_3_bubble_graphs$delta_all+
 scale_y_continuous(limits = c(-600, 600))+
 scale_x_continuous(limits = c(-600, 600))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2012 - 2019 Overall dollar gains in average wage by nativity status")



period_4_bubble_graphs$delta_all+
 scale_y_continuous(limits = c(-500, 500))+
 scale_x_continuous(limits = c(-500, 500))+
 theme_classic()+
  scale_y_continuous(labels = function(x){scales::dollar(x)})+
  scale_x_continuous(labels = function(x){scales::dollar(x)})+
  theme(text = element_text(family = "times"))+
 labs(title = "2020 - 2023 Overall dollar gains in average wage by nativity status")




period_5_bubble_graphs$delta_all+
 scale_y_continuous(limits = c(-1200, 2800))+
 scale_x_continuous(limits = c(-1200, 2800))+
 theme_classic()+
 labs(title = "2001 - 2023 Overall dollar gains in average wage by nativity status")

