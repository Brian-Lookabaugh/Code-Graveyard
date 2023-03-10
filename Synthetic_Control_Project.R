#######-------Synthetic Control-------#######

pacman::p_load(
  "tidyverse", # Data Manipulation and Visualization
  "tidysynth", # Tidy Implementation of the Synthetic Control Method
  "DescTools", # Carrying Values of Observations Forward (LOCF)
  "haven", # Importing Data from Stata
  "readxl", # Importing Data from Excel
  install = FALSE,
)

############################################################
########--------Data Collection and Cleaning--------########
############################################################

## UCDP Data

load("Data/ucdp_prio_acd_221.RData")
ucdp <- ucdp_prio_acd_221

# Drop Non-Civil War Cases (1K Threshold)

ucdp <- ucdp %>%
  mutate(gwno_a = as.numeric(gwno_a)) %>% # Convert the country code to a numeric
  filter(type_of_conflict == 3 & cumulative_intensity == 1) %>%
  mutate(civ_war = 1) %>%
  
  # Collapse Data Into Country-Year Units
  group_by(gwno_a, year) %>%
  summarise(civ_war = max(civ_war)) %>%
  ungroup()

# Merge COW Country-Year Data

states <- read.csv("Data/system2016.csv")

synth_data <- full_join(ucdp, states,
                        by = c("gwno_a" = "ccode", "year")) %>%
  
  # Generate Prior Civil War Variable
  
  rename(ccode = gwno_a) %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(prior_civ_war = LOCF(civ_war)) %>%
  ungroup() %>%
  mutate(civ_war = if_else( # Replace NA Civil War Values With 0
    is.na(civ_war), 0, civ_war
  )) %>%
  
  # Generate Conflict Recurrence and Termination Variables
  
  group_by(ccode) %>%
  mutate(lag_civ_war = lag(civ_war, n = 1, order_by = ccode)) %>%
  ungroup() %>%
  mutate(recur = if_else(
    civ_war == 1 & lag_civ_war == 0, 1, 0
  )) %>%
  mutate(recur = if_else(
    is.na(recur), 0, recur
  )) %>%
  mutate(termination = if_else(
    civ_war == 0 & lag_civ_war == 1, 1, 0
  )) %>%
  mutate(termination = if_else(
    is.na(termination), 0, termination
  ))

## PKO Data

pko <- read_dta("Data/formattedmullenbach2013pkodata.dta")

synth_data <- left_join(synth_data, pko,
                        by = c("ccode", "year")) %>%
  group_by(ccode) %>%
  mutate(pko_pres = LOCF(PKO)) %>% 
  mutate(pko_pres = if_else(
    is.na(pko_pres), 0, pko_pres
  )) %>%
  mutate(ever_pko = pko_pres) %>% # Simply a Dummy for Whether a PKO Was Present In a Country At All (For Visualizations)
  fill(ever_pko, .direction = "downup") %>%
  fill(ever_pko, .direction = "updown") %>%
  mutate(ever_pko = if_else(
    is.na(ever_pko), 0, ever_pko
  )) %>%
  ungroup()

## V-Dem Data

vdem <- read.csv("Data/selectedvdemdata.csv")

vdem <- vdem %>%
  rename(ccode = COWcode) %>%
  mutate(lgdppc = log(e_gdppc + 1)) %>%
  mutate(lpop = log(e_pop)) %>%
  mutate(l_wb_pop = log(e_wb_pop)) %>%
  mutate(l_mi_pop = log(e_mipopula))

synth_data <- left_join(synth_data, vdem,
                        by = c("ccode", "year"))

## Military Capacity Data

mil_per <- read.csv("Data/NMC-60-abridged.csv")

mil_per <- mil_per %>%
  filter(milper != -9) %>%
  mutate(lmilper = log(milper + 1)) %>%
  select(ccode, year, lmilper)

synth_data <- left_join(synth_data, mil_per,
                        by = c("ccode", "year"))

## Merge Global Terrorism Data

gtd <- read_excel("Data/selectedgtddata.xlsx")

gtd_deaths <- gtd %>% # Collapse Data to Get the Number of Casualties
  group_by(country_txt, iyear) %>%
  summarise(terr_deaths = sum(nkill, na.rm = TRUE)) %>%
  ungroup()

gtd_count <- gtd %>% # Collapse Data to Get the Number of Events
  group_by(country_txt, iyear) %>%
  summarise(event_count = n_distinct(eventid, na.rm = TRUE)) %>%
  ungroup()

gtd_combined <- inner_join(gtd_deaths, gtd_count,
                           by = c("country_txt", "iyear")) %>%
  mutate(country_txt = str_replace( # Rename Country Names for Merging
    country_txt, "Bosnia-Herzegovina", "Bosnia and Herzegovina"
  )) %>%
  mutate(country_txt = str_replace(
    country_txt, "Mynanmar", "Burma/Myanmar"
  )) %>%
  mutate(country_txt = str_replace(
    country_txt, "Vietnam", "Republic of Vietnam"
  )) %>%
  mutate(country_txt = str_replace(
    country_txt, "Serbia-Montenegro", "Serbia"
  )) %>%
  mutate(country_txt = str_replace(
    country_txt, "Soviet Union", "Russia"
  )) %>%
  mutate(country_txt = str_replace(
    country_txt, "Yugoslavia", "Serbia"
  ))

synth_data <- left_join(synth_data, gtd_combined,
                        by = c("country_name" = "country_txt", "year" = "iyear"))

## Merge SCAD Data

scad_africa <- read.csv("Data/SCAD2018Africa_Final.csv")

scad_africa <- scad_africa %>%
  group_by(ccode, styr) %>%
  filter(ndeath >= 0) %>% # Remove NA Values
  summarise(ll_deaths = sum(ndeath, na.rm = TRUE)) %>%
  ungroup()

synth_data <- left_join(synth_data, scad_africa,
                        by = c("ccode", "year" = "styr"))

scad_latam <- read.csv("Data/SCAD2018LatinAmerica_Final.csv")

scad_latam <- scad_latam %>%
  group_by(ccode, styr) %>%
  filter(ndeath >= 0) %>% 
  summarise(ll_deaths = sum(ndeath, na.rm = TRUE)) %>%
  ungroup()

synth_data <- left_join(synth_data, scad_latam,
                        by = c("ccode", "year" = "styr")) %>%
  unite(ll_deaths, ll_deaths.x:ll_deaths.y, na.rm = TRUE) %>% # Combine SCAD Africa/LATAM Low-Level Deaths Values
  mutate(ll_deaths = as.numeric(ll_deaths)) # Re-Convert to Numeric

## Final Data Cleaning

synth_data <- synth_data %>%
  select(-c(stateabb, version, lag_civ_war, PKO, e_pt_coup, e_gdppc)) %>% # Remove Unnecessary Columns
  select(country_name, ccode, year, terr_deaths, event_count, ll_deaths, pko_pres, ever_pko, everything()) %>% # Ordering Rows
  rename(democracy = v2x_polyarchy, imr = e_peinfmor) %>%
  # Filter Countries That Have No Predictor/Outcome Values
  filter(country_name != "NA") %>%
  # Filer Countries That Have Never Experienced Conflict
  filter(prior_civ_war != "NA")

########################################################
########--------Synthetic Control Set-Up--------########
########################################################

## Generate Last Observed Data Transformation to Reduce Missingness

synth_data <- synth_data %>%
  group_by(ccode) %>%
  mutate(rep_pop = e_pop) %>%
  fill(rep_pop, .direction = "downup") %>%
  fill(rep_pop, .direction = "updown") %>%
  mutate(rep_democ = democracy) %>%
  fill(rep_democ, .direction = "downup") %>%
  fill(rep_democ, .direction = "updown") %>%
  mutate(rep_imr = imr) %>%
  fill(rep_imr, .direction = "downup") %>%
  fill(rep_imr, .direction = "updown") %>%
  mutate(rep_lgdppc = lgdppc) %>%
  fill(rep_lgdppc, .direction = "downup") %>%
  fill(rep_lgdppc, .direction = "updown") %>%
  mutate(rep_lmilper = lmilper) %>%
  fill(rep_lmilper, .direction = "downup") %>%
  fill(rep_lmilper, .direction = "updown") %>%
  mutate(rep_events = event_count) %>%
  fill(rep_events, .direction = "downup") %>%
  fill(rep_events, .direction = "updown") %>%
  ungroup() %>%
  # Filter Remaining Countries That Have No Information on Outcome and Predictor Values
  filter(rep_events != "NA" & rep_imr != "NA")

## Generate Per Capita Variables for Synthetic Control Outcome

synth_data <- synth_data %>%
  mutate(event_pc = (event_count / imp_pop)) %>%
  mutate(terr_deaths_pc = (terr_deaths / imp_pop))

## Create the Synthetic Unit

scm_object <- synth_data %>%
  
  ## Create the Synthetic Control Object
  
  synthetic_control(outcome = rep_events,
                    unit = ccode,
                    time = year,
                    i_unit = "Guatemala",
                    i_time = 1997,
                    generate_placebos = T
  ) %>%
  
  ## Generate Average Predictors
  
  generate_predictor(time_window = 1974:1997,
                     mn_lgdppc = mean(rep_lgdppc, na.rm = T),
                     mn_imr = mean(rep_imr, na.rm = T),
                     mn_democ = mean(rep_democ, na.rm = T),
                     mn_lmilper = mean(rep_lmilper, na.rm = T)
  ) %>%
  
  ## Generate Weights
  
  generate_weights(optimization_window = 1974:1997) %>%
  
  ## Generate the Synthetic Control
  
  generate_control()

#####################################################################
########--------Synthetic Control Graphics and Tables--------########
#####################################################################

## Time Series Plot

ts_plot <- scm_object %>%
  plot_trends()

## Difference in Synthetic and Observed Plot

diff_plot <- scm_object %>%
  plot_differences()

## Unit and Variable Weights Plot

w_plot <- scm_object %>%
  plot_weights()

## Table Comparing Synthetic to Observed

scm_object %>%
  grab_balance_table()

## Unit Placebos

placebo <- scm_object %>%
  plot_placebos()

## Unit Placebos With Extreme Values

placebo_ext <- scm_object %>%
  plot_placebos(prune = FALSE)

## Time Placebo

## Post/Pre-MSPE

mspe_plot <- scm_object %>%
  plot_mspe_ratio()

## Tables With Significance of Unit Treatments

scm_object %>%
  grab_signficance()

# Robustness Check 1: Leave-One-Out

# Robustness Check 2: Alternative Predictors