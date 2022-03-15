# prep life history parameters
# jane.sullivan@noaa.gov
# february 2022

# 1) pull 

# setup ----
libs <- c("tidyverse", "googlesheets4")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

YEAR <- 2021

out_path <- paste0("results")
dir.create(out_path)
out_path <- paste0("results/", YEAR)
dir.create(out_path)

source('R/M_fxns.R')

# data ----

# see rockfish_M_updates google sheet in ROX_M_updates_2021 google folder
lh <- read_sheet("https://docs.google.com/spreadsheets/d/1TvE1MaVEIdS_7YMnR3gwxATbY5Q8jzmEUiD4nsnAZ5M/edit?pli=1#gid=0",
           sheet = 'life_history_parameters')
lh$species <- sub('_', ' ', lh$species)

nrow(lh)
lh
lh$use
# tmax estimates based on combined fishery and survey age data. see tmax.R
tmax <- read_csv(paste0(out_path, '/alldat_tmax.csv'))
tmax
unique(tmax$area)

# test to see if the spreadsheet has any errors identifying which region the
# data represent. Should be zero rows, and if it's not GOA:WC should be 0 and
# multi_region should be 1
lh %>% 
  mutate(multi_region_test = GOA+BS+AI+BC+WC) %>% 
  filter(multi_region_test > 1)

# bind tmax estimates from AFSC fishery and survey data to user-defined life
# history parameter data
lhfull <- tmax %>% 
  select(species = common_name, data_area = area, 
         # afsc_allages_q99 = q99, # decided as a group not to use this one
         afsc_allages_max_age = max_age, afsc_allages_mean_top5 = mean_top5) %>% 
  pivot_longer(cols = -c(species, data_area), names_to = 'method_or_source', values_to = 'estimate') %>% 
  mutate(lh_param = 'maxage_yr',
         use = 1,
         GOA = ifelse(data_area == 'GOA', 1, 0),
         BS = ifelse(data_area == 'BS', 1, 0),
         AI = ifelse(data_area == 'AI', 1, 0),
         BC = 0,
         WC = 0,
         multi_region = 0) %>% 
  bind_rows(lh) 

unique(lhfull$lh_param)

# omit any data that aren't going to get used only keep parameter inputs that
# are relevant to the four selected methods (see M_fxns.R)
input_data <- lhfull %>%
  filter(use == 1) %>% 
  filter(lh_param %in% c('maxage_yr',
                         'gsi',
                         'dry_weight_g',
                         'temp_C',
                         'vbgf_k_cm-1',
                         'vbgf_linf_cm')) %>% 
  select(-use) 

input_data %>% filter(BC == 1 | WC == 1)

# create new column for input_area, get rid of 0 values, create a version number
# for each species, input_area, and lh_param
input_data <- input_data %>% 
  pivot_longer(cols = c('GOA', 'BS', 'AI', 'BC', 'WC', 'multi_region'),
               names_to = 'input_area', values_to = 'use') %>% 
  filter(use == 1) %>% 
  select(-use) %>% 
  tidyr::complete(species, input_area, lh_param) %>% 
  group_by(species, input_area, lh_param) %>% 
  mutate(version = row_number()) %>% 
  ungroup()

species_ls <- unique(input_data$species)
area_ls <- unique(input_data$input_area)

write_csv(input_data, paste0('M_analysis_input_data.csv'))

for(i in 1:length(species_ls)) {
  for(j in 1:length(area_ls)) {
    
    # i = 1; j = 1; k = 1
    df <- input_data %>% 
      filter(species == species_ls[i] & input_area == area_ls[j]) 
    
    # max age estimator ----
    tmpdf <- df %>% filter(lh_param == 'maxage_yr')
    
    # occasionally there are more than one version of the area and species
    # specific life history parameters. loop through these
    amax_out <- data.frame(method = 'amax',
                           version = unique(tmpdf$version),
                           Mest = NA)
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      amax_out[k, 'Mest'] <- calcM_amax(input_amax = v_tmpdf$estimate)
    }
    
    # gsi estimator ----
    tmpdf <- df %>% filter(lh_param == 'gsi')
    
    gsi_out <- data.frame(method = 'gsi',
                          version = unique(tmpdf$version),
                          Mest = NA)
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      gsi_out[k, 'Mest'] <- calcM_gsi(input_gsi = v_tmpdf$estimate)
    }
    
    # temp/dry mass estimator ----
    tmpdf <- df %>% filter(lh_param %in% c('dry_weight_g', 'temp_C'))
    
    masstemp_out <- data.frame(method = 'mass_temp',
                               version = unique(tmpdf$version),
                               Mest = NA)
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      masstemp_out[k, 'Mest'] <- calcM_mass_temp(input_dry_mass = v_tmpdf$estimate[v_tmpdf$lh_param == 'dry_weight_g'],
                                                 input_temp = v_tmpdf$estimate[v_tmpdf$lh_param == 'temp_C'])
    }
    
    # lvb estimator ----
    tmpdf <- df %>% filter(lh_param %in% c('vbgf_k_cm-1', 'vbgf_linf_cm'))
    
    lvb_out <- data.frame(method = 'lvb',
                          version = unique(tmpdf$version),
                          Mest = NA)
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      lvb_out[k, 'Mest'] <- calcM_lvb(input_linf = v_tmpdf$estimate[v_tmpdf$lh_param == 'vbgf_linf_cm'],
                                      input_k = v_tmpdf$estimate[v_tmpdf$lh_param == 'vbgf_k_cm-1'])
    }
    
    # Bind and output data
    out <- bind_rows(amax_out, gsi_out, masstemp_out, lvb_out) %>% 
      mutate(species = species_ls[i],
             input_area = area_ls[j]) %>% 
      select(species, input_area, M_method = method, M_estimate = Mest, version)
    
    if(i == 1 & j == 1) {
      fullout <- out 
    } else {
      fullout <- bind_rows(fullout, out)
    }
  }
}

nrow(fullout)
write_csv(fullout, paste0(out_path, "/l_M_estimates.csv"))
l_fullout <- fullout

fullout <- l_fullout %>% 
  pivot_wider(id_cols = c(species, version, M_method),
              names_from = input_area,
              values_from = M_estimate) %>% 
  select(species, version, M_method, GOA, BS, AI, BC, WC, multi_region) %>% 
  arrange(species, M_method, version)

write_csv(fullout, paste0(out_path, "/M_estimates.csv"))

# test lvb k
linf <- 519
kapp <- 0.065
t0 <- 0.25
agevec <- 2:25
lens1 <- linf * (1 - exp(-k * (agevec - t0)))
lens2 <- ((linf/10) * (1 - exp(-(k) * (agevec - t0)))) * 10
lens1;lens2
round(lens2, 5) == round(lens1, 5)
