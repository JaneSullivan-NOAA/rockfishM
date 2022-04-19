# M analysis
# jane.sullivan@noaa.gov
# march 2022

# 1) pull in life history parameters from google sheets
# 2) append tmax estimates obtained from norpac and racebase specimen data
# 3) use selected empirical estimators for M
# 4) format output, save

# setup ----
libs <- c("tidyverse", "googlesheets4", "writexl")
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
tmax <- read_csv(paste0(out_path, '/tmax_summary.csv')) %>% 
  select(common_name = `Species`, area = Region, 
       max_age = `Maximum age observed`, 
       mean_top5 = `Mean top 5 ages`)

# test to see if the spreadsheet has any errors identifying which region the
# data represent. Should be zero rows, and if it's not GOA:WC should be 0 and
# multi_region should be 1
lh %>% 
  mutate(multi_region_test = GOA+BS+AI+BC+WC+multi_region) %>% 
  filter(multi_region_test > 1)

# bind tmax estimates from AFSC fishery and survey data to user-defined life
# history parameter data
lhfull <- tmax %>% 
  select(species = common_name, data_area = area, 
         # afsc_allages_q99 = q99, # decided as a group not to use this one
         `AFSC max age` = max_age, `AFSC mean top 5` = mean_top5) %>% 
  pivot_longer(cols = -c(species, data_area), names_to = 'method_or_source', values_to = 'estimate') %>% 
  mutate(lh_param = 'maxage_yr',
         use = 1,
         GOA = ifelse(data_area == 'GOA', 1, 0),
         BS = ifelse(data_area == 'BS', 1, 0),
         AI = ifelse(data_area == 'AI', 1, 0),
         BC = 0,
         WC = 0,
         multi_region = 0) %>% 
  bind_rows(lh %>% 
              select(-notes)) 

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
               names_to = 'input_area', values_to = 'use') %>% # print(n=Inf)
  filter(use == 1) %>% 
  select(-use) %>% 
  tidyr::complete(species, input_area, lh_param) %>% 
  group_by(species, input_area, lh_param) %>% 
  mutate(version = row_number()) %>% 
  ungroup()

input_data %>% print(n=Inf)

# Run analysis ----

# calculate four M estimators for each input species and area combo

species_ls <- unique(input_data$species)
area_ls <- unique(input_data$input_area)

# save inputs so they can be version controlled as we refine our spreadsheet
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
                           Mest = NA,
                           method_or_source = unique(tmpdf$method_or_source))
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      amax_out[k, 'Mest'] <- calcM_amax(input_amax = v_tmpdf$estimate)
    }
    
    # gsi estimator ----
    tmpdf <- df %>% filter(lh_param == 'gsi')
    
    gsi_out <- data.frame(method = 'gsi',
                          version = unique(tmpdf$version),
                          Mest = NA,
                          method_or_source = unique(tmpdf$method_or_source))
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      gsi_out[k, 'Mest'] <- calcM_gsi(input_gsi = v_tmpdf$estimate)
    }
    
    # temp/dry mass estimator ----
    tmpdf <- df %>% filter(lh_param %in% c('dry_weight_g', 'temp_C'))
    
    masstemp_out <- data.frame(method = 'temp',
                               version = unique(tmpdf$version),
                               Mest = NA,
                               method_or_source = unique(tmpdf$method_or_source))
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      masstemp_out[k, 'Mest'] <- calcM_mass_temp(input_dry_mass = v_tmpdf$estimate[v_tmpdf$lh_param == 'dry_weight_g'],
                                                 input_temp = v_tmpdf$estimate[v_tmpdf$lh_param == 'temp_C'])
    }
    
    # lvb estimator ----
    tmpdf <- df %>% filter(lh_param %in% c('vbgf_k_cm-1', 'vbgf_linf_cm'))
    
    lvb_out <- data.frame(method = 'lvb',
                          version = unique(tmpdf$version),
                          Mest = NA,
                          method_or_source = unique(tmpdf$method_or_source))
    
    for(k in 1:length(unique(tmpdf$version))) {
      v_tmpdf <- tmpdf %>% filter(version == k)
      lvb_out[k, 'Mest'] <- calcM_lvb(input_linf = v_tmpdf$estimate[v_tmpdf$lh_param == 'vbgf_linf_cm'],
                                      input_k = v_tmpdf$estimate[v_tmpdf$lh_param == 'vbgf_k_cm-1'])
    }
    
    # Bind and output data
    out <- bind_rows(amax_out, gsi_out, masstemp_out, lvb_out) %>% 
      mutate(species = species_ls[i],
             input_area = area_ls[j]) %>% 
      select(species, input_area, M_method = method, M_estimate = Mest, 
             version, method_or_source)
    
    if(i == 1 & j == 1) {
      fullout <- out 
    } else {
      fullout <- bind_rows(fullout, out)
    }
  }
}

# M results ----
nrow(fullout)
fullout %>% select(-method_or_source)
write_csv(fullout, paste0(out_path, "/l_M_estimates.csv"))
l_fullout <- fullout

fullout <- l_fullout %>% 
  select(-method_or_source) %>% 
  pivot_wider(id_cols = c(species, version, M_method),
              names_from = input_area,
              values_from = M_estimate) %>% 
  select(species, version, M_method, GOA, BS, AI, BC, WC, multi_region) %>%
  arrange(species, M_method, version) 

fullout #%>% View()

write_csv(fullout, paste0(out_path, "/M_estimates.csv"))

# join inputs to M outputs
l_fullout %>% as_tibble()
unique(l_fullout$M_method)
unique(input_data$lh_param)

summ <- input_data %>% 
  mutate(M_method = case_when(lh_param %in% c('maxage_yr') ~ 'amax',
                              lh_param %in% c('dry_weight_g', 'temp_C') ~ 'temp',
                              lh_param %in% c('gsi') ~ 'gsi',
                              lh_param %in% c('vbgf_k_cm-1', 'vbgf_linf_cm') ~ 'lvb'))
summ <- summ %>% 
  filter(M_method %in% c('gsi', 'amax')) %>% 
  mutate(lh_param = ifelse(lh_param == 'gsi', 'GSI', 'Max age (y)')) %>% 
  rename(input_names = lh_param, input_values = estimate) %>%
  mutate(input_values = ifelse(input_names == 'GSI',
                               formatC(round(input_values, 4), format = 'f', digits = 4),
                               ifelse(grepl('mean top five', method_or_source),
                                      formatC(round(input_values, 1), format = 'f', digits = 1),
                                      formatC(round(input_values, 0), format = 'f', digits = 0)))) %>% 
  bind_rows(summ %>% 
              filter(M_method %in% c('lvb')) %>% 
              mutate(lh_param = ifelse(lh_param == 'vbgf_k_cm-1', 'k', 'linf')) %>% 
              pivot_wider(id_cols = c('species', 'input_area', 'data_area', 'version', 'M_method', 'method_or_source'),
                          names_from = lh_param, values_from = estimate) %>% 
              mutate(input_names = 'VBGF Linf (cm) / k',
                     input_values = ifelse(is.na(k), NA,
                                           paste0(formatC(round(linf, 1), digits = 1, format = 'f'), 
                                                  " / ", 
                                                  formatC(round(k, 3), format = 'f', digits = 3)))) %>% 
              select(-k, -linf)) %>% 
  bind_rows(summ %>% 
              filter(M_method %in% c('temp')) %>% 
              pivot_wider(id_cols = c('species', 'input_area', 'data_area', 'version', 'M_method', 'method_or_source'),
                          names_from = lh_param, values_from = estimate) %>% 
              mutate(input_names = 'Temperature (C) / Dry weight (g)',
                     input_values = ifelse(is.na(temp_C), NA,
                                           paste0(formatC(round(temp_C, 1), digits = 1, format = 'f'), 
                                                  " / ", 
                                                  prettyNum(round(dry_weight_g, 0), format = 'f', big.mark = ',')))) %>% 
              select(-temp_C, -dry_weight_g))

summ
nrow(summ) == nrow(l_fullout) # should be true. if not you probably messed up reformatting the data input values

summ <- l_fullout %>% 
  left_join(summ, by = c("species", "input_area", "M_method", "version", "method_or_source")) %>% 
  filter(!is.na(M_estimate)) %>% 
  mutate(M_estimate = formatC(round(M_estimate, 3), format = 'f', digits = 3),
         M_method = paste0(M_method, '.v', version),
         area = ifelse(input_area == 'multi_region', data_area, input_area)) %>% 
  select(species, area, version, data_input = input_names, 
         data_input_values = input_values, M_estimate,
         references = method_or_source) 

summ <- summ %>% filter(species != 'northern rockfish') 
summ <- summ %>% 
  mutate(species = factor(species, 
                          labels = c('dusky rockfish', 'harlequin rockfish',
                                     'rebs rockfish','rougheye rockfish','blackspotted rockfish',
                                     'redbanded rockfish', 'redstripe rockfish', 
                                     'sharpchin rockfish', 'shortraker rockfish','silvergray rockfish',
                                     'yelloweye rockfish', 'shortspine thornyhead'),
                          levels = c('dusky rockfish', 'harlequin rockfish',
                                     'rebs rockfish','rougheye rockfish','blackspotted rockfish',
                                     'redbanded rockfish', 'redstripe rockfish', 
                                     'sharpchin rockfish', 'shortraker rockfish','silvergray rockfish',
                                     'yelloweye rockfish', 'shortspine thornyhead'),
                          ordered = TRUE)) %>% 
  arrange(species, area, data_input, version)
summ
summ %>% write_csv(paste0(out_path, '/formatted_M_results.csv'))

# write results to separate excel sheets using writexl and purrr ----
summls <- summ %>% 
  dplyr::group_split(species)

# these become the names of each sheet in excel
names(summls) <- summls %>% 
  purrr::map(~pull(., species)) %>% 
  map(~as.character(.)) %>% 
  map(~unique(.))

summls %>% 
  writexl::write_xlsx(path = paste0(out_path, '/M_species_tables.xlsx'))

summ %>% 
  left_join(reference_lkup) %>% 
  select(Species = species, Region = area, `Parameter(s)` = data_input,
         `Parameter values(s)` = data_input_values, `M estimate` = M_estimate, Reference = references) %>% 
  write_xlsx(path = paste0(out_path, '/M_species_tables_full.xlsx'))

# reference_lkup <- summ %>% 
#   distinct(references) %>% 
#   mutate(reference_number = row_number()) 
# 
# reference_lkup %>% 
#   mutate(txt = paste0(reference_number, ') ', references))

# Figures ----

l_fullout <- l_fullout %>% 
  select(-method_or_source) %>% 
  mutate(M_method2 = paste0(M_method, '.v', version)) %>% 
  mutate(input_area = factor(input_area,
                             labels = c('GOA', 'BS', 'AI', 'BC', 'WC', 'multi_region'),
                             levels = c('GOA', 'BS', 'AI', 'BC', 'WC', 'multi_region'),
                             ordered = TRUE))

mybarplot <- function(df = plot_data,
                      title = 'your plot title (e.g., species name)') {
  df %>%
    filter(!is.na(M_estimate)) %>% 
    ggplot(aes(x = M_method2, y = M_estimate, fill = M_method, col = M_method)) +
    geom_bar(stat = 'identity') +
    facet_grid(species~input_area) +
    labs(x = NULL, y = 'Natural mortality (M)',
         title = title) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  
}

# Rougheye/blackspotted rockfish ----

theme_set(theme_bw(base_size = 12))

# species specific when available, combined as 'rebs' when not
plot_data <- l_fullout %>% 
  filter(species %in% c('rebs rockfish', 'rougheye rockfish', 'blackspotted rockfish')) %>% 
  mutate(species = factor(species,
                          labels = c('rebs rockfish', 'rougheye rockfish', 'blackspotted rockfish'),
                          levels = c('rebs rockfish', 'rougheye rockfish', 'blackspotted rockfish'),
                          ordered = TRUE))

mybarplot(title = 'Rougheye/blackspotted rockfish')
ggsave(paste0(out_path, '/rebs_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 9)

# Shortraker ----

plot_data <- l_fullout %>% 
  filter(species %in% c('shortraker rockfish')) 

mybarplot(title = 'Shortraker rockfish')
ggsave(paste0(out_path, '/shortraker_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Shortspine thornyhead ----

plot_data <- l_fullout %>% 
  filter(species %in% c('shortspine thornyhead')) 

mybarplot(title = 'Shortspine thornyhead')
ggsave(paste0(out_path, '/shortspine_thornyhead_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Dusky rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('dusky rockfish')) 

mybarplot(title = 'Dusky rockfish')
ggsave(paste0(out_path, '/dusky_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Harlequin rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('harlequin rockfish')) 

mybarplot(title = 'Harlequin rockfish')
ggsave(paste0(out_path, '/harlequin_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Redbanded, redstripe, sharpchin, slivergray, yelloweye rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('redbanded rockfish', 'redstripe rockfish',
                        'sharpchin rockfish', 'silvergray rockfish',
                        'yelloweye rockfish')) 

mybarplot(title = 'Redbanded, redstripe, sharpchin, silvergray, and yelloweye rockfish')
ggsave(paste0(out_path, '/orox_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 8)

# Redbanded rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('redbanded rockfish')) 

mybarplot(title = 'Redbanded rockfish')
ggsave(paste0(out_path, '/redbanded_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Redstripe rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('redstripe rockfish')) 

mybarplot(title = 'Redstripe rockfish')
ggsave(paste0(out_path, '/redstripe_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Sharpchin rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('sharpchin rockfish')) 

mybarplot(title = 'Sharpchin rockfish')
ggsave(paste0(out_path, '/sharpchin_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Slivergray rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('silvergray rockfish')) 

mybarplot(title = 'Silvergray rockfish')
ggsave(paste0(out_path, '/silvergray_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

# Yelloweye rockfish ----

plot_data <- l_fullout %>% 
  filter(species %in% c('yelloweye rockfish')) 

mybarplot(title = 'Yelloweye rockfish')
ggsave(paste0(out_path, '/yelloweye_M_results.png'),
       dpi = 300, units = 'in', width = 7, height = 3)

