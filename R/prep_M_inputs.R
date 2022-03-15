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

# tmax estimates based on combined fishery and survey age data. see tmax.R
tmax <- read_csv(paste0(out_path, '/alldat_tmax.csv'))

lhfull <- tmax %>% 
  select(species = common_name, area, afsc_allages_q99 = q99, afsc_allages_max_age = max_age, afsc_allages_mean_top5 = mean_top5) %>% 
  pivot_longer(cols = -c(species, area), names_to = 'method_or_source', values_to = 'estimate') %>% 
  mutate(lh_param = 'maxage_yr') %>% 
  bind_rows(lh) %>% 
  tidyr::complete(species, area, lh_param)
View(lhfull)
  group_by(species, area, lh_param) %>% 

  mutate(version = row_number())

unique(lhfull$area)

amax <- lhfull %>% 
  filter(lh_param == 'maxage_yr')

for(i in 1:length(unique(lhfull$species))) {
  for(j in 1:length(unique(lhfull$area)))
}