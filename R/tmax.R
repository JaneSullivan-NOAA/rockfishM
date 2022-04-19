# get tmax using data available from Alaska-based surveys and fisheries
# contact jane.sullivan@noaa.gov
# last updated 2022-03-14

# get three options for tmax 
# 1) max age in sample - use
# 2) mean of top 5 ages in sample - use
# 3) 99th percentile in sample - don't use

# setup ----
libs <- c("tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

YEAR <- 2021
# use queries.R to get updated data

out_path <- paste0("results")
dir.create(out_path)
out_path <- paste0("results/", YEAR)
dir.create(out_path)

dat_path <- paste0("data/", YEAR)

# data ----

srvbio <- read_csv(paste0(dat_path, "/survey_ages.csv")) # use survey_ages_akfin.csv to reproduce what's in results/YEAR/using_akfin_survey_specimen_data. RACE only delivers data once to AKFIN per year
fshbio <- read_csv(paste0(dat_path, "/fishery_ages.csv"))
spp <- read_csv(paste0(dat_path, '/race_spp.csv'))

# deal with rougheye/blackspotted unidentified (i.e. rebs). fishery data never
# has rebs to species.
spp <- spp %>% mutate(common_name = ifelse(species_code == 30050, 'rebs rockfish', common_name))

# survey ----

srvbio <- srvbio %>% 
  left_join(spp %>% select(-species_name)) %>% 
  select(area = region, source = survey_name, year, common_name, sex, age, length, weight) 
  
srvbio %>% 
  group_by(source, year, area) %>% 
  summarize(n = n()) %>% 
  arrange(n) %>% 
  print(n=Inf)

# get rid of the oddball observations that lack relevant sampling plan
# documentation, including specimens collected (1) on the west coast in a
# cooperative hake survey in 1988 and 1990, and (2) in the BS in a US-Japanese
# cooperative survey in 1979. Keep pre-survey data (e.g. GRFSH & RFSH KODK and AI)
srvbio <- srvbio %>% filter(!area %in% c('WC', 'HWC') & !source %in% c('USJPN COOP'))

tmpsrv <- srvbio %>% 
  mutate(length = length / 10,
         weight = weight / 1e3,
         sex = case_when(sex == 1 ~ 'M',
                         sex == 2 ~ 'F',
                         sex == 3 ~ 'U'),
         gear = 'AFSC bottom trawl survey',
         source = 'RACEBASE')

names(tmpsrv)

# fishery ----

tmpfsh <- fshbio %>%
  mutate(common_name = ifelse(common_name == 'rougheye rockfish', 'rebs rockfish', common_name),
         gear = case_when(gear_description == 'NON PELAGIC' ~ 'Bottom trawl fishery',
                            gear_description == 'PELAGIC' ~ 'Pelagic trawl fishery',
                            gear_description == 'LONGLINER' ~ 'Hook-and-line fishery'), 
         source = 'NORPAC') %>% 
  select(area, source, year, common_name, sex, age, length, weight, gear) 

# combined ----

comb <- bind_rows(tmpsrv, tmpfsh) %>% 
  filter(common_name != 'northern rockfish') %>% 
  mutate(common_name = factor(common_name,
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
  arrange(common_name, source, year)

write_csv(comb, paste0(dat_path, '/all_age_data_used.csv'))

# treat rougheye or blackspotted vs. rebs (unidentified) separately
# comb <- comb %>% 
#   bind_rows(comb %>% 
#               filter(common_name %in% c('rougheye rockfish', 'blackspotted rockfish')) %>% 
#               mutate(common_name = 'rebs rockfish'))

combsum <- comb %>% 
  group_by(common_name, area) %>% 
  summarise(n_total = n(),
            n_survey = length(which(source == 'RACEBASE')),
            n_fishery = length(which(source == 'NORPAC')),
            q99 = quantile(age, 0.99),
            max_age = max(age)) %>% 
  left_join(comb %>% 
              group_by(common_name, area) %>% 
              slice_max(order_by = age, n = 5) %>% 
              summarize(mean_top5 = mean(age))) %>% 
  left_join(comb %>%
              group_by(common_name, area) %>% 
              summarise(years_survey = paste(sort(unique(year[which(source == 'RACEBASE')])), collapse = ', '),
                        years_fishery = paste(sort(unique(year[which(source == 'NORPAC')])), collapse = ', '))) %>% 
  arrange(common_name, area) %>% 
  select(`Species` = common_name, Region = area, `Total N` = n_total,
         `Survey N` = n_survey, `Fishery N` = n_fishery,
         `Maximum age observed` = max_age,
         `Mean top 5 ages` = mean_top5,
         `Survey years` = years_survey, `Fishery years` = years_fishery)
  

combsum %>% print(n=Inf)

write_csv(combsum, paste0(out_path, '/tmax_summary.csv'))

top5 <- comb %>% 
  group_by(common_name, area) %>% 
  slice_max(order_by = age, n = 5) %>% 
  arrange(common_name, area, -age) %>%
  select(`Species` = common_name, Region = area, `Year sampled` = year, Sex = sex,
         `Age (yr)` = age, `Fork length (cm)` = length, `Weight (kg)` = weight, 
        Gear = gear,  Source = source) 

top5 %>% print(n=Inf)

write_csv(top5, paste0(out_path, '/top5_tmax_detailed.csv'))

# graphic Cindy requested 
comb <- read_csv(paste0(dat_path, '/all_age_data_used.csv'))
df <- comb %>% 
  droplevels() %>% 
  group_by(common_name, area, source, year) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  tidyr::complete(common_name, area, source, year, fill = list(n = 0)) 

# https://github.com/ben-williams/funcr/blob/master/R/tickr.R
tickr <- function(data, var, to = 5, start = NULL, end = NULL, min = NULL){
  
  data %>%
    dplyr::summarise(min = min({{var}}, na.rm = T),
                     max = max({{var}}, na.rm = T)) -> out
  
  if(is.null(start) & is.null(end)){
    
    data.frame(breaks = out$min:out$max) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * min(breaks) / to,
                                          max(breaks), by = to), breaks, ""))
    
  } else if(!is.null(start) & is.null(end) & is.null(min)){
    
    data.frame(breaks = start:out$max) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * start / to, max(breaks),
                                          by = to), breaks, ""))
    
  } else if(!is.null(start) & is.null(end) & !is.null(min)){
    data.frame(breaks = start:out$max) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * start / to, max(breaks),
                                          by = to), breaks, "")) %>%
      dplyr::filter(breaks >= min) -> lb
    lb$labels[1] <- lb$breaks[1]
    lb
    
  } else if(is.null(start) & !is.null(end)){
    
    data.frame(breaks = out$min:end) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * min(breaks) / to, end, by = to),
                                    breaks, ""))
  } else {
    
    data.frame(breaks = start:end) %>%
      dplyr::mutate(labels = ifelse(breaks %in%
                                      seq(to * start / to, end, by = to),
                                    breaks, ""))
  }
}

internet_theme <- theme_set(axis.text.x = theme_text(angle = 90,
                                                     hjust = 1), 
                            panel.grid.major = theme_line(colour = "grey90"),
                            panel.grid.minor = theme_blank(), 
                            panel.background = theme_blank(),
                            axis.ticks = theme_blank(), 
                            legend.position = "none")

tmp <- df %>%  filter(area == 'GOA') 

xaxis <- tickr(tmp, year, 10, min = 1978, start = 1980)
tmp %>% 
  ggplot(aes(x = year, y = n, fill = source)) +
  geom_col(width = 0.7) +
  facet_wrap(~ common_name, scales = 'free') +
  scale_x_continuous(labels = xaxis$labels, breaks = xaxis$breaks) +
  labs(x = NULL, y = 'Number of observations', 
       title = 'GOA specimen data summary')

# just rebs species ----
tmp <- df %>%  
  filter(common_name %in% c('rebs rockfish',
                            'rougheye rockfish',
                            'blackspotted rockfish')) %>% 
  mutate(common_name = factor(common_name,
                              labels = c('rougheye rockfish',
                                         'blackspotted rockfish',
                                         'rebs rockfish'),
                              levels = c('rougheye rockfish',
                                         'blackspotted rockfish',
                                         'rebs rockfish'),
                              ordered = TRUE))
# xaxis <- tickr(tmp, year, 10, min = 1978, start = 1980)
tmp %>% 
  filter(n > 0) %>%
  ggplot(aes(x = year, y = n, fill = source)) +
  geom_col(width = 0.8, col = NA) +
  scale_fill_grey() +
  # scale_colour_grey() +
  facet_grid(common_name ~ area, scales = 'free') +
  scale_x_continuous(limits = c(min(tmp$year), max(tmp$year))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  # scale_x_continuous(labels = xaxis$labels, breaks = xaxis$breaks) +
  labs(x = NULL, y = 'Number of samples') + #, 
       # title = 'Rougheye and blackspotted rockfish age data summary') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = 'black', fill = 'white'),
        axis.ticks = element_blank())

ggsave(paste0(out_path, '/rebs_age_data_samplesize.png'),
       height = 6, width = 8, dpi = 300, units = 'in')

# just species have only GOA specimens----
tmp <- df %>%  
  filter(!common_name %in% c('rebs rockfish',
                            'rougheye rockfish',
                            'blackspotted rockfish'))  %>% 
  filter(!area %in% c('BS', 'AI')) %>% 
  filter(!common_name %in% c('dusky rockfish',
                             'harlequin rockfish',
                             'shortraker rockfish'))

# xaxis <- tickr(tmp, year, 10, min = 1978, start = 1980)
tmp %>% 
  filter(n > 0) %>%
  # blank row to keep both source values
  bind_rows(data.frame(source = 'NORPAC', year = 2020, n = 0,
                       common_name = unique(tmp$common_name),
                       area = 'GOA')) %>%
  ggplot(aes(x = year, y = n, fill = source, col = source)) +
  geom_col(width = 0.8) +
  scale_fill_grey() +
  scale_colour_grey() +
  facet_grid(common_name ~ area) +
  scale_x_continuous(limits = c(min(tmp$year), max(tmp$year))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  # scale_x_continuous(labels = xaxis$labels, breaks = xaxis$breaks) +
  labs(x = NULL, y = 'Number of samples') + #, 
       # title = 'Silvergray, redstripe, and sharpchin rockfish\nage data summary') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = 'black', fill = 'white'),
        axis.ticks = element_blank())

ggsave(paste0(out_path, '/goaorox_age_data_samplesize.png'),
       height = 6, width = 5, dpi = 300, units = 'in')

# shortraker, dusky, harlequin specimens----
tmp <- df %>% 
  filter(common_name %in% c('dusky rockfish',
                             'harlequin rockfish',
                             'shortraker rockfish')) %>%  
  filter(!area %in% c('BS')) 

tmp %>% 
  filter(n > 0) %>%
  ggplot(aes(x = year, y = n, fill = source, col = source)) +
  geom_col(width = 0.8) +
  scale_fill_grey() +
  scale_colour_grey() +
  facet_grid(common_name ~ area) +
  # scale_x_continuous(labels = xaxis$labels, breaks = xaxis$breaks) +
  scale_x_continuous(limits = c(min(tmp$year), max(tmp$year))) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(x = NULL, y = 'Number of samples') +#, 
       # title = 'Dusky, harlequin, and shortraker rockfish age data summary') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = 'black', fill = 'white'),
        axis.ticks = element_blank())

ggsave(paste0(out_path, '/aigoaorox_sr_age_data_samplesize.png'),
       height = 6, width = 6, dpi = 300, units = 'in')

# All rockfish combined ----

df %>% 
  ggplot(aes(x = year, y = n, fill = source)) + #, col = source
  geom_col(width = 0.8, col = NA) +
  scale_fill_grey() +
  facet_grid(common_name ~ area) +
  labs(x = NULL, y = 'Number of observations',
       fill = NULL, col = NULL,
       title = 'Age data summary') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = 'black', fill = 'white'),
        axis.ticks = element_blank())

ggsave(paste0(out_path, '/age_data_samplesize.png'),
       height = 13, width = 9, dpi = 300, units = 'in')
