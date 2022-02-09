# get tmax
# jane.sullivan@noaa.gov
# february 2022

# get three options for tmax 
# 1) max age in sample
# 2) mean of top 5 ages in sample
# 3) 99th percentile in sample

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

srvbio <- read_csv(paste0(dat_path, "/survey_ages.csv"))
fshbio <- read_csv(paste0(dat_path, "/fishery_ages.csv"))
spp <- read_csv(paste0(dat_path, '/race_spp.csv'))

# survey ----
srvsum <- srvbio %>% 
  left_join(spp %>% select(-species_name)) %>% 
  group_by(common_name, survey) %>% 
  summarise(n = n(),
            q99 = quantile(age, 0.99),
            max_age = max(age)) %>% 
  left_join(srvbio %>% 
              left_join(spp %>% select(-species_name)) %>% 
              group_by(common_name, survey) %>% 
              slice_max(order_by = age, n = 5) %>% 
              summarize(mean_top5 = mean(age))) 

top5_srvsum <- srvbio %>% 
  left_join(spp %>% select(-species_name)) %>% 
  group_by(common_name, survey) %>% 
  slice_max(order_by = age, n = 5) %>% 
  select(year, survey, common_name, sex, age, length, weight, age_determination_method) 

write_csv(srvsum, paste0(out_path, '/srv_tmax.csv'))
write_csv(top5_srvsum, paste0(out_path, '/srv_top5.csv'))

tmpsrv <- srvbio %>% 
  mutate(source = 'survey') %>% 
  left_join(spp) %>% 
  select(source, common_name, area = survey, age) %>% 
  mutate(area = ifelse(area == 'EBS_SLOPE', 'BS', area))
names(tmpsrv)

# fishery ----

fshsum <- fshbio  %>% 
  group_by(common_name, area) %>% 
  summarise(n = n(),
            q99 = quantile(age, 0.99),
            max_age = max(age)) %>% 
  left_join(fshbio %>% 
              group_by(common_name, area) %>% 
              slice_max(order_by = age, n = 5) %>% 
              summarize(mean_top5 = mean(age))) 

top5_fshsum <- fshbio %>% 
  group_by(common_name, area) %>% 
  slice_max(order_by = age, n = 5) %>% 
  select(year, area, common_name, sex, age, length, weight, gear = gear_description) 

write_csv(fshsum, paste0(out_path, '/fsh_tmax.csv'))
write_csv(top5_fshsum, paste0(out_path, '/fsh_top5.csv'))

tmpfsh <- fshbio %>% 
  mutate(source = 'fishery') %>% 
  select(source, common_name, area, age)

# combined ----

comb <- bind_rows(tmpsrv, tmpfsh) 

combsum <- comb %>% 
  group_by(common_name, area) %>% 
  summarise(n = n(),
            q99 = quantile(age, 0.99),
            max_age = max(age)) %>% 
  left_join(comb %>% 
              group_by(common_name, area) %>% 
              slice_max(order_by = age, n = 5) %>% 
              summarize(mean_top5 = mean(age))) 

write_csv(combsum, paste0(out_path, '/alldat_tmax.csv'))
