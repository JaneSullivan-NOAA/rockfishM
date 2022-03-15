# Max age REBS

# Set up ----

# Assessment year (most recent year with complete data set)
YEAR <- 2021

libs <- c("tidyverse", "RODBC")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

db <- read_csv("database.csv")
database_akfin <- db$database
username_akfin <- db$username 
password_akfin <- db$password
channel_akfin <- odbcConnect(database_akfin, uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

database_afsc <- "afsc"
username_afsc <- db$un_afsc 
password_afsc <- db$pw_afsc 
channel_afsc <- odbcConnect(database_afsc, uid = username_afsc, pwd = password_afsc, believeNRows=FALSE)


# Create a year subdirectory to store annual data
dat_path <- paste0("data")
dir.create(dat_path)
dat_path <- paste0("data/", YEAR)
dir.create(dat_path)
raw_path <- paste0(dat_path, "/raw") # raw data
dir.create(raw_path) 

# race species_codes ----
query <- paste0("select   species_code, species_name, common_name
                 from     afsc.race_racespeciescodes")

spp <- sqlQuery(channel_akfin, query) %>% 
  rename_all(tolower) 

spp <- spp %>% 
  filter((grepl("rougheye|blackspotted|dusky|harlequin|northern|yelloweye|shortspine|sharpchin|silvergray|redbanded|redstripe|shortraker|thornyhead", common_name)) &
           (grepl('Sebastolobus|Sebastes', species_name))) %>% 
  # grepl("rockfish", common_name)
  # remove the 'black and dusky unid' species code
  filter(!species_code %in% c(30150)) %>% 
  # remove longspine thornyhead and broadfin thornyhead and thornyhead unidentified
  filter(!species_code %in% c(30030, 30025, 30010)) 
  
myspp <- spp %>% pull(species_code)
myspp_string <- toString(sprintf("'%s'", myspp)) # allows you to pass vector into sql query

write_csv(spp, paste0(dat_path, '/race_spp.csv'))

# survey cruise metadata ----
query1 <- paste0("select   *
                 from     afsc.race_biennialsurveysaigoa")
query2 <- paste0("select   *
                 from     afsc.race_surveys_ebsshelf")
query3 <- paste0("select   *
                 from     afsc.race_surveys_ebsslope")

cruise <- sqlQuery(channel_akfin, query1) %>% 
  bind_rows(sqlQuery(channel_akfin, query2)) %>% 
  bind_rows(sqlQuery(channel_akfin, query3)) %>% 
  rename_all(tolower) %>% 
  select(-akfin_load_date)

# survey age data ----
query1 <- paste0("select   *
                 from     afsc.race_specimenaigoa
                 where    species_code in (%s) and
                          age IS NOT NULL")

query2 <- paste0("select   *
                 from     afsc.race_specimen_ebsshelf
                 where    species_code in (%s) and
                          age IS NOT NULL")

query3 <- paste0("select   *
                 from     afsc.race_specimen_ebsslope
                 where    species_code in (%s) and
                          age IS NOT NULL")


srvbio <- sqlQuery(channel_akfin, sprintf(query1, myspp_string)) %>% 
  rbind(sqlQuery(channel_akfin, sprintf(query2, myspp_string))) %>% 
  rbind(sqlQuery(channel_akfin, sprintf(query3, myspp_string)) %>% 
              select(-SURVEY)) %>% 
  rename_all(tolower) %>% 
  select(-akfin_load_date) %>% 
  left_join(cruise)
  
write_csv(srvbio, paste0(dat_path, "/survey_ages_akfin.csv"))

# using AFSC specimen table from RACEBASE
query <- paste0("select   *
                 from     racebase.specimen
                 where    species_code in (%s) and
                          age IS NOT NULL")

srvbio <- sqlQuery(channel_afsc, sprintf(query, myspp_string)) %>% 
   rename_all(tolower) %>% 
  left_join(cruise)

write_csv(srvbio, paste0(dat_path, "/survey_ages.csv"))

# fishery data ----

# akr species code translation for observer data
akrspp <- read_csv("data/species_translation_akr_obs.csv")

akrspp <- akrspp %>% 
  filter(grepl('Rougheye|Blackspotted|Dusky|Thornyhead|Harlequin|Northern|Yelloweye|Shortspine|Sharpchin|Silvergray|Redbanded|Redstripe|Shortraker', akr_name) &                 
           grepl("Rockfish", akr_name)) %>% 
  # print(n=Inf)
  # get rid of combine Shortraker/Rougheye samples
  filter(! akr_code %in% c(171)) %>% 
  distinct(akr_name, akr_code)

akrspp <- akrspp %>% 
  mutate(common_name = tolower(akr_name)) %>% 
  left_join(spp)

myakrspp <- akrspp %>% pull(akr_code)
myakrspp_string <- toString(sprintf("'%s'", myakrspp)) # allows you to pass vector into sql query

query <- paste0("select   *
                 from     norpac.debriefed_age_flat_mv
                 where    akr_species_codes in (%s) and
                          age IS NOT NULL and
                          fmp_area in ('GOA', 'BSAI')")

fshbio <- sqlQuery(channel_akfin, sprintf(query, myakrspp_string)) %>% 
  rename_all(tolower)
  
fshbio <- fshbio %>% 
  mutate(area = ifelse(fmp_subarea %in% c('AI', 'BS'), fmp_subarea, 'GOA'))

fshbio <- fshbio %>% 
  left_join(akrspp %>% 
              select(akr_species_codes = akr_code, common_name))

write_csv(fshbio, paste0(dat_path, "/fishery_ages.csv"))


# dusky lengths for Todd Tenbrink 2022-02-09 ----

query <- paste0("select   *
                 from     norpac.debriefed_age_flat_mv
                 where    akr_species_codes in ('172') and
                          fmp_area in ('BSAI')")

duskylengths <- sqlQuery(channel_akfin, query) %>% 
  rename_all(tolower)

write_csv(duskylengths, paste0(dat_path, "/bsai_fishery_dusky_specimens_20220209.csv"))

# check on AI harlequin 2022-02-10 ----

query <- paste0("select   *
                 from     afsc.race_specimenaigoa
                 where    species_code in ('30535') and
                          region in ('AI') and
                          age IS NOT NULL")

harl <- sqlQuery(channel_akfin, query) %>% 
  rename_all(tolower)
harl
