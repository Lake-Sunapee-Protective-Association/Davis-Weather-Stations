#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.0.5                                          */
#* R Studio:      1.4.1103                                       */
#* PROJECT:       lake sunapee davis weather station             */
#* PURPOSE:       process and collate lake sunapee weather data  */
#* DATE CREATED:  06Nov2019                                      */
#* LAST ADD:      27Jul through Jul012020                        */
#*                21Jan2020 - through Jan 01 2020                */
#*                07Oct2020 - added through Oct 1, but all others*/
#*                re-downloaded because of additional resoluion  */
#*                09Mar2022 - added through 12/31/2021; moved to */
#*                GH repo                                        */
#*                                                               */
#*****************************************************************/

library(plyr) #1.8.6
library(tidyverse) #1.3.1
library(ggthemes) #4.2.4

#### lists ####
varlist <- c('datetime', 'pressure_hpa', 'temp_c', 'hightemp_c', 'lowtemp_c', 'humidity_perc', 'dewpoint_c',
             'wetbulb_c', 'windsp_mps', 'winddir', 'windrun_m', 'highwindsp_mps', 'highwinddir',
             'windchill_c', 'heatindex_c', 'thwindex_c', 'thswindex_c', 'rain_mm', 'rainrate_mmph', 
             'solarradiation_wpm2', 'solarenergy_ly', 'highsolarrad_wpm2', 'evapotrans_mm', 'uvindex',
             'uvdose_meds', 'highuvindex', 'heatingdegdays', 'coolingdegdays')

#note: all data are recorded in local time with DST - because of the way the data are stored, spring forward happens, but it doesn't look like 'fall back' happens - 
# I'm guessing this is an artifact of the storage format. because of this, all data are treated as America/New_York timezone, which observes DST, and are transformed
# to Etc/GMT+5 for no DST column

#### Georges Mills ####
gm_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/Georges_Mills/'
# read in files
GM_list <- list.files(gm_dir) #this will read file names for csv files in the directory (However, make sure there are no other files in directory other than the required csv files)
GM_prefix_pattern <- 'Georges_Mills_'
end_pattern <- '\\_3_Month'

#write loop to iteratively read csv files
GM <- NULL #create blank dataframe to write csvs to
for (i in GM_list){
  b <- read_csv(file.path(gm_dir, i), 
                skip = 6,
                col_types = c('cnnnnnnnncnncnnnnnnnnnnnnnnn'),
                col_names = varlist,
                na = c('--', ''))
  b <- b %>%
    dplyr::mutate(instrument_datetime = as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p', tz='America/New_York')) %>% 
    dplyr::mutate(datetime_noDST = lubridate::with_tz(as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p'), tzone = 'Etc/GMT+5')) %>%  
    dplyr::mutate(date = as.Date(datetime_noDST)) %>%
    dplyr::select(-datetime)
  #program spits out first date of next round - so filter to remove last observation
  last_datetime = max(b$instrument_datetime)
  b <- b %>% 
    filter(instrument_datetime < last_datetime)
  name <- gsub(paste('.*', GM_prefix_pattern, '(.*)', end_pattern, '.*', sep = ''), "\\1", i)
  name <- gsub('_', ' ', name)
  b$location <- 'GM'
  GM[[i]] <- b
}
GM_data <- ldply(GM, data.frame)

#### Herrick Cove ####
hc_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/Herrick_Cove/'
# read in files
HC_list <- list.files(hc_dir) #this will read file names for csv files in the directory (However, make sure there are no other files in directory other than the required csv files)
HC_prefix_pattern <- 'Herrick_Cove_'
end_pattern <- '\\_3_Month'

#write loop to iteratively read csv files
HC <- NULL #create blank dataframe to write csvs to
for (i in HC_list){
  b <- read_csv(file.path(hc_dir, i), 
                skip = 6,
                col_types = c('cnnnnnnnncnncnnnnnnnnnnnnnnn'),
                col_names = varlist,
                na = c('--', ''))
  b <- b %>%
    dplyr::mutate(instrument_datetime = as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p', tz='America/New_York')) %>% 
    dplyr::mutate(datetime_noDST = lubridate::with_tz(as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p'), tzone = 'Etc/GMT+5')) %>%  
    dplyr::mutate(date = as.Date(datetime_noDST)) %>%
    dplyr::select(-datetime)
  #program spits out first date of next round - so filter to remove last observation
  last_datetime = max(b$instrument_datetime)
  b <- b %>% 
    filter(instrument_datetime < last_datetime)
  name <- gsub(paste('.*', HC_prefix_pattern, '(.*)', end_pattern, '.*', sep = ''), "\\1", i)
  name <- gsub('_', ' ', name)
  b$location <- 'HC'
  HC[[i]] <- b
}
HC_data <- ldply(HC, data.frame)

str(HC_data)

#### South Fells ####
sf_dir = ('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/South_Fells/')
# read in files
SF_list <- list.files(sf_dir) #this will read file names for csv files in the directory (However, make sure there are no other files in directory other than the required csv files)
SF_prefix_pattern <- 'South_Fells_'
end_pattern <- '\\_3_Month'

#write loop to iteratively read csv files
SF <- NULL #create blank dataframe to write csvs to
for (i in SF_list){
  b <- read_csv(file.path(sf_dir, i), 
                skip = 6,
                col_types = c('cnnnnnnnncnncnnnnnnnnnnnnnnn'),
                col_names = varlist,
                na = c('--', ''))
  b <- b %>%
    dplyr::mutate(instrument_datetime = as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p', tz='America/New_York')) %>% 
    dplyr::mutate(datetime_noDST = lubridate::with_tz(as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p'), tzone = 'Etc/GMT+5')) %>%  
    dplyr::mutate(date = as.Date(datetime_noDST)) %>%
    dplyr::select(-datetime)
  #program spits out first date of next round - so filter to remove last observation
  last_datetime = max(b$instrument_datetime)
  b <- b %>% 
    filter(instrument_datetime < last_datetime)
  name <- gsub(paste('.*', SF_prefix_pattern, '(.*)', end_pattern, '.*', sep = ''), "\\1", i)
  name <- gsub('_', ' ', name)
  b$location <- 'SF'
  SF[[i]] <- b
}
SF_data <- ldply(SF, data.frame)


#### Join all data ####

weather_data <- full_join(GM_data, HC_data) %>% 
  full_join(., SF_data) %>% 
  arrange(location, datetime_noDST) %>% 
  dplyr::rename(source = '.id') 

countobs_noDST <- weather_data %>%
  dplyr::mutate(date = as.Date(datetime_noDST, tz = 'Etc/GMT+5')) %>%
  group_by(date, location) %>%
  dplyr::summarize(nobs = length(datetime_noDST)) %>%
  filter(nobs != 48)

# DST DATES: 2019-11-03, 2020-03-08, 2020-11-01, 2021-03-13, 2021-11-07
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2019-11-03', tz = 'Etc/GMT+5')]) # this will skip 2:30 and 3:00a
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2020-03-08', tz = 'Etc/GMT+5')])
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2020-11-01', tz = 'Etc/GMT+5')])# this will skip 2:30 and 3:00a
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2021-03-13', tz = 'Etc/GMT+5')])
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2021-11-07', tz = 'Etc/GMT+5')])# this will skip 2:30 and 3:00a

# update date for export
start_year = min(format(weather_data$datetime_noDST, '%Y'))
end_year = max(format(weather_data$datetime_noDST, '%Y'))

weather_data %>%
  mutate(datetime_noDST = as.character(datetime_noDST),
         instrument_datetime = as.character(instrument_datetime)) %>%
  write_csv(., paste0('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/L0 data/davis_weather_data_', start_year, '-', end_year, '_L0_', Sys.Date(), '.csv'))




