#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.3.2                                          */
#* R Studio:      2023.12.0-369                                  */
#* PROJECT:       lake sunapee davis weather station             */
#* PURPOSE:       process and collate lake sunapee weather data  */
#* DATE CREATED:  06Nov2019                                      */
#* LAST ADD:      27Jul through Jul012020                        */
#*                21Jan2020 - through Jan 01 2020                */
#*                07Oct2020 - added through Oct 1, but all others*/
#*                re-downloaded because of additional resolution */
#*                09Mar2022 - added through 12/31/2021; moved to */
#*                GH repo                                        */
#*                13Aug2023 - added through 07/01/2023; switched */
#*                to functions instead of loops                  */   
#*                27Dec2023 - added through 10/01/2023; fixed    */
#*                DST conversion                                 */
#*                                                               */
#*****************************************************************/

library(tidyverse) #2.0.0
library(ggthemes)

#### lists ####
varlist <- c('datetime', 'pressure_hpa', 'temp_c', 'hightemp_c', 'lowtemp_c', 'humidity_perc', 'dewpoint_c',
             'wetbulb_c', 'windsp_mps', 'winddir', 'windrun_m', 'highwindsp_mps', 'highwinddir',
             'windchill_c', 'heatindex_c', 'thwindex_c', 'thswindex_c', 'rain_mm', 'rainrate_mmph', 
             'solarradiation_wpm2', 'solarenergy_ly', 'highsolarrad_wpm2', 'evapotrans_mm', 'uvindex',
             'uvdose_meds', 'highuvindex', 'heatingdegdays', 'coolingdegdays')

#note: all data are recorded in local time with DST - because of the way the data are stored,
# spring forward happens, but it doesn't look like 'fall back' happens - 
# I'm guessing this is an artifact of the storage format. because of this, all data are 
# treated as America/New_York timezone, which observes DST, and are transformed
# to Etc/GMT+5 for no DST column

#### Georges Mills ####
gm_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/Georges_Mills/'
# read in files
GM_list <- list.files(gm_dir) #this will read file names for csv files in the directory (However, make sure there are no other files in directory other than the required csv files)
GM_prefix_pattern <- 'Georges_Mills_'
end_pattern <- '\\_3_Month'

#write funciton to read and format .csv files
read_and_format_GM <- function(fp) {
  b <- read_csv(file.path(gm_dir, fp), 
                skip = 6,
                col_types = c('cnnnnnnnncnncnnnnnnnnnnnnnnn'),
                col_names = varlist,
                na = c('--', ''))
  b <- b %>%
    dplyr::mutate(instrument_datetime = force_tz(as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p'), tzone = 'America/New_York')) %>% 
    dplyr::mutate(datetime_noDST = lubridate::with_tz(instrument_datetime, tzone = 'Etc/GMT+5')) %>%  
    dplyr::mutate(date = as.Date(datetime_noDST)) %>%
    dplyr::select(-datetime)
  #program spits out first date of next round - so filter to remove last observation
  last_datetime = max(b$instrument_datetime)
  b <- b %>% 
    filter(instrument_datetime < last_datetime)
  b$location <- 'GM'
  b
}

GM_data <- map_dfr(GM_list, read_and_format_GM)

#### Herrick Cove ####
hc_dir = 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/Herrick_Cove/'
# read in files
HC_list <- list.files(hc_dir) #this will read file names for csv files in the directory (However, make sure there are no other files in directory other than the required csv files)
HC_prefix_pattern <- 'Herrick_Cove_'
end_pattern <- '\\_3_Month'

read_and_format_HC <- function(fp) {
  b <- read_csv(file.path(hc_dir, fp), 
                skip = 6,
                col_types = c('cnnnnnnnncnncnnnnnnnnnnnnnnn'),
                col_names = varlist,
                na = c('--', ''))
  b <- b %>%
    dplyr::mutate(instrument_datetime = force_tz(as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p'), tzone = 'America/New_York')) %>% 
    dplyr::mutate(datetime_noDST = lubridate::with_tz(instrument_datetime, tzone = 'Etc/GMT+5')) %>%  
    dplyr::mutate(date = as.Date(datetime_noDST)) %>%
    dplyr::select(-datetime)
  #program spits out first date of next round - so filter to remove last observation
  last_datetime = max(b$instrument_datetime)
  b <- b %>% 
    filter(instrument_datetime < last_datetime)
  b$location <- 'HC'
  b
}

HC_data <- map_dfr(HC_list, read_and_format_HC)


#### South Fells ####
sf_dir = ('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/South_Fells/')
# read in files
SF_list <- list.files(sf_dir) #this will read file names for csv files in the directory (However, make sure there are no other files in directory other than the required csv files)
SF_prefix_pattern <- 'South_Fells_'
end_pattern <- '\\_3_Month'

read_and_format_SF <- function(fp) {
  b <- read_csv(file.path(sf_dir, fp), 
                skip = 6,
                col_types = c('cnnnnnnnncnncnnnnnnnnnnnnnnn'),
                col_names = varlist,
                na = c('--', ''))
  b <- b %>%
    dplyr::mutate(instrument_datetime = force_tz(as.POSIXct(datetime, format = '%m/%d/%y %I:%M %p'), tzone = 'America/New_York')) %>% 
    dplyr::mutate(datetime_noDST = lubridate::with_tz(instrument_datetime, tzone = 'Etc/GMT+5')) %>%  
    dplyr::mutate(date = as.Date(datetime_noDST)) %>%
    dplyr::select(-datetime)
  #program spits out first date of next round - so filter to remove last observation
  last_datetime = max(b$instrument_datetime)
  b <- b %>% 
    filter(instrument_datetime < last_datetime)
  b$location <- 'SF'
  b
}

SF_data <- map_dfr(SF_list, read_and_format_SF)


#### Join all data ####

weather_data <- full_join(GM_data, HC_data) %>% 
  full_join(., SF_data) %>% 
  arrange(location, datetime_noDST) 

countobs_noDST <- weather_data %>%
  dplyr::mutate(date = as.Date(datetime_noDST, tz = 'Etc/GMT+5')) %>%
  group_by(date, location) %>%
  dplyr::summarize(nobs = length(datetime_noDST)) %>%
  filter(nobs != 48)

# DST DATES: 2019-11-03 
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2019-11-03', tz = 'Etc/GMT+5')], n = 10) # this will skip 00:30 and 1:00a
#2020-03-08, 2020-11-01
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2020-03-08', tz = 'Etc/GMT+5')], n = 10)
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2020-11-01', tz = 'Etc/GMT+5')], n = 10)# this will skip 00:30 and 1:00a
#2021-03-13, 2021-11-07
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2021-03-13', tz = 'Etc/GMT+5')], n = 10)
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2021-11-07', tz = 'Etc/GMT+5')], n = 10)# this will skip 00:30 and 1:00a
#2022-03-13, 2022-11-06
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2022-03-13', tz = 'Etc/GMT+5')], n = 10)
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2022-11-06', tz = 'Etc/GMT+5')], n = 10)# this will skip 00:30 and 1:00a
#2023-03-12, 2023-11-05
head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2023-03-11', tz = 'Etc/GMT+5')], n = 10)
#head(weather_data$datetime_noDST[weather_data$datetime_noDST >= as.POSIXct('2023-11-05', tz = 'Etc/GMT+5')], n = 10)# this will skip 00:30 and 1:00a

# update date for export
start_year = min(format(weather_data$instrument_datetime, '%Y-%m'))
end_year = max(format(weather_data$instrument_datetime, '%Y-%m'))

export_per_station <- function(station) {
  weather_data %>%
    filter(location == station) %>% 
    write_csv(., paste0('C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/L0 data/davis_weather_data_', station, '_', start_year, '-', end_year, '_L0_', Sys.Date(), '.csv'))
} 

walk(c('GM', 'SF', 'HC'), export_per_station)


