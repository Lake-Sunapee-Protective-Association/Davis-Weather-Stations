#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.3.2                                         */
#* R Studio:      2023.12.0                                      */
#* PROJECT:       lake sunapee davis weather stations            */
#* PURPOSE:       clean data for 2022                            */
#* DATE CREATED:  28Dec2023023                                   */
#*****************************************************************/

library(tidyverse)
library(ggthemes)

# source lists and functions
source("lists_functions.R")

#set up directory paths
datadir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/'

metfigdirL1 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L1/2023/'
metfigdirL05 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L0.5/2023/'

#create dirs if not present
dirstocheck <- c(metfigdirL1, metfigdirL05)
for (i in 1:length(dirstocheck)){
  if(!dir.exists(dirstocheck[i])){
    dir.create(dirstocheck[i])
  }
}

dumpdir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/L1 data/'

L1_versiondate <- Sys.Date()

#read in raw data
GM_weather_L0 <- read_csv(paste0(datadir, 'L0 data/davis_weather_data_GM_2019-07-2023-09_L0_2023-12-27.csv'))
HC_weather_L0 <- read_csv(paste0(datadir, 'L0 data/davis_weather_data_HC_2019-07-2023-09_L0_2023-12-27.csv'))
SF_weather_L0 <- read_csv(paste0(datadir, 'L0 data/davis_weather_data_SF_2019-07-2023-09_L0_2023-12-27.csv'))

# join together
weather_L0 <- bind_rows(GM_weather_L0, HC_weather_L0, SF_weather_L0) %>% 
  #coerce to tz-aware time, file export defaults to UTC time
  mutate(instrument_datetime = with_tz(instrument_datetime, "America/New_York"),
         datetime_noDST = with_tz(instrument_datetime, "Etc/GMT+5"))

#set time period of interest:
start_date = '2023-01-01'
end_date = '2024-01-01'

#filter for this time period
weather_L0_2023 <- weather_L0 %>% 
  filter(instrument_datetime >= as.POSIXct('2023-01-01 00:00', tz = 'America/New_York'),
         instrument_datetime < as.POSIXct('2024-01-01 00:00', tz = 'America/New_York'))

#create a new dataframe for data cleaning to be stored
weather_L1 <- weather_L0_2023 

## 2023 data 2-week vis ####

#This script runs iteratively over two-week periods for each variable, plotting the data at all 3 locations for the year of data

#create a list of 2 weeks during time period of interest
biweekly_2023 <- seq(as.Date(start_date), as.Date(end_date), '2 weeks')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  slice(1:26) %>% #only grab the 26 weeks
  add_row(date = as.Date(end_date)) #add last date
biweekly_2023

## issue dates ####

### deal with time shifts at HC ----

#when power goes out, HC clock does not continue to run, causing data 
#to be recorded at the wrong time

#### 1: 26h from last year ----

# apply shift from last year to this year (26h)
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = datetime_noDST + hours(26)) %>% 
  mutate(time_flag = 'm: +26h')

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

# plot all the pressure data together to see time issues at HC
pwalk(list(start = biweekly_2023$date[1:(nrow(biweekly_2023)-1)], 
           end = biweekly_2023$date[2:nrow(biweekly_2023)]), 
      data = weather_L1_vert,
      offset = 26,
      make_pressure_plots)
                                    
                                    
#### 2: Top of record 30 minuts ---- 
date_start = "2023-01-02"
date_end = "2023-01-03"
weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()

#30m shift when record starts 1/02 2 am
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2023-01-02 2:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +26.5',
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date(date_start) & 
                                       datetime_noDST < as.Date(date_end))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date(date_start) & 
                                              datetime_noDST < as.Date(date_end))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2023$date[1:(nrow(biweekly_2023)-1)], 
           end = biweekly_2023$date[2:nrow(biweekly_2023)]), 
      data = weather_L1_vert,
      offset = 26.5,
      make_pressure_plots)                                  
                                    

#### 3: April 24  ---- 
date_start = "2023-04-24"
date_end = "2023-04-25"
weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()

#30m shift 04/24 14:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2023-04-24 14:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +27',
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date(date_start) & 
                                       datetime_noDST < as.Date(date_end))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date(date_start) & 
                                              datetime_noDST < as.Date(date_end))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2023$date[1:(nrow(biweekly_2023)-1)], 
           end = biweekly_2023$date[2:nrow(biweekly_2023)]), 
      data = weather_L1_vert,
      offset = 27,
      make_pressure_plots)                                  


#### 4: May10 ---- 
date_start = "2023-05-10"
date_end = "2023-05-11"
weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()

#30m shift when record starts 05/10 18:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2023-05-10 12:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +27.5',
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date(date_start) & 
                                       datetime_noDST < as.Date(date_end))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date(date_start) & 
                                              datetime_noDST < as.Date(date_end))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2023$date[1:(nrow(biweekly_2023)-1)], 
           end = biweekly_2023$date[2:nrow(biweekly_2023)]), 
      data = weather_L1_vert,
      offset = 27.5,
      make_pressure_plots)  

#### 5: Sept 11 ----
date_start = "2023-09-11"
date_end = "2023-09-12"

weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()

#60m shift when record starts 09/11 12:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2023-09-11 12:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(60),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +28.5',
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date(date_start) & 
                                       datetime_noDST < as.Date(date_end))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date(date_start) & 
                                              datetime_noDST < as.Date(date_end))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2023$date[1:(nrow(biweekly_2023)-1)], 
           end = biweekly_2023$date[2:nrow(biweekly_2023)]), 
      data = weather_L1_vert,
      offset = 28.5,
      make_pressure_plots)  


#### 6: Sept 12 ----
date_start = "2023-05-10"
date_end = "2023-05-11"
weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()

#30m shift when record starts 05/10 18:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2023-05-10 12:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +',28.5
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date(date_start) & 
                                       datetime_noDST < as.Date(date_end))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date(date_start) & 
                                              datetime_noDST < as.Date(date_end))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2023$date[1:(nrow(biweekly_2023)-1)], 
           end = biweekly_2023$date[2:nrow(biweekly_2023)]), 
      data = weather_L1_vert,
      offset = , 28.5
      make_pressure_plots)  


### Re-run L0.5 graphs after time change ----
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      make_L05_plots, 
      data = weather_L1_vert)

# data look good at a 2-week level. no flags or recoding needed.

## Overall data flags ----

#Now to add flags to the data that are consistently wonky due to weather station placement:
  
#SF uv/rad flagged as obscured in AM hours
#SF bar pressure flagged because it's consistently lower than other 2 locations

weather_L1 <- weather_L1 %>% 
  mutate(UV_flag = case_when(location == 'SF' ~ 'oAM',
                             TRUE ~ ''),
         SR_flag = case_when(location == 'SF' ~ 'oAM',
                             TRUE ~ ''),
         BP_flag = case_when(location == 'SF' ~ 'l',
                             TRUE ~ '')) 

## Export QAQC Data ----

weather_L1 %>%
  mutate(instrument_datetime = as.character(instrument_datetime),
         datetime_noDST = as.character(datetime_noDST)) %>%
  write_csv(paste0(datadir, 'L1 data/davis_weather_data_', start_date, '_', end_date, '_L1_v', L1_versiondate, '.csv'))

