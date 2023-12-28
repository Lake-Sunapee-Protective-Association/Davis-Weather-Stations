#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.3.2                                          */
#* R Studio:      2023.12.00                                     */
#* PROJECT:       lake sunapee davis weather stations            */
#* PURPOSE:       clean data for 2022                            */
#* DATE CREATED:  13August2023                                   */
#* UPDATED:       27Dec2023 to move 2023 data to its own script  */
#*****************************************************************/

library(tidyverse)
library(ggthemes)

#set up directory paths
datadir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/'

metfigdirL1 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L1/2022/'
metfigdirL05 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L0.5/2022/'

#create dirs if not present
dirstocheck <- c(metfigdirL1, metfigdirL05)
for (i in 1:length(dirstocheck)){
  if(!dir.exists(dirstocheck[i])){
    dir.create(dirstocheck[i])
  }
}

dumpdir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/L1 data/'

#create variable lists - these are the non-derived variables. these will tell us whether or not other data is inaccurate.
dataforviz = c('pressure_hpa', 'temp_c', 'humidity_perc', 'windsp_mps', 'winddir', 'rain_mm', 'solarradiation_wpm2', 'uvindex')
allvars = c( "pressure_hpa", "temp_c", "hightemp_c", "lowtemp_c", "humidity_perc", "dewpoint_c", "wetbulb_c", "windsp_mps", "winddir", "windrun_m", 
             "highwindsp_mps", "highwinddir", "windchill_c", "heatindex_c", "thwindex_c", "thswindex_c", "rain_mm", "rainrate_mmph", "solarradiation_wpm2", 
             "solarenergy_ly", "highsolarrad_wpm2", "evapotrans_mm", "uvindex", "uvdose_meds", "highuvindex", "heatingdegdays", "coolingdegdays")
tempvars = c("temp_c", "hightemp_c", "lowtemp_c", "humidity_perc", "dewpoint_c", "wetbulb_c", "windchill_c", "heatindex_c", "thwindex_c", "thswindex_c", "heatingdegdays", "coolingdegdays")
uvvars = c("uvindex", "uvdose_meds", "highuvindex")
srvars = c("solarradiation_wpm2", "solarenergy_ly", "highsolarrad_wpm2", "evapotrans_mm")
precipvars = c("rain_mm", "rainrate_mmph")
windvars = c("windsp_mps", "winddir", "windrun_m", "highwindsp_mps", "highwinddir")
allnumvars = c("pressure_hpa", "temp_c", "hightemp_c", "lowtemp_c", "humidity_perc", "dewpoint_c", "wetbulb_c", "windsp_mps", "windrun_m", 
               "highwindsp_mps", "windchill_c", "heatindex_c", "thwindex_c", "thswindex_c", "rain_mm", "rainrate_mmph", "solarradiation_wpm2", 
               "solarenergy_ly", "highsolarrad_wpm2", "evapotrans_mm", "uvindex", "uvdose_meds", "highuvindex", "heatingdegdays", "coolingdegdays")
allcharvars = c("winddir", "highwinddir")

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
start_date = '2022-01-01'
end_date = '2023-01-01'

#filter for this time period
weather_L0_2022 <- weather_L0 %>% 
  filter(instrument_datetime >= as.POSIXct('2022-01-01 00:00', tz = 'America/New_York'),
         instrument_datetime < as.POSIXct('2023-01-01 00:00', tz = 'America/New_York'))

#create a new dataframe for data cleaning to be stored
weather_L1 <- weather_L0_2022 

#create a vertical dataset for ggplot
pivot_data_for_ggplot <- function(data) {
  data %>% 
    select(location, datetime_noDST, all_of(dataforviz)) %>% #select only the data for visualization
    mutate(winddir = case_when(winddir == 'N' ~ '0',
                               winddir == 'NNE' ~ '1', 
                               winddir == 'NE' ~ '2',
                               winddir == 'ENE' ~'3',
                               winddir == 'E' ~ '4',
                               winddir == 'ESE' ~ '5',
                               winddir == 'SE' ~ '6',
                               winddir == 'SSE' ~ '7',
                               winddir == 'S' ~ '8',
                               winddir == 'SSW' ~ '9',
                               winddir == 'SW' ~ '10', 
                               winddir == 'WSW' ~ '11', 
                               winddir == 'W' ~ '12',
                               winddir == 'WNW' ~ '13',
                               winddir == 'NW' ~ '14',
                               winddir == 'NNW' ~ '15'),
           winddir = as.numeric(winddir)) %>% 
    pivot_longer(cols = all_of(dataforviz),
                 names_to = "variable", 
                 values_to = "value")
}

weather_L0_vert <- pivot_data_for_ggplot(weather_L0_2022)

## 2022 data 2-week vis ####

#This script runs iteratively over two-week periods for each variable, plotting the data at all 3 locations for the year of data


#create a list of 2 weeks during time period of interest
biweekly_2022 <- seq(as.Date(start_date), as.Date(end_date), '2 weeks')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  slice(1:26) %>% #only grab the 26 weeks
  add_row(date = as.Date(end_date)) #add last date
biweekly_2022

#plot all L0.5 plots and save to appropriate figdir
make_L05_plots <- function(start, end, data) {
  data %>% 
    filter(datetime_noDST>= start &
             datetime_noDST < end) %>% 
    ggplot(., aes(x=datetime_noDST, y=value)) + 
    geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', start, ' through ', end),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  ggsave(paste0(metfigdirL05, 
                '2wk_L0.5_plots_', 
                start, 
                '-', 
                end, 
                '.jpeg'),
         width = 10, 
         height = 8, 
         units = 'in')
}

pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      make_L05_plots, 
      data = weather_L0_vert)

# plot all the pressure data together to confirm time issues at HC

make_pressure_plots <- function(start, end, data, offset) {
  pressureonly <- data %>% 
    filter(variable == 'pressure_hpa', 
           datetime_noDST>= start &
             datetime_noDST < end) %>%
  ggplot(., aes(x=datetime_noDST, y=value)) + 
    geom_point(aes(color = location)) +
    labs(title=paste0('Raw Pressure Data ', 
                      start, 
                      ' through ', 
                      end),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  ggsave(paste0(metfigdirL05, 
                '2wk_L0.5_pres_plots_',
                offset,
                "h_",
                start, 
                '-', 
                end, 
                '.jpeg'),
         width = 10, height =4, units = 'in')
}

pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L0_vert,
      offset = 0,
      make_pressure_plots)

## issue dates ####

### deal with time shifts at HC ----

#when power goes out, HC clock does not continue to run, causing data 
#to be recorded at the wrong time

#### 1: 11h from last year ----

# apply shift from last year to this year (11h)
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = datetime_noDST + hours(11)) %>% 
  mutate(time_flag = 'm: +11h')

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2022-01-01') & 
                                      datetime_noDST < as.Date('2022-01-07'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2022-01-01') & 
                                              datetime_noDST < as.Date('2022-01-07'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 11,
      make_pressure_plots)

#### 2: Feb 10th ish ----

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct('2022-02-11 04:00', tz = 'Etc/GMT+5'), 
                                  datetime_noDST + hours(1),
                                  datetime_noDST)) %>%
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct('2022-02-11 04:00', tz = 'Etc/GMT+5'), 
                             'm: +12h',
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2022-02-11') & 
                                       datetime_noDST < as.Date('2022-02-13'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2022-02-11') & 
                                              datetime_noDST < as.Date('2022-02-13'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 12,
      make_pressure_plots)


#### 3: feb 21 ----
weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST>as.Date('2022-02-21') & 
           datetime_noDST < as.Date('2022-02-23')) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#30m shift starts 2/21 13:30 
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct('2022-02-21 13:30', tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct('2022-02-21 13:30', tz = 'Etc/GMT+5'),
                             'm: +12.5',
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2022-02-21') & 
                                       datetime_noDST < as.Date('2022-02-23'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2022-02-21') & 
                                              datetime_noDST < as.Date('2022-02-23'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 12.5,
      make_pressure_plots)

#### 4: mar 7 ----

weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST>as.Date('2022-03-07') & 
           datetime_noDST < as.Date('2022-03-09')) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#30m shift starts 03/07 23:30

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct('2022-03-07 23:30', tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct('2022-03-07 23:30', tz = 'Etc/GMT+5'),
                             'm: +13',
                             time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2022-03-07') & 
                                       datetime_noDST < as.Date('2022-03-09'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2022-03-07') & 
                                              datetime_noDST < as.Date('2022-03-09'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- pivot_data_for_ggplot(weather_L1)

#cycle through pressure only data again
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 13,
      make_pressure_plots)                       
                                    
                                    
#### 5: March 12 ---- 
date_start = "2022-03-12"
date_end = "2022-03-13"
weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#30m shift starts 3/12 11a
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2022-03-12 11:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +13.5',
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
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 13.5,
      make_pressure_plots)                                  
                                    

#### 6: March 18 ----
date_start = "2022-03-18"
date_end = "2022-03-19"

weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#30m shift starts 3/18 9:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2022-03-18 9:30"
shift_time2 = "2022-03-18 13:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>%
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +14',
                             time_flag)) %>%
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time2, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +14.5',
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
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 14.5,
      make_pressure_plots)   

#### 7: March 29 ----
date_start = "2022-03-29"
date_end = "2022-03-30"

weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# 30m shift starts 3/29 11:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2022-03-29 11:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(30),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +15',
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
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 15,
      make_pressure_plots)   

#### 8: Mar 31 ----
date_start = "2022-03-31"
date_end = "2022-04-01"

weather_L1_vert %>% 
  filter(variable == 'pressure_hpa' &
           datetime_noDST > as.Date(date_start) & 
           datetime_noDST < as.Date(date_end)) %>% 
  ggplot(., aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# 1h shift starts 3/31 18:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2022-03-31 18:00"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + minutes(60),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +16',
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
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 16,
      make_pressure_plots)   


#### 9: Aug 4-5 ----
date_start = "2022-08-04"
date_end = "2022-08-06"

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

# 9h shift starts 08/04 17:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2022-08-04 17:30"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + hours(9),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +25',
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
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 25,
      make_pressure_plots)   


#### 10: Nov 23 ----
date_start = "2022-11-23"
date_end = "2022-11-24"

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

# 1h shift starts 11/23 14:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

shift_time = "2022-11-23 14:30"

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                                  datetime_noDST + hours(1),
                                  datetime_noDST)) %>% 
  mutate(time_flag = if_else(datetime_noDST >= as.POSIXct(shift_time, tz = 'Etc/GMT+5'),
                             'm: +26',
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
pwalk(list(start = biweekly_2022$date[1:(nrow(biweekly_2022)-1)], 
           end = biweekly_2022$date[2:nrow(biweekly_2022)]), 
      data = weather_L1_vert,
      offset = 26,
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

