#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.1.3                                          */
#* R Studio:      1.4.1103                                       */
#* PROJECT:       lake sunapee davis weather stations            */
#* PURPOSE:       clean data for 2021                            */
#* DATE CREATED:  09March2022                                    */
#*****************************************************************/

library(tidyverse)
library(lubridate)
library(ggthemes)

#set up directory paths
datadir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/'

metfigdirL1 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L1/2021/'
metfigdirL05 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L0.5/2021/'

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
weather_L0 <- read.csv(paste0(datadir, 'L0 data/davis_weather_data_2019-2022_L0_2022-03-09.csv'))

#filter for this time period
weather_L0_2021 <- weather_L0 %>% 
  mutate(datetime_noDST = as.POSIXct(datetime_noDST, tz = 'Etc/GMT+5'),
         instrument_datetime = as.POSIXct(instrument_datetime, tz = 'America/New_York')) %>% 
  filter(instrument_datetime >= as.POSIXct('2021-01-01', tz = 'America/New_York'))

#create a new dataframe for data cleaning to be stored
weather_L1 <- weather_L0_2021 

#create a vertical dataset for ggplot
weather_L0_vert <- weather_L0_2021 %>% 
  select(location, datetime_noDST, source, all_of(dataforviz)) %>% #select only the data for visualization
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
                             winddir == 'NNW' ~ '15')) %>% 
  gather(variable, value, -location, - datetime_noDST, -source) %>% 
  mutate(value = as.numeric(value))


## 2021 data 2-week vis ####

#This script runs iteratively over two-week periods for each variable, plotting the data at all 3 locations for the year of data

#set time period of interest:
start_date = '2021-01-01'
end_date = '2022-01-01'

#create a list of 2 weeks during time period of interest
biweekly_2021 <- seq(as.Date(start_date), as.Date(end_date), '2 weeks')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  slice(1:26) %>% #only grab the 26 weeks
  add_row(date = as.Date(end_date)) #add last date


#plot all L0.5 plots and save to appropriate figdir
for (i in 1:(nrow(biweekly_2021)-1)){
  gg_met <- ggplot(subset(weather_L0_vert, subset=(datetime_noDST>biweekly_2021$date[i] & 
                                                     datetime_noDST < biweekly_2021$date[i+1])), aes(x=datetime_noDST, y=value)) + 
    geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', biweekly_2021$date[i], ' through ', biweekly_2021$date[i+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL05, '2wk_L0.5_plots_', biweekly_2021$date[i], '-', biweekly_2021$date[i+1], '.jpeg'),
        width = 10, height = 8, units = 'in')
}


# plot all the pressure data together to confirm time issues at HC
pressureonly <- weather_L0_vert %>% 
  filter(variable == 'pressure_hpa')
for (x in 1:(nrow(biweekly_2021)-1)){
  gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2021$date[x] & 
                                                     datetime_noDST < biweekly_2021$date[x+1])), aes(x=datetime_noDST, y=value)) + 
    geom_point(aes(color = location)) +
    labs(title=paste0('Raw Pressure Data ', biweekly_2021$date[x], ' through ', biweekly_2021$date[x+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL05, '2wk_L0.5_pres_plots_', biweekly_2021$date[x], '-', biweekly_2021$date[x+1], '.jpeg'),
         width = 10, height =4, units = 'in')
}

## issue dates ####

### deal with time shifts at HC ----

#when power goes out, HC clock does not continue to run, causing data to be recorded at the wrong time

#time shift at HC Mar 1,Sept 25-26, Dec 11

#### march 1-2 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2021-03-01') & 
                                      datetime_noDST < as.Date('2021-03-03'))), 
              aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#1h shift starts 2021-03-01 21:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2021-03-01 21:30', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(1),
                   TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2021-03-01 21:30', tz = 'Etc/GMT+5') ~ 'm: +1h',
                               TRUE ~ ''))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2021-03-01') & 
                                      datetime_noDST < as.Date('2021-03-03'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2021-03-01') & 
                                              datetime_noDST < as.Date('2021-03-03'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
  select(location, datetime_noDST, source, all_of(dataforviz)) %>% #select only the data for visualization
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
                             winddir == 'NNW' ~ '15')) %>% 
  gather(variable, value, -location, - datetime_noDST, -source) %>% 
  mutate(value = as.numeric(value))

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
for (x in 1:(nrow(biweekly_2021)-1)){
  gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2021$date[x] & 
                                                  datetime_noDST < biweekly_2021$date[x+1])), aes(x=datetime_noDST, y=value)) + 
    geom_point(aes(color = location)) +
    labs(title=paste0('Raw Pressure Data 1h', biweekly_2021$date[x], ' through ', biweekly_2021$date[x+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL05, '2wk_L0.5_1h_pres_plots_', biweekly_2021$date[x], '-', biweekly_2021$date[x+1], '.jpeg'),
         width = 10, height = 4, units = 'in')
}

#### sept 25/26 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2021-09-25') & 
                                      datetime_noDST < as.Date('2021-09-27'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#4h shift starts '2021-09-25 23:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2021-09-25 23:30', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(4),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2021-09-25 23:30', tz = 'Etc/GMT+5') ~ 'm: +5h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2021-09-25') & 
                                       datetime_noDST < as.Date('2021-09-27'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2021-09-25') & 
                                              datetime_noDST < as.Date('2021-09-27'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
  select(location, datetime_noDST, source, all_of(dataforviz)) %>% #select only the data for visualization
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
                             winddir == 'NNW' ~ '15')) %>% 
  gather(variable, value, -location, - datetime_noDST, -source) %>% 
  mutate(value = as.numeric(value))

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
for (x in 1:(nrow(biweekly_2021)-1)){
  gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2021$date[x] & 
                                                  datetime_noDST < biweekly_2021$date[x+1])), aes(x=datetime_noDST, y=value)) + 
    geom_point(aes(color = location)) +
    labs(title=paste0('Raw Pressure Data 1h, 5h', biweekly_2021$date[x], ' through ', biweekly_2021$date[x+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL05, '2wk_L0.5_1h_5h_pres_plots_', biweekly_2021$date[x], '-', biweekly_2021$date[x+1], '.jpeg'),
         width = 10, height = 4, units = 'in')
}

#### dec 11/12 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2021-12-11') & 
                                      datetime_noDST < as.Date('2021-12-15'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#6h shift starts 2021-03-01 21:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2021-12-12 00:30', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(6),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2021-12-12 00:30', tz = 'Etc/GMT+5') ~ 'm: +11h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2021-12-11') & 
                                       datetime_noDST < as.Date('2021-12-15'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2021-12-11') & 
                                              datetime_noDST < as.Date('2021-12-15'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
  select(location, datetime_noDST, source, all_of(dataforviz)) %>% #select only the data for visualization
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
                             winddir == 'NNW' ~ '15')) %>% 
  gather(variable, value, -location, - datetime_noDST, -source) %>% 
  mutate(value = as.numeric(value))

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
for (x in 1:(nrow(biweekly_2021)-1)){
  gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2021$date[x] & 
                                                  datetime_noDST < biweekly_2021$date[x+1])), aes(x=datetime_noDST, y=value)) + 
    geom_point(aes(color = location)) +
    labs(title=paste0('Raw Pressure Data 1h, 5h, 11h ', biweekly_2021$date[x], ' through ', biweekly_2021$date[x+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL05, '2wk_L0.5_1h_5h_11h_pres_plots_', biweekly_2021$date[x], '-', biweekly_2021$date[x+1], '.jpeg'),
         width = 10, height = 4, units = 'in')
}

### location-specific issues ----

#### precip at HC plugged/unplugged Aug 2 ----
ggplot(subset(weather_L1, subset=(datetime_noDST>as.Date('2021-08-02') & 
                                      datetime_noDST < as.Date('2021-08-03'))), 
       aes(x=datetime_noDST, y=rain_mm)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

weather_L1 <- weather_L1 %>% 
  mutate(rain_mm = case_when(location == 'HC' & datetime_noDST == as.POSIXct('2021-08-02 14:30', tz = 'Etc/GMT+5') ~ NA_real_,
                             TRUE ~ rain_mm)) %>% 
  mutate(rain_flag = case_when(location == 'HC' & datetime_noDST == as.POSIXct('2021-08-02 14:30', tz = 'Etc/GMT+5') ~ 'pine needle removed from rain gauge; data prior may not be precise due to clogged gauge',
                               TRUE ~ ''))

ggplot(subset(weather_L1, subset=(datetime_noDST>as.Date('2021-08-02') & 
                                    datetime_noDST < as.Date('2021-08-03'))), 
       aes(x=datetime_noDST, y=rain_mm)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#### questionable precip at GM sept 24 ----
ggplot(subset(weather_L1, subset=(datetime_noDST>as.Date('2021-09-24') & 
                                    datetime_noDST < as.Date('2021-09-25'))), 
       aes(x=datetime_noDST, y=rain_mm)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind() 

weather_L1 <- weather_L1 %>% 
  mutate(rain_flag = case_when(location == 'GM' & datetime_noDST == as.POSIXct('2021-09-24 10:30', tz = 'Etc/GMT+5') ~ 's',
                               TRUE ~ rain_flag))

#### questionable precip at GM dec 11 ----
ggplot(subset(weather_L1, subset=(datetime_noDST>as.Date('2021-12-11') & 
                                    datetime_noDST < as.Date('2021-12-12'))), 
       aes(x=datetime_noDST, y=rain_mm)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind() 

weather_L1 <- weather_L1 %>% 
  mutate(rain_flag = case_when(location == 'GM' & datetime_noDST == as.POSIXct('2021-12-11 14:00', tz = 'Etc/GMT+5') ~ 's',
                               TRUE ~ rain_flag))

#### precip unclogged at HC Dec 14 ----

ggplot(subset(weather_L1, subset=(datetime_noDST>as.Date('2021-12-14') & 
                                    datetime_noDST < as.Date('2021-12-15'))), 
       aes(x=datetime_noDST, y=rain_mm)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#no data to recode, just add a flag

weather_L1 <- weather_L1 %>% 
  mutate(rain_flag = case_when(location == 'HC' & datetime_noDST == as.POSIXct('2021-12-14 12:00', tz = 'Etc/GMT+5') ~ 'pine needle removed from rain gauge; data prior may not be precise due to clogged gauge',
                               TRUE ~ rain_flag))

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


## 2021 L1 2-week vis check ----
weather_L1_vert <- weather_L1 %>% 
  select(location, datetime_noDST, source, all_of(dataforviz)) %>% #select only the data for visualization
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
                             winddir == 'NNW' ~ '15')) %>% 
  gather(variable, value, -location, - datetime_noDST, -source) %>% 
  mutate(value = as.numeric(value))

#plot all L1 plots and save to appropriate figdir
for (i in 1:(nrow(biweekly_2021)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>biweekly_2021$date[i] & 
                                                     datetime_noDST < biweekly_2021$date[i+1])), aes(x=datetime_noDST, y=value)) + 
    geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Clean Met Data ', biweekly_2021$date[i], ' through ', biweekly_2021$date[i+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L1_plots_', biweekly_2021$date[i], '-', biweekly_2021$date[i+1], '.jpeg'),
         width = 10, height = 8, units = 'in')
}


## join with previous data and export ----

