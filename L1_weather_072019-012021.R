#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.2.3                                          */
#* R Studio:      2024.12.0-367                                  */
#* PROJECT:       lake sunapee davis weather stations            */
#* PURPOSE:       clean data for 2019-2020                       */
#* LAST MODIFIED: 27Dec2023 re run with updated DST and          */
#*                by-station export                              */
#*****************************************************************/

library(tidyverse)
library(ggthemes)

#set up directory paths
datadir <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/'

metfigdirL1 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L1/2020/'
metfigdirL05 <- 'C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L0.5/2020/'

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

# Davis Weather Station QAQC ----

# This file tracks the QAQC of the Sunapee Davis Weather Stations July 2019-December 2021, 
# which are owned/operated by the LSPA and and QAQC'd by B. Steele of the Weathers' Lab, 
# Cary Institute of Ecosystem Studies, Millbrook, NY. Data are QAQC'd in this file 
# to remove obviously errant data. 

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
start_date = '2019-07-01'
end_date = '2021-01-01'

#filter data for time
weather_L0 <- weather_L0 %>% 
  filter(instrument_datetime < as.POSIXct('2021-01-01', tz = 'America/New_York'))

#create a new dataframe for data cleaning
weather_L1 <- weather_L0 

#create a vertical dataset for ggplot
weather_L0_vert <- weather_L0 %>% 
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
               names_to = 'variable', 
               values_to = 'value') %>% 
  mutate(value = as.numeric(value))


#create a list of 2 weeks during time period of interest
biweekly_2020 <- seq(as.Date(start_date), as.Date(end_date), '2 weeks')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  slice(1:39) %>% 
  add_row(date = as.Date(end_date)) #add last date

## time issues at HC ----

# plot all the pressure data together to confirm time issues at HC
pressureonly <- weather_L0_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] &
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) +
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data ', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height =4, units = 'in')
# }

### deal with problematic time at HC ----
# note, that while coding these, I will often comment out sections that require more 
# time/processing (i.e. the for-loops) to ease
#processing when I need to rerun any preceeding code. 

#### 1: 2020-01-13 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-01-13') & 
                                      datetime_noDST < as.Date('2020-01-14'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#2h shift starts 2020-01-13 11:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-01-13 11:00', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(2),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-01-13 11:00', tz = 'Etc/GMT+5') ~ 'm: +2h',
                               TRUE ~ ''))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-01-13') & 
                                       datetime_noDST < as.Date('2020-01-14'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-01-13') & 
                                              datetime_noDST < as.Date('2020-01-14'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm1_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }

#### 2: 2020-01-25 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-01-25') & 
                                      datetime_noDST < as.Date('2020-01-26'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#1h shift starts 2020-01-25 19:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-01-25 19:00', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(1),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-01-25 19:00', tz = 'Etc/GMT+5') ~ 'm: +3h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-01-25') & 
                                       datetime_noDST < as.Date('2020-01-26'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-01-25') & 
                                              datetime_noDST < as.Date('2020-01-26'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm2_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }


#### 3: 2020-05-15 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-05-15') & 
                                      datetime_noDST < as.Date('2020-05-16'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#1h shift starts 2020-05-15 18:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-05-15 8:00', tz = 'Etc/GMT+5') ~ datetime_noDST + minutes(90),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-05-15 8:00', tz = 'Etc/GMT+5') ~ 'm: +4.5h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-05-15') & 
                                       datetime_noDST < as.Date('2020-05-16'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-05-15') & 
                                              datetime_noDST < as.Date('2020-05-16'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm3_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }


#### 4: 2020-06-24 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-06-24') & 
                                      datetime_noDST < as.Date('2020-06-25'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-06-26') & 
                                      datetime_noDST < as.Date('2020-06-28'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#22.5h shift starts 2020-06-24 17:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-06-24 16:30', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(22),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-06-24 16:30', tz = 'Etc/GMT+5') ~ 'm: +26.5h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-06-24') & 
                                      datetime_noDST < as.Date('2020-06-28'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-06-24') & 
                                              datetime_noDST < as.Date('2020-06-28'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm4_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }


#### 5: 2020-07-10 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-07-10') & 
                                      datetime_noDST < as.Date('2020-07-11'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#match low at HC 7/13 with low at others 7/23
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-07-13') & 
                                      datetime_noDST < as.Date('2020-07-14'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()
#2020-07-13 8:00
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-07-23') & 
                                      datetime_noDST < as.Date('2020-07-24'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()
#2020-07-23 15:00

#10d 7h shift starts 2020-07-10 10:30
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-07-09 10:30', tz = 'Etc/GMT+5') ~ datetime_noDST + days(10) + hours(6) + minutes(30),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-07-09 10:30', tz = 'Etc/GMT+5') ~ 'm: +11d 9h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-07-17') & 
                                       datetime_noDST < as.Date('2020-07-21'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-07-17') & 
                                              datetime_noDST < as.Date('2020-07-21'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# start of record is wrong, but it doesn't look like data before it, so
# we'll clean it during 2-week processing

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-07-23') & 
                                       datetime_noDST < as.Date('2020-07-24'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-07-23') & 
                                              datetime_noDST < as.Date('2020-07-24'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm5_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }

#### 6: 08-04-2020 time is reset! ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-08-04') & 
                                      datetime_noDST < as.Date('2020-08-05'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#match low at HC 8/16
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-08-16') & 
                                      datetime_noDST < as.Date('2020-08-17'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#data ahead should be moved back to 17:30 or low at 18:00
#join back together and add a flag of 'm' for modified time

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-08-16 3:00', tz = 'Etc/GMT+5') ~ datetime_noDST - days(11) - hours(9),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-08-16 3:00', tz = 'Etc/GMT+5') ~ '',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-08-04') & 
                                       datetime_noDST < as.Date('2020-08-05'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-08-04') & 
                                              datetime_noDST < as.Date('2020-08-05'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm6_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }

#### 7: 2020-09-06 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-09-06') & 
                                      datetime_noDST < as.Date('2020-09-07'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# 1h behind 2020-09-06 6:00

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-09-06 6:00', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(1),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-09-06 6:00', tz = 'Etc/GMT+5') ~ 'm: +1h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-09-04') & 
                                       datetime_noDST < as.Date('2020-09-08'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-09-04') & 
                                              datetime_noDST < as.Date('2020-09-08'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm7_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }


#### 8: 2020-11-01 ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-11-01') & 
                                      datetime_noDST < as.Date('2020-11-03'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# 1.5h behind 2020-11-01 23:30

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-11-01 23:30', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(1) + minutes(30),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-11-01 23:30', tz = 'Etc/GMT+5') ~ 'm: +2.5h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-11-01') & 
                                       datetime_noDST < as.Date('2020-11-03'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-11-01') & 
                                              datetime_noDST < as.Date('2020-11-03'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

#look later, too
#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-11-02') & 
                                       datetime_noDST < as.Date('2020-11-05'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-11-02') & 
                                              datetime_noDST < as.Date('2020-11-05'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# 2020-11-03
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-11-03') & 
                                       datetime_noDST < as.Date('2020-11-04'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-11-03') & 
                                              datetime_noDST < as.Date('2020-11-04'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-11-03 09:30', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(1),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-11-03 09:30', tz = 'Etc/GMT+5') ~ 'm: +3.5h',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-11-02') & 
                                       datetime_noDST < as.Date('2020-11-05'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-11-02') & 
                                              datetime_noDST < as.Date('2020-11-05'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm8_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }


#### 9: 2020-11-19 time reset  ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-11-19') & 
                                      datetime_noDST < as.Date('2020-11-20'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# clock reset at 2020-11-19 23:00

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-11-19 23:00', tz = 'Etc/GMT+5') ~ datetime_noDST -hours(3) -minutes(30),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-11-19 23:00', tz = 'Etc/GMT+5') ~ '',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-11-19') & 
                                       datetime_noDST < as.Date('2020-11-20'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-11-19') & 
                                              datetime_noDST < as.Date('2020-11-20'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-11-15') & 
                                       datetime_noDST < as.Date('2020-11-30'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-11-15') & 
                                              datetime_noDST < as.Date('2020-11-30'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
# for (x in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] & 
#                                                   datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_tm9_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
#          width = 10, height = 4, units = 'in')
# }

#### 10: 2020-12-05 blip offline ----
ggplot(subset(pressureonly, subset=(datetime_noDST>as.Date('2020-12-05') & 
                                      datetime_noDST < as.Date('2020-12-07'))), 
       aes(x=datetime_noDST, y=value)) + 
  geom_point(aes(color = location)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# data off starting 21:30 2020-12-06 08:30; need to move to 22:00

HC_timechange <- weather_L1 %>% 
  filter(location == 'HC')
noHC_L1 <- weather_L1 %>% 
  filter(location != 'HC')

HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = case_when(datetime_noDST >= as.POSIXct('2020-12-05 21:30', tz = 'Etc/GMT+5')&
                                      datetime_noDST <= as.POSIXct('2020-12-06 08:30', tz = 'Etc/GMT+5') ~ datetime_noDST + hours(12) + minutes(30),
                                    TRUE ~ datetime_noDST)) %>% 
  mutate(time_flag = case_when(datetime_noDST >= as.POSIXct('2020-12-05 21:30', tz = 'Etc/GMT+5')&
                                 datetime_noDST <= as.POSIXct('2020-12-06 08:30', tz = 'Etc/GMT+5') ~ 'm: +12.5',
                               TRUE ~ time_flag))

#make sure this worked
ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-12-05') & 
                                       datetime_noDST < as.Date('2020-12-07'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1, subset=(datetime_noDST>as.Date('2020-12-05') & 
                                                datetime_noDST < as.Date('2020-12-07'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

ggplot(subset(HC_timechange, subset=(datetime_noDST>as.Date('2020-12-05') & 
                                       datetime_noDST < as.Date('2020-12-20'))), 
       aes(x=datetime_noDST, y=pressure_hpa)) + 
  geom_point() +
  geom_point(data = subset(noHC_L1,subset=(datetime_noDST>as.Date('2020-12-05') & 
                                             datetime_noDST < as.Date('2020-12-20'))), 
             aes(x = datetime_noDST, y = pressure_hpa), shape = 21) +
  theme_bw() +
  scale_x_datetime(minor_breaks = '1 day') +
  scale_color_colorblind()

# re join with non-time change data
weather_L1 = full_join(HC_timechange, noHC_L1)

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#cycle through pressure only data again
pressureonly <- weather_L1_vert %>% 
  filter(variable == 'pressure_hpa')
for (x in 1:(nrow(biweekly_2020)-1)){
  gg_met <- ggplot(subset(pressureonly, subset=(datetime_noDST>biweekly_2020$date[x] &
                                                  datetime_noDST < biweekly_2020$date[x+1])), aes(x=datetime_noDST, y=value)) +
    geom_point(aes(color = location)) +
    labs(title=paste0('Raw Pressure Data 1h', biweekly_2020$date[x], ' through ', biweekly_2020$date[x+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL05, '2wk_L0.5_tm10_pres_plots_', biweekly_2020$date[x], '-', biweekly_2020$date[x+1], '.jpeg'),
         width = 10, height = 4, units = 'in')
}

## Clean data ----

### 2-week plots of all data ----

# #plot all L0.5 plots and save to appropriate figdir
# for (i in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>biweekly_2020$date[i] & 
#                                                      datetime_noDST < biweekly_2020$date[i+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point() +
#     facet_grid(variable ~ location, scales = 'free_y') +
#     labs(title=paste0('Raw Met Data ', biweekly_2020$date[i], ' through ', biweekly_2020$date[i+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_plots_', biweekly_2020$date[i], '-', biweekly_2020$date[i+1], '.jpeg'),
#          width = 10, height = 8, units = 'in')
# }

# #plot all together in paneled, too
# for (i in 1:(nrow(biweekly_2020)-1)){
#   gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>biweekly_2020$date[i] & 
#                                                      datetime_noDST < biweekly_2020$date[i+1])), aes(x=datetime_noDST, y=value)) + 
#     geom_point(aes(color = location)) +
#     facet_grid(variable ~ ., scales = 'free_y') +
#     labs(title=paste0('Raw Met Data ', biweekly_2020$date[i], ' through ', biweekly_2020$date[i+1]),
#          x='date',
#          y=NULL) +
#     theme_bw() +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     scale_x_datetime(minor_breaks = '1 day') +
#     scale_color_colorblind()
#   print(gg_met)
#   ggsave(paste0(metfigdirL05, '2wk_L0.5_plots_nofacet_', biweekly_2020$date[i], '-', biweekly_2020$date[i+1], '.jpeg'),
#          width = 6, height = 8, units = 'in')
# }


### Q3 2019 ----
#deal with deployment and early pressure issues 
# GM until Jul 10
# HC until Jul 3
# barometric pressure until Jul 9

#set time period of interest:
location_a = 'GM'
start_date_a = '2019-07-10'
end_date_a = '2019-07-11'

location_b = 'HC'
start_date_b = '2019-07-03'
end_date_b = '2019-07-04'

location_c = 'SF'
start_date_c = '2019-07-09'
end_date_c = '2019-07-10'

ggplot(subset(weather_L1_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_a, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
#GM station online 2019-07-10 9:30
ix=which(weather_L1$location == 'GM' & weather_L1$datetime_noDST<as.POSIXct('2019-07-10 8:30', tz='Etc/GMT+5'))
for (i in c(allvars)) {weather_L1[ix,i]=NA}

ggplot(subset(weather_L1_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_b, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ',format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
#HC station online 2019-07-03 12:00
ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST<as.POSIXct('2019-07-03 12:00', tz='Etc/GMT+5'))
for (i in c(allvars)) {weather_L1[ix,i]=NA}

ggplot(subset(weather_L1_vert, subset=(location == location_c & datetime_noDST>= as.POSIXct(start_date_c, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_c, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_c, ' ',format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

#SF barometer fixed 2019-07-09 04:00
ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST<as.POSIXct('2019-07-09 04:00', tz='Etc/GMT+5'))
for (i in 'pressure_hpa') {weather_L1[ix,i]=NA}

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#And then check the work to make sure the correct point was recoded:
ggplot(subset(weather_L1_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_a, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_a, ' ',format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

ggplot(subset(weather_L1_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_b, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

ggplot(subset(weather_L1_vert, subset=(location == location_c & datetime_noDST>= as.POSIXct(start_date_c, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_c, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_c, ' ',format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

#Observations:
#It appears that the SF UV and Radiation sensors are consistently blocked from full view of sun until afternoon, so that data should be flagged. It also appears that the SF pressure is significantly lower than HC or GM. Will have to have LSPA folks look into that.

### Q4 2019 ----
# HC some errant data around Oct 31-Nov 1
# intermittend data reporting in UV and SR at SF beginning Nov 29 through end of this download period; needs flag
# SF errant temp and humidity Dec30 - 31

#set time period of interest:
location_a = 'HC'
start_date_a = '2019-10-31'
end_date_a = '2019-11-02'

ggplot(subset(weather_L1_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_a, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

# HC all data from 2019-10-31 through 2019-11-01 17:00; end of intermittent data reporting
ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST>=as.POSIXct('2019-10-31', tz='Etc/GMT+5') & 
           weather_L1$datetime_noDST<as.POSIXct('2019-11-01 17:00', tz='Etc/GMT+5'))
for (i in c(allvars)) {weather_L1[ix,i]=NA}

location_b = 'SF'
start_date_b = '2019-12-25'
end_date_b = '2020-01-01'

ggplot(subset(weather_L1_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_b, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

#Errant data observations:
# SF UV and SR should be flagged as inconsistently reporting 'i' from 2019-11-29 until end of period
# *SF humidity wrong Dec 30-31

ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2019-12-30 22:00', tz='Etc/GMT+5') & 
           weather_L1$datetime_noDST<as.POSIXct('2019-12-31 12:00', tz='Etc/GMT+5'))
for (i in c(tempvars)) {weather_L1[ix,i]=NA}

ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2019-11-29', tz='Etc/GMT+5') & 
           weather_L1$datetime_noDST<as.POSIXct('2020-01-01', tz='Etc/GMT+5'))
weather_L1$UV_flag = NA_character_
weather_L1$SR_flag = NA_character_
for (i in c('UV_flag', 'SR_flag')) {weather_L1[ix,i]='i'}

 
#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

# And the check work:

ggplot(subset(weather_L1_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_a, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

ggplot(subset(weather_L1_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_b, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

### Q1 2020 ----
# Data observations:
# * UV and SR data at SF intermittent until Jan 10th - flat-lined on the 13th, need to see if there is any positive value there, or may need to flag as suspect.
#set time period of interest:
location_a = 'SF'
start_date_a = '2020-01-10'
end_date_a = '2020-01-11'

location_b = 'SF'
start_date_b = '2020-01-13'
end_date_b = '2020-01-14'

ggplot(subset(weather_L1_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_a, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
# * SF UV and SR should be flagged as inconsistently reporting 'i' from beginning of period until 2020-01-10 08:00
ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-01', tz='Etc/GMT+5') & weather_L1$datetime_noDST<as.POSIXct('2020-01-11', tz='Etc/GMT+5'))
for (i in c('UV_flag', 'SR_flag')) {weather_L1[ix,i]='i'}

ggplot(subset(weather_L1_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'Etc/GMT+5') & datetime_noDST < as.POSIXct(end_date_b, tz= 'Etc/GMT+5'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')

# * SF UV and SR should be flagged as suspect 's' on 2020-01-13, 16, 20, 23 due to no values recorded above 0 or strange looking irradiance curves
ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-13', tz='Etc/GMT+5') & 
           weather_L1$datetime_noDST<as.POSIXct('2020-01-14', tz='Etc/GMT+5') |
           weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-16', tz='Etc/GMT+5') & 
           weather_L1$datetime_noDST<as.POSIXct('2020-01-17', tz='Etc/GMT+5') |
           weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-20', tz='Etc/GMT+5') & 
           weather_L1$datetime_noDST<as.POSIXct('2020-01-21', tz='Etc/GMT+5') |
           weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-23', tz='Etc/GMT+5') & 
           weather_L1$datetime_noDST<as.POSIXct('2020-01-24', tz='Etc/GMT+5'))
for (i in c('UV_flag', 'SR_flag')) {weather_L1[ix,i]='s'}

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

# Plot to check, data should only show up if flagged:
weather_L1 %>%
  filter(location == location_a &
         datetime_noDST >= as.POSIXct('2020-01-05', tz='Etc/GMT+5') &
         datetime_noDST < as.POSIXct('2020-01-30', tz='Etc/GMT+5')) %>%
    ggplot(., aes(x = datetime_noDST, y = solarradiation_wpm2, shape = SR_flag)) +
  geom_point() +
  labs(title = 'SF SR flagged data, early January') +
  theme_bw()
  
### Q2 2020 ----
# data generally look good 

### Q3 2020 ----

# Observations:
# 
# * SF offline beginning May 1
# * precip data at HC need to be recoded after sensors go offline May 24 through end of period, since it 
#appears that precip is not recording any rain events seen at other stations 
# * HC barometer issue Jul 19-20
# * HC barometer issue ~Aug 15

# 2020-05-24
location_a = 'HC'
start_date_a = '2020-05-23'
end_date_a = '2020-07-21'
data <- subset(weather_L1_vert, subset=(location == location_a & 
                                          datetime_noDST>= as.POSIXct(start_date_a, tz= 'Etc/GMT+5') & 
                                          datetime_noDST < as.POSIXct(end_date_a, tz= 'Etc/GMT+5') &
                                          !is.na(value)))
ggplot(data,  
       aes(x=datetime_noDST, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour')

ix = which(weather_L1$location == 'HC' & 
             weather_L1$datetime_noDST>=as.POSIXct('2020-05-24 10:00', tz='Etc/GMT+5') & 
             weather_L1$datetime_noDST<as.POSIXct('2020-07-20 16:00', tz='Etc/GMT+5'))
for (i in allvars) {weather_L1[ix,i]=NA}

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#plot to check
data <- subset(weather_L1_vert, subset=(location == location_a & 
                                          datetime_noDST>= as.POSIXct(start_date_a, tz= 'Etc/GMT+5') & 
                                          datetime_noDST < as.POSIXct(end_date_a, tz= 'Etc/GMT+5')))

ggplot(data,  
       aes(x=datetime_noDST, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour')

# 2020-07-19/20
location_b = 'HC'
start_date_b = '2020-08-15'
end_date_b = '2020-08-17'

data <- subset(weather_L1_vert, subset=(location == location_b & 
                                          datetime_noDST>= as.POSIXct(start_date_b, tz= 'Etc/GMT+5') & 
                                          datetime_noDST < as.POSIXct(end_date_b, tz= 'Etc/GMT+5') &
                                          !is.na(value)))
ggplot(data,  
       aes(x=datetime_noDST, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour')

# where baro is < 1006, recode all data. Looks like there are some artifacts here that create dupclicates
ix = which(weather_L1$location == 'HC' & 
             weather_L1$pressure_hpa < 1006 &
             weather_L1$datetime_noDST>=as.POSIXct('2020-08-15', tz='Etc/GMT+5') & 
             weather_L1$datetime_noDST<as.POSIXct('2020-08-17', tz='Etc/GMT+5'))
for (i in allvars) {weather_L1[ix,i]=NA}

#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#check work
data <- subset(weather_L1_vert, subset=(location == location_b & 
                                          datetime_noDST>= as.POSIXct(start_date_b, tz= 'Etc/GMT+5') & 
                                          datetime_noDST < as.POSIXct(end_date_b, tz= 'Etc/GMT+5') &
                                          !is.na(value)))
ggplot(data,  
       aes(x=datetime_noDST, y=value)) +
  geom_point() +
  facet_grid(variable ~ ., scales = 'free_y') +
  labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(minor_breaks = '1 hour')

### Q4 2020----

# # recode and flag errant precip measure at HC because of pine needle

weather_L1 <- weather_L1 %>%
  mutate_at(vars(all_of(precipvars)),
            ~(case_when(location == 'HC' &
                              datetime_noDST == as.POSIXct('2020-12-01 14:00', tz= 'Etc/GMT+5') ~ NA_real_,
                            TRUE ~ .))) %>% 
  mutate(rain_flag = case_when(location == 'HC' & datetime_noDST == as.POSIXct('2020-12-01 14:00', tz = 'Etc/GMT+5') ~ 'pine needle removed from rain gauge; data prior may not be precise due to clogged gauge',
                                         TRUE ~ ''))


## Overall data flags ----
 
# Now to add flags to the data that are consistently wonky due to weather station placement:
# 
# * SF uv/rad flagged as obscured in AM hours
# * SF bar pressure flagged because it's consistently lower than other 2 locations

weather_L1 <- weather_L1 %>%
  mutate(across(c("UV_flag", "SR_flag"),
                ~ (case_when(location == 'SF' & is.na(.) ~ 'oAM',
                           location == 'SF' & !is.na(.) ~ paste('oAM', ., sep = '; '),
                           TRUE ~ .)))) %>%
  mutate(BP_flag = case_when(location == 'SF' ~ 'l',
                             TRUE ~ ''))


## Print 2-week plots to check all work ----
#create a vertical dataset for ggplot
weather_L1_vert <- weather_L1 %>% 
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
               names_to = 'variable', values_to = 'value') 

#plot all L0.5 plots and save to appropriate figdir
for (i in 1:(nrow(biweekly_2020)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>biweekly_2020$date[i] &
                                                     datetime_noDST < biweekly_2020$date[i+1])), aes(x=datetime_noDST, y=value)) +
    geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Clean Met Data ', biweekly_2020$date[i], ' through ', biweekly_2020$date[i+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L1_plots_', biweekly_2020$date[i], '-', biweekly_2020$date[i+1], '.jpeg'),
         width = 10, height = 8, units = 'in')
}

#plot all together in paneled, too
for (i in 1:(nrow(biweekly_2020)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>biweekly_2020$date[i] &
                                                     datetime_noDST < biweekly_2020$date[i+1])), aes(x=datetime_noDST, y=value)) +
    geom_point(aes(color = location)) +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Clean Met Data ', biweekly_2020$date[i], ' through ', biweekly_2020$date[i+1]),
         x='date',
         y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L1_plots_nofacet_', biweekly_2020$date[i], '-', biweekly_2020$date[i+1], '.jpeg'),
         width = 6, height = 8, units = 'in')
}

# Export QAQC Data

weather_L1 %>%
  mutate(instrument_datetime = as.character(instrument_datetime),
         datetime_noDST = as.character(datetime_noDST)) %>%
  write_csv(paste0(datadir, 'L1 data/davis_weather_data_', start_date, '_', end_date, '_L1_v', L1_versiondate, '.csv'))
