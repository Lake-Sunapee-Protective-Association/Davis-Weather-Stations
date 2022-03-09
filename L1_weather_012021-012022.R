#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.0.5                                          */
#* R Studio:      1.4.1103                                       */
#* PROJECT:       lake sunapee davis weather station             */
#* PURPOSE:       
#* DATE CREATED:  09March2022                                    */
#*****************************************************************/

library(tidyverse)
library(lubridate)
library(ggthemes)
detach("package:plyr", unload = TRUE) 

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
  ggsave(paste0(metfigdirL05, '2wk_L0.5_plots_', biweekly_2021$date[i], '-', biweekly_2021$date[i+1], '.jpeg'))
}

## issue dates ####
#time shift at HC Sept 25, Dec 11
#precip at HC plugged/unplugged Aug 2
#precip unclogged at HC Dec 14
