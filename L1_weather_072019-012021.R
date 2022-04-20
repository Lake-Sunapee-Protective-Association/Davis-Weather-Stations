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

# Davis Weather Station QAQC ----

#This Markdown file tracks the QAQC of the Sunapee Davis Weather Stations July 2019-December 2021, which are owned/operated by the #LSPA and QAQC'd by B. Steele of the Weathers' Lab, Cary Institute of Ecosystem Studies, Millbrook, NY. Data are QAQC'd in this file #to remove obviously errant data. 

#read in weather data
weather_L0 <- read_csv(paste0(datadir, 'L0 data/davis_weather_data_07-2019_', L0_enddate, '_L0.csv'))

#create a new dataframe for data cleaning
weather_L1 <- weather_L0 

#create a vertical dataset for ggplot
weather_L0_vert <- weather_L0 %>% 
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

#set time period of interest:
start_date = '2019-07-01'
end_date = '2021-01-01'

#create a list of weeks during time period of interest, chose first and 3rd
week_2019Q3 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  mutate(month = format(date, '%m')) %>% 
  group_by(month) %>%
  slice(., c(1,3)) %>% 
  ungroup() %>% 
  add_row(date = as.Date(end_date), month = '10') #change this to the inclusive end date so that for-loop properly displays final graph

#plot all L0.5 plots and save to appropriate figdir
for (i in 1:(nrow(week_2019Q3)-1)){
  gg_met <- ggplot(subset(weather_L0_vert, subset=(datetime_noDST>week_2019Q3$date[i] & datetime_noDST < week_2019Q3$date[i+1])), aes(x=datetime_noDST, y=value)) + 
   geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', week_2019Q3$date[i], ' through ', week_2019Q3$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_', week_2019Q3$date[i], '-', week_2019Q3$date[i+1], '.jpeg'))
}
 

#Observed errant data:

# At GM until Jul 10
# at HC until Jul 3
# at SF barometric pressure until Jul 9

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

ggplot(subset(weather_L0_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_a, '-', end_date_a, '.jpeg'))


ggplot(subset(weather_L0_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ',format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_b, '-', end_date_b, '.jpeg'))

ggplot(subset(weather_L0_vert, subset=(location == location_c & datetime_noDST>= as.POSIXct(start_date_c, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_c, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_c, ' ',format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_c, '-', end_date_c, '.jpeg'))


#GM station online 2019-07-10 9:30
#HC station online 2019-07-03 12:00
#SF barometer fixed 2019-07-09 05:00

ix=which(weather_L1$location == 'GM' & weather_L1$datetime_noDST<as.POSIXct('2019-07-10 9:30', tz='UTC'))
for (i in c(allvars)) {weather_L1[ix,i]=NA}

ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST<as.POSIXct('2019-07-03 12:00', tz='UTC'))
for (i in c(allvars)) {weather_L1[ix,i]=NA}

ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST<as.POSIXct('2019-07-09 05:00', tz='UTC'))
for (i in 'pressure_hpa') {weather_L1[ix,i]=NA}

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

#And then check the work to make sure the correct point was recoded:
ggplot(subset(weather_L1_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_a, ' ',format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_a, '-', end_date_a, '.jpeg'))


ggplot(subset(weather_L1_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_b, '-', end_date_b, '.jpeg'))


ggplot(subset(weather_L1_vert, subset=(location == location_c & datetime_noDST>= as.POSIXct(start_date_c, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_c, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_c, ' ',format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_c, '-', end_date_c, '.jpeg'))



#Observations:
#It appears that the SF UV and Radiation sensors are consistently blocked from full view of sun until afternoon, so that data should be flagged. It also appears that the SF pressure is significantly lower than HC or GM. Will have to have LSPA folks look into that. 



#Errant data observations:
# HC offline thrrough Oct 31, some errant data around Oct 31-Nov 1
# intermittend data reporting in UV and SR at SF beginning Nov 29 through end of this download period; needs flag
# SF errant temp and humidity Dec30 - 31 

#set time period of interest:
location_a = 'HC'
start_date_a = '2019-10-31'
end_date_a = '2019-11-02'

location_b = 'SF'
start_date_b = '2019-12-30'
end_date_b = '2020-01-01'

ggplot(subset(weather_L0_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_a, '-', end_date_a, '.jpeg'))


ggplot(subset(weather_L0_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_b, '-', end_date_b, '.jpeg'))

#Errant data observations:
# HC all data from 2019-10-31 through 2019-11-01 17:00; end of intermittent data reporting
# SF UV and SR should be flagged as inconsistently reporting 'i' from 2019-11-29 until end of period

ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST>=as.POSIXct('2019-10-31', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2019-11-01 17:00', tz='UTC'))
for (i in c(allvars)) {weather_L1[ix,i]=NA}

ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2019-12-30 22:00', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2019-12-31 12:00', tz='UTC'))
for (i in c(tempvars)) {weather_L1[ix,i]=NA}

ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2019-11-29', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-01-01', tz='UTC'))
weather_L1$UV_flag = NA_character_
weather_L1$SR_flag = NA_character_
for (i in c('UV_flag', 'SR_flag')) {weather_L1[ix,i]='i'}

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
```

And the check work:
```{r, L1 Oct 2020}
#set time period of interest:
ggplot(subset(weather_L1_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_a, '-', end_date_a, '.jpeg'))

ggplot(subset(weather_L1_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_b, '-', end_date_b, '.jpeg'))

```

Look for outliers by plotting all sites on same graph over 3-month period:
```{r L1 2019Q4 plots sites on same graph}
#plot all L1 plots and save to appropriate figdir
for (i in 1:(nrow(week_2019Q4)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>week_2019Q4$date[i] & datetime_noDST < week_2019Q4$date[i+1])), aes(x=datetime_noDST, y=value, color = location)) + 
   geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', week_2019Q4$date[i], ' through ', week_2019Q4$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_overlay_', week_2019Q4$date[i], '-', week_2019Q4$date[i+1], '.jpeg'))
}
```

***

## 2020-04 download 

```{r Jan-Apr 2020 download L0}
#set time period of interest:
start_date = '2020-01-01'
end_date = '2020-04-01'

#create a list of weeks during time period of interest, chose first and 3rd
week_2020Q1 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  mutate(month = format(date, '%m')) %>% 
  group_by(month) %>%
  slice(., c(1,3)) %>% 
  ungroup()  

#plot all L0.5 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q1)-1)){
  gg_met <- ggplot(subset(weather_L0_vert, subset=(datetime_noDST>week_2020Q1$date[i] & datetime_noDST < week_2020Q1$date[i+1])), aes(x=datetime_noDST, y=value)) + 
   geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', week_2020Q1$date[i], ' through ', week_2020Q1$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day')
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_', week_2020Q1$date[i], '-', week_2020Q1$date[i+1], '.jpeg'))
}
```

Data observations:
* UV and SR data at SF intermittent until Jan 10th - flat-lined on the 13th, need to see if there is any positive value there, or may need to flag as suspect.

```{r 2020Q1 L0 errant data}
#set time period of interest:
location_a = 'SF'
start_date_a = '2020-01-10'
end_date_a = '2020-01-11'

location_b = 'SF'
start_date_b = '2020-01-13'
end_date_b = '2020-01-14'

ggplot(subset(weather_L0_vert, subset=(location == location_a & datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_a, '-', end_date_a, '.jpeg'))


ggplot(subset(weather_L0_vert, subset=(location == location_b & datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour')
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_plots_', start_date_b, '-', end_date_b, '.jpeg'))
```

Obseravations:
* SF UV and SR should be flagged as inconsistently reporting 'i' from beginning of period until 2020-01-10 08:00
* SF UV and SR should be flagged as suspect 's' on 2020-01-13 due to no values recorded above 0.

```{r flag 2020Q1 data}
ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-01', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-01-11', tz='UTC'))
for (i in c('UV_flag', 'SR_flag')) {weather_L1[ix,i]='i'}

ix=which(weather_L1$location == 'SF' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-13', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-01-14', tz='UTC'))
for (i in c('UV_flag', 'SR_flag')) {weather_L1[ix,i]='s'}
```

```{r, echo=F}
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
```

Plot to check, data should only show up if flagged:
```{r 2020Q1 plot flag check}
weather_L1 %>% 
  filter(location == location_a &
         datetime_noDST >= as.POSIXct('2020-01-01', tz='UTC') &
         datetime_noDST < as.POSIXct('2020-01-20', tz='UTC')) %>% 
    ggplot(., aes(x = datetime_noDST, y = solarradiation_wpm2, shape = SR_flag)) +
  geom_point() +
  labs(title = 'SF SR flagged data, early January') +
  theme_bw()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_a, '-', end_date_a, '.jpeg'))


weather_L1 %>% 
  filter(location == location_a &
         datetime_noDST >= as.POSIXct('2020-01-01', tz='UTC') &
         datetime_noDST < as.POSIXct('2020-01-20', tz='UTC')) %>% 
    ggplot(., aes(x = datetime_noDST, y = uvindex, shape = UV_flag)) +
  geom_point() +
  labs(title = 'SF UV flagged data, early January') +
  theme_bw()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_b, '-', end_date_b, '.jpeg'))

```

Look for outliers by plotting all sites on same graph over 3-month period:
```{r L1 2020Q1 plots sites on same graph}
#plot all L1 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q1)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>week_2020Q1$date[i] & datetime_noDST < week_2020Q1$date[i+1])), aes(x=datetime_noDST, y=value, color = location)) + 
   geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', week_2020Q1$date[i], ' through ', week_2020Q1$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_overlay_', week_2020Q1$date[i], '-', week_2020Q1$date[i+1], '.jpeg'))
}
```

Data observations:
* GM temp/humidity errant Mar 8 & 9
* it appears that HC is an hour ahead? Offset in temp/pressure/humidity starts in mid January and gets worse (?) over time

```{r flagging temp and hum at GM Mar 8/9}
#set time period of interest:
location_a = 'HC'
start_date_a = '2020-01-14'
end_date_a = '2020-01-15'

location_b = 'GM'
start_date_b = '2020-03-08'
end_date_b = '2020-03-10'

ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_a, '-', end_date_a, '.jpeg'))

ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_b, '-', end_date_b, '.jpeg'))

```

```{r 2020Q1 flag pt 2}
weather_L1$temp_flag = NA_character_

# flag those times for suspicious data
ix=which(weather_L1$location == 'GM' & weather_L1$datetime_noDST>=as.POSIXct('2020-03-08 19:00', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-03-09 10:00', tz='UTC'))
for (i in c('temp_flag')) {weather_L1[ix,i]='s'}
```

```{r, echo=F}
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
```

Plot to check, data should only show up if flagged:
```{r 2020Q1 pt 2 plot check}
weather_L1 %>% 
  filter(location == location_a &
         datetime_noDST >= as.POSIXct(start_date_a, tz='UTC') &
         datetime_noDST < as.POSIXct(end_date_a, tz='UTC')) %>% 
    ggplot(., aes(x = datetime_noDST, y = temp_c, shape = temp_flag)) +
  geom_point() +
  labs(title = 'GM temp flagged data, early March') +
  theme_bw()
```

Dealing with HC offset:
* need to find time of pressure low on Feb 7, Feb 27, Mar 13, Mar 20. Is offset the same or varying? If same, can move data if varying, need to recode HC data

```{r inspection of timing offset}
date1a = '2020-01-08'
date1b = '2020-01-09'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date1a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date1b, tz='UTC')) %>% 
  arrange(temp_c) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            temp_c = first(temp_c))

date2a = '2020-01-18'
date2b = '2020-01-19'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date2a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date2b, tz='UTC')) %>% 
  arrange(-temp_c) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            temp_c = first(temp_c))

date3a = '2020-01-30'
date3b = '2020-01-31'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date3a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date3b, tz='UTC')) %>% 
  arrange(-temp_c) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            temp_c = first(temp_c))

date4a = '2020-02-01'
date4b = '2020-02-02'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date3a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date3b, tz='UTC')) %>% 
  arrange(-temp_c) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            temp_c = first(temp_c))

date5a = '2020-02-07'
date5b = '2020-02-08'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date5a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date5b, tz='UTC')) %>% 
  arrange(pressure_hpa) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            pressure = first(pressure_hpa))

date6a = '2020-02-21'
date6b = '2020-02-22'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date6a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date6b, tz='UTC')) %>% 
  arrange(temp_c) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            temp_c = first(temp_c))

date7a = '2020-02-27'
date7b = '2020-02-28'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date7a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date7b, tz='UTC')) %>% 
  arrange(pressure_hpa) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            pressure = first(pressure_hpa))

date8a = '2020-03-13'
date8b = '2020-03-14'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date8a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date8b, tz='UTC')) %>% 
  arrange(pressure_hpa) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            pressure = first(pressure_hpa))

date9a = '2020-03-20'
date9b = '2020-03-21'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date9a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date9b, tz='UTC')) %>% 
  arrange(pressure_hpa) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            pressure = first(pressure_hpa))

```

Herrick Cove data is consistently reporting 3 hours prior to the other stations by end of this download period. Unsure of when this starts - not easily discernible from the data and no noted maintenance reported from LSPA. Flagging this data with 't' to note that the time is not precise.

```{r flag HC data for time issue}
ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST>=as.POSIXct('2020-01-14', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-04-01', tz='UTC'))
weather_L1$time_flag = NA_character_
weather_L1$time_flag[ix]='t'

```

 
***

## 2020-07 download 

```{r 2020Q2 download L0}
#set time period of interest:
start_date = '2020-04-01'
end_date = '2020-07-01'

#create a list of weeks during time period of interest, chose first and 3rd
week_2020Q2 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  mutate(month = format(date, '%m')) %>% 
  group_by(month) %>%
  slice(., c(1,3)) %>% 
  ungroup() 

#plot all L0.5 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q2)-1)){
  gg_met <- ggplot(subset(weather_L0_vert, subset=(datetime_noDST>week_2020Q2$date[i] & datetime_noDST < week_2020Q2$date[i+1])), aes(x=datetime_noDST, y=value)) + 
   geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', week_2020Q2$date[i], ' through ', week_2020Q2$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day')
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_', week_2020Q2$date[i], '-', week_2020Q2$date[i+1], '.jpeg'))

}
```

Observations:

* SF offline beginning May 1
* precip data at HC need to be recoded after sensors go offline May 24 through end of period, since it appears that precip is not recording any rain events seen at other stations 
* jump in HC pressure Jun 24, looks like data incorrect after that jump

Look for outliers by plotting all sites on same graph over 3-month period:
```{r L1 2020Q2 plots sites on same graph}
#plot all L1 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q2)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>week_2020Q2$date[i] & datetime_noDST < week_2020Q2$date[i+1])), aes(x=datetime_noDST, y=value, color = location)) + 
   geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', week_2020Q2$date[i], ' through ', week_2020Q2$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_overlay_', week_2020Q2$date[i], '-', week_2020Q2$date[i+1], '.jpeg'))

}
```
Observations:
* HC timing still off; HC completely wrong beginning Jun 24

Document offest at HC:
```{r document offset of timing at HC}
date1a = '2020-04-13'
date1b = '2020-04-14'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date1a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date1b, tz='UTC')) %>% 
  arrange(pressure_hpa) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            pressure = first(pressure_hpa))

date2a = '2020-04-21'
date2b = '2020-04-22'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date2a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date2b, tz='UTC')) %>% 
  arrange(pressure_hpa) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            pressure = first(pressure_hpa))

date3a = '2020-05-01'
date3b = '2020-05-02'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date3a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date3b, tz='UTC')) %>% 
  arrange(-temp_c) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            temp_c = first(temp_c))

date4a = '2020-05-18'
date4b = '2020-05-19'
weather_L1 %>% 
  filter(datetime_noDST >= as.POSIXct(date4a, tz= 'UTC') & 
           datetime_noDST < as.POSIXct(date4b, tz='UTC')) %>% 
  arrange(-temp_c) %>% 
  group_by(location) %>% 
  summarize(datetime_noDST = first(datetime_noDST),
            temp_c = first(temp_c))

```

And then flag time as 't' until Jun 24 like in previous download. First plot to see when that *very* errant time begins:
```{r flag HC 2020Q2}
location_a = 'HC'
start_date_a = '2020-06-24'
end_date_a = '2020-06-25'

ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_a, '-', end_date_a, '.jpeg'))
```

Data jump occurs Jun 24 at 12:00 (2020-06-24 12:00), as seen in next download, that data is actually a time issue, and can be pushed forward. This processing is done in next download section.

```{r flag HC data for time issue again}
ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST>=as.POSIXct('2020-04-01', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-06-24 12:00', tz='UTC'))
weather_L1$time_flag[ix]='t'

```



***

## 2020-10 download 

```{r 2020Q3 download L0}
#set time period of interest:
start_date = '2020-07-01'
end_date = '2020-10-01'

#create a list of weeks during time period of interest, chose first and 3rd
week_2020Q3 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  mutate(month = format(date, '%m')) %>% 
  group_by(month) %>%
  slice(., c(1,3)) %>% 
  ungroup() %>%  #change this to the inclusive end date so that for-loop properly displays final graph
  add_row(date = as.Date(end_date), month = '10')

#plot all L0.5 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q3)-1)){
  gg_met <- ggplot(subset(weather_L0_vert, subset=(datetime_noDST>week_2020Q3$date[i] & datetime_noDST < week_2020Q3$date[i+1])), aes(x=datetime_noDST, y=value)) + 
   geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', week_2020Q3$date[i], ' through ', week_2020Q3$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day')
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_', week_2020Q3$date[i], '-', week_2020Q3$date[i+1], '.jpeg'))
}
```

Obesrvations: 

* Data need to be recoded at HC prior to Jul 09 except bar pressure
* Data at HC from Jul 09-Jul 24 need to be moved to end August 4

```{r 2020Q3plot days to see timing for QAQC}
location_a = 'HC'
start_date_a = '2020-07-09'
end_date_a = '2020-07-10'

location_b = 'HC'
start_date_b = '2020-07-24'
end_date_b = '2020-07-25'

location_c = 'HC'
start_date_c = '2020-08-04'
end_date_c = '2020-08-05'


ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_a, '-', end_date_a, '.jpeg'))

ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_b, '-', end_date_b, '.jpeg'))

ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_c, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_c, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_c, ' ', format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_c, '-', end_date_c, '.jpeg'))

```

Jul 24 07:00  is actually Aug 4 17:00- need to move data 11 days 10 hours ahead

Recode, mutate and flag data as needed:
```{r QAQC for 2020Q3}
# recode random data showing up in HC feed
weather_L1 <- weather_L1 %>% 
  mutate_at(vars(all_of(allnumvars)),
            ~(case_when(location == 'HC' & 
                              datetime_noDST >= as.POSIXct('2020-06-24 12:00', tz= 'UTC') & 
                              datetime_noDST < as.POSIXct('2020-07-09 7:00', tz= 'UTC') ~ NA_real_,
                            TRUE ~ .))) %>% 
  mutate_at(vars(all_of(allcharvars)),
            ~(case_when(location == 'HC' & 
                              datetime_noDST >= as.POSIXct('2020-06-24 12:00', tz= 'UTC') & 
                              datetime_noDST < as.POSIXct('2020-07-09 7:00', tz= 'UTC') ~ NA_character_,
                            TRUE ~ .)))

#pull out datetime that needs to be adjusted
HC_timechange <- weather_L1 %>% 
  filter(location == 'HC' &
           datetime_noDST >= as.POSIXct('2020-07-09 7:00', tz='UTC') &
           datetime_noDST < as.POSIXct('2020-08-04 16:30', tz='UTC'))
#create new column with adjusted datetime, dplyr::rename column
HC_timechange <- HC_timechange %>% 
  mutate(corr_datetime_noDST = as.POSIXct(datetime_noDST + days(11) + hours(10))) %>% 
  filter(corr_datetime_noDST < as.POSIXct('2020-08-04 16:30', tz='UTC')) %>% 
  select(-datetime_noDST) %>% 
  dplyr::rename(datetime_noDST = corr_datetime_noDST)
#round times to nearest 30 minutes, there are some wonky time readings in this dataset
HC_timechange <- HC_timechange %>% 
  mutate(datetime_noDST = round_date(datetime_noDST, unit = '30 min'))
#create a list of datetimes for adding back into the full datset
HCdatetimelist = seq(from = as.POSIXct('2020-07-09 7:00', tz = 'UTC'), 
                          to = as.POSIXct('2020-08-04 16:00', tz = 'UTC'),
                          by = '30 min')
#create a data frame from the list and add location/source info
HCdatetimelist <- as.data.frame(HCdatetimelist) %>% 
  dplyr::rename(datetime_noDST = HCdatetimelist) %>% 
  mutate(location = 'HC',
         source = 'Herrick_Cove_7-1-20_12-00_AM_3_Month_1602083357_v2.csv')
#join back together and add a flag of 'm' for modified time
HC_timechange <- left_join(HCdatetimelist, HC_timechange) %>% 
  mutate(time_flag = 'm: +11d 10h')

#create a weather_L1 dataset without the above data in it
allotherdata <- weather_L1 %>% 
  filter(!(location == 'HC' &
           datetime_noDST >= as.POSIXct('2020-07-09 7:00', tz='UTC') &
           datetime_noDST < as.POSIXct('2020-08-04 16:30', tz='UTC')))

weather_L1 <- full_join(allotherdata, HC_timechange) %>% 
  arrange(datetime_noDST, location)

#note, this will create 'more' observations in the dataset than before, that's because there are missing data in the HC stream for this time period
```

```{r, echo=F}
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
```

Plot to see if the change lines up:
```{r 2020Q3 plot days for QAQC check}
ggplot(subset(weather_L1_vert, subset=(datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_a, '-', end_date_a, '.jpeg'))

ggplot(subset(weather_L1_vert, subset=(datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_b, '-', end_date_b, '.jpeg'))

ggplot(subset(weather_L1_vert, subset=(datetime_noDST>= as.POSIXct(start_date_c, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_c, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_c, ' ', format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_c, '-', end_date_c, '.jpeg'))

```

Look for outliers by plotting all sites on same graph over 3-month period:
```{r L1 2020Q3 plots sites on same graph}
#plot all L1 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q3)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>week_2020Q3$date[i] & datetime_noDST < week_2020Q3$date[i+1])), aes(x=datetime_noDST, y=value, color = location)) + 
   geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', week_2020Q3$date[i], ' through ', week_2020Q3$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_overlay_', week_2020Q3$date[i], '-', week_2020Q3$date[i+1], '.jpeg'))
}
```

It does appear that HC is a little 'off' beggining Sept 6. Flagging with 't'.

```{r flagging 2020Q3 HC for discrepencies}
ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST>=as.POSIXct('2020-09-06', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-10-01', tz='UTC'))
weather_L1$time_flag[ix]='t'
```

***

## 2021-01 download 

```{r 2020Q4 download L0}
#set time period of interest:
start_date = '2020-10-01'
end_date = '2021-01-01'

#create a list of weeks during time period of interest, chose first and 3rd
week_2020Q4 <- seq(as.Date(start_date), as.Date(end_date), 'week')  %>% 
  as.data.frame(.) %>% 
  dplyr::rename(date = '.') %>% 
  mutate(month = format(date, '%m')) %>% 
  group_by(month) %>%
  slice(., c(1,3)) %>% 
  ungroup() %>%  #change this to the inclusive end date so that for-loop properly displays final graph
  add_row(date = as.Date(end_date), month = '01')

#plot all L0.5 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q4)-1)){
  gg_met <- ggplot(subset(weather_L0_vert, subset=(datetime_noDST>week_2020Q4$date[i] & datetime_noDST < week_2020Q4$date[i+1])), aes(x=datetime_noDST, y=value)) + 
   geom_point() +
    facet_grid(variable ~ location, scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', week_2020Q4$date[i], ' through ', week_2020Q4$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day')
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_', week_2020Q4$date[i], '-', week_2020Q4$date[i+1], '.jpeg'))
}
```

Obesrvations: 

* need to look more closely at Oct 11-12, odd blip in temp at GM
* HC gap in data on Dec 5-6; step in data in early hours

Notes from LSPA:

* Dec 1 rain at HC there was a pine needle in sensor
* Dec 5, HC stopped recording, may have timing issue

```{r 2020Q4plot days to see timing for QAQC}
location_a = 'GM'
start_date_a = '2020-10-12'
end_date_a = '2020-10-14'

location_b = 'HC'
start_date_b = '2020-12-01'
end_date_b = '2020-12-02'

location_c = 'HC'
start_date_c = '2020-12-05'
end_date_c = '2020-12-07'


ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_a, '-', end_date_a, '.jpeg'))

ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_b, '-', end_date_b, '.jpeg'))

ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_c, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_c, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_c, ' ', format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
  ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_c, '-', end_date_c, '.jpeg'))

```

Details:

* Oct 11 at GM looks fine, probably due to wind direction change.
* Dec 1, rain at HC should be recoded 2020-12-01 14:00 to NA
* Dec 5-6 at HC, data needs to be moved from 2020-12-05 21:30 to 2020-12-06 9:00 such that the data end at 22:00

```{r QAQC for 2020Q4}
# recode errant precip measure at HC because of pine needle
weather_L1 <- weather_L1 %>% 
  mutate_at(vars(precipvars), 
            funs(case_when(location == 'HC' & 
                              datetime_noDST == as.POSIXct('2020-12-01 14:00', tz= 'UTC') ~ NA_real_,
                            TRUE ~ .)))

#pull out datetime that needs to be adjusted
HC_timechange <- weather_L1 %>% 
  filter(location == 'HC' &
           datetime_noDST >= as.POSIXct('2020-12-05 21:30', tz='UTC') &
           datetime_noDST < as.POSIXct('2020-12-06 9:00', tz='UTC'))
#create new column with adjusted datetime, dplyr::rename column
HC_timechange <- HC_timechange %>% 
  mutate(corr_datetime_noDST = as.POSIXct(datetime_noDST + hours(13))) %>% 
  filter(corr_datetime_noDST < as.POSIXct('2020-12-06 22:00', tz='UTC')) %>% 
  select(-datetime_noDST) %>% 
  dplyr::rename(datetime_noDST = corr_datetime_noDST)

#create a list of datetimes for adding back into the full datset
HCdatetimelist = seq(from = as.POSIXct('2020-12-05 21:30', tz = 'UTC'), 
                          to = as.POSIXct('2020-12-06 21:30', tz = 'UTC'),
                          by = '30 min')
#create a data frame from the list and add location/source info
HCdatetimelist <- as.data.frame(HCdatetimelist) %>% 
  dplyr::rename(datetime_noDST = HCdatetimelist) %>% 
  mutate(location = 'HC',
         source = 'Herrick_Cove_10-1-20_12-00_AM_3_Month_1609777537_v2.csv')
#join back together and add a flag of 'm' for modified time
HC_timechange <- left_join(HCdatetimelist, HC_timechange) %>% 
  mutate(time_flag = 'm: +13h')

#create a weather_L1 dataset without the above data in it
allotherdata <- weather_L1 %>% 
  filter(!(location == 'HC' &
           datetime_noDST >= as.POSIXct('2020-12-05 21:30', tz='UTC') &
           datetime_noDST < as.POSIXct('2020-12-06 22:00', tz='UTC')))

weather_L1 <- full_join(allotherdata, HC_timechange) %>% 
  arrange(datetime_noDST, location)

#note, this will create 'more' observations in the dataset than before, that's because there are missing data in the HC stream for this time period
```

```{r, echo=F}
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
```

Now to plot to check if QAQC was successful
```{r 2020Q4 plot days for QAQC check}
ggplot(subset(weather_L1_vert, subset=(datetime_noDST>= as.POSIXct(start_date_b, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_b, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_b, ' ', format(as.Date(start_date_b), '%b'), format(as.Date(start_date_b), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L1_flag_plots_', start_date_b, '-', end_date_b, '.jpeg'))

ggplot(subset(weather_L1_vert, subset=(datetime_noDST>= as.POSIXct(start_date_c, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_c, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', location_c, ' ', format(as.Date(start_date_c), '%b'), format(as.Date(start_date_c), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
  ggsave(paste0(metfigdirL1, 'zoom_L1_flag_plots_', start_date_c, '-', end_date_c, '.jpeg'))

```


Look for outliers by plotting all sites on same graph over 3-month period:
```{r L1 2020Q4 plots sites on same graph, warning=F, message=F}
#plot all L1 plots and save to appropriate figdir
for (i in 1:(nrow(week_2020Q4)-1)){
  gg_met <- ggplot(subset(weather_L1_vert, subset=(datetime_noDST>week_2020Q4$date[i] & datetime_noDST < week_2020Q4$date[i+1])), aes(x=datetime_noDST, y=value, color = location)) + 
   geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('QAQC Met Data ', week_2020Q4$date[i], ' through ', week_2020Q4$date[i+1]),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 day') +
    scale_color_colorblind()
  print(gg_met)
  ggsave(paste0(metfigdirL1, '2wk_L0.5_plots_overlay_', week_2020Q4$date[i], '-', week_2020Q4$date[i+1], '.jpeg'))
}

```

Notes:
* HC was a little off at end of last quarter, need to continue flag 't' until Nov 19, when there was a data gap.
```{r plot HC timing issue 20201119}
location_a = 'HC'
start_date_a = '2020-11-19'
end_date_a = '2020-11-20'


ggplot(subset(weather_L0_vert, subset=(datetime_noDST>= as.POSIXct(start_date_a, tz= 'UTC') & datetime_noDST < as.POSIXct(end_date_a, tz= 'UTC'))),  aes(x=datetime_noDST, y=value, color = location)) +
    geom_point() +
      facet_grid(variable ~ ., scales = 'free_y') +
    labs(title=paste0('Raw Met Data ', location_a, ' ', format(as.Date(start_date_a), '%b'), format(as.Date(start_date_a), '%Y')),
       x='date',
       y=NULL) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(minor_breaks = '1 hour') +
  scale_color_colorblind()
ggsave(paste0(metfigdirL1, 'zoom_L0.5_flag_plots_', start_date_a, '-', end_date_a, '.jpeg'))
```

Data gap begins at 16:00, flag data up until that point.

```{r}
ix=which(weather_L1$location == 'HC' & weather_L1$datetime_noDST>=as.POSIXct('2020-10-01', tz='UTC') & weather_L1$datetime_noDST<as.POSIXct('2020-11-19 16:00', tz='UTC'))
weather_L1$time_flag[ix]='t'
```


***

# Overall data flags

Now to add flags to the data that are consistently wonky due to weather station placement:

* SF uv/rad flagged as obscured in AM hours
* SF bar pressure flagged because it's consistently lower than other 2 locations

```{r overall data flags}
weather_L1 <- weather_L1 %>% 
  mutate_at(vars(UV_flag, SR_flag),
            funs(case_when(location == 'SF' & is.na(.) ~ 'oAM',
                           location == 'SF' & !is.na(.) ~ paste('oAM', ., sep = '; '),
                           TRUE ~ .))) %>% 
  mutate(BP_flag = case_when(location == 'SF' ~ 'l',
                             TRUE ~ NA_character_)) 
         
```

***

# Export QAQC Data

```{r}
weather_L1 %>% 
  mutate(instrument_datetime = as.character(instrument_datetime),
         datetime_noDST = as.character(datetime_noDST)) %>% 
  write_csv(paste0(datadir, 'L1 data/davis_weather_data_07-2019_', L0_enddate, '_L1_v', L1_versiondate, '.csv'))
```
