#*****************************************************************/
#*      Cary Institute of Ecosystem Studies (Millbrook, NY)      */
#*                                                               */
#* AUTHOR:        B. Steele (steeleb@caryinstitute.org)          */
#* SYSTEM:        Lenovo ThinkCentre/Dell XPS                    */
#* R Version:     4.3.2                                          */
#* R Studio:      2023.12.0-367                                  */
#* PROJECT:       lake sunapee davis weather station             */
#* PURPOSE:       quick visualize lake sunapee weather data      */
#* DATE CREATED:  09March2022                                    */
#*****************************************************************/

library(tidyverse) #2.0.0
library(ggthemes) 

# 3-month L0 visualizations ####

make_3mo_vis <- function(start_date, end_date){
  three_months_of_data <- weather_data %>% 
    subset(instrument_datetime >= start_date & instrument_datetime < end_date)
  
  dump_dir = file.path("C:/Users/steeleb/Dropbox/Lake Sunapee/monitoring/weather/LSPA_Davis_stations/graphs/L0", 
                       paste0(as.character(ymd(end_date)), ' download'))
  
  #barometer variables
  ggplot(three_months_of_data, aes(x = instrument_datetime, y = pressure_hpa, color = location)) +
    geom_point() +
    facet_grid(location ~ .) +
    labs(title = 'L0 barometric pressure') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 barometric pressure.jpg'), width = 10, height = 6, units = 'in')
  
  
  #air temperature variables
  weather_3month_airtemp <- three_months_of_data%>% 
    select(instrument_datetime, location, temp_c, hightemp_c, lowtemp_c, windchill_c) %>% 
    gather(variable, temp_deg_c, -instrument_datetime, - location)
  ggplot(weather_3month_airtemp, aes(x = instrument_datetime, y = temp_deg_c, color = location)) +
    geom_point() +
    facet_grid(variable ~ .) +
    labs(title = 'L0 air temperature (deg C)') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 air temperature.jpg'), width = 10, height = 6, units = 'in')
  
  #humidity
  ggplot(weather_3month, aes(x = instrument_datetime, y = humidity_perc, color = location)) +
    geom_point() +
    facet_grid(location ~ .) +
    labs(title = 'L0 humidity') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 humidity.jpg'), width = 10, height = 6, units = 'in')
  
  #dewpoint, wetbulb
  weather_3month_degree <- three_months_of_data%>% 
    select(instrument_datetime, location, dewpoint_c, wetbulb_c) %>% 
    gather(variable, value, -instrument_datetime, - location)
  
  ggplot(weather_3month_degree, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 dewpoint, wetbulb') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 dewpoint wetbulb.jpg'), width = 10, height = 6, units = 'in')
  
  
  #wind speed
  weather_3month_windsp <- three_months_of_data%>% 
    select(instrument_datetime, location, windsp_mps, windrun_m, highwindsp_mps) %>% 
    gather(variable, value, -instrument_datetime, - location)
  
  ggplot(weather_3month_windsp, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 wind') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 wind speed.jpg'), width = 10, height = 6, units = 'in')
  
  #wind direction
  winddir_levels = c('N', 'NNE', 'NE', 'ENE', 
                     'E', 'ESE', 'SE', 'SSE',
                     'S', 'SSW', 'SW', 'WSW',
                     'W', 'WNW', 'NW', 'NNW')
  weather_3month_winddir <- three_months_of_data%>% 
    select(instrument_datetime, location, winddir, highwinddir) %>% 
    gather(variable, value, -instrument_datetime, - location) %>% 
    mutate(value = factor(value, levels = winddir_levels))
  
  ggplot(weather_3month_winddir, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 wind') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 wind dir.jpg'), width = 10, height = 6, units = 'in')
  
  #index
  weather_3month_index <- three_months_of_data%>% 
    select(instrument_datetime, location, heatindex_c, thwindex_c, thswindex_c) %>% 
    gather(variable, value, -instrument_datetime, - location)
  
  ggplot(weather_3month_index, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 indices') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 heat indicies.jpg'), width = 10, height = 6, units = 'in')
  
  #precip
  weather_3month_precip <- three_months_of_data%>% 
    select(instrument_datetime, location, rain_mm, rainrate_mmph) %>% 
    gather(variable, value, -instrument_datetime, - location)
  
  ggplot(weather_3month_precip, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 precip') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 precipitation.jpg'), width = 10, height = 6, units = 'in')
  
  #solar
  weather_3month_solar <- three_months_of_data%>% 
    select(instrument_datetime, location, solarradiation_wpm2, solarenergy_ly, highsolarrad_wpm2, evapotrans_mm) %>% 
    gather(variable, value, -instrument_datetime, - location)
  
  ggplot(weather_3month_solar, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 solar') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 solar.jpg'), width = 10, height = 6, units = 'in')
  
  #uv
  weather_3month_uv <- three_months_of_data%>% 
    select(instrument_datetime, location, uvindex, uvdose_meds, highuvindex) %>% 
    gather(variable, value, -instrument_datetime, - location)
  
  ggplot(weather_3month_uv, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 UV') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 UV.jpg'), width = 10, height = 6, units = 'in')
  
  
  #degree days
  weather_3month_degreedays <- three_months_of_data%>% 
    select(instrument_datetime, location, heatingdegdays, coolingdegdays) %>% 
    gather(variable, value, -instrument_datetime, - location)
  
  ggplot(weather_3month_degreedays, aes(x = instrument_datetime, y = value, color = location)) +
    geom_point() +
    facet_grid(variable ~ ., scales = 'free_y') +
    labs(title = 'L0 degree days') +
    theme_bw() +
    scale_x_datetime(breaks = '1 month') +
    scale_color_colorblind()
  ggsave(file.path(dump_dir, 'L0 degree days.jpg'), width = 10, height = 6, units = 'in')
}

#Jan-Mar
make_3mo_vis(as.POSIXct('2023-01-01', tz = 'America/New_York'), 
             as.POSIXct('2023-04-01', tz = 'America/New_York'))
#Apr-Jun
make_3mo_vis(as.POSIXct('2023-04-01', tz = 'America/New_York'), 
             as.POSIXct('2023-07-01', tz = 'America/New_York'))
#Jul-Oct
make_3mo_vis(as.POSIXct('2023-07-01', tz = 'America/New_York'), 
             as.POSIXct('2023-10-01', tz = 'America/New_York'))
#Nov-Dec
make_3mo_vis(as.POSIXct('2023-10-01', tz = 'America/New_York'), 
             as.POSIXct('2024-01-01', tz = 'America/New_York'))




