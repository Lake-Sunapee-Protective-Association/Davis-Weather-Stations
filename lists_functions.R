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

# pressure plots to correct for HC timestamp issues
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
