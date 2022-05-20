# Davis-Weather-Stations

This repo of code is used to collate and QAQC Davis Weather Station Data at Lake Sunapee.

All data are downloaded in 3-month chunks and processed/cleaned at least 1x per year. Data are prepped for submission once per year. 

All data created with these scritps are archived at EDI (https://portal.edirepository.org/nis/mapbrowse?packageid=edi.736.2). Please review the metadata at EDI for data caveats, notes, and code definitions. 

All questions about these scripts and data should be directed to B Steele (steeleb@caryinstitute.org) and Kathie Weathers (weathersk@caryinstitute.org).

# File Descriptions

The order of scripts, as presented here, is the order in which they are run. Note, all raw data are stored locally (B. Steele, Cary Institute, steeleb@caryinstitute.org) and the following programs reference locally-sourced data.

## processing and collation

 * L0_weather.R - this file collates downloaded 3-month .csv files from the Davis Weatherlink site
 * L0_3month_visuals.R - this file spits out jpgs of each variable at a scale of 3-months

## cleaning and export
 
 * L1_weather_072019-012021.R - this script cleans/qaqc's the data prior to 2021
 * L1_weather_012021-012022.R - this script cleans/qaqc's the 2021 data

## prep for push to EDI

 * weather_prepforsubmission.Rmd - this script re-orders columns and applies ODM2 controlled vocabulary
 * sunapee_davisweather_eml_v20May2022.Rmd - this script creates the EML metadata for upload to EDI

