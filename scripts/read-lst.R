#December 19, 2023
#This script reads in and manages the modis lst data
#Jan 5, 2024: the version uploading .hdf files is old. Use appeears instead.
#See here for attempts in that direction
#bangladesh-project/scripts/read-lst-hdf.R
#More user-friendly

library(tidyverse)
library(sf)
library(here)
library(mapview)
library(raster)
library(terra)
library(tidyterra)



# Load tif files from appeears------
setwd(here("data-input",
           "remote-sensing",
           "appeears-data",
           "bangladesh-modis-request", "tif-files"))

#very helpfully, they provide a summary statistics file, which includes the date of each file. Cool.
lst_20230525=rast("MYD21A2.061_LST_Day_1KM_doy2023145_aid0001.tif")
lst_20230525$MYD21A2.061_LST_Day_1KM_doy2023145_aid0001 %>% plot()#works! yay

lst_20230602=rast("MYD21A2.061_LST_Day_1KM_doy2023153_aid0001.tif")
lst_20230610=rast("MYD21A2.061_LST_Day_1KM_doy2023161_aid0001.tif")
lst_20230618=rast("MYD21A2.061_LST_Day_1KM_doy2023169_aid0001.tif")
lst_20230626=rast("MYD21A2.061_LST_Day_1KM_doy2023177_aid0001.tif")

#check image quality
lst_20230525 %>% mapview()#good image
lst_20230602 %>% mapview()#good image
lst_20230610 %>% mapview()#bad image
lst_20230618 %>% mapview() #also not great
lst_20230626 %>% mapview() #also a bad image

# Crop images------
#Note this relies on this being run first
source(here("scripts","read-wrangle-trajectories.R"))
#crop these around so that they're smaller
lst_20230525_crop=lst_20230525 %>% 
  terra::crop(buff_circular_for_crop)

lst_20230602_crop=lst_20230602 %>% 
  terra::crop(buff_circular_for_crop)

lst_20230525_crop %>% plot()
lst_20230525_crop %>% 
  mapview(layer.name="LST (Kelvin)")

lst_20230602_crop %>% 
  mapview(layer.name="LST (Kelvin)")

# save---
#save some rasters for use in Rmarkdown
setwd(here("data-processed"))
terra::writeRaster(
  lst_20230602_crop,
  overwrite=TRUE,
  filename = "lst_20230602_crop.tif" 
)


