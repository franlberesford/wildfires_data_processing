### running stuff #### 

library(sf)
library(viridis)
library(st)
library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(exactextractr)
library(mapview)
library(maps)
library(maptools)
library(admisc)
library(leaflet)
library(lubridate)
library(tidyverse)
library("reshape2")
library(dplyr)
library(magrittr)
library(broom)
library(raster)
library(spData)
#library(spDataLarge)
setwd("/home/ics-home/Wildfires/FRP")


# all_fire_path <- "Data/largefires_2020.gpkg"
# all_fires <- st_read(all_fire_path, layer = "newfirepix")
# st_layers(all_fire_path)

# ### check that vegetation re-code csvs are loaded but don't load them within function 
# road_class <- read.csv("Roads_220.csv")
# evt_class <-read.csv("EVT_200.csv")

####  define global variables ####
landfire_names <- c("elev", "asp","slo", "road")
landfire_types <- c(1, 1, 1,0)
years <- seq(2012, 2021 , by = 1 )
EPSGUTM <- 5070

#temp_fireids <- temp_fireids[5:length(temp_fireids)]

#### loop for all fires (of a given year ) ####

for (year in years){
  #define fire path for detections and perimeters
  temp_fire_path <- paste0( "Data/largefires_" , year , ".gpkg")
  
  #load detections and perimeters and redefine time variable, subset to only variables we are about  
  temp_fires_nfp <- st_read(temp_fire_path, layer = "nfplist")
  temp_fires_nfp$time <- format(strptime(temp_fires_nfp$t,'%Y-%m-%dT%H:%M:%S' ), '%Y-%m-%d %H:%M:%S') 
  temp_fires_nfp <- temp_fires_nfp[,c("frp", "ampm", "time", "FireID", "FRAPid",  "DS", "DT", "geom")]
  
  temp_fires_perim <- st_read(temp_fire_path, layer = "perimeter") 
  temp_fires_perim$time <- format(strptime(temp_fires_perim$t,'%Y-%m-%dT%H:%M:%S' ), '%Y-%m-%d %H:%M:%S') 
  temp_fires_perim <- temp_fires_perim[c("n_newpixels", "meanFRP", "FireID",   "FRAPid","geom", "time")]
  
  #define path for biomass since this does not change for each individual fire 
  temp_biomass_path <- paste0("Data/",year, "_cropped_biomass.gpkg" )
  
  #define number of fires 
  temp_fireids <- unique(temp_fires_nfp$FireID)
  temp_num_fires <- length(temp_fireids)
  
  #loop through individual fires 
  for (id in temp_fireids){
    temp_dat_pix <- temp_fires_nfp[temp_fires_nfp$FireID == id, ]
    temp_dat_perim <- temp_fires_perim[temp_fires_perim$FireID == id, ]
    temp_fire_num <- id
    temp_lf_path <- paste0("Data/fire_", year, "_", temp_fire_num, "_LF.tif")
    num_times <- length(unique(temp_fires_nfp$time[temp_fires_nfp$FireID == id]))
    temp_output <- fire_summaries(detecs_frp = temp_dat_pix,
                          detecs_line = temp_dat_perim, 
                          num_times = num_times, 
                          landfire_layers = temp_lf_path , 
                          landfire_names = landfire_names, 
                          landfire_types = landfire_types,
                          landfire_evt_layer = paste0("Data/fire_", year, "_", temp_fire_num, "_EVT_LF.tif"),
                          biomass_data = temp_biomass_path,
                          cell_width = 500, 
                          EPSGlonlat  = 4326, 
                          EPSGUTM = 5070,   
                          w = 2,  h= 3)
    save(temp_output, file = paste0("/home/ics-home/Wildfires/FRP/Output/fire_",year, "_", temp_fire_num, "_frp_info.RData"))
    print(paste0("Saved ", which(temp_fireids == id ), " out of ", temp_num_fires, " total fires in ", year))
  }
}




