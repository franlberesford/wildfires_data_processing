<<<<<<< HEAD
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
#setwd("/home/ics-home/Wildfires/FRP")
#source("fire_setup_functions.R")

# all_fire_path <- "Data/largefires_2020.gpkg"
# all_fires <- st_read(all_fire_path, layer = "newfirepix")
# st_layers(all_fire_path)

# ### check that vegetation re-code csvs are loaded but don't load them within function 
# road_class <- read.csv("Roads_220.csv")
# evt_class <-read.csv("EVT_200.csv")

####  define global variables ####
landfire_names <- c("elev", "asp","slo", "road")
landfire_types <- c(1, 1, 1,0)
#years <- seq(2012, 2021 , by = 1 )
year = 2020 
id = 613
EPSGUTM <- 5070
EPSGlonlat <- 4326

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
  
  
    temp_dat_pix <- temp_fires_nfp[temp_fires_nfp$FireID == id, ]
    temp_dat_perim <- temp_fires_perim[temp_fires_perim$FireID == id, ]
    temp_fire_num <- id
    temp_lf_path <- paste0("Data/fire_", year, "_", temp_fire_num, "_LF.tif")
    num_times <- length(unique(temp_fires_nfp$time[temp_fires_nfp$FireID == id]))
    detecs_frp = temp_dat_pix
    detecs_line = temp_dat_perim
    num_times = num_times
    landfire_layers = temp_lf_path 
    landfire_names = landfire_names 
    landfire_types = landfire_types
    landfire_evt_layer = paste0("Data/fire_", year, "_", temp_fire_num, "_EVT_LF.tif")
    biomass_data = temp_biomass_path
    cell_width = 500
    w = 4
    h= 5

    
#### prelim functions we need ####
    
    
    
    ### taking mean of FRP excluding 0s 
temp_mean_fun <- function(x){
      if (sum(x) == 0 ){
        return(0)
      } else {
        x <- x[x>0]
        return(mean(x)) #only want mean FRP of time steps where there were actual detections 
      }
    }
    
    #want function to print out graphs as it makes them as well as little comments saying what stage we are up to ! Also need to know the general fire bounds in order to download right part of landfire 
    
    
    ### move to 0 standardising time series function 
frp_move_to_zero <- function(x, times = num_times, stamps = time_stamps ){
      #times with a non-zero FRP i.e. a detection
      all_non_zero <- which(x>0)
      
      if (length(all_non_zero)==0){
        return(rep(NA, times+3))
      } else {
        #time of first detection
        first_non_zero <- all_non_zero[1]
        #time between detections
        diffs <- diff(all_non_zero)
        
        #want to make sure that if we have 2 periods with detections then we keep the correct number of observations 
        if (sum(diffs>1) >0 ){
          len_non_zero <- max(all_non_zero)  - min(all_non_zero) +1 
          num_na <- num_times  - len_non_zero
        } else {
          num_na <- times - length(all_non_zero)
        }
        
        if (hour(stamps[min(all_non_zero)]) == 12 ){
          #time index of first instance, whether first instance was night or day, non zero series
          #extra NA so that time series that started at midday are lined up and those that started at midnight are lined up 
          #since there is quite a difference in the FRP values between night and day 
          return(c(min(all_non_zero), hour(stamps[min(all_non_zero)]),NA, x[c(min(all_non_zero):max(all_non_zero))], rep(NA, num_na)))
        } else {
          return(c(min(all_non_zero), hour(stamps[min(all_non_zero)]),x[c(min(all_non_zero):max(all_non_zero))], rep(NA, num_na+1)))
        }
        
        
      }
    }
    
    
    

    ### find_cell function 
find_cell <- function(x, polys){
      
      ## find which grid cell the pixel is in, pull out the id 
      which_cell <- st_intersects(x, polys$x, sparse=F) 
      if (sum(which_cell) == 0 ) {
        return(NA)
      } else{
        return(polys$id[which_cell])
      }
    }
    
    
    ### graphing functions 
plot_grid_summaries <- function(data_grid, data_color, fill_label, title_label, col_option, direc_option = -1){
      #grid plotted with ggplot 
      ggplot() + 
        geom_sf(data = data_grid, 
                aes(fill =data_color ), color = NA) + 
        scale_fill_viridis(option = col_option,direction = direc_option, 
                           #breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                           limits=c(min(data_color, na.rm=T), max(data_color, na.rm=T ))
                           , na.value="transparent") +
        labs(fill = fill_label, 
             title = title_label) +
        theme(plot.title = element_text(hjust = 0.5, size = 12), 
              legend.title=element_text(size=10))   
    }
    
    
    ### plotting landfire layers before summarising by grid cell
plot_landfire_layers <- function(lf_layer, col_label ){
      #plotting the layers just to check they look ok 
      print( ggplot() +
               geom_sf(data = lf_layer,
                       aes(geometry=geometry, col = value )) +
               geom_sf(data = detecs_line[detecs_line$time == max(detecs_line$time),],
                       aes(geometry = geom), fill = NA, col = "red" ) +
               labs(col = col_label)     ) 
    }
    


#### everything that would be in the full function #### 



print("Loading and re-formatting data.")

#want to make it so it plots UTM values on the axes and not the 

detecs_frp$geom <- st_transform(detecs_frp$geom, EPSGlonlat)
detecs_line$geom <- st_transform(detecs_line$geom, EPSGlonlat)
detecs_frp$geom2 <- st_transform(detecs_frp$geom, EPSGUTM)
detecs_line$geom2 <- st_transform(detecs_line$geom, EPSGUTM)

### define number of time steps 
time_stamps <- unique(detecs_frp$time)
num_times <- length(time_stamps)
#print(num_times)

time_stamps_am <- unique(detecs_frp$time[detecs_frp$ampm == "AM"])
time_stamps_pm <- unique(detecs_frp$time[detecs_frp$ampm == "PM"])


### buff perimeter 
final_perim_utm <- detecs_line$geom2[nrow(detecs_line)]
buffed_perim_utm <- st_buffer(final_perim_utm, 500)
buffed_perim_utm_poly <- buffed_perim_utm %>% st_as_sf( crs = EPSGUTM) %>%  st_cast("MULTIPOLYGON")
#num_islands <- length(buffed_perim[[1]]) 

### bounds in utm 
east_range <- c(attr(buffed_perim_utm, "bbox")[1], attr(buffed_perim_utm, "bbox")[3])
north_range <- c(attr(buffed_perim_utm, "bbox")[2], attr(buffed_perim_utm, "bbox")[4]) 


### adding grid to data 
detecs_grid_utm <-  st_make_grid(buffed_perim_utm , cellsize = 500, square = T,  crs = st_crs(detecs_line$geom2))
detecs_grid_utm <- detecs_grid_utm %>% st_as_sf( crs = EPSGUTM) %>%  st_cast("POLYGON") 


### filter the grid by points that are within the final fire perimeter 
detecs_grid_utm <- detecs_grid_utm[st_intersects(detecs_grid_utm, final_perim_utm, sparse =F),]
detecs_grid_utm$id <- c(1:length(detecs_grid_utm$x))
detecs_grid <- st_transform(detecs_grid_utm, crs = EPSGlonlat)


print("Calculating FRP at each time point within each grid cell.")

### properties of grid through time loop 
fire_grid_proponfire <- numeric()
num_cells <- length(detecs_grid$id)
#num_cellsUTM <- length(detecs_grid_utm) #they are the same - this makes sense right??? 


##matrix of time by cells will be num_times * num_cells 
fire_mat_frp <- fire_mat_numpix <- fire_mat_indpix <- fire_mat_ds <- fire_mat_dt <- matrix(NA, ncol = num_cells, nrow = num_times )
for (i in 1:num_cells){
  #want to do a vector of indicators for if fire was in that region, and mean FRP per grid cell by time 
  temp_poly <- detecs_grid[i,]
  
  #want to move on if the grid cell has no fire at any point 
  if (sum(st_intersects(temp_poly, detecs_frp, sparse =F)) == 0 ){
    #if there are no detections at any time pointin a given grid cell then we can assign all the values to be 0
    fire_mat_frp[,i] <-  fire_mat_indpix[,i] <-  fire_mat_numpix[,i] <- fire_mat_ds[,i] <- fire_mat_dt[,i]  <- rep(0, num_times)
    fire_grid_proponfire <- append(fire_grid_proponfire, 0)
    next #move to next grid cell without doing whole loop 
  }
  
  temp_frp_vec <- temp_ds_vec <- temp_dt_vec <-  numeric()
  temp_pix_mat <- matrix(NA, ncol = 2, nrow = num_times) 
  
  
  for (j in 1:num_times){
    temp_time <- time_stamps[j]
    temp_frp <- detecs_frp[detecs_frp$time == temp_time, ]
    #number of fire pixels in grid cell 
    temp_pix_num <- dim(st_filter(temp_frp, temp_poly))[1]
    if (temp_pix_num >0 ){
      # total average FRP / pixel / cell 
      temp_pix <-  st_filter(temp_frp, temp_poly)
      total_frp <- sum(temp_pix$frp)
      av_frp <- total_frp/ temp_pix_num  #taking the arithmetic mean of the FRP if multiple detections per cell
      av_ds <- sum(temp_pix$DS)/temp_pix_num
      av_dt <- sum(temp_pix$DT)/temp_pix_num
      pix_ind <- 1 
    } else {
      total_frp = 0 
      av_frp <- av_ds <- av_dt <-  0 
      pix_ind <- 0 
    }
    temp_frp_vec  <- c(temp_frp_vec, av_frp)  ##adding av frp to the vector 
    temp_ds_vec  <- c(temp_ds_vec, av_ds) ## adding av ds to vector 
    temp_dt_vec  <- c(temp_dt_vec, av_dt) ## adding av dt to vector 
    temp_pix_mat[j,] <- c(temp_pix_num, pix_ind) #first column is number of pixels, second column is indicator for any pixels at all 
  }
  
  
  #add the cell summaries over time to the vectors/ matrices including proportion of time that a fire was detected in a given grid cell 
  time_on_fire <- mean(temp_pix_mat[,2])
  fire_mat_frp[,i] <-  temp_frp_vec
  fire_mat_ds[,i] <-  temp_ds_vec
  fire_mat_dt[,i] <-  temp_dt_vec
  fire_mat_indpix[,i] <- temp_pix_mat[,2] #indicator for any fire pixels at  a given time
  fire_mat_numpix[,i] <- temp_pix_mat[,1]  #count of fire pixels at  a given time
  fire_grid_proponfire <- append(fire_grid_proponfire, time_on_fire)
  
  if (floor(i/100 ) ==( i/100) ){
    print(paste(i, " out of ", num_cells, sep = ""))
  }
  
}
cells_with_fire_ind <- (fire_grid_proponfire>0)
cells_with_fire <- which(fire_grid_proponfire>0)
num_fire_cells <- length(cells_with_fire)

### define matrix/ data.frame with cell summaries. 
### time spent on fire 
fire_grid_props <- data.frame(fire_grid_proponfire)
colnames(fire_grid_props) <- "FIRE_PROP"
fire_grid_props$FIRE_PROP_NA <- sapply(fire_grid_props$FIRE_PROP,  FUN = function(x) ifelse(x == 0 , NA, x ) ) 
fire_grid_props$FIRE_STEPS_NA <- num_times*fire_grid_props$FIRE_PROP_NA
fire_grid_props$FIRE_PERCENT_NA <- fire_grid_props$FIRE_PROP_NA*100

if (num_times > 1){
  ###FRP values 
  fire_grid_props$TEMP_TOT_FRP_NA <- apply(fire_mat_frp[,c(1:num_cells)], 2, sum) 
  fire_grid_props$TEMP_TOT_FRP_NA <-   sapply(fire_grid_props$TEMP_TOT_FRP_NA,  FUN = function(x) ifelse(x == 0 , NA, x )) 
  fire_grid_props$log_TEMP_TOT_FRP <- log(fire_grid_props$TEMP_TOT_FRP_NA)
  fire_grid_props$TEMP_AV_FRP_NA <- apply(fire_mat_frp[,c(1:num_cells)], 2, temp_mean_fun) 
  fire_grid_props$TEMP_AV_DS <- apply(fire_mat_ds[,c(1:num_cells)], 2, temp_mean_fun) 
  fire_grid_props$TEMP_AV_DT <- apply(fire_mat_dt[,c(1:num_cells)], 2, temp_mean_fun) 
  fire_grid_props$TEMP_TOT_DS <- apply(fire_mat_ds[,c(1:num_cells)], 2, sum) 
  fire_grid_props$TEMP_TOT_DT <- apply(fire_mat_dt[,c(1:num_cells)], 2, sum) 
  fire_grid_props$TEMP_AV_FRP_NA <- sapply(fire_grid_props$TEMP_AV_FRP_NA,  FUN = function(x) ifelse(x == 0 , NA, x ) )
  fire_grid_props$log_TEMP_AV_FRP <- log(fire_grid_props$TEMP_AV_FRP)
} else if ( num_times == 1 ){
  fire_grid_props$TEMP_TOT_FRP_NA <- t(fire_mat_frp)
  fire_grid_props$log_TEMP_TOT_FRP_NA <- log(fire_grid_props$TEMP_TOT_FRP_NA)
  fire_grid_props$log_TEMP_TOT_FRP_NA <- sapply(fire_grid_props$log_TEMP_TOT_FRP_NA,  FUN = function(x) ifelse(x == -Inf, NA, x ) )
  fire_grid_props$TEMP_AV_FRP_NA <- fire_grid_props$TEMP_TOT_FRP_NA
  fire_grid_props$TEMP_AV_DS <- t(fire_mat_ds)
  fire_grid_props$TEMP_AV_DT <- t(fire_mat_dt)
  fire_grid_props$TEMP_TOT_DS <- t(fire_mat_ds)
  fire_grid_props$TEMP_TOT_DT <- t(fire_mat_dt)
  fire_grid_props$log_TEMP_AV_FRP <- fire_grid_props$log_TEMP_TOT_FRP
}


###matrices also with time series of frp, cumulative frp, pixels per grid cell 
#time series of FRP of each of the grid cells 
fire_mat_frp <- data.frame(fire_mat_frp)
fire_mat_ds <- data.frame(fire_mat_ds)
fire_mat_dt <- data.frame(fire_mat_dt)
fire_mat_frp$time <- fire_mat_ds$time <- fire_mat_dt$time <- c(1:num_times)
#fire_mat_frpNA <- apply(fire_mat_frp, c(1,2), function(x) ifelse(x== 0, NA, x))
fire_mat_cumtotfrp <- apply(fire_mat_frp, 2, cumsum)
fire_mat_logfrp <- log(fire_mat_frp)
fire_mat_logfrp <- apply(fire_mat_logfrp, c(1,2), function(x) ifelse(x== -Inf, NA, x))
fire_mat_logfrp <- data.frame(fire_mat_logfrp)
fire_mat_logfrp$time <- c(1:num_times)


### standardising time series to start at time 1, only include cells with actual detections, include cell reference, hour at first detection, time at first detection 
if (num_times > 1 ){
  fire_frp_ts <- apply(fire_mat_frp, 2, frp_move_to_zero, times = num_times, stamps = time_stamps )
  fire_frp_ts <- fire_frp_ts[rowSums(is.na(fire_frp_ts))<ncol(fire_frp_ts),]
  fire_frp_ts <- data.frame(fire_frp_ts, row.names = NULL)
  fire_frp_ts$time <- c(1,hour(time_stamps[1]), c(1:(nrow(fire_frp_ts)-2)))
} else {
  fire_frp_ts <- NA 
}


print("Plotting FRP temporal summaries over the gridded region.")

### plots (probably move below )
plot_total_frp <- plot_grid_summaries(data_grid = detecs_grid , 
                                      data_color = fire_grid_props$TEMP_TOT_FRP, 
                                      fill_label = "Total FRP \nper cell" , 
                                      title_label = "Temporal total FRP per 500m grid cell",
                                      col_option = "plasma" ,
                                      direc_option = -1)


plot_log_total_frp <- plot_grid_summaries(data_grid = detecs_grid , 
                                          data_color = fire_grid_props$log_TEMP_TOT_FRP, 
                                          fill_label = "log-Total FRP \nper cell (base e)" , 
                                          title_label = "Temporal log-total FRP per 500m grid cell",
                                          col_option = "inferno",
                                          direc_option = -1)



plot_fire_percent <- plot_grid_summaries(data_grid = detecs_grid , 
                                         data_color = fire_grid_props$FIRE_PERCENT_NA, 
                                         fill_label = "Percentage" , 
                                         title_label = "Percentage of time each cell was on fire",
                                         col_option ="magma"  ,
                                         direc_option = -1)

plot_fire_steps <- plot_grid_summaries(data_grid = detecs_grid , 
                                       data_color = fire_grid_props$FIRE_STEPS_NA, 
                                       fill_label = "Number" , 
                                       title_label = "Number of time stamps each cell was on fire",
                                       col_option = "mako",
                                       direc_option = -1)

### put plots into easily accessibly list 
plot_list <- list(plot_total_frp, plot_log_total_frp, plot_fire_percent, plot_fire_steps)
names(plot_list) <- c("total_frp", "log_total_frp" , "fire_percent", "fire_steps")


print("Defining quadrants to speed up cell number labelling.")

#define quadrants / octants ? - do east/north because some of the perimeters go funny with lon/lat 
# want to make a 3x2 grid across rectangle of final fire outline to speed up find_cell process 
num_quads = w*h
quads_east <- seq(east_range[1], east_range[2],  length.out = w+1) 
quads_north <- seq(north_range[1], north_range[2], length.out = h+1)

quadrants <- expand.grid(x = quads_east , y = quads_north)

lr <- quadrants[-c(1:(w+1)),]
ul <- quadrants[c(1:(nrow(quadrants)-(w+1))),]

corners <- cbind(upper_left=ul, lower_right=lr)
corners <- corners[1:nrow(corners) %% w != 0,]
rownames(corners) <- NULL
head(corners)

quad_polys <- st_make_grid(st_as_sf(quadrants, coords = c("x", "y"), crs = EPSGUTM), n=c(w,h))

#browser() 

quad_polys_sf <- data.frame(quad_polys) %>% st_as_sf()

### make list of which grid cells are within each quadrant so that can more easily search when doing 'find cell' 
detecs_quadrant_grid_cells <- list()
for (i in 1:num_quads){
  temp_list <- st_intersects(detecs_grid_utm, quad_polys[[i]], sparse = F) 
  detecs_quadrant_grid_cells[[i]] <- detecs_grid_utm[temp_list,]
}

#browser()

print("Loading landfire layers, re-projecting and assigning grid cell label")

## topology 
#number of layers (length of paths vector )
num_layers <- length(landfire_names)
road_index = which(landfire_names== "road")

### load evt first, so can test if same length as other layers, then don't need to duplicate the find cell bit because this is what takes time 
temp_evt_raster <-  raster(landfire_evt_layer )
temp_evt_raster <- projectRaster(temp_evt_raster, crs=EPSGUTM, method = "ngb")
temp_evt_cropped <-  raster::mask(temp_evt_raster,as_Spatial(final_perim_utm))
evt_df  <- raster::as.data.frame(temp_evt_cropped , xy=TRUE , na.rm=T )
colnames(evt_df)[3] <- "value"
evt_df <- st_as_sf(evt_df, coords  = c("x", "y"), remove = F)
evt_df <- evt_df %>% st_set_crs(EPSGUTM)
## only want to assign grid cell label once so will do this first with EVT layer and then just copy 


### make list of which pixels are within each quadrant so that can more easily search when doing 'find cell' 
pixel_quadrants_subset <- list()
for (i in 1:num_quads){
  temp_list <- st_intersects(evt_df, quad_polys[[i]], sparse = F) 
  pixel_quadrants_subset[[i]] <- evt_df[temp_list,]
  #print(i)
}


##now want to apply find cell function / apply st_intersects for each of the 10 subsets
#Sys.time()
for (i in 1:num_quads){
  temp_layer <-  pixel_quadrants_subset[[i]]
  temp_fire_grid <- detecs_quadrant_grid_cells[[i]]
  temp_layer$cell <- sapply(temp_layer$geometry, find_cell, polys =  temp_fire_grid)
  print(i)
  pixel_quadrants_subset[[i]] <- temp_layer 
}
#Sys.time()
evt_df <- do.call(rbind, pixel_quadrants_subset)
temp_obs_with_cell <- which(!is.na(evt_df$cell))

### assign grid cell to each of the layers 
landfire_list <- list()
landfire_tifs_list <- list()
for (i in 1:num_layers){ 
  temp_raster <- raster(landfire_layers, band = i )
  temp_raster <- projectRaster(temp_raster, crs=EPSGUTM, method = "ngb")
  temp_cropped <-  raster::mask(temp_raster,as_Spatial(final_perim_utm ))
  landfire_tifs_list[[i]] <- temp_cropped 
  landfire_list[[i]] <- raster::as.data.frame(temp_cropped , xy=TRUE , na.rm=T )
  colnames(landfire_list[[i]])[3] <- "value"
  landfire_list[[i]] <- st_as_sf(landfire_list[[i]], coords  = c("x", "y"), remove = F)
  landfire_list[[i]] <- landfire_list[[i]] %>% st_set_crs(EPSGUTM)
  ### add in this because we only want to use the find_cell function once because it takes a long time and then just copy it to the other layers. usually the landfire layers are mapped onto the same grid so we can just duplicate the cell variable 
  if (sum(evt_df$x)  == sum(landfire_list[[i]]$x)){
    landfire_list[[i]]$cell <- evt_df$cell 
  } else{
    #this shouldn't ever happen, I downloaded the data from landfire altogether so the points should be the same all the time . it will also take ages so really hope this doesn't happen 
    print("Calculating grid cells for a second landfire layer. ")
    landfire_list[[i]]$cell <- sapply(temp_cropped$geometry, find_cell, polys = detecs_grid_utm)
  }
  #get rid of pixels not in any cell 
  landfire_list[[i]] <- landfire_list[[i]][temp_obs_with_cell,]
}
names(landfire_list) <- landfire_names
names(landfire_tifs_list) <- landfire_names
evt_df <- evt_df[temp_obs_with_cell,]

num_lanfir_pixels <-  nrow(landfire_list[[1]])

#browser()

print("Assigning labels to numerical categorical variables (namely Road, EVT).")
road_class <- read.csv("Roads_220.csv")
evt_class1 <-read.csv("EVT_CLASS1.csv")
evt_class2 <- read.csv("EVT_CLASS2.csv")


### assign category names to roads and EVT 
if (!is.na(road_index)){
  ## recode 
  landfire_list[[road_index]]$label <- admisc::recode(as.factor(landfire_list[[road_index]]$value), paste(road_class$Value," = '",road_class$Class_Name, "'", sep = "" , collapse= ";" ))
  
  ## re define as factors 
  landfire_list[[road_index]]$label <- factor(landfire_list[[road_index]]$label , levels = unique(landfire_list[[road_index]]$label ))
}

## check which set of evt classfications to use
if (max(evt_df$value) < min(evt_class2$VALUE[evt_class2$VALUE>0 ])){
  evt_class <- evt_class1
} else{
  evt_class <- evt_class2
}


evt_df$label <- admisc::recode(as.factor(evt_df$value), paste(evt_class$VALUE," = '",evt_class$EVT_LF, "'", sep = "" , collapse= ";" ))
## re define as factors 
evt_df$label <- factor(evt_df$label , levels = unique(evt_df$label ))

#browser()

### defining number of the landfire layers which are categorical vs continuous. EVT layer not included in this because i downloaded the tif files separately   
num_categorical <- length(which(landfire_types == 0 ))
num_continuous <- length(which(landfire_types == 1 ))

### number of levels in each of the factor / categorical variables, then additional layer for 2nd variable derived from EVT 
num_categories <- numeric()
for (i in 1:num_layers){
  levs <- ifelse( landfire_types[i] ==0, nlevels(landfire_list[[i]]$label), NA )
  num_categories <- c(num_categories, levs )
}

### define matrices for each of the landfire variable summaries  
fire_grid_evt <- matrix(NA, nrow= num_cells, ncol = nlevels(evt_df$label))
fire_grid_layers <- vector("list" , length= (num_layers) )
for (k in 1:num_layers){
  if (landfire_types[k] == 0){
    fire_grid_layers[[k]] <- matrix(NA, nrow = num_cells, ncol = num_categories[k] )
    colnames( fire_grid_layers[[k]]) <- levels(landfire_list[[k]]$label)
  } else {
    fire_grid_layers[[k]] <- matrix(NA, nrow = num_cells, ncol = 9 )
    colnames(fire_grid_layers[[k]])  <- c("mean", "variance", "range","sum", "min", "low_quart", "median", "upp_quart", "max")
  }
}

print("Landfire layer loop through grid cells, making summaries")                                             

for  (i in 1:num_cells){
  
  #get the points of the raster that are within each of the grid cells and their respective properties , although the grid cells actually describe a 30m square pixel and not a point 
  
  #only going to look at 2 or 3 of the quadrants at most which should really speed up the process 
  temp_inds <- which(landfire_list[[1]]$cell == i)
  temp_dat <- lapply(landfire_list, FUN = function(x) x <- x[temp_inds,]) 
  
  ## will assign the different layers different depending on continuous or categorical (input vector is 0 = cat, 1= cont )
  ##first do categorical layers 
  for (k in 1:num_categorical) {
    #number of categories that we expect to be in our table - need to be careful about this, need to make sure the variable is coded to have all X layers so taht when we make a table we get a X number table and not just like 1 or 2 numbers if only say water and developed are present. 
    num_categs <- num_categories[landfire_types == 0][k]
    
    #which layer (out of categorical and continuous) are we working with - need this to assign prop.table 
    lay_num <- which(landfire_types==0 )[k]
    
    #table saying how many landfire observations in a given cell are of which type 
    temp_props <- prop.table(table(temp_dat[[lay_num]]$label)) 
    
    ## add to output list noting that we need to move the second EVT thing to the end 
    fire_grid_layers[[lay_num]][i, ] <- temp_props
  }
  
  ##first do continuous layers 
  for (k in 1:num_continuous) {
    #layer number out of all layers 
    lay_num <- which(landfire_types==1 )[k]
    
    #landfire observations within given grid cell 
    dat <- temp_dat[[lay_num]]$value
    
    #assign basic summary stats and 5 number summary to correct row of matrix 
    fire_grid_layers[[lay_num]][i, ] <-  c(mean(dat, na.rm=T), 
                                           var(dat, na.rm=T) , 
                                           diff(range(dat)), 
                                           sum(dat, na.rm=T),
                                           quantile(dat, c(0, 0.25, 0.5, 0.75, 1))) 
  }
  
  temp_evt_dat <- evt_df[evt_df$cell==i ,]
  fire_grid_evt[i,] <- prop.table(table(temp_evt_dat$label)) 
  
  ### print to say where we are in loop of assigning topography etc. actually not very helpful because the long bit is the grid cell reference and not this iterative thing but oh well. 
  if (floor(i/100 ) == i/100){
    print(paste(i, " out of ", num_cells, sep = ""))
  }
  
}
colnames(fire_grid_evt) <- levels(evt_df$label)
colnames(fire_grid_layers[[road_index]]) <- levels(landfire_list[[road_index]]$label)
names(fire_grid_layers) <- landfire_names

print("Loading biomass layer.")


#### load biomass layer (gpkg file ), crop so that we only have the bits within our final fire perimet 
biomass <- st_read(biomass_data)
biomass <- biomass[st_intersects(biomass, buffed_perim_utm_poly, sparse =F),]
colnames(biomass)[3] <- "value"

###change to raster so can save it in output 
biomass_raster <- rasterFromXYZ(st_set_geometry(biomass,NULL), crs = EPSGUTM) 


#### add grid cell to biomass layer 
### make list of which pixels are within each quadrant so that can more easily search when doing 'find cell' 
biomass_quadrants_subset <- list()
for (i in 1:num_quads){
  temp_list <- st_intersects(biomass, quad_polys[[i]], sparse = F) 
  biomass_quadrants_subset[[i]] <- biomass[temp_list,]
  #print(i)
}

##now want to apply find cell function / apply st_intersects for each of the 10 subsets
Sys.time()
for (i in 1:num_quads){
  temp_layer <-  biomass_quadrants_subset[[i]]
  temp_fire_grid <- detecs_quadrant_grid_cells[[i]]
  temp_layer$cell <- sapply(temp_layer$geom, find_cell, polys =  temp_fire_grid)
  print(i)
  biomass_quadrants_subset[[i]] <- temp_layer 
}
Sys.time()
biomass <- do.call(rbind, biomass_quadrants_subset)
biomass <- biomass[!is.na(biomass$cell), ] 
#num_bio_pixels <-  nrow(biomass )

print("Biomass loop through grid cells, making summaries")                                             

#predefine matrix 
fire_grid_biomass <- matrix(NA, nrow= num_cells , ncol = 9 )

#summarise biomass by grid cell 
for  (i in 1:num_cells){
  
  #get the points of the raster that are within each of the grid cells and their respective properties , although the grid cells actually describe a 30m square pixel and not a point 
  #only going to look at 2 or 3 of the quadrants at most which should really speed up the process 
  temp_dat <- biomass$value[biomass$cell == i ]
  
  if (length(temp_dat) == 0 ) { fire_grid_biomass[i, ] <-  rep(NA, 9 ) ; next } 
  
  #summarise biomass over the grid cell 
  fire_grid_biomass[i, ] <-  c(mean(dat, na.rm=T),
                               var(temp_dat, na.rm=T) , 
                               diff(range(temp_dat)), 
                               sum(temp_dat, na.rm=T),
                               quantile(temp_dat, c(0, 0.25, 0.5, 0.75, 1))) 
  
  
  ### print to say where we are in loop of assigning topography etc. actually not very helpful because the long bit is the grid cell reference and not this iterative thing but oh well. 
  if (floor(i/100 ) == i/100){
    print(paste(i, " out of ", num_cells, sep = ""))
  }
  
}
colnames(fire_grid_biomass)  <- c("mean" ,"variance", "range", "sum", "min", "low_quart", "median", "upp_quart", "max")


###
output <- list("frp_detecs" = detecs_frp , 
               "grid_polys" = detecs_grid_utm , 
               "grid_props" = fire_grid_props, 
               "length_grid" = num_cells ,
               "num_fire_cells" = num_fire_cells, 
               "frp_ts" = fire_frp_ts, 
               "frp_matrix" = fire_mat_frp,   
               "cumu_frp_matrix" =  fire_mat_cumtotfrp,  
               "detec_ind_mat" = fire_mat_numpix, 
               "ds_matrix" =fire_mat_ds , 
               "dt_matrix" = fire_mat_dt,
               "lf_rasters" = landfire_tifs_list, 
               "evt_raster" = temp_evt_cropped,
               "lf_data" = landfire_list, 
               "evt_data" = evt_df, 
               "grid_lf"  = fire_grid_layers , 
               "grid_evt" = fire_grid_evt, 
               "biom_data" = biomass,
                "biom_raster" = biomass_raster,
               "grid_biom" = fire_grid_biomass,
               "frp_plots" = plot_list , 
               "landfire_plots" = plot_list )




#### saving output #### 
    
save(output, file = paste0("/Output/TEST_fire_",year, "_", id , "_frp_info.RData"))

print(paste0("Saved ", which(temp_fireids == id ), " out of ", temp_num_fires, " total fires in ", year))

    
    
    
    



=======
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
#setwd("/home/ics-home/Wildfires/FRP")
#source("fire_setup_functions.R")

# all_fire_path <- "Data/largefires_2020.gpkg"
# all_fires <- st_read(all_fire_path, layer = "newfirepix")
# st_layers(all_fire_path)

# ### check that vegetation re-code csvs are loaded but don't load them within function 
# road_class <- read.csv("Roads_220.csv")
# evt_class <-read.csv("EVT_200.csv")

####  define global variables ####
landfire_names <- c("elev", "asp","slo", "road")
landfire_types <- c(1, 1, 1,0)
#years <- seq(2012, 2021 , by = 1 )
year = 2020 
id = 613
EPSGUTM <- 5070
EPSGlonlat <- 4326

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
  
  
    temp_dat_pix <- temp_fires_nfp[temp_fires_nfp$FireID == id, ]
    temp_dat_perim <- temp_fires_perim[temp_fires_perim$FireID == id, ]
    temp_fire_num <- id
    temp_lf_path <- paste0("Data/fire_", year, "_", temp_fire_num, "_LF.tif")
    num_times <- length(unique(temp_fires_nfp$time[temp_fires_nfp$FireID == id]))
    detecs_frp = temp_dat_pix
    detecs_line = temp_dat_perim
    num_times = num_times
    landfire_layers = temp_lf_path 
    landfire_names = landfire_names 
    landfire_types = landfire_types
    landfire_evt_layer = paste0("Data/fire_", year, "_", temp_fire_num, "_EVT_LF.tif")
    biomass_data = temp_biomass_path
    cell_width = 500
    w = 4
    h= 5

    
#### prelim functions we need ####
    
    
    
    ### taking mean of FRP excluding 0s 
temp_mean_fun <- function(x){
      if (sum(x) == 0 ){
        return(0)
      } else {
        x <- x[x>0]
        return(mean(x)) #only want mean FRP of time steps where there were actual detections 
      }
    }
    
    #want function to print out graphs as it makes them as well as little comments saying what stage we are up to ! Also need to know the general fire bounds in order to download right part of landfire 
    
    
    ### move to 0 standardising time series function 
frp_move_to_zero <- function(x, times = num_times, stamps = time_stamps ){
      #times with a non-zero FRP i.e. a detection
      all_non_zero <- which(x>0)
      
      if (length(all_non_zero)==0){
        return(rep(NA, times+3))
      } else {
        #time of first detection
        first_non_zero <- all_non_zero[1]
        #time between detections
        diffs <- diff(all_non_zero)
        
        #want to make sure that if we have 2 periods with detections then we keep the correct number of observations 
        if (sum(diffs>1) >0 ){
          len_non_zero <- max(all_non_zero)  - min(all_non_zero) +1 
          num_na <- num_times  - len_non_zero
        } else {
          num_na <- times - length(all_non_zero)
        }
        
        if (hour(stamps[min(all_non_zero)]) == 12 ){
          #time index of first instance, whether first instance was night or day, non zero series
          #extra NA so that time series that started at midday are lined up and those that started at midnight are lined up 
          #since there is quite a difference in the FRP values between night and day 
          return(c(min(all_non_zero), hour(stamps[min(all_non_zero)]),NA, x[c(min(all_non_zero):max(all_non_zero))], rep(NA, num_na)))
        } else {
          return(c(min(all_non_zero), hour(stamps[min(all_non_zero)]),x[c(min(all_non_zero):max(all_non_zero))], rep(NA, num_na+1)))
        }
        
        
      }
    }
    
    
    

    ### find_cell function 
find_cell <- function(x, polys){
      
      ## find which grid cell the pixel is in, pull out the id 
      which_cell <- st_intersects(x, polys$x, sparse=F) 
      if (sum(which_cell) == 0 ) {
        return(NA)
      } else{
        return(polys$id[which_cell])
      }
    }
    
    
    ### graphing functions 
plot_grid_summaries <- function(data_grid, data_color, fill_label, title_label, col_option, direc_option = -1){
      #grid plotted with ggplot 
      ggplot() + 
        geom_sf(data = data_grid, 
                aes(fill =data_color ), color = NA) + 
        scale_fill_viridis(option = col_option,direction = direc_option, 
                           #breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                           limits=c(min(data_color, na.rm=T), max(data_color, na.rm=T ))
                           , na.value="transparent") +
        labs(fill = fill_label, 
             title = title_label) +
        theme(plot.title = element_text(hjust = 0.5, size = 12), 
              legend.title=element_text(size=10))   
    }
    
    
    ### plotting landfire layers before summarising by grid cell
plot_landfire_layers <- function(lf_layer, col_label ){
      #plotting the layers just to check they look ok 
      print( ggplot() +
               geom_sf(data = lf_layer,
                       aes(geometry=geometry, col = value )) +
               geom_sf(data = detecs_line[detecs_line$time == max(detecs_line$time),],
                       aes(geometry = geom), fill = NA, col = "red" ) +
               labs(col = col_label)     ) 
    }
    


#### everything that would be in the full function #### 



print("Loading and re-formatting data.")

#want to make it so it plots UTM values on the axes and not the 

detecs_frp$geom <- st_transform(detecs_frp$geom, EPSGlonlat)
detecs_line$geom <- st_transform(detecs_line$geom, EPSGlonlat)
detecs_frp$geom2 <- st_transform(detecs_frp$geom, EPSGUTM)
detecs_line$geom2 <- st_transform(detecs_line$geom, EPSGUTM)

### define number of time steps 
time_stamps <- unique(detecs_frp$time)
num_times <- length(time_stamps)
#print(num_times)

time_stamps_am <- unique(detecs_frp$time[detecs_frp$ampm == "AM"])
time_stamps_pm <- unique(detecs_frp$time[detecs_frp$ampm == "PM"])

### buff perimeter 
buffed_perim_utm <- st_buffer(final_perim_utm, 500)
buffed_perim_utm_poly <- buffed_perim_utm %>% st_as_sf( crs = EPSGUTM) %>%  st_cast("MULTIPOLYGON")
#num_islands <- length(buffed_perim[[1]]) 

### adding grid to data 
final_perim_utm <- detecs_line$geom2[nrow(detecs_line)]
detecs_grid_utm <-  st_make_grid(buffed_perim_utm , cellsize = 500, square = T,  crs = st_crs(detecs_line$geom2))
detecs_grid_utm <- detecs_grid_utm %>% st_as_sf( crs = EPSGUTM) %>%  st_cast("POLYGON") 

### begin by filtering the grid by points that are within the final fire perimeter 
detecs_grid_utm <- detecs_grid_utm[st_intersects(detecs_grid_utm, final_perim_utm, sparse =F),]
detecs_grid_utm$id <- c(1:length(detecs_grid_utm$x))
detecs_grid <- st_transform(detecs_grid_utm, crs = EPSGlonlat)

### bounds in utm 
east_range <- c(attr(buffed_perim_utm, "bbox")[1], attr(buffed_perim_utm, "bbox")[3])
north_range <- c(attr(buffed_perim_utm, "bbox")[2], attr(buffed_perim_utm, "bbox")[4]) 




print("Calculating FRP at each time point within each grid cell.")

### properties of grid through time loop 
fire_grid_proponfire <- fire_grid_frp <- numeric()
num_cells <- length(detecs_grid$id)
#num_cellsUTM <- length(detecs_grid_utm) #they are the same - this makes sense right??? 


##matrix of time by cells will be num_times * num_cells 
fire_mat_frp <- fire_mat_numpix <- fire_mat_indpix <- fire_mat_ds <- fire_mat_dt <- matrix(NA, ncol = num_cells, nrow = num_times )
for (i in 1:num_cells){
  #want to do a vector of indicators for if fire was in that region, and mean FRP per grid cell by time 
  temp_poly <- detecs_grid[i,]
  
  #want to move on if the grid cell has no fire at any point 
  if (sum(st_intersects(temp_poly, detecs_frp, sparse =F)) == 0 ){
    #if there are no detections at any time pointin a given grid cell then we can assign all the values to be 0
    fire_mat_frp[,i] <-  fire_mat_indpix[,i] <-  fire_mat_numpix[,i] <- fire_mat_ds[,i] <- fire_mat_dt[,i]  <- rep(0, num_times)
    fire_grid_proponfire <- append(fire_grid_proponfire, 0)
    next #move to next grid cell without doing whole loop 
  }
  
  temp_frp_vec <- temp_ds_vec <- temp_dt_vec <-  numeric()
  temp_pix_mat <- matrix(NA, ncol = 2, nrow = num_times)
  
  
  for (j in 1:num_times){
    temp_time <- time_stamps[j]
    temp_frp <- detecs_frp[detecs_frp$time == temp_time, ]
    #number of fire pixels in grid cell 
    temp_pix_num <- dim(st_filter(temp_frp, temp_poly))[1]
    if (temp_pix_num >0 ){
      # total average FRP / pixel / cell 
      temp_pix <-  st_filter(temp_frp, temp_poly)
      total_frp <- sum(temp_pix$frp)
      av_frp <- total_frp/ temp_pix_num
      av_ds <- sum(temp_pix$DS)/temp_pix_num
      av_dt <- sum(temp_pix$DT)/temp_pix_num
      pix_ind <- 1 
    } else {
      total_frp = 0 
      av_frp <- av_ds <- av_dt <-  0 
      pix_ind <- 0 
    }
    temp_frp_vec  <- c(temp_frp_vec, av_frp) #taking the arithmetic mean of the FRP if multiple detections per cell 
    temp_ds_vec  <- c(temp_ds_vec, av_ds) 
    temp_dt_vec  <- c(temp_dt_vec, av_dt) 
    temp_pix_mat[j,] <- c(temp_pix_num, pix_ind)
  }
  
  
  #add the cell summaries to the vectors/ matrices including proportion of time that a fire was detected in a given grid cell 
  time_on_fire <- mean(temp_pix_mat[,2])
  fire_mat_frp[,i] <-  temp_frp_vec
  fire_mat_ds[,i] <-  temp_ds_vec
  fire_mat_dt[,i] <-  temp_dt_vec
  fire_mat_indpix[,i] <- temp_pix_mat[,2]
  fire_mat_numpix[,i] <- temp_pix_mat[,1]
  fire_grid_proponfire <- append(fire_grid_proponfire, time_on_fire)
  
  if (floor(i/100 ) ==( i/100) ){
    print(paste(i, " out of ", num_cells, sep = ""))
  }
  
}
cells_with_fire_ind <- (fire_grid_proponfire>0)
cells_with_fire <- which(fire_grid_proponfire>0)
num_fire_cells <- length(cells_with_fire)

### define matrix/ data.frame with cell summaries. 
### time spent on fire 
fire_grid_props <- data.frame(fire_grid_proponfire)
colnames(fire_grid_props) <- "FIRE_PROP"
fire_grid_props$FIRE_PROP_NA <- sapply(fire_grid_props$FIRE_PROP,  FUN = function(x) ifelse(x == 0 , NA, x ) ) 
fire_grid_props$FIRE_STEPS_NA <- num_times*fire_grid_props$FIRE_PROP_NA
fire_grid_props$FIRE_PERCENT_NA <- fire_grid_props$FIRE_PROP_NA*100

if (num_times > 1){
  ###FRP values 
  fire_grid_props$TEMP_TOT_FRP_NA <- apply(fire_mat_frp[,c(1:num_cells)], 2, sum) 
  fire_grid_props$TEMP_TOT_FRP_NA <-   sapply(fire_grid_props$TEMP_TOT_FRP_NA,  FUN = function(x) ifelse(x == 0 , NA, x )) 
  fire_grid_props$log_TEMP_TOT_FRP <- log(fire_grid_props$TEMP_TOT_FRP_NA)
  fire_grid_props$TEMP_AV_FRP_NA <- apply(fire_mat_frp[,c(1:num_cells)], 2, temp_mean_fun) 
  fire_grid_props$TEMP_AV_DS <- apply(fire_mat_ds[,c(1:num_cells)], 2, temp_mean_fun) 
  fire_grid_props$TEMP_AV_DT <- apply(fire_mat_dt[,c(1:num_cells)], 2, temp_mean_fun) 
  fire_grid_props$TEMP_TOT_DS <- apply(fire_mat_ds[,c(1:num_cells)], 2, sum) 
  fire_grid_props$TEMP_TOT_DT <- apply(fire_mat_dt[,c(1:num_cells)], 2, sum) 
  fire_grid_props$TEMP_AV_FRP_NA <- sapply(fire_grid_props$TEMP_AV_FRP_NA,  FUN = function(x) ifelse(x == 0 , NA, x ) )
  fire_grid_props$log_TEMP_AV_FRP <- log(fire_grid_props$TEMP_AV_FRP)
} else if ( num_times == 1 ){
  fire_grid_props$TEMP_TOT_FRP_NA <- t(fire_mat_frp)
  fire_grid_props$log_TEMP_TOT_FRP_NA <- log(fire_grid_props$TEMP_TOT_FRP_NA)
  fire_grid_props$log_TEMP_TOT_FRP_NA <- sapply(fire_grid_props$log_TEMP_TOT_FRP_NA,  FUN = function(x) ifelse(x == -Inf, NA, x ) )
  fire_grid_props$TEMP_AV_FRP_NA <- fire_grid_props$TEMP_TOT_FRP_NA
  fire_grid_props$TEMP_AV_DS <- t(fire_mat_ds)
  fire_grid_props$TEMP_AV_DT <- t(fire_mat_dt)
  fire_grid_props$TEMP_TOT_DS <- t(fire_mat_ds)
  fire_grid_props$TEMP_TOT_DT <- t(fire_mat_dt)
  fire_grid_props$log_TEMP_AV_FRP <- fire_grid_props$log_TEMP_TOT_FRP
}


###matrices also with time series of frp, cumulative frp, pixels per grid cell 
#time series of FRP of each of the grid cells 
fire_mat_frp <- data.frame(fire_mat_frp)
fire_mat_ds <- data.frame(fire_mat_ds)
fire_mat_dt <- data.frame(fire_mat_dt)
fire_mat_frp$time <- fire_mat_ds$time <- fire_mat_dt$time <- c(1:num_times)
#fire_mat_frpNA <- apply(fire_mat_frp, c(1,2), function(x) ifelse(x== 0, NA, x))
fire_mat_cumtotfrp <- apply(fire_mat_frp, 2, cumsum)
fire_mat_logfrp <- log(fire_mat_frp)
fire_mat_logfrp <- apply(fire_mat_logfrp, c(1,2), function(x) ifelse(x== -Inf, NA, x))
fire_mat_logfrp <- data.frame(fire_mat_logfrp)
fire_mat_logfrp$time <- c(1:num_times)


### standardising time series to start at time 1, only include cells with actual detections, include cell reference, hour at first detection, time at first detection 
if (num_times > 1 ){
  fire_frp_ts <- apply(fire_mat_frp, 2, frp_move_to_zero, times = num_times, stamps = time_stamps )
  fire_frp_ts <- fire_frp_ts[rowSums(is.na(fire_frp_ts))<ncol(fire_frp_ts),]
  fire_frp_ts <- data.frame(fire_frp_ts, row.names = NULL)
  fire_frp_ts$time <- c(1,hour(time_stamps[1]), c(1:(nrow(fire_frp_ts)-2)))
} else {
  fire_frp_ts <- NA 
}


print("Plotting FRP temporal summaries over the gridded region.")

### plots (probably move below )
plot_total_frp <- plot_grid_summaries(data_grid = detecs_grid , 
                                      data_color = fire_grid_props$TEMP_TOT_FRP, 
                                      fill_label = "Total FRP \nper cell" , 
                                      title_label = "Temporal total FRP per 500m grid cell",
                                      col_option = "plasma" ,
                                      direc_option = -1)


plot_log_total_frp <- plot_grid_summaries(data_grid = detecs_grid , 
                                          data_color = fire_grid_props$log_TEMP_TOT_FRP, 
                                          fill_label = "log-Total FRP \nper cell (base e)" , 
                                          title_label = "Temporal log-total FRP per 500m grid cell",
                                          col_option = "inferno",
                                          direc_option = -1)



plot_fire_percent <- plot_grid_summaries(data_grid = detecs_grid , 
                                         data_color = fire_grid_props$FIRE_PERCENT_NA, 
                                         fill_label = "Percentage" , 
                                         title_label = "Percentage of time each cell was on fire",
                                         col_option ="magma"  ,
                                         direc_option = -1)

plot_fire_steps <- plot_grid_summaries(data_grid = detecs_grid , 
                                       data_color = fire_grid_props$FIRE_STEPS_NA, 
                                       fill_label = "Number" , 
                                       title_label = "Number of time stamps each cell was on fire",
                                       col_option = "mako",
                                       direc_option = -1)

### put plots into easily accessibly list 
plot_list <- list(plot_total_frp, plot_log_total_frp, plot_fire_percent, plot_fire_steps)
names(plot_list) <- c("total_frp", "log_total_frp" , "fire_percent", "fire_steps")


print("Defining quadrants to speed up cell number labelling.")

#define quadrants / octants ? - do east/north because some of the perimeters go funny with lon/lat 
# want to make a 3x2 grid across rectangle of final fire outline to speed up find_cell process 
num_quads = w*h
quads_east <- seq(east_range[1], east_range[2],  length.out = w+1) 
quads_north <- seq(north_range[1], north_range[2], length.out = h+1)

quadrants <- expand.grid(x = quads_east , y = quads_north)

lr <- quadrants[-c(1:(w+1)),]
ul <- quadrants[c(1:(nrow(quadrants)-(w+1))),]

corners <- cbind(upper_left=ul, lower_right=lr)
corners <- corners[1:nrow(corners) %% w != 0,]
rownames(corners) <- NULL
head(corners)

quad_polys <- st_make_grid(st_as_sf(quadrants, coords = c("x", "y"), crs = EPSGUTM), n=c(w,h))

#browser() 

quad_polys_sf <- data.frame(quad_polys) %>% st_as_sf()

### make list of which grid cells are within each quadrant so that can more easily search when doing 'find cell' 
detecs_quadrant_grid_cells <- list()
for (i in 1:num_quads){
  temp_list <- st_intersects(detecs_grid_utm, quad_polys[[i]], sparse = F) 
  detecs_quadrant_grid_cells[[i]] <- detecs_grid_utm[temp_list,]
}

#browser()

print("Loading landfire layers, re-projecting and assigning grid cell label")

## topology 
#number of layers (length of paths vector )
num_layers <- length(landfire_names)
road_index = which(landfire_names== "road")

### load evt first, so can test if same length as other layers, then don't need to duplicate the find cell bit because this is what takes time 
temp_evt_raster <-  raster(landfire_evt_layer )
temp_evt_raster <- projectRaster(temp_evt_raster, crs=EPSGUTM, method = "ngb")
temp_evt_cropped <-  raster::mask(temp_evt_raster,as_Spatial(final_perim_utm))
evt_df  <- raster::as.data.frame(temp_evt_cropped , xy=TRUE , na.rm=T )
colnames(evt_df)[3] <- "value"
evt_df <- st_as_sf(evt_df, coords  = c("x", "y"), remove = F)
evt_df <- evt_df %>% st_set_crs(EPSGUTM)
## only want to assign grid cell label once so will do this first with EVT layer and then just copy 


### make list of which pixels are within each quadrant so that can more easily search when doing 'find cell' 
pixel_quadrants_subset <- list()
for (i in 1:num_quads){
  temp_list <- st_intersects(evt_df, quad_polys[[i]], sparse = F) 
  pixel_quadrants_subset[[i]] <- evt_df[temp_list,]
  #print(i)
}

##now want to apply find cell function / apply st_intersects for each of the 10 subsets
#Sys.time()
for (i in 1:num_quads){
  temp_layer <-  pixel_quadrants_subset[[i]]
  temp_fire_grid <- detecs_quadrant_grid_cells[[i]]
  temp_layer$cell <- sapply(temp_layer$geometry, find_cell, polys =  temp_fire_grid)
  print(i)
  pixel_quadrants_subset[[i]] <- temp_layer 
}
#Sys.time()
evt_df <- do.call(rbind, pixel_quadrants_subset)
temp_obs_with_cell <- which(!is.na(evt_df$cell))

### assign grid cell to each of the layers 
landfire_list <- list()
landfire_tifs_list <- list()
for (i in 1:num_layers){ 
  temp_raster <- raster(landfire_layers, band = i )
  temp_raster <- projectRaster(temp_raster, crs=EPSGUTM, method = "ngb")
  temp_cropped <-  raster::mask(temp_raster,as_Spatial(final_perim_utm ))
  landfire_tifs_list[[i]] <- temp_cropped 
  landfire_list[[i]] <- raster::as.data.frame(temp_cropped , xy=TRUE , na.rm=T )
  colnames(landfire_list[[i]])[3] <- "value"
  landfire_list[[i]] <- st_as_sf(landfire_list[[i]], coords  = c("x", "y"), remove = F)
  landfire_list[[i]] <- landfire_list[[i]] %>% st_set_crs(EPSGUTM)
  ### add in this because we only want to use the find_cell function once because it takes a long time and then just copy it to the other layers. usually the landfire layers are mapped onto the same grid so we can just duplicate the cell variable 
  if (sum(evt_df$x)  == sum(landfire_list[[i]]$x)){
    landfire_list[[i]]$cell <- evt_df$cell 
  } else{
    #this shouldn't ever happen, I downloaded the data from landfire altogether so the points should be the same all the time . it will also take ages so really hope this doesn't happen 
    print("Calculating grid cells for a second landfire layer. ")
    landfire_list[[i]]$cell <- sapply(temp_cropped$geometry, find_cell, polys = detecs_grid_utm)
  }
  #get rid of pixels not in any cell 
  landfire_list[[i]] <- landfire_list[[i]][temp_obs_with_cell,]
}
names(landfire_list) <- landfire_names
names(landfire_tifs_list) <- landfire_names
evt_df <- evt_df[temp_obs_with_cell,]

num_lanfir_pixels <-  nrow(landfire_list[[1]])

#browser()

print("Assigning labels to numerical categorical variables (namely Road, EVT).")
road_class <- read.csv("Roads_220.csv")
evt_class1 <-read.csv("EVT_CLASS1.csv")
evt_class2 <- read.csv("EVT_CLASS2.csv")


### assign category names to roads and EVT 
if (!is.na(road_index)){
  ## recode 
  landfire_list[[road_index]]$label <- admisc::recode(as.factor(landfire_list[[road_index]]$value), paste(road_class$Value," = '",road_class$Class_Name, "'", sep = "" , collapse= ";" ))
  
  ## re define as factors 
  landfire_list[[road_index]]$label <- factor(landfire_list[[road_index]]$label , levels = unique(landfire_list[[road_index]]$label ))
}

## check which set of evt classfications to use
if (max(evt_df$value) < min(evt_class2$VALUE[evt_class2$VALUE>0 ])){
  evt_class <- evt_class1
} else{
  evt_class <- evt_class2
}


evt_df$label <- admisc::recode(as.factor(evt_df$value), paste(evt_class$VALUE," = '",evt_class$EVT_LF, "'", sep = "" , collapse= ";" ))
## re define as factors 
evt_df$label <- factor(evt_df$label , levels = unique(evt_df$label ))

#browser()

### defining number of the landfire layers which are categorical vs continuous. EVT layer not included in this because i downloaded the tif files separately   
num_categorical <- length(which(landfire_types == 0 ))
num_continuous <- length(which(landfire_types == 1 ))

### number of levels in each of the factor / categorical variables, then additional layer for 2nd variable derived from EVT 
num_categories <- numeric()
for (i in 1:num_layers){
  levs <- ifelse( landfire_types[i] ==0, nlevels(landfire_list[[i]]$label), NA )
  num_categories <- c(num_categories, levs )
}

### define matrices for each of the landfire variable summaries  
fire_grid_evt <- matrix(NA, nrow= num_cells, ncol = nlevels(evt_df$label))
fire_grid_layers <- vector("list" , length= (num_layers) )
for (k in 1:num_layers){
  if (landfire_types[k] == 0){
    fire_grid_layers[[k]] <- matrix(NA, nrow = num_cells, ncol = num_categories[k] )
    colnames( fire_grid_layers[[k]]) <- levels(landfire_list[[k]]$label)
  } else {
    fire_grid_layers[[k]] <- matrix(NA, nrow = num_cells, ncol = 8 )
    colnames(fire_grid_layers[[k]])  <- c("mean", "variance", "range", "min", "low_quart", "median", "upp_quart", "max")
  }
}

print("Landfire layer loop through grid cells, making summaries")                                             

for  (i in 1:num_cells){
  
  #get the points of the raster that are within each of the grid cells and their respective properties , although the grid cells actually describe a 30m square pixel and not a point 
  
  #only going to look at 2 or 3 of the quadrants at most which should really speed up the process 
  temp_inds <- which(landfire_list[[1]]$cell == i)
  temp_dat <- lapply(landfire_list, FUN = function(x) x <- x[temp_inds,]) 
  
  ## will assign the different layers different depending on continuous or categorical (input vector is 0 = cat, 1= cont )
  ##first do categorical layers 
  for (k in 1:num_categorical) {
    #number of categories that we expect to be in our table - need to be careful about this, need to make sure the variable is coded to have all X layers so taht when we make a table we get a X number table and not just like 1 or 2 numbers if only say water and developed are present. 
    num_categs <- num_categories[landfire_types == 0][k]
    
    #which layer (out of categorical and continuous) are we working with - need this to assign prop.table 
    lay_num <- which(landfire_types==0 )[k]
    
    #table saying how many landfire observations in a given cell are of which type 
    temp_props <- prop.table(table(temp_dat[[lay_num]]$label)) 
    
    ## add to output list noting that we need to move the second EVT thing to the end 
    fire_grid_layers[[lay_num]][i, ] <- temp_props
  }
  
  ##first do continuous layers 
  for (k in 1:num_continuous) {
    #layer number out of all layers 
    lay_num <- which(landfire_types==1 )[k]
    
    #landfire observations within given grid cell 
    dat <- temp_dat[[lay_num]]$value
    
    #assign basic summary stats and 5 number summary to correct row of matrix 
    fire_grid_layers[[lay_num]][i, ] <-  c(mean(dat, na.rm=T), 
                                           var(dat, na.rm=T) , 
                                           diff(range(dat)), 
                                           quantile(dat, c(0, 0.25, 0.5, 0.75, 1))) 
  }
  
  temp_evt_dat <- evt_df[evt_df$cell==i ,]
  fire_grid_evt[i,] <- prop.table(table(temp_evt_dat$label)) 
  
  ### print to say where we are in loop of assigning topography etc. actually not very helpful because the long bit is the grid cell reference and not this iterative thing but oh well. 
  if (floor(i/100 ) == i/100){
    print(paste(i, " out of ", num_cells, sep = ""))
  }
  
}
colnames(fire_grid_evt) <- levels(evt_df$label)
colnames(fire_grid_layers[[road_index]]) <- levels(landfire_list[[road_index]]$label)
names(fire_grid_layers) <- landfire_names

print("Loading biomass layer.")


#### load biomass layer (gpkg file ), crop so that we only have the bits within our final fire perimet 
biomass <- st_read(biomass_data)
biomass <- biomass[st_intersects(biomass, buffed_perim_utm_poly, sparse =F),]
colnames(biomass)[3] <- "value"

#### add grid cell to biomass layer 
### make list of which pixels are within each quadrant so that can more easily search when doing 'find cell' 
biomass_quadrants_subset <- list()
for (i in 1:num_quads){
  temp_list <- st_intersects(biomass, quad_polys[[i]], sparse = F) 
  biomass_quadrants_subset[[i]] <- biomass[temp_list,]
  #print(i)
}

##now want to apply find cell function / apply st_intersects for each of the 10 subsets
Sys.time()
for (i in 1:num_quads){
  temp_layer <-  biomass_quadrants_subset[[i]]
  temp_fire_grid <- detecs_quadrant_grid_cells[[i]]
  temp_layer$cell <- sapply(temp_layer$geom, find_cell, polys =  temp_fire_grid)
  print(i)
  biomass_quadrants_subset[[i]] <- temp_layer 
}
Sys.time()
biomass <- do.call(rbind, biomass_quadrants_subset)
biomass <- biomass[!is.na(biomass$cell), ] 
#num_bio_pixels <-  nrow(biomass )

print("Biomass loop through grid cells, making summaries")                                             

#predefine matrix 
fire_grid_biomass <- matrix(NA, nrow= num_cells , ncol = 9 )

#summarise biomass by grid cell 
for  (i in 1:num_cells){
  
  #get the points of the raster that are within each of the grid cells and their respective properties , although the grid cells actually describe a 30m square pixel and not a point 
  #only going to look at 2 or 3 of the quadrants at most which should really speed up the process 
  temp_dat <- biomass$value[biomass$cell == i ]
  
  if (length(temp_dat) == 0 ) { fire_grid_biomass[i, ] <-  rep(NA, 9 ) ; next } 
  
  #summarise biomass over the grid cell 
  fire_grid_biomass[i, ] <-  c(mean(dat, na.rm=T),
                               var(temp_dat, na.rm=T) , 
                               diff(range(temp_dat)), 
                               sum(temp_dat),
                               quantile(temp_dat, c(0, 0.25, 0.5, 0.75, 1))) 
  
  
  ### print to say where we are in loop of assigning topography etc. actually not very helpful because the long bit is the grid cell reference and not this iterative thing but oh well. 
  if (floor(i/100 ) == i/100){
    print(paste(i, " out of ", num_cells, sep = ""))
  }
  
}
colnames(fire_grid_biomass)  <- c("mean" ,"variance", "range", "sum", "min", "low_quart", "median", "upp_quart", "max")


###
output <- list("frp_detecs" = detecs_frp , 
               "grid_polys" = detecs_grid_utm , 
               "grid_props" = fire_grid_props, 
               "length_grid" = num_cells ,
               "num_fire_cells" = num_fire_cells, 
               "frp_ts" = fire_frp_ts, 
               "frp_matrix" = fire_mat_frp,   
               "cumu_frp_matrix" =  fire_mat_cumtotfrp,  
               "detec_ind_mat" = fire_mat_numpix, 
               "ds_matrix" =fire_mat_ds , 
               "dt_matrix" = fire_mat_dt,
               "lf_rasters" = landfire_tifs_list, 
               "evt_raster" = temp_evt_cropped,
               "lf_data" = landfire_list, 
               "evt_data" = evt_df, 
               "grid_lf"  = fire_grid_layers , 
               "grid_evt" = fire_grid_evt, 
               "biom_data" = ,
                "biom_raster" ,
               "grid_biom" = fire_grid_biomass,
               "frp_plots" = plot_list , 
               "landfire_plots" = plot_list )




#### saving output #### 
    
save(output, file = paste0("/Output/TEST_fire_",year, "_", id , "_frp_info.RData"))

print(paste0("Saved ", which(temp_fireids == id ), " out of ", temp_num_fires, " total fires in ", year))

    
    
    
    



>>>>>>> acc7d65ceb8143a7d79f73f099f4afc5aefacd12
