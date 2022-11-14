# Paul Cleary
# EAPS 515
# Professors Tung & Cleveland

library(tidyverse)
library(data.table)
library(lattice)
library(classInt)
library(sf)
#################################
#-----Import the data-----
crime_data <- fread("ChiCrime1000.csv") 
#################################

#############################################
#------Divide the data by primary type-------
#############################################

# This is similar to the map reduce step in RHipe
crime_data_ls <- crime_data %>% group_split(`Primary Type`)

#####################################################
#------Count incidence of each primary type----------
#Function to extract the data from the list of divided data
#####################################################


num_prim_typ_func <- function(x) {
  type = x$`Primary Type`[1]
  num = dim(x)[1]
  df = data.frame(Occurance_by_Type = num, `Primary Type` = type)
  df1 = data.frame()
  return(df)
}

num_prim_typ <- lapply(crime_data_ls,num_prim_typ_func)

########################################################
#--------------Primary type by ward--------------------- 
#function to count the number of primary type occurances 
#by ward then arrange them in descending order  
########################################################

prim_typ_ward_func <- function(x) {
  type = x$`Primary Type`[1]
  by_ward = x %>% count(Ward) %>% arrange(desc(n))
  df = data.frame(ward = by_ward$Ward, Count_byWard = by_ward$n, Primary_Type = type)
  return(df)
}

prim_typ_by_ward <- lapply(crime_data_ls, prim_typ_ward_func)

#################################################################
#-----Determine the mean hour of the day crimes are commited----
#-----For the whole city and by ward----------------------------
#################################################################

avg_crime_hr_func <- function(x) {
x$Date = as.POSIXct(strptime(x$Date, "%m/%d/%Y %I:%M:%S %p"))
x = as.data.table(x)
x = x[, hour_of_day := as.integer(strftime(Date, "%H"))]
x = x[, avg_hr_crime_byward := mean(hour_of_day), by = .(Ward)][, avg_hr_crime_ctywide := mean(hour_of_day)][,.(Ward,avg_hr_crime_byward,avg_hr_crime_ctywide, hour_of_day,`Primary Type`)]
return(x)
}

avg_crime_hr <- lapply(crime_data_ls, avg_crime_hr_func)

##########################################################
#----Graph of crimes by type by hour of day --------------
# Produce a histogram of crimes commited by hour of the day
##########################################################

crimes_by_hr_func <- function(x) {
crime = x$`Primary Type`[1]
histogram(~hour_of_day,
          data = x,
          xlab="Hour of the Day",
          ylab="Number of Crimes",
          sub = paste(crime,": Committed by Hour of the Day"))
}

crimes_by_hour_hists <- lapply(avg_crime_hr, crimes_by_hr_func)

########################################################################
#-----Is there a correlation between time of crime and geography?-------
# Define a function to break the day in to 4 seperate parts
########################################################################

am_pm_func <- function(x){
  if(x >= 0 & x < 6){
    x = "Early Morning"
  } else if(x >= 6 & x < 12) {
    x = "Morning"
  } else if(x >= 12 & x < 18) {
    x = "Afternoon"
  } else x = "Night"
}

#function to plot crimes by location and time commited
loc_vs_time_func <- function(x){
  crime = x$`Primary Type`[1]
  x$Date = as.POSIXct(strptime(x$Date, "%m/%d/%Y %I:%M:%S %p"))
  x = as.data.table(x)
  x = x[, hour_of_day := as.integer(strftime(Date, "%H"))]
  x <- x %>% mutate(day_4_zones = sapply(x$hour_of_day,am_pm_func))
  
  map <-  xyplot(Latitude~Longitude,
             groups = as.factor(day_4_zones),
             data = x,
             auto.key = list(columns=4),
             type = "p",
             scales = list(y=list(tick.number=8)),
             xlab="Longitude (deg E)",
             ylab="Latitude (deg N)",
             sub = paste(crime,"Crimes by Location & Time"))
  return(map)
}

loc_vs_time_plot <- lapply(crime_data_ls, loc_vs_time_func)

################################################################
#--------------Choropleth Map of Wards and Crimes--------------- 
################################################################

#Read in the shapefile chicago Wards
wards_shp <- st_read("Boundaries_Wards/geo_export_968053d1-d5ac-4589-afa9-25f2d11d8785.shp")

#Create a function to create a chloropleth map of the Wards by Primary Type occurance
choropleth_map_func <- function(a){
  a <- prim_typ_ward_func(a)
  a$Count_byWard <- as.numeric(a$Count_byWard)
  a$ward <- as.numeric(a$ward)
  wards_shp$ward <- as.numeric(wards_shp$ward)
  wards_shp <- wards_shp %>% left_join(a, by = "ward")
  crime = a$Primary_Type[1]
  if(unique(a$Count_byWard >= 4)){
    breaks <- classIntervals(unique(wards_shp$Count_byWard), n= 4, style = "quantile")
    wards_shp$breaks <-cut(wards_shp$Count_byWard, breaks = breaks$brks, include.lowest = TRUE)
  choropleth_map_crime_type <- ggplot() +
    geom_sf(data = wards_shp,
            aes(fill = breaks),
            alpha = 0.8,
            colour = 'white',
            size = 0.3) +
    scale_fill_brewer(palette = "PuBu",
                      name = "Crime Count by Ward") +
    labs(x = NULL, y = NULL,
         title = paste(crime,"Crimes by Ward 24-25 September 2019")) +
    theme(line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank()) +
    coord_sf(datum = NA)
  } else {
    choropleth_map_crime_type <- ggplot() +
      geom_sf(data = wards_shp, 
              mapping = aes(fill = Primary_Type),
              alpha = 0.8,
              colour = 'white',
              size = 0.3) +
      labs(x = NULL, y = NULL,
           title = paste(crime,"Crimes by Ward 24-25 September 2019")) +
      theme(line = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.background = element_blank())
    }
    return(choropleth_map_crime_type)
}

Choropleth_maps <- lapply(avg_crime_hr, choropleth_map_func)

