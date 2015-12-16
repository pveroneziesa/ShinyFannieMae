#Author: Pedro Veronezi
#email: veronezi.pedro@gmail.com
#date: 11/12/15
#last update: 11/12/15

#Description
#This script works only with for this application and has as objective load all libraries and
#functions to draw the graphs for this development


#load the functions to get the data
source("functions.R")
source("helpers.R")
library(maps)
library(mapproj)
library(maptools)
library(ggplot2)
library(ggmap)
library(choroplethrMaps)
library(choroplethr)
library(R6)
library(RgoogleMaps)


#function to generate the map and apply a layer of google maps on top of it
#it HAS to use a datafram compatible
generatemap <- function(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  zip_data_set <- getdata_zip(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  
  #creating a map with google maps behind
  
  ChoroplethSatellite = R6Class("ChoroplethSatellite",inherit = StateChoropleth,
                                public = list(
                                  
                                  get_reference_map = function()
                                  {
                                    # note: center is (long, lat) but MaxZoom is (lat, long)
                                    
                                    center = c(mean(self$choropleth.df$long+10),
                                               mean(self$choropleth.df$lat))
                                    
                                    max_zoom = MaxZoom(range(self$choropleth.df$lat),
                                                       range(self$choropleth.df$long))
                                    
                                    get_map(location = center,
                                            zoom    = max_zoom,
                                            maptype = "roadmap", #can be hybrid / roadmap, satellite
                                            color   = "color")
                                  }
                                )
  )
  
  #create a new instance for the map
  c = ChoroplethSatellite$new(zip_data_set)
  #c$set_num_colors(4)
  #c$title  = "2013 New York City ZIP Code Tabulated Areas"
  #c$legend = "Per Capita Income"
  c$render_with_reference_map()
  rm(state.regions, zipcode, pos=".GlobalEnv")
}


generatemap_county <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  zip_data_set <- getdata_zip(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  
  percent_map(zip_data_set$value,"steelblue","% # LOANS")
  
}



generate_bcs_hist <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  bcs_data_set <- getdata_hist_bcs(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  
  p <- ggplot(data = bcs_data_set, aes(x=bin,y=count))+ geom_bar(stat="identity",fill="steelblue") + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
  
  p
}


generate_oir_hist <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  oir_data_set <- getdata_hist_oir(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  
  p <- ggplot(data = oir_data_set, aes(x=bin,y=count))+ geom_bar(stat="identity",fill="steelblue") + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
  
  p
}


generate_olv_hist <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  olv_data_set <- getdata_hist_olv(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  
  p <- ggplot(data = olv_data_set, aes(x=bin,y=count))+ geom_bar(stat="identity",fill="steelblue") + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
  
  p
}


generate_dir_hist <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  dir_data_set <- getdata_hist_dir(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  
  p <- ggplot(data = dir_data_set, aes(x=bin,y=count))+ geom_bar(stat="identity",fill="steelblue") + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())
  
  p
}

generate_banks <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  banks_data_list <- getdata_banks_list(year_lower, year_upper)
  
  banks_data_set <- getdata_banks(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks)
  
  banks_data_set$sn <- word(banks_data_set$sn,1)
  
  p <- ggplot(data = banks_data_set, aes(x=sn,y=count))+ geom_bar(stat="identity",fill="steelblue") + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank()) + coord_flip()
  
  p
}


generate_yearly <- function (){
  
  result <- getdata_yearly()
  p <- ggplot(data = result, aes(x=YEAR,y=Loans))+ geom_bar(stat="identity",fill="steelblue") + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10),panel.background = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+geom_text(aes(label=Loans), vjust=1.6, color="white", size=3.5)
  p
}


