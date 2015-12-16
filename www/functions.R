#Author: Pedro Veronezi
#email: veronezi.pedro@gmail.com
#date: 11/11/15
#last update: 11/12/15

#Description
#This script works only with for this application and has as objective load all libraries and
#functions for this development

#load libraries
library(RPostgreSQL)
library(stringr)
library(plyr)
library(zipcode)



#load functions

#This function has the objetive to get the fannie mae data from postgre data base, considering
#all different variables, with being able to crossfilter the data over the graphs
getdata_hist_bcs <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){

  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  bcs_l <- as.double(bcs_lower)
  bcs_u <- as.double(bcs_upper)
  
  oir_l <- as.double(oir_lower)
  oir_u <- as.double(oir_upper)
  
  olv_l <- as.double(olv_lower)
  olv_u <- as.double(olv_upper)
  
  dir_l <- as.double(dir_lower)
  dir_u <- as.double(dir_upper)
  
  bank_n <- bank_name
  
  #builds the query for the bank filtration
  if (all_banks == TRUE){
    query_bank = paste0(" ")
  }else{
    query_bank = paste0(" and (sn in ('",bank_name,"'))")
  }
  
  #gets the first query in the database 
  result <- dbGetQuery(con,paste0("select width_bucket(bcs,500,800,28),count(*) from ",list_acquisition[year_l]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
  
  #does the looping to get all years necessary
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select width_bucket(bcs,500,800,28),count(*) from ",list_acquisition[i]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
    result <- merge(result,temp,by="width_bucket")
  }
  
  #does the summation through the lines 
  for (i in 1:length(result[,1])){
    result[i,(2+year_diff+1)] <- sum(result[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  result<-result[,-(2:(2+year_diff))]
  
  #takes out the last line which are the NA values

  #creates the matrix that is goint to be matched and parsed as result
  hist_table <- matrix(nrow=30,ncol=2)
  
  ###hist_table <- as.data.frame(c("500 - 510", "510 - 520","520 - 530","530 - 540","540 - 550","550 - 560","560 - 570","570 - 580","580 - 590","590 - 600","600 - 610","610 - 620","620 - 630","630 - 640","640 - 650","650 - 660","660 - 670","670 - 680","680 - 690","690 - 700","700 - 710","710 - 720","720 - 730","730 - 740","740 - 750","750 - 760","760 - 770","770 - 780","780 - 790","790 - 800"))
  #enumerate the lines to match/merge with the database result, this step was necessary since the
  #result for the query, when the data is too restrict, do not full fill the graph, leadin to errors
  hist_table[,1] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29)
  hist_table[,2] <- 0
  
  #merge the two data.frames
  hist_table <- merge(hist_table,result,by.x="V1",by.y="width_bucket",all.x=TRUE)
  colnames(hist_table) <- c("bin","exclude","count")
  
  #order the matrix
  hist_table <- hist_table[order(hist_table$exclude),]
  
  #change the buckets values
  hist_table[,1] <- c("500 - 510", "510 - 520","520 - 530","530 - 540","540 - 550","550 - 560","560 - 570","570 - 580","580 - 590","590 - 600","600 - 610","610 - 620","620 - 630","630 - 640","640 - 650","650 - 660","660 - 670","670 - 680","680 - 690","690 - 700","700 - 710","710 - 720","720 - 730","730 - 740","740 - 750","750 - 760","760 - 770","770 - 780","780 - 790","790 - 800")
  
  #deletes the second column
  hist_table <- hist_table[,-2]
  
  #replace the NA values with zero and changes the col names for the dataframe
  hist_table[is.na(hist_table)] <- 0
  
  #disconnects from the database
  dbDisconnect(con)
  
  return (hist_table)
  
}


#this function has as objective return the values for the map construction, its get the values summarized by zip code
#cross that reference with a rpackage database called zipcode, and then gets the lat and lon, so from that data,
#it gets the county reference, because the helpers.R file uses as input the data in this format
getdata_zip <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  bcs_l <- as.double(bcs_lower)
  bcs_u <- as.double(bcs_upper)
  
  oir_l <- as.double(oir_lower)
  oir_u <- as.double(oir_upper)
  
  olv_l <- as.double(olv_lower)
  olv_u <- as.double(olv_upper)
  
  dir_l <- as.double(dir_lower)
  dir_u <- as.double(dir_upper)
  
  bank_n <- bank_name
  
  #builds the query for the bank filtration
  if (all_banks == TRUE){
    query_bank = paste0(" ")
  }else{
    query_bank = paste0(" and (sn in ('",bank_name,"'))")
  }
  
  #gets the first query in the database 
  result <- dbGetQuery(con,paste0("select z,count(*) from ",list_acquisition[year_l]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by z"))
  
  #does the looping to get all years necessary
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select z,count(*) from ",list_acquisition[year_l]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by z"))
    result <- merge(result,temp,by="z")
  }
  
  #does the summation through the lines 
  for (i in 1:length(result[,1])){
    result[i,(2+year_diff+1)] <- sum(result[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  result<-result[,-(2:(2+year_diff))]
  #format the zipcode to 3 digit
  result$z <- str_pad(result$z,3,pad="0")
  
  #get the list of zipcodes of USA and the relationship with the State
  setwd("C:/Users/PedroHenrique/OneDrive/Financial Engineering/Fannie Mae/fannie_mae/www")
  zip_state_county <- read.csv("www/zip_state_county.csv")
  #clean the data leaving just the 3 first digits
  zip_state_county[,2] <- substr(zip_state_county[,2],0,3)
  #remove all duplicates on the zip column
  zip_state_county <- zip_state_county[!duplicated(zip_state_county$region),]
  #clean the zipcode database
  zip_state_county <- zip_state_county[,-1]
  zip_state_county <- zip_state_county[,-(2:3)]

  result <- merge(result,zip_state_county,by.x="z",by.y="region")
  
  #clean the data.frame
  result <- result[,-1]
  colnames(result) <- c("value","region")
  
  result <- ddply(result,"region",transform,count=sum(value))
  result <- subset(result,!duplicated(region))
  result <- result[,-1]
  colnames(result) <- c("region","value")
  #coerce the data for the format required
  
  dbDisconnect(con)
  
  return(result)
}


#This function has the objetive to get the fannie mae data from postgre data base, considering
#all different variables, with being able to crossfilter the data over the graphs
getdata_hist_oir <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  bcs_l <- as.double(bcs_lower)
  bcs_u <- as.double(bcs_upper)
  
  oir_l <- as.double(oir_lower)
  oir_u <- as.double(oir_upper)
  
  olv_l <- as.double(olv_lower)
  olv_u <- as.double(olv_upper)
  
  dir_l <- as.double(dir_lower)
  dir_u <- as.double(dir_upper)
  
  bank_n <- bank_name
  
  #builds the query for the bank filtration
  if (all_banks == TRUE){
    query_bank = paste0(" ")
  }else{
    query_bank = paste0(" and (sn in ('",bank_name,"'))")
  }
  
  #gets the first query in the database 
  result <- dbGetQuery(con,paste0("select width_bucket(oir,3,12,16),count(*) from ",list_acquisition[year_l]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
  
  #does the looping to get all years necessary
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select width_bucket(oir,3,12,16),count(*) from ",list_acquisition[i]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
    result <- merge(result,temp,by="width_bucket")
  }
  
  #does the summation through the lines 
  for (i in 1:length(result[,1])){
    result[i,(2+year_diff+1)] <- sum(result[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  result<-result[,-(2:(2+year_diff))]
  
  #takes out the last line which are the NA values
  
  #creates the matrix that is goint to be matched and parsed as result
  hist_table <- matrix(nrow=18,ncol=2)
  
  ###hist_table <- as.data.frame(c("500 - 510", "510 - 520","520 - 530","530 - 540","540 - 550","550 - 560","560 - 570","570 - 580","580 - 590","590 - 600","600 - 610","610 - 620","620 - 630","630 - 640","640 - 650","650 - 660","660 - 670","670 - 680","680 - 690","690 - 700","700 - 710","710 - 720","720 - 730","730 - 740","740 - 750","750 - 760","760 - 770","770 - 780","780 - 790","790 - 800"))
  #enumerate the lines to match/merge with the database result, this step was necessary since the
  #result for the query, when the data is too restrict, do not full fill the graph, leadin to errors
  hist_table[,1] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
  hist_table[,2] <- 0
  
  #merge the two data.frames
  hist_table <- merge(hist_table,result,by.x="V1",by.y="width_bucket",all.x=TRUE)
  colnames(hist_table) <- c("bin","exclude","count")
  
  #order the matrix
  hist_table <- hist_table[order(hist_table$bin),]
  
  #change the buckets values
  hist_table[,1] <- c("3 - 3.5" , "3.5 - 4" , "4 - 4.5" , "4.5 - 5" , "5 - 5.5" , "5.5 - 6" , "6 - 6.5" , "6.5 - 7" , "7 - 7.5" , "7.5 - 8" , "8 - 8.5" , "8.5 - 9" , "9 - 9.5" , "9.5 - 10" , "10 - 10.5" , "10.5 - 11" , "11 - 11.5" , "11.5 - 12")
  
  #deletes the second column
  hist_table <- hist_table[,-2]
  
  #replace the NA values with zero and changes the col names for the dataframe
  hist_table[is.na(hist_table)] <- 0
  
  #disconnects from the database
  dbDisconnect(con)
  
  return (hist_table)
  
}


#all different variables, with being able to crossfilter the data over the graphs
getdata_hist_olv <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  bcs_l <- as.double(bcs_lower)
  bcs_u <- as.double(bcs_upper)
  
  oir_l <- as.double(oir_lower)
  oir_u <- as.double(oir_upper)
  
  olv_l <- as.double(olv_lower)
  olv_u <- as.double(olv_upper)
  
  dir_l <- as.double(dir_lower)
  dir_u <- as.double(dir_upper)
  
  bank_n <- bank_name
  
  #builds the query for the bank filtration
  if (all_banks == TRUE){
    query_bank = paste0(" ")
  }else{
    query_bank = paste0(" and (sn in ('",bank_name,"'))")
  }
  
  #gets the first query in the database 
  result <- dbGetQuery(con,paste0("select width_bucket(olv,30,100,12),count(*) from ",list_acquisition[year_l]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
  
  #does the looping to get all years necessary
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select width_bucket(olv,30,100,12),count(*) from ",list_acquisition[i]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
    result <- merge(result,temp,by="width_bucket")
  }
  
  #does the summation through the lines 
  for (i in 1:length(result[,1])){
    result[i,(2+year_diff+1)] <- sum(result[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  result<-result[,-(2:(2+year_diff))]
  
  #takes out the last line which are the NA values
  
  #creates the matrix that is goint to be matched and parsed as result
  hist_table <- matrix(nrow=14,ncol=2)
  
  ###hist_table <- as.data.frame(c("500 - 510", "510 - 520","520 - 530","530 - 540","540 - 550","550 - 560","560 - 570","570 - 580","580 - 590","590 - 600","600 - 610","610 - 620","620 - 630","630 - 640","640 - 650","650 - 660","660 - 670","670 - 680","680 - 690","690 - 700","700 - 710","710 - 720","720 - 730","730 - 740","740 - 750","750 - 760","760 - 770","770 - 780","780 - 790","790 - 800"))
  #enumerate the lines to match/merge with the database result, this step was necessary since the
  #result for the query, when the data is too restrict, do not full fill the graph, leadin to errors
  hist_table[,1] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)
  hist_table[,2] <- 0
  
  #merge the two data.frames
  hist_table <- merge(hist_table,result,by.x="V1",by.y="width_bucket",all.x=TRUE)
  colnames(hist_table) <- c("bin","exclude","count")
  
  #order the matrix
  hist_table <- hist_table[order(hist_table$exclude),]
  
  #change the buckets values
  hist_table[,1] <- c("30 - 35" , "35 - 40" , "40 - 45" , "45 - 50" , "50 - 55" , "55 - 60" , "60 - 65" , "65 - 70" , "70 - 75" , "75 - 80" , "80 - 85" , "85 - 90" , "90 - 95","95 - 100")
  
  #deletes the second column
  hist_table <- hist_table[,-2]
  
  #replace the NA values with zero and changes the col names for the dataframe
  hist_table[is.na(hist_table)] <- 0
  
  #disconnects from the database
  dbDisconnect(con)
  
  return (hist_table)
  
}


#all different variables, with being able to crossfilter the data over the graphs
getdata_hist_dir <- function (year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  bcs_l <- as.double(bcs_lower)
  bcs_u <- as.double(bcs_upper)
  
  oir_l <- as.double(oir_lower)
  oir_u <- as.double(oir_upper)
  
  olv_l <- as.double(olv_lower)
  olv_u <- as.double(olv_upper)
  
  dir_l <- as.double(dir_lower)
  dir_u <- as.double(dir_upper)
  
  bank_n <- bank_name
  
  #builds the query for the bank filtration
  if (all_banks == TRUE){
    query_bank = paste0(" ")
  }else{
    query_bank = paste0(" and (sn in ('",bank_name,"'))")
  }
  
  #gets the first query in the database 
  result <- dbGetQuery(con,paste0("select width_bucket(dir,1,64,11),count(*) from ",list_acquisition[year_l]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
  
  #does the looping to get all years necessary
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select width_bucket(dir,1,64,11),count(*) from ",list_acquisition[i]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by 1 order by 1"))
    result <- merge(result,temp,by="width_bucket")
  }
  
  #does the summation through the lines 
  for (i in 1:length(result[,1])){
    result[i,(2+year_diff+1)] <- sum(result[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  result<-result[,-(2:(2+year_diff))]
  
  #takes out the last line which are the NA values
  
  #creates the matrix that is goint to be matched and parsed as result
  hist_table <- matrix(nrow=13,ncol=2)
  
  ###hist_table <- as.data.frame(c("500 - 510", "510 - 520","520 - 530","530 - 540","540 - 550","550 - 560","560 - 570","570 - 580","580 - 590","590 - 600","600 - 610","610 - 620","620 - 630","630 - 640","640 - 650","650 - 660","660 - 670","670 - 680","680 - 690","690 - 700","700 - 710","710 - 720","720 - 730","730 - 740","740 - 750","750 - 760","760 - 770","770 - 780","780 - 790","790 - 800"))
  #enumerate the lines to match/merge with the database result, this step was necessary since the
  #result for the query, when the data is too restrict, do not full fill the graph, leadin to errors
  hist_table[,1] <- c(0,1,2,3,4,5,6,7,8,9,10,11,12)
  hist_table[,2] <- 0
  
  #merge the two data.frames
  hist_table <- merge(hist_table,result,by.x="V1",by.y="width_bucket",all.x=TRUE)
  colnames(hist_table) <- c("bin","exclude","count")
  
  #order the matrix
  hist_table <- hist_table[order(hist_table$exclude),]
  
  #change the buckets values
  hist_table[,1] <- c("1 - 6" , "6 - 11" , "11 - 16" , "16 - 21" , "21 - 26" , "26 - 31" , "31 - 36" , "36 - 41" , "41 - 46" , "46 - 51" , "51 - 56" , "56 - 61" , "61 - 66")
  
  #deletes the second column
  hist_table <- hist_table[,-2]
  
  #replace the NA values with zero and changes the col names for the dataframe
  hist_table[is.na(hist_table)] <- 0
  
  #disconnects from the database
  dbDisconnect(con)
  
  return (hist_table)
  
}


#all different variables, with being able to crossfilter the data over the graphs
getdata_banks_list <- function (year_lower, year_upper){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  #get the list of the the banks that sold more mortgages, limited to 20 entries
  banks_list <- dbGetQuery(con,paste0("select sn, count(sn) from ",list_acquisition[year_l]," group by sn order by count desc limit 20"))
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select sn, count(sn) from ",list_acquisition[i]," group by sn order by count desc limit 20"))
    banks_list <- merge(banks_list,temp,by.x="sn",by.y="sn",all=TRUE)
  }
  #does the summation through the lines 
  for (i in 1:length(banks_list[,1])){
    banks_list[i,(2+year_diff+1)] <- sum(banks_list[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  banks_list<-banks_list[,-(2:(2+year_diff))]
  
  colnames(banks_list) <- c("sn","count")
  
  #order the list
  banks_list <- banks_list[order(banks_list$count),]
  banks_list[is.na(banks_list)] <- 0
  
  banks_list <- banks_list[-(16:length(banks_list[,1])),]
  colnames(banks_list) <- c("sn","count")
  banks_list <- banks_list[,-2]
  
  #disconnects from the database
  
  dbDisconnect(con)
  
  return(banks_list)
}


getdata_banks <- function(year_lower, year_upper,bcs_lower,bcs_upper,oir_lower,oir_upper,olv_lower,olv_upper,dir_lower,dir_upper,bank_name,all_banks){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  bcs_l <- as.double(bcs_lower)
  bcs_u <- as.double(bcs_upper)
  
  oir_l <- as.double(oir_lower)
  oir_u <- as.double(oir_upper)
  
  olv_l <- as.double(olv_lower)
  olv_u <- as.double(olv_upper)
  
  dir_l <- as.double(dir_lower)
  dir_u <- as.double(dir_upper)
  
  banks_list <- getdata_banks_list(year_l,year_u)
  
  bank_n <- bank_name
  
  #builds the query for the bank filtration
  if (all_banks == TRUE){
    query_bank = paste0(" ")
  }else{
    query_bank = paste0(" and (sn in ('",bank_name,"'))")
  }
  
  #gets the first query in the database 
  result <- dbGetQuery(con,paste0("select sn ,count(sn) from ",list_acquisition[year_l]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by sn"))
  
  #does the looping to get all years necessary
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select sn ,count(sn) from ",list_acquisition[i]," where (oir between ",oir_l," and ",oir_u,") and (olv between ",olv_l," and ",olv_u,") and (bcs between ",bcs_l," and ",bcs_u,")",query_bank," and (dir between ",dir_l," and ",dir_u,") group by sn"))
    result <- merge(result,temp,by="sn")
  }
  
  #does the summation through the lines 
  for (i in 1:length(result[,1])){
    result[i,(2+year_diff+1)] <- sum(result[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  result<-result[,-(2:(2+year_diff))]
  
  #creates the matrix that is goint to be matched and parsed as result
  banks_table <- matrix(nrow=15,ncol=2)
  
  #inserts the value of bankslist inside hist_table
  
  banks_table[,1] <- banks_list
  banks_table[,2] <- 0
  
  banks_table <- merge(banks_table,result,by.x="V1",by.y="sn",all.x=TRUE)
  colnames(banks_table) <- c("sn","exclude","count")
  
  #deletes the second column
  banks_table <- banks_table[,-2]
  
  #replace the NA values with zero and changes the col names for the dataframe
  banks_table[is.na(banks_table)] <- 0
  
  #disconnects from the database
  dbDisconnect(con)
  
  return(banks_table)
}

getdata_yearly <- function(){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  result <- dbGetQuery(con,paste0("select count(sn) from ",list_acquisition[1]))
  for (i in 2:15){
    temp <- dbGetQuery(con,paste0("select count(sn) from ",list_acquisition[i]))
    result <- rbind(result,temp)
  }
  result[,2] <- seq(2000,2014)
  
  colnames(result) <- c("Loans","YEAR")
  #disconnects from the database
  
  dbDisconnect(con)
  
  return(result)
}

#all different variables, with being able to crossfilter the data over the graphs
getdata_zip_list <- function (year_lower, year_upper){
  
  #connect with the database
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, host="155.246.103.74", dbname="dbfanniemae", user="usfanniemae", password= "Jxn54nNUyFOC")
  
  ##################### defining tables and variables ##########################
  
  table_list <- dbListTables(con) #gets all the tables available in the database
  table_list <- sort(as.vector(table_list)) #save the availables table as vector and sort them an increasing order
  
  #separation of the acquisition and performance table_list
  
  list_acquisition <- head(table_list,15)
  list_performance <- tail(table_list,16)
  
  list_performance <- head(list_performance,-1)
  rm(table_list)#clear the memory
  
  #read the variables
  year_l <- as.integer(year_lower)
  year_u <- as.integer(year_upper)
  year_diff <- year_u - year_l
  
  #get the list of the the banks that sold more mortgages, limited to 20 entries
  z_list <- dbGetQuery(con,paste0("select z, count(z) from ",list_acquisition[year_l]," group by z"))
  for (i in (year_l+1):year_u){
    temp <- dbGetQuery(con,paste0("select z, count(z) from ",list_acquisition[i]," group by z"))
    z_list <- merge(z_list,temp,by.x="z",by.y="z",all=TRUE)
  }
  #does the summation through the lines 
  for (i in 1:length(z_list[,1])){
    z_list[i,(2+year_diff+1)] <- sum(z_list[i,2:(2+year_diff)])
  }
  
  #clears the result by excluding the intermediate columns
  z_list<-z_list[,-(2:(2+year_diff))]
  
  colnames(z_list) <- c("z","count")
  
  #order the list
  z_list <- z_list[order(z_list$count),]
  z_list[is.na(z_list)] <- 0
  
  colnames(z_list) <- c("z","count")
  z_list <- z_list[,-2]
  z_list <- str_pad(z_list,3,pad="0")
  
  #disconnects from the database
  
  dbDisconnect(con)
  
  return(z_list)
}


