#-------------------------------------------------
#  This script creates a series of rectangular convex hulls around a scatter of lat-long data.
#  In particular, this file will narrow the size of ERDDAP data downloads by breaking data into 
#  a smaller set of downloads. The output is a dataframe of min and max lat and longs that can be fed
#  into an extraction routine for the Pacific VMS data. 
#  Jordan Watson (jordan.watson@noaa.gov)
#  Date created: 11/17/2018
#-------------------------------------------------
library(tidyverse)
library(raster)
library(DBI)
library(odbc)
library(dbplyr)

#data <- readRDS("Data/SampleVMS.RDS") %>% 
#  rename_all(tolower) %>% 
#  rename(lat=vms_latitude,
#         lon=vms_longitude) %>% 
#  mutate(lon=ifelse(lon>0,lon-360,lon),
#         lon2=floor(lon),
#         lat2=floor(lat))

#  Below is the sql query I used for the sample dataset, "vms_export.csv".
#spool W:\vms_export.csv
#select /*csv*/ local_date,fmc_logves_id as id,track_longitude as longitude,track_latitude as latitude from vessel_positions@vms_dblink
#where local_date between to_date('08-FEB-18 00:00','DD-MON-YY HH24:MI')
#and to_date('08-FEB-18 23:59','DD-MON-YY HH24:MI');
#spool off


con <- dbConnect(odbc::odbc(), "akfin", UID="jwatson", PWD= rstudioapi::askForPassword("Enter AKFIN Password"))
my_tbl <- dbSendQuery(con,"select distinct to_char(utc_date,'yyyy-mm-dd'),floor(track_longitude) as lon,floor(track_latitude) as lat from AKFIN_MARTS.VMS_VESSEL_POSITION_V")


data <- dbFetch(my_tbl) %>% 
  rename_all(tolower) %>% 
  mutate(lon=ifelse(lon>0,lon-360,lon)) %>% 
  rename(date=`to_char(utc_date,'yyyy-mm-dd')`)

data %>% 
  summarise(length(unique(date)))


#  The following won't work until the Oracle spatial column is removed from the view.
#newtbl <- tbl(con,"AKFIN_MARTS.VMS_VESSEL_POSITION_V")

#data <- read_csv("Data/vms_export.csv",skip=3) %>% 
#  rename_all(tolower) %>% 
#  rename(lat=latitude,
#         lon=longitude) %>% 
#  mutate(lon=ifelse(lon>0,lon-360,lon),
#         lon2=floor(lon),
#         lat2=floor(lat),
#         date=as.Date(substr(local_date,1,9),format="%d-%b-%y"))


x11();data %>% 
  mutate(date_id=group_indices(.,date)) %>% 
  filter(date_id<10) %>% 
  ggplot() + 
  geom_point(aes(lon,lat)) + 
  theme_bw() + 
  facet_wrap(~date)

x11();data %>% 
  ggplot() + 
  geom_point(aes(lon,lat)) + 
  theme_bw()

#  Create a list of the unique dates. 
#  This will become the looping index
mydates <- unique(data$date)

myoutdf <- data.frame()
#i=1
#  This is where the loop will begin, iterated by day
for(i in 1:length(unique(mydates))){
  
  mydat <- data %>% 
    filter(date==mydates[i])
  
  minlat <- floor(min(mydat$lat))
  maxlat <- ifelse(ceiling(max(mydat$lat))==minlat,ceiling(max(mydat$lat))+1,ceiling(max(mydat$lat)))
  minlon <- floor(min(mydat$lon))
  maxlon <- ifelse(ceiling(max(mydat$lon))==minlon,ceiling(max(mydat$lon))+1,ceiling(max(mydat$lon)))
  
  #  Create a data frame that includes all possible lat-lon combinations within
  #  the boundaries of all the VMS data. 
  #  Then join the actual locations of grid cells that contain VMS data, populating
  #  a dummy column for the cells with VMS data.
  mydf <- expand.grid(lon=minlon:maxlon,lat=maxlat:minlat) %>% 
    left_join(mydat %>% 
                distinct(lon,lat) %>% 
                mutate(dummy=1))
  
  #  Convert first two columns as lon-lat and third as value into a raster.
  #  Then use the "clump" function to label clumps of data based on adjacency in 8 directions.
  clumps <- as.matrix(clump(rasterFromXYZ(mydf), directions=8))
  colnames(clumps) <- unique(mydf$lon) # Name the columns with the longitudes
  rownames(clumps) <- rev(unique(mydf$lat)) # Name rows with latitudes
  
  #  Melt the raster matrix into a data frame, group the data by cluster, and determine the 
  #  spatial extent (bounds) of each cluster.
  myout <- as.data.frame(clumps) %>%
    mutate(lat=unique(mydf$lat)) %>% 
    gather(lon,cluster,-lat) %>% 
    mutate(lon=as.numeric(lon)) %>% 
    filter(!is.na(cluster)) %>% 
    group_by(cluster) %>% 
    summarise(minlat=min(lat),
              maxlat=max(lat),
              minlon=min(lon),
              maxlon=max(lon),
              date=mydates[i])
  
  myoutdf <- bind_rows(myoutdf,myout)
}

write_csv(myoutdf,"polygon_ExtractionExtents.csv")


  #----------------------------------------------------------------------------
  #  Plotting of data (optional)
  #----------------------------------------------------------------------------
  WorldData <- map_data('world') %>% fortify()
  #WorldData <- fortify(WorldData)
  

  #  For the sake of plotting, create a new, ordered data frame that will 
  #  facilitate drawing a box around each of the clusters. 
  myout2 <- myout %>% 
    gather(ycoord,y,-c(cluster,minlon,maxlon,date)) %>% 
    gather(xcoord,x,-c(cluster,y,ycoord,date)) %>% 
    mutate(mycoords=paste(ycoord,xcoord),
           mycoords=fct_relevel(mycoords,
                                "minlat minlon",
                                "minlat maxlon",
                                "maxlat maxlon",
                                "maxlat minlon")) %>% 
    arrange(cluster,mycoords)
  
  
  x11();as.data.frame(clumps) %>%
    mutate(lat2=unique(mydf$lat2)) %>% 
    gather(lon2,cluster,-lat2) %>%
    mutate(lon2=as.numeric(lon2)) %>% 
    filter(!is.na(cluster)) %>% 
  ggplot(aes(lon2,lat2)) +
    geom_raster(aes(fill = factor(cluster))) + 
    geom_polygon(data=myout2,aes(x,y,group=cluster),fill=NA,color="black") + 
    geom_map(data=WorldData, map=WorldData,
             aes(x=long, y=lat, group=group, map_id=region),
             fill="grey", colour="#7f7f7f", size=0.5) + 
    xlim(-180,-80)





#  Plot all of the dates.
myout2 <- myoutdf %>% 
  gather(ycoord,y,-c(cluster,minlon,maxlon,date)) %>% 
  gather(xcoord,x,-c(cluster,y,ycoord,date)) %>% 
  mutate(mycoords=paste(ycoord,xcoord),
         mycoords=fct_relevel(mycoords,
                              "minlat minlon",
                              "minlat maxlon",
                              "maxlat maxlon",
                              "maxlat minlon"),
         cluster=paste(cluster,date)) %>% 
  arrange(cluster,mycoords)

myout2 %>% 
  ggplot() +
  geom_polygon(aes(x,y,group=cluster,color=factor(date)),fill=NA)


#---------------------------------------------------------------------
library(RCurl)
library(ncdf4)
head(myoutdf)

i=1
tempdat <- myoutdf %>% filter(date==mydates[i]) %>% 
  filter(minlon>(-180) & maxlon>(-180)) 
tempdat2 <- tempdat %>% 
  filter(minlon>(-180) & maxlon>(-180)) %>% 
  summarise(minlat=min(minlat),
            maxlat=max(maxlat),
            minlon=min(minlon),
            maxlon=max(maxlon),
            date=mydates[i])

## The following takes about 140 seconds. 
system.time({
  x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",
                           mydates[i],
                           "T09:00:00Z):1:(",
                           mydates[i],
                           "T09:00:00Z)][(",
                           tempdat2$minlat,
                           "):(",
                           tempdat2$maxlat,
                           ")][(",
                           tempdat2$minlon,
                           "):(",
                           tempdat2$maxlon,
                           ")]"))
  
  #  Convert and open the netcdf file
  tmpSST <- tempfile(pattern="xwB", fileext=".nc")
  writeBin(object=x, con=tmpSST)
  nc <- nc_open(tmpSST)
  
  tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                         lat=ncvar_get(nc, varid = "latitude")) %>% 
                         {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
    mutate(sst=ifelse(sst<(-2),NA,sst))
  nc_close(nc)
})


#  Takes approximately 40 seconds. 
negdf <- data.frame()
#j=1
system.time({
  for(j in 1:nrow(tempdat)){
    x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",
                             mydates[i],
                             "T09:00:00Z):1:(",
                             mydates[i],
                             "T09:00:00Z)][(",
                             tempdat$minlat[j],
                             "):(",
                             tempdat$maxlat[j],
                             ")][(",
                             tempdat$minlon[j],
                             "):(",
                             tempdat$maxlon[j],
                             ")]"))
    
    #  Convert and open the netcdf file
    tmpSST <- tempfile(pattern="xwB", fileext=".nc")
    writeBin(object=x, con=tmpSST)
    nc <- nc_open(tmpSST)
    
    tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                           lat=ncvar_get(nc, varid = "latitude")) %>% 
                           {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
      mutate(sst=ifelse(sst<(-2),NA,sst))
    nc_close(nc)
    negdf <- bind_rows(tempneg,negdf)
  }
})

