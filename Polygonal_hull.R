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

data <- readRDS("Data/SampleVMS.RDS") %>% 
  rename_all(tolower) %>% 
  rename(lat=vms_latitude,
         lon=vms_longitude) %>% 
  mutate(lon=ifelse(lon>0,lon-360,lon),
         lon2=floor(lon),
         lat2=floor(lat))

#  Create a list of the unique dates. 
#  This will become the looping index
mydates <- unique(data$date)

myoutdf <- data.frame()
#  This is where the loop will begin, iterated by day
for(i in 1:length(unique(mydates))){

mydat <- data %>% 
  filter(date==mydates[i])

minlat <- floor(min(mydat$lat))
maxlat <- ceiling(max(mydat$lat))
minlon <- floor(min(mydat$lon))
maxlon <- ceiling(max(mydat$lon))

#  Create a data frame that includes all possible lat-lon combinations within
#  the boundaries of all the VMS data. 
#  Then join the actual locations of grid cells that contain VMS data, populating
#  a dummy column for the cells with VMS data.
mydf <- expand.grid(lon2=minlon:maxlon,lat2=maxlat:minlat) %>% 
  left_join(mydat %>% 
              distinct(lon2,lat2) %>% 
              mutate(dummy=1))

#  Convert first two columns as lon-lat and third as value into a raster.
#  Then use the "clump" function to label clumps of data based on adjacency in 8 directions.
clumps <- as.matrix(clump(rasterFromXYZ(mydf), directions=8))
colnames(clumps) <- unique(mydf$lon2) # Name the columns with the longitudes
rownames(clumps) <- rev(unique(mydf$lat2)) # Name rows with latitudes

#  Melt the raster matrix into a data frame, group the data by cluster, and determine the 
#  spatial extent (bounds) of each cluster.
myout <- as.data.frame(clumps) %>%
  mutate(lat2=unique(mydf$lat2)) %>% 
  gather(lon2,cluster,-lat2) %>% 
  mutate(lon2=as.numeric(lon2)) %>% 
  filter(!is.na(cluster)) %>% 
  group_by(cluster) %>% 
  summarise(minlat=min(lat2),
            maxlat=max(lat2),
            minlon=min(lon2),
            maxlon=max(lon2),
            date=mydates[i])

myoutdf <- bind_rows(myoutdf,myout)

#----------------------------------------------------------------------------
#  Plotting of data (optional)
#----------------------------------------------------------------------------

#  For the sake of plotting, create a new, ordered data frame that will 
#  facilitate drawing a box around each of the clusters. 
#myout2 <- myout %>% 
#  gather(ycoord,y,-c(cluster,minlon,maxlon)) %>% 
#  gather(xcoord,x,-c(cluster,y,ycoord)) %>% 
#  mutate(mycoords=paste(ycoord,xcoord),
#         mycoords=fct_relevel(mycoords,
#                              "minlat minlon",
#                              "minlat maxlon",
#                              "maxlat maxlon",
#                              "maxlat minlon")) %>% 
#  arrange(cluster,mycoords)
  
  
#as.data.frame(clumps) %>%
#  mutate(lat2=unique(mydf$lat2)) %>% 
#  gather(lon2,cluster,-lat2) %>%
#  mutate(lon2=as.numeric(lon2)) %>% 
#ggplot(aes(lon2,lat2)) +
#  geom_raster(aes(fill = factor(cluster))) + 
#  geom_polygon(data=myout2,aes(x,y,group=cluster),fill=NA,color="black")

#plot(dfr)
}

write_csv(myoutdf,"SampleExtractionExtents.csv")


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


