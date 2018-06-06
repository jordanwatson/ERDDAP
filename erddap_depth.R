#-------------------------------------------------------------------------------------------------------------------------------
#  Find the average depth for each stat area but merging stat area spatial data with NOAA ngdc bathymetry.
#  Also calculate the standard deviation of depth for each area, which will help to identify areas on the Slope.
#  Date created: 06/06/2018
#  Date last modified: 06/06/2018
#  Created by: Jordan Watson jordan.watson@noaa.gov
#-------------------------------------------------------------------------------------------------------------------------------



library(tidyverse)
library(marmap)
library(ncdf4)
library(RCurl)
library(raster)
library(rgdal)


#  Load the full stat area dataset from ADF&G
#  https://soa-adfg.opendata.arcgis.com/datasets/groundfish-statistical-areas-2001?geometry=-222.188%2C53.09%2C-79.981%2C67.923
stat <- readOGR(dsn="Data",layer="Groundfish_Statistical_Areas_2001")
stat <- spTransform(stat,CRS("+proj=longlat +datum=WGS84"))

#  It is easiest (though perhaps slightly more clunky) if we divide the shapefile into two - one with positive longitudes
#  and one with negative longitudes. We could identify the positive and negative polygons (stat areas) by extracting the 
#  bounding box for each polygon and then filtering, or we can just use the dataset we already have that includes these coordinates
#  lapply(stat@polygons, bbox) #  To extract bounding box coordinates from a shapefile:

data <- readRDS("Data/mur_SST_stat6_grid.RDS")

#  Create spatialpolygonsdataframes for the positive and negative longitude stat areas
neglon <- stat[stat$STAT_AREA %in% data$STAT_AREA[data$maxlon<0],] # 1509 polygons
poslon <- stat[stat$STAT_AREA %in% data$STAT_AREA[data$maxlon>0],] # 229 polygons

#  Extract bathymetry for the negative longitude regions of Alaskan waters
getNOAA.bathy(lon1=-180,lon2=-129,lat1=46,lat2=69, resolution=1) -> AKBath
r.ak <- marmap::as.raster(AKBath)

x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-01T09:00:00Z):1:(2010-01-01T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"))

#  Process the netcdf temperature file.
tmpSST <- tempfile(pattern="xwB", fileext=".nc"); writeBin(object=x, con=tmpSST)
nc <- nc_open(tmpSST)

#  Extract long, lat, and sst from the netcdf data. 
#  Filter out those coordinates without sst data, as we did for our temperature algorithm.
#  Finally, use the long and lat coordinates from the temperature file and extract depths for those coordinates from the r.ak bathymetry raster.
#  and remove those depths that are above sea level.
tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                       lat=ncvar_get(nc, varid = "latitude")) %>% 
                       {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
  filter(!is.na(sst)) %>% 
mutate(depth=round(raster::extract(r.ak,cbind(lon,lat),method="bilinear"),0)) %>% 
  filter(depth<0)

####  Now we need to bin the depth data by stat area and find the average depth per stat area, as we did the temperature

#  Create a spatial dataset from our sst dataset and match projection to that of the negative longitude stat areas shapefile.
temp <- SpatialPointsDataFrame(coords = tempneg[,c("lon","lat")], data = tempneg,
                               proj4string = CRS(proj4string(neglon)))


#  Perform point in polygon function that puts each coordinate into a stat area.
testneg <- over(neglon, temp[,4],returnList=TRUE)

#  For each stat area, calculate the average and standard deviation of depths
junk <-  bind_cols(Reduce(rbind,lapply(names(testneg),function(x) testneg[[x]] %>% 
                                         summarise_all(funs(round(mean(.,na.rm=TRUE),2))) %>% 
                                         setNames(paste0(names(.),"mean")))),
                   Reduce(rbind,lapply(names(testneg),function(x) testneg[[x]] %>% 
                                         summarise_all(funs(round(sd(.,  na.rm=TRUE),2))) %>% 
                                         setNames(paste0(names(.),"sd")))))
#  The index field will be used to match depth data with shapefiles.
junk$index <- 1:nrow(junk)

#  Join spatial data with depth data. 
#  Because we subset the original shapefile into negative and positive data frames, do not use row.names for matching.
#  Use indices instead. By creating an index field from 1:n(), this alleviates any issues with matching id's across datasets.
#  row numbers so that they'll match. 
dataneg <- neglon@data %>% 
  mutate(index=1:n(),
         STAT_AREA=as.character(STAT_AREA)) %>% 
  dplyr::select(STAT_AREA,index) %>% 
  left_join(junk)

dataneg %>% 
  group_by(STAT_AREA) %>% 
  summarise(mean(depthmean))

rm(temp,junk)

#--------------------------------------------------------------------------------------------
#####  Now do the same thing with positive longitudes #####
#--------------------------------------------------------------------------------------------

#  Extract bathymetry for the negative longitude regions of Alaskan waters
getNOAA.bathy(lon1=166,lon2=180,lat1=46,lat2=61, resolution=1) -> AKBath
r.ak <- marmap::as.raster(AKBath)

x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-01T09:00:00Z):1:(2010-01-01T09:00:00Z)][(47):(60)][(166):(180)]"))

#x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],
#                         "T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(60)][(167):(179.9)]"))

#  Process the netcdf temperature file.
tmpSST <- tempfile(pattern="xwB", fileext=".nc"); writeBin(object=x, con=tmpSST)
nc <- nc_open(tmpSST)

#  Extract long, lat, and sst from the netcdf data. 
#  Filter out those coordinates without sst data, as we did for our temperature algorithm.
#  Finally, use the long and lat coordinates from the temperature file and extract depths for those coordinates from the r.ak bathymetry raster.
#  and remove those depths that are above sea level.
temppos <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                       lat=ncvar_get(nc, varid = "latitude")) %>% 
                       {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
  filter(!is.na(sst)) %>% 
  mutate(depth=round(raster::extract(r.ak,cbind(lon,lat),method="bilinear"),0)) %>% 
  filter(depth<0)

####  Now we need to bin the depth data by stat area and find the average depth per stat area, as we did the temperature

#  Create a spatial dataset from our sst dataset and match projection to that of the negative longitude stat areas shapefile.
temp <- SpatialPointsDataFrame(coords = temppos[,c("lon","lat")], data = temppos,
                               proj4string = CRS(proj4string(poslon)))


#  Perform point in polygon function that puts each coordinate into a stat area.
testpos <- over(poslon, temp[,4],returnList=TRUE)

#  For each stat area, calculate the average and standard deviation of depths
junk <-  bind_cols(Reduce(rbind,lapply(names(testpos),function(x) testpos[[x]] %>% 
                                         summarise_all(funs(round(mean(.,na.rm=TRUE),2))) %>% 
                                         setNames(paste0(names(.),"mean")))),
                   Reduce(rbind,lapply(names(testpos),function(x) testpos[[x]] %>% 
                                         summarise_all(funs(round(sd(.,  na.rm=TRUE),2))) %>% 
                                         setNames(paste0(names(.),"sd")))))
#  The index field will be used to match depth data with shapefiles.
junk$index <- 1:nrow(junk)

#  Join spatial data with depth data. 
#  Because we subset the original shapefile into negative and positive data frames, do not use row.names for matching.
#  Use indices instead. By creating an index field from 1:n(), this alleviates any issues with matching id's across datasets.
#  row numbers so that they'll match. 
datapos <- poslon@data %>% 
  mutate(index=1:n(),
         STAT_AREA=as.character(STAT_AREA)) %>% 
  dplyr::select(STAT_AREA,index) %>% 
  left_join(junk)

#  There are two stat areas that are broken into two polygons in the ADFG stat area file. 
#  Find the average depth and average standard deviation of depth across these and combine into one record.
depthdata <- bind_rows(datapos,dataneg) %>% 
  group_by(STAT_AREA) %>% 
  summarise(m.depth=mean(depthmean),
            sd.depth=sqrt(sum(depthsd^2)/n()))


saveRDS(depthdata,file="Depth_by_STAT_AREA.rds")


data <- readRDS("Data/mur_SST_stat6_grid.RDS")