library(tidyverse)
library(broom)
library(dplyr)
library(tibble)
library(ncdf4)
library(rgdal)
library(RCurl)

#---------------------------------------------------------------------------------------------------
#  This chunk shows where the positive and negative longitude shapefiles came from. Should be 
#  no need to run it. 
#---------------------------------------------------------------------------------------------------

#  This dataset includes the bounding boxes for all of the STAT_AREA polygons
#load("mydat.RData")

#  Load the full stat area dataset.
#stat <- readOGR(dsn="Data",layer="pvg_stat_2001")
#stat <- spTransform(stat,CRS("+proj=longlat +datum=NAD83"))

#  Save a version of this file that contains only negative longitudes
#writeOGR(stat[as.character(stat$STAT_AREA)%in%c(as.character((mydat %>% filter(x2<0))$STAT_AREA)),],
#         dsn="Data",
#         layer="ADFG_Stat_areas_neg_long",driver="ESRI Shapefile")

#  Save a version of this file that contains only positive longitudes
#writeOGR(stat[as.character(stat$STAT_AREA)%in%c(as.character((mydat %>% filter(x2>0))$STAT_AREA)),],
#         dsn="Data",
#         layer="ADFG_Stat_areas_pos_long",driver="ESRI Shapefile")
#---------------------------------------------------------------------------------------------------


#  Load spatial lookup table
statlkp <- readRDS("Stat_Lookup.RDS") %>% 
  mutate(STAT_AREA=as.character(STAT_AREA))

mydate <- data.frame(mydate=seq(as.Date("2018-05-10"), as.Date("2018-05-10"), "days")) %>% 
  mutate(year=format(mydate,"%Y")) %>% 
  group_by(year)

newdat <- list()

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
myyear <- 2018
myyearmin <- min(which(mydate$year==myyear))
myyearmax <- max(which(mydate$year==myyear))

saveRDS(newdat,file=paste0("testmyyear_",myyear,".RDS"))
i=1

system.time({
  for(i in myyearmin:myyearmax){
    tryCatch({
      
      #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
      x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(68.5)][(-179.99):(-130.01)]"))
      
      tmpSST <- tempfile(pattern="xwB", fileext=".nc")
      writeBin(object=x, con=tmpSST)
      nc <- nc_open(tmpSST)
      
      tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                          lat=ncvar_get(nc, varid = "latitude")) %>% 
                          {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
        mutate(sst=ifelse(sst<(-2),-2,sst)) %>% 
        filter(!is.na(sst))
      nc_close(nc)
      
      x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(60)][(167):(179.9)]"))
      tmpSST <- tempfile(pattern="xwB", fileext=".nc")
      writeBin(object=x, con=tmpSST)
      nc <- nc_open(tmpSST)
      
      #nc <- nc_open(dest2)
      temppos <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                          lat=ncvar_get(nc, varid = "latitude")) %>% 
                          {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
        mutate(sst=ifelse(sst<(-2),-2,sst))%>% 
        filter(!is.na(sst))
      nc_close(nc)
      
      templkp <- bind_rows(tempneg,temppos) %>% 
        inner_join(statlkp %>% 
                     mutate(lon=round(lon,2),
                            lat=round(lat,2)),
                   templkp %>% mutate(lon=round(lon,2),
                                      lat=round(lat,2))) %>% 
        group_by(STAT_AREA) %>% 
        summarise(sstmean=round(mean(sst,na.rm=TRUE),2),
                  sstsd=round(sd(sst,na.rm=TRUE),2),
                  records=length(!is.na(sst)),
                  date=mydate$mydate[i])
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
})