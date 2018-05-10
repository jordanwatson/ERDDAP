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


#  Load two different shapefiles - one for positive and one for negative longitudes.
statneg <- readOGR(dsn="Data",layer="ADFG_Stat_areas_neg_long")
statpos <- readOGR(dsn="Data",layer="ADFG_Stat_areas_pos_long")

mydate <- data.frame(mydate=seq(as.Date("2010-01-01"), as.Date("2018-05-01"), "days")) %>% 
  mutate(year=format(mydate,"%Y"),
         month=format(mydate,"%m")) %>% 
  group_by(year) %>% 
  mutate(julian=1:n()) %>% 
  ungroup %>% 
  group_by(year,month) %>% 
  mutate(moday=1:n(),
         momax=ifelse(moday==max(moday),1,0)) %>% 
  data.frame %>% 
  mutate(moidx = group_indices(., year,month))

newdat <- list()

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
myyear <- 2010
myyearmin <- min(which(mydate$year==myyear))
myyearmax <- max(which(mydate$year==myyear))
movec <- unique(mydate$month)
saveRDS(newdat,file=paste0("myyear_",myyear,".RDS"))
i=1
i=121

system.time({
  #for(i in myyearmin:myyearmax){
  for(i in 1:4){
    tryCatch({ 
      print(i)
      #  Query the satellite data as a netcdf file.
      #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
      x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],
                               "T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"))
      #x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(55):(56)][(-175.99):(-175.01)]"))
      
      #  Convert and open the netcdf file
      tmpSST <- tempfile(pattern="xwB", fileext=".nc")
      writeBin(object=x, con=tmpSST)
      nc <- nc_open(tmpSST)
      
      #  For the first day of each month create a new container for the data.
      if(mydate$moday[i]==1) {
        #basket <- data.frame()
        holderdat <- data.frame()
        #  For the first day of each month, extract the first three columns from the netcdf (long, lat, sst)
        tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                               lat=ncvar_get(nc, varid = "latitude")) %>% 
                               {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
          mutate(sst=ifelse(sst<(-2),NA,sst))
        
        #  So that we don't store the date as an extra column, rename the sst column with the date. 
        names(tempneg)[3] <- as.character(mydate$mydate[i])
        
        #  Rename this data frame, which will become important for binding in the next iteration of the loop
        basket <- tempneg
        
        #  For all days except the first of each month. 
      } else {
        #  Extract netcdf data but only keep the 3rd column, which contains the sst.
        holderdat <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                                 lat=ncvar_get(nc, varid = "latitude")) %>% 
                                 {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
          mutate(sst=ifelse(sst<(-2),NA,sst))
        
        holderdat <- data.frame(holderdat[,3])
        #  Rename the sst column to be the date. 
        names(holderdat) <- as.character(mydate$mydate[i])
        
        #  Bind the data from the previous iteration to the new data. This will include data from all previous iterations.
        basket <- bind_cols(basket,holderdat)
        
        rm(holderdat,x)
      }
      
      #  Close the netcdf file.
      nc_close(nc)
      
      #  For the last day of each month, compile the monthly data and summarise.
      #if(mydate$momax[i]==1){
        if(i==4){
          
        #  Create a spatial dataset using the stat areas in our monthly data.
        #  Match projection to that of the stat areas shapefile.
        temp <- SpatialPointsDataFrame(coords = basket[,c(1,2)], data = basket,
                                       proj4string = CRS(proj4string(statneg)))
        
        #  Determine number of columns, which will be used in the "over" function        
        mycols <- ncol(basket)
        #  Perform point in polygon function. This is a slow function but by putting each day as a separate column, 
        #  we only have to run this function once for each month instead of once daily.
        testneg <- over(statneg, temp[,3:mycols],returnList=TRUE)  
        
        #  For each stat area, calculate the average and standard deviation of temperatures
        junk <-  bind_cols(Reduce(rbind,lapply(names(testneg),function(x) testneg[[x]] %>% 
                                                 summarise_all(funs(round(mean(.,na.rm=TRUE),2))) %>% 
                                                 setNames(paste0(names(.),"mean")))),
                           Reduce(rbind,lapply(names(testneg),function(x) testneg[[x]] %>% 
                                                 summarise_all(funs(round(sd(.,  na.rm=TRUE),2))) %>% 
                                                 setNames(paste0(names(.),"sd")))))
        #  The index field will be used to match temperature data with shapefiles.
        junk$index <- 1:nrow(junk)
        
        #  Join spatial data with temperature data. 
        #  The shapefile starts rownumbers at 0 but our index starts at 1. The "index" field adds one to these
        #  row numbers so that they'll match. 
        dataneg <- statneg@data %>% 
          rownames_to_column(var="index") %>% 
          mutate(index=as.numeric(as.character(index))+1,
                 STAT_AREA=as.character(STAT_AREA),
                 date=mydate$mydate[i]) %>% 
          dplyr::select(STAT_AREA,index) %>% 
          left_join(junk)
        
        #  Because we are compiling data monthly to reduce the point-in-polygon operation, there will be 12 list levels
        #  for each year. Extract the month index for this iteration, and save the data to this level in the list. 
        #myidx <- mydate$moidx[i]
        #newdat[[myidx]] <- dataneg
        
        rm(basket,mycols,nc)
      }

      
      x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],
                               "T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(60)][(167):(179.9)]"))

      tmpSST <- tempfile(pattern="xwB", fileext=".nc")
      writeBin(object=x, con=tmpSST)
      ncpos <- nc_open(tmpSST)

      
      #  For the first day of each month create a new container for the data.
      if(mydate$moday[i]==1) {
        #basket <- data.frame()
        holderdatpos <- data.frame()
        #  For the first day of each month, extract the first three columns from the netcdf (long, lat, sst)
        temppos <- expand.grid(lon=ncvar_get(ncpos, varid = "longitude"),
                               lat=ncvar_get(ncpos, varid = "latitude")) %>% 
                               {bind_cols(.,data.frame(sst=as.vector(ncvar_get(ncpos,"analysed_sst"))))} %>% 
          mutate(sst=ifelse(sst<(-2),NA,sst))
        
        #  So that we don't store the date as an extra column, rename the sst column with the date. 
        names(temppos)[3] <- as.character(mydate$mydate[i])
        
        #  Rename this data frame, which will become important for binding in the next iteration of the loop
        basketpos <- temppos
        
        #  For all days except the first of each month. 
      } else {
        #  Extract netcdf data but only keep the 3rd column, which contains the sst.
        holderdatpos <- expand.grid(lon=ncvar_get(ncpos, varid = "longitude"),
                                 lat=ncvar_get(ncpos, varid = "latitude")) %>% 
                                 {bind_cols(.,data.frame(sst=as.vector(ncvar_get(ncpos,"analysed_sst"))))} %>% 
          mutate(sst=ifelse(sst<(-2),NA,sst))
        
        holderdatpos <- data.frame(holderdatpos[,3])
        #  Rename the sst column to be the date. 
        names(holderdatpos) <- as.character(mydate$mydate[i])
        
        #  Bind the data from the previous iteration to the new data. This will include data from all previous iterations.
        basketpos <- bind_cols(basketpos,holderdatpos)
        
        rm(holderdatpos,x)
      }
      
      #  Close the netcdf file.
      nc_close(ncpos)
      
      #  For the last day of each month, compile the monthly data and summarise.
      #if(mydate$momax[i]==1){
        if(i==4){
        #  Create a spatial dataset using the stat areas in our monthly data.
        #  Match projection to that of the stat areas shapefile.
        temppos <- SpatialPointsDataFrame(coords = basketpos[,c(1,2)], data = basketpos,
                                       proj4string = CRS(proj4string(statpos)))
        
        #  Determine number of columns, which will be used in the "over" function        
        mycols <- ncol(basketpos)
        #  Perform point in polygon function. This is a slow function but by putting each day as a separate column, 
        #  we only have to run this function once for each month instead of once daily.
        testpos <- over(statpos, temppos[,3:mycols],returnList=TRUE)  
        
        #  For each stat area, calculate the average and standard deviation of temperatures
        junk <-  bind_cols(Reduce(rbind,lapply(names(testpos),function(x) testpos[[x]] %>% 
                                                 summarise_all(funs(round(mean(.,na.rm=TRUE),2))) %>% 
                                                 setNames(paste0(names(.),"mean")))),
                           Reduce(rbind,lapply(names(testpos),function(x) testpos[[x]] %>% 
                                                 summarise_all(funs(round(sd(.,  na.rm=TRUE),2))) %>% 
                                                 setNames(paste0(names(.),"sd")))))
        #  The index field will be used to match temperature data with shapefiles.
        junk$index <- 1:nrow(junk)
        
        #  Join spatial data with temperature data. 
        #  The shapefile starts rownumbers at 0 but our index starts at 1. The "index" field adds one to these
        #  row numbers so that they'll match. 
        datapos <- statpos@data %>% 
          rownames_to_column(var="index") %>% 
          mutate(index=as.numeric(as.character(index))+1,
                 STAT_AREA=as.character(STAT_AREA),
                 date=mydate$mydate[i]) %>% 
          dplyr::select(STAT_AREA,index) %>% 
          left_join(junk)
        
        #  Because we are compiling data monthly to reduce the point-in-polygon operation, there will be 12 list levels
        #  for each year. Extract the month index for this iteration, and save the data to this level in the list. 
        myidx <- mydate$moidx[i]
        newdat[[myidx]] <- bind_rows(dataneg %>% dplyr::select(-index),
                                     datapos %>% dplyr::select(-index))
        
        rm(basketpos,dataneg,datapos)
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
})

      
      
    
      
      
      
      
      
        
      
      
      ## Junk below here.
      
      
      
      
      
      
      
      #nc <- nc_open(dest2)
      temp <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                          lat=ncvar_get(nc, varid = "latitude")) %>% 
                          {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
        mutate(ifelse(sst<(-2),NA,sst))
      nc_close(nc)
      
      temp <- SpatialPointsDataFrame(coords = temp[,c(1,2)], data = temp,
                                     proj4string = CRS(proj4string(statpos)))
      
      testpos <- over(statpos, temp[,3],returnList=TRUE)    
      posmean <- Reduce(rbind,
                        lapply(names(testpos),
                               function(x)data.frame(index=as.numeric(as.character(x))+1,
                                                     sstmean=mean(testpos[[x]]$sst),
                                                     sstsd=sd(testpos[[x]]$sst))))
      
      datapos <- statpos@data %>% 
        rownames_to_column(var="index") %>% 
        mutate(index=as.numeric(as.character(index))+1,
               STAT_AREA=as.character(STAT_AREA),
               date=mydate$mydate[i]) %>% 
        left_join(posmean)%>% 
        dplyr::select(STAT_AREA,
                      date,
                      sstmean,
                      sstsd)
      
      rm(temp)
      
      newdat[[mydate$julian[i]]] <- bind_rows(
        dataneg,
        datapos
      ) %>% 
        mutate(sstmean=round(sstmean,3),
               sstsd=round(sstsd,3))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
})
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

saveRDS(newdat,file=paste0("myyear_",mydate$year[i],".RDS"))

#---------------------------------------------------------------------------------------------------
#  Which of the iterations had download errors? These probably just need to be rerun and don't necessarily mean that there aren't data.
#---------------------------------------------------------------------------------------------------

data.frame(rbind(lapply(newdat,function(x)(nrow(x))))) %>% 
  gather(julian,count) %>% 
  filter(count=="NULL")


#---------------------------------------------------------------------------------------------------
#  Manually run the days that didn't work
#---------------------------------------------------------------------------------------------------

myonedayfun <- function(i){
  URL <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]")
  #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
  x <- getBinaryURL(URL)
  tmpSST <- tempfile(pattern="xwB", fileext=".nc")
  writeBin(object=x, con=tmpSST)
  nc <- nc_open(tmpSST)
  
  temp <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                      lat=ncvar_get(nc, varid = "latitude")) %>% 
                      {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
    filter(sst>(-2))
  nc_close(nc)
  
  temp <- SpatialPointsDataFrame(coords = temp[,c(1,2)], data = temp,
                                 proj4string = CRS(proj4string(stat)))
  
  testpos <- over(stat, temp[,3], fn = mean) %>% 
    rownames_to_column(var="index") %>% 
    mutate(index=as.numeric(as.character(index))+1) %>% 
    filter(!is.na(sst))
  
  dataneg <- stat@data %>% 
    rownames_to_column(var="index") %>% 
    mutate(index=as.numeric(as.character(index))+1,
           STAT_AREA=as.character(STAT_AREA),
           date=mydate$mydate[i]) %>% 
    left_join(testpos) %>% 
    dplyr::select(STAT_AREA,
                  date,
                  sst)
  
  rm(temp)
  
  URL <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(60)][(167):(179.9)]")
  #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
  x <- getBinaryURL(URL)
  tmpSST <- tempfile(pattern="xwB", fileext=".nc")
  writeBin(object=x, con=tmpSST)
  nc <- nc_open(tmpSST)
  
  #nc <- nc_open(dest2)
  temp <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                      lat=ncvar_get(nc, varid = "latitude")) %>% 
                      {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
    filter(sst>(-2))
  nc_close(nc)
  
  temp <- SpatialPointsDataFrame(coords = temp[,c(1,2)], data = temp,
                                 proj4string = CRS(proj4string(stat)))
  
  testpos <- over(stat, temp[,3], fn = mean) %>% 
    rownames_to_column(var="index") %>% 
    mutate(index=as.numeric(as.character(index))+1) %>% 
    filter(!is.na(sst))
  
  datapos <- stat@data %>% 
    rownames_to_column(var="index") %>% 
    mutate(index=as.numeric(as.character(index))+1,
           STAT_AREA=as.character(STAT_AREA),
           date=mydate$mydate[i]) %>% 
    left_join(testpos)%>% 
    dplyr::select(STAT_AREA,
                  date,
                  sst)
  
  rm(temp)
  
  return(
    bind_rows(
      dataneg,
      datapos) %>% 
      distinct() %>% 
      filter(!is.na(sst)) %>% 
      mutate(sst=round(sst,3)))
}

newdat[[mydate$julian[4]]] <- myonedayfun(4)
newdat[[mydate$julian[333]]] <- myonedayfun(333)

saveRDS(newdat,file=paste0("myyear_",mydate$year[i],".RDS"))


data.frame(day=1:365,sst=unlist(lapply(newdat,function(x)mean(x[,"sst"])))) %>% 
  ggplot(aes(day,sst)) + 
  geom_line()






head(tidy(statneg))
tstat <- tidy(statneg) %>% 
  mutate(myid=as.numeric(as.character(id))+1)


plotdat <- Reduce(rbind,lapply(1:12,function(x)data.frame(
  STAT_AREA=newdat[[x]][,1],
  myid=newdat[[x]][,2],
  sst=newdat[[x]][,3],
  month=x)))

junktest <- tstat %>% 
  left_join(plotdat)


junktest %>% 
  ggplot() + 
  geom_polygon(aes(long,lat,group=group,fill=sst)) + 
  facet_wrap(~factor(month))

