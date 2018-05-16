library(tidyverse)
library(rgdal)
library(RCurl)


#------------------------------------------------------------------------------------------------------------------
#  Convert the wide format data for each year to long format and resave as long format.
#------------------------------------------------------------------------------------------------------------------

myyearvec <- 2003:2018

for(i in 1:length(myyearvec)){
  print(myyearvec[i])
  newdat <- readRDS(paste0("myyear_",myyearvec[i],".RDS"))
  
  myvec <- which(unlist(lapply(1:length(newdat),function(x)is.null(newdat[[x]])))==FALSE)
  
  test <- Reduce(rbind,lapply(min(myvec):max(myvec),function(x)
    newdat[[x]] %>% 
      dplyr::select(STAT_AREA,contains("mean")) %>% 
      gather(date,sst.mean,-STAT_AREA) %>% 
      mutate(date=substr(date,1,10)) %>% 
      inner_join(
        newdat[[x]] %>% 
          dplyr::select(STAT_AREA,contains("sd")) %>% 
          gather(date,sst.sd,-STAT_AREA) %>% 
          mutate(date=substr(date,1,10))
      )
  ))
  
  saveRDS(test,file=paste0("myyear_",myyearvec[i],"long.RDS"))      
  rm(newdat,test)
}

#  End reformat code chunk
#------------------------------------------------------------------------------------------------------------------





#------------------------------------------------------------------------------------------------------------------
#  Fixing errors
#
#  If an error occurred while downloading the negative longitudes, the date will be missing altogether.
#  If an error occurred while downloading the positive longitudes, only the last 232 or so records may be missing
#  and the second half of the data show as NAs. Identify any dates that meet either of these situations and extract 
#  their indices from mydate for rerunning
#------------------------------------------------------------------------------------------------------------------

#  Load two different shapefiles - one for positive and one for negative longitudes.
statneg <- readOGR(dsn="Data",layer="ADFG_Stat_areas_neg_long")
statpos <- readOGR(dsn="Data",layer="ADFG_Stat_areas_pos_long")

mydate <- data.frame(mydate=seq(as.Date("2003-01-01"), as.Date("2018-05-15"), "days")) %>% 
  mutate(year=format(mydate,"%Y"),
         month=format(mydate,"%m")) %>% 
  group_by(year) %>% 
  mutate(julian=1:n()) %>% 
  ungroup %>% 
  group_by(year,month) %>% 
  mutate(moday=1:n(),
         momax=ifelse(moday==max(moday),1,0)) %>% 
  data.frame %>% 
  mutate(moidx = group_indices(., paste(year,month)),
         mydatechar=as.character(mydate))

myyearvec <- 2003:2018
myerrorvec <- vector()
for(i in 1:length(myyearvec)){
  print(myyearvec[i])
  print(paste0("myyear_",myyearvec[i],".RDS"))
  newdat <- readRDS(paste0("myyear_",myyearvec[i],".RDS"))
  myvec <- which(unlist(lapply(1:length(newdat),function(x)is.null(newdat[[x]])))==FALSE)
  
  error.1 <- mydate[!mydate$mydatechar %in% 
           unlist(lapply(min(myvec):max(myvec),function(x)unique(substr(names(newdat[[x]][,2:ncol(newdat[[x]])]),1,10)))) & 
           mydate$year==myyearvec[i],]
  
  error.2 <- (data.frame(lapply(min(myvec):max(myvec),function(x) newdat[[x]][,2:ncol(newdat[[x]])] %>% 
                       summarise_all(funs(sum(is.na(.))))),check.names = FALSE) %>% 
      gather(date,count) %>% 
      filter(count>230) %>% 
      mutate(mydatechar=substr(date,1,10)) %>% 
      distinct(mydatechar))$mydatechar
  
  if(length(error.1)>0 & length(error.2)>0){
  myerrors <- which(mydate$mydatechar %in% 
                      (bind_rows(error.1,mydate %>% filter(mydatechar %in% error.2)))$mydatechar)
  }  else if (length(error.1)>0 & length(error.2)==0) {
    myerrors <- which(mydate$mydatechar %in% error.1)
  }  else if (length(error.1)==0 & length(error.2)>0){
    myerrors <- which(mydate$mydatechar %in% error.2)    
  }  else {
  print("No Errors")
  }
  myerrorvec <- c(myerrorvec,myerrors)
  }
  
myerrors <- myerrorvec

#  Now rerun the satellite data query for each of the missing days above (there were 14 from 2003 - May 2018)

errorlist <- list()
system.time({
  for(i in 1:length(myerrors)){
    tryCatch({ 
      print(i)
      print(mydate[myerrors[i],])
      #  Query the satellite data as a netcdf file.
      #URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
      x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[myerrors[i]],
                               "T09:00:00Z):1:(",mydate$mydate[myerrors[i]],"T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"))
      #x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(55):(56)][(-175.99):(-175.01)]"))
      
      #  Convert and open the netcdf file
      tmpSST <- tempfile(pattern="xwB", fileext=".nc")
      writeBin(object=x, con=tmpSST)
      nc <- nc_open(tmpSST)
      
      #  For the first day of the error list
      if(i==1) {
        #basket <- data.frame()
        holderdat <- data.frame()
        #  Extract the first three columns from the netcdf (long, lat, sst)
        tempneg <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                               lat=ncvar_get(nc, varid = "latitude")) %>% 
                               {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
          mutate(sst=ifelse(sst<(-2),NA,sst))
        
        #  So that we don't store the date as an extra column, rename the sst column with the date. 
        names(tempneg)[3] <- as.character(mydate$mydate[myerrors[i]])
        
        #  Rename this data frame, which will become important for binding in the next iteration of the loop
        basket <- tempneg
        
        #  For all remaining days in the error file 
      } else {
        #  Extract netcdf data but only keep the 3rd column, which contains the sst.
        holderdat <- expand.grid(lon=ncvar_get(nc, varid = "longitude"),
                                 lat=ncvar_get(nc, varid = "latitude")) %>% 
                                 {bind_cols(.,data.frame(sst=as.vector(ncvar_get(nc,"analysed_sst"))))} %>% 
          mutate(sst=ifelse(sst<(-2),NA,sst))
        
        holderdat <- data.frame(holderdat[,3])
        #  Rename the sst column to be the date. 
        names(holderdat) <- as.character(mydate$mydate[myerrors[i]])
        
        #  Bind the data from the previous iteration to the new data. This will include data from all previous iterations.
        basket <- bind_cols(basket,holderdat)
        
        rm(holderdat,x)
      }
      
      #  Close the netcdf file.
      nc_close(nc)
      print(dim(basket))
      #  For the last day of each month, compile the monthly data and summarise.
      if(i==length(myerrors)){
        
        
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
                 date=mydate$mydate[myerrors[i]]) %>% 
          dplyr::select(STAT_AREA,index) %>% 
          left_join(junk)
        
        
        rm(basket,mycols,nc)
      }
      
      
      x <- getBinaryURL(paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[myerrors[i]],
                               "T09:00:00Z):1:(",mydate$mydate[myerrors[i]],"T09:00:00Z)][(47):(60)][(167):(179.9)]"))
      
      tmpSST <- tempfile(pattern="xwB", fileext=".nc")
      writeBin(object=x, con=tmpSST)
      ncpos <- nc_open(tmpSST)
      
      
      #  For the first day of the errors create a new container for the data.
      if(i==1) {
        #basket <- data.frame()
        holderdatpos <- data.frame()
        #  extract the first three columns from the netcdf (long, lat, sst)
        temppos <- expand.grid(lon=ncvar_get(ncpos, varid = "longitude"),
                               lat=ncvar_get(ncpos, varid = "latitude")) %>% 
                               {bind_cols(.,data.frame(sst=as.vector(ncvar_get(ncpos,"analysed_sst"))))} %>% 
          mutate(sst=ifelse(sst<(-2),NA,sst))
        
        #  So that we don't store the date as an extra column, rename the sst column with the date. 
        names(temppos)[3] <- as.character(mydate$mydate[myerrors[i]])
        
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
        names(holderdatpos) <- as.character(mydate$mydate[myerrors[i]])
        
        #  Bind the data from the previous iteration to the new data. This will include data from all previous iterations.
        basketpos <- bind_cols(basketpos,holderdatpos)
        
        rm(holderdatpos,x)
      }
      
      #  Close the netcdf file.
      nc_close(ncpos)
      print(dim(basketpos))
      
      #  For the last day of each month, compile the monthly data and summarise.
      if(i==length(myerrors)){
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
                 date=mydate$mydate[myerrors[i]]) %>% 
          dplyr::select(STAT_AREA,index) %>% 
          left_join(junk)
        
        
        errorlist[[i]] <- bind_rows(dataneg %>% dplyr::select(-index),
                               datapos %>% dplyr::select(-index))
        
        rm(basketpos,dataneg,datapos)
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
})

saveRDS(errorlist,file="ExtraDays.RDS")

myvec <- which(unlist(lapply(1:length(errorlist),function(x)is.null(errorlist[[x]])))==FALSE)

testerrors <- errorlist[[myvec]] %>% 
  dplyr::select(STAT_AREA,contains("mean")) %>% 
  gather(date,sst.mean,-STAT_AREA) %>% 
  mutate(date=substr(date,1,10)) %>% 
  inner_join(
    errorlist[[myvec]] %>% 
      dplyr::select(STAT_AREA,contains("sd")) %>% 
      gather(date,sst.sd,-STAT_AREA) %>% 
      mutate(date=substr(date,1,10))
  )

saveRDS(testerrors,file="ExtraDays_long.RDS")

#  End Fixing Errors code chunk
#------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
#  Now combine the data from all years with the rerun data
#------------------------------------------------------------------------------------------------------------------

#  I first created a temporary dataset to make sure I didn't overwrite the good data. It can be deleted. 
alldat <- data.frame()
for(i in 1:length(myyearvec)){
  print(i)
longdat <- readRDS(paste0("myyear_",myyearvec[i],"long.RDS"))
alldat <- bind_rows(alldat,
                    longdat)
}
saveRDS(alldat,file="SST_2003_2018_temp.RDS")
      
#  Some of the days that were rerun (the ones in errorlist above) appeared in the full data but with NAs. 
#  When the rerun days are bound to the full data, remove those days that were rerun. 
alldat <-  bind_rows(readRDS("SST_2003_2018_temp.RDS") %>% 
                       filter(!date%in%testerrors$date),testerrors)

#  Add additional temporal fields
test <- alldat %>% 
  mutate(date=as.Date(date,format="%Y-%m-%d"),
           year=as.numeric(format(date,"%Y")),
         month=as.numeric(format(date,"%m")),
         julian=as.POSIXlt(date)$yday+1,
         week=as.numeric(format(date,"%U"))+1) %>% 
  arrange(date)

saveRDS(test,file="SST_2003_2018.RDS")
#  End Combine data code chunk
#------------------------------------------------------------------------------------------------------------------

