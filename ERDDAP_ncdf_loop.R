library(tidyverse)
library(ncdf4)
library(rgdal)
library(RCurl)

stat <- readOGR(dsn="Data",layer="pvg_stat_2001")
stat <- spTransform(stat,CRS("+proj=longlat +datum=NAD83"))
#dest <-  "myerddapneg.nc" 
#dest2 <-  "myerddappos.nc" 


mydate <- data.frame(mydate=seq(as.Date("2010-01-01"), as.Date("2018-05-01"), "days")) %>% 
  mutate(year=format(mydate,"%Y")) %>% 
  group_by(year) %>% 
  mutate(julian=1:n())

newdat <- list()

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------


system.time({
  for(i in 1:365){
    if(mydate$julian[i]==1) saveRDS(newdat,file=paste0("myyear_",mydate$year[i],".RDS"))
    tryCatch({

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
      
      #download.file(url=paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(60)][(167):(179.9)]"),
      #              destfile=dest,
      #              mode="wb",
      #              quiet=TRUE)
      
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
      
      newdat[[mydate$julian[i]]] <- bind_rows(
        dataneg,
        datapos
      ) %>% 
        distinct() %>% 
        filter(!is.na(sst)) %>% 
        mutate(sst=round(sst,3))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
})
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

saveRDS(newdat,file=paste0("myyear_",mydate$year[i],".RDS"))

rbind((lapply(newdat,function(x)(nrow(x)))))
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





system.time({
  for(i in 1:2){
    if(mydate$julian[i]==1) saveRDS(newdat,file=paste0("myyear_",mydate$year[i],".RDS"))
    tryCatch({
      system.time({
        download.file(url=paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"),
                      destfile=dest,
                      mode="wb",
                      quiet=TRUE)
      })
      
      
      URL <- paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]")
      URL <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(2010-01-04T09:00:00Z):1:(2010-01-04T09:00:00Z)][(47):(68)][(-179.99):(-130.01)]"
      x <- getURL(URL)
      
      options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))   
      URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
      x <- getBinaryURL(URL)
      
      SSTGet <- getBinaryURL(URL_SST, ftp.use.epsv = FALSE) 
      tmpSST <- tempfile(pattern="xwB", fileext=".nc")
      writeBin(object=x, con=tmpSST)
      nc <- nc_open(tmpSST)
      
      nc <- nc_open(dest)
      nc <- nc_open(x)
      
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
        left_join(testpos)
      
      rm(temp)
      
      download.file(url=paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41.nc?analysed_sst[(",mydate$mydate[i],"T09:00:00Z):1:(",mydate$mydate[i],"T09:00:00Z)][(47):(60)][(167):(179.9)]"),
                    destfile=dest,
                    mode="wb",
                    quiet=TRUE)
      
      nc <- nc_open(dest2)
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
        left_join(testpos)
      
      rm(temp)
      
      newdat[[mydate$julian[i]]] <- bind_rows(
        dataneg,
        datapos
      ) %>% 
        distinct() %>% 
        filter(!is.na(sst))
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
})
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

