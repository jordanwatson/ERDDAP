

#---------------------------------------------------------------------------------------------------
library(tidyverse)
library(rgdal)
#---------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------
#  Create the spatial lookup table for ADFG groundfish statistical areas 
#  Shapefile downloaded from: 
#  http://soa-adfg.opendata.arcgis.com/datasets/groundfish-statistical-areas-2001?geometry=-222.188%2C53.09%2C-79.981%2C67.923
#  Accessed on: 12/19/2018
statareas <- readOGR(dsn="Data",layer="Groundfish_Statistical_Areas_2001_v2018")
#  Extra the extents for positive and negative longitudes
dfstat <- fortify(statareas) %>% 
  summarise(maxlonpos=max(long[long>0]),
            maxlatpos=max(lat[long>0]),
            minlonpos=min(long[long>0]),
            minlatpos=min(lat[long>0]),
            maxlonneg=max(long[long<0]),
            maxlatneg=max(lat[long<0]),
            minlonneg=min(long[long<0]),
            minlatneg=min(lat[long<0]))

#  Create a lat-lon grid in 0.01 degree increments.
mygrid <- bind_rows(expand.grid(lon=seq(dfstat$minlonpos,dfstat$maxlonpos,by=0.01),
                                lat=seq(dfstat$minlatpos,dfstat$maxlatpos,by=0.01)),
                    expand.grid(lon=seq(dfstat$minlonneg,dfstat$maxlonneg,by=0.01),
                                lat=seq(dfstat$minlatneg,dfstat$maxlatneg,by=0.01))) %>% 
  mutate(index=as.character(1:n()))

#  Before we convert the lat-lon grid to a Spatial (S4) object, make a copy.
mygrid2 <- mygrid
#  Convert lat-lon grid to a SpatialPointsDataFrame object
coordinates(mygrid) <- ~lon + lat
#  Match the grid projection to that of the statistical areas shapefile
proj4string(mygrid) <- proj4string(statareas)

#  Perform a point-in-polygon (PIP) operation. The way that the "over" function is written, it will output the full shapefile dataframe.
#  For rows where a point did not fall within a polygon, all rows will be NA. 
#  For rows where a point fell within a polygon, the data for the matching polygon will appear.
#  The row numbers correspond to the index value from the SpatialPointsDataFrame. 
#  We will create a column with these row numbers so that we can then join to the orinal lat-lon grid, which contains the grid coordinates.

test1 <- over(mygrid,statareas) %>% # Perform PIP operation
  dplyr::select(STAT_AREA) %>% # Extract the stat_area field
  rownames_to_column(var="index") %>% # Create a column of the row numbers
  filter(!is.na(STAT_AREA)) %>% #  Filter out unmatched points
  inner_join(mygrid2)  # Match to the lat-lon grid
#---------------------------------------------------------------------------------------------------

library(readxl)
disc <- read_excel("DISCREPANCIES.xlsx",sheet="DISCREPANCIES")

boblkp <- read_csv("STAT_AREA_Lookup.csv")

attrdata <- read_csv("Data/stat_area_attribute_table.txt") %>% 
  dplyr::select(STAT_AREA,area=Shape__Are) %>% 
  mutate(STAT_AREA=as.character(STAT_AREA))

newlkp <- readRDS("lkptable_output_05102018.RDS")

spjoin <- read_csv("CompareShapes.csv")


tempneg %>% filter(between(lon,-148.71,-148.58) & between(lat,68.45,68.5))

# newdat is read-in from the ERDDAP shapefile. 
compdata <- as.data.frame(newdat) %>% 
  inner_join(disc)

testsum <- test1 %>% 
  group_by(STAT_AREA) %>% 
  summarise(newlkprecords=n())


lkp <- data %>% 
  group_by(STAT_AREA) %>% 
  summarise(lkpcount=n()) %>% 
  ungroup %>% 
  data.frame %>% 
  mutate(STAT_AREA=as.character(STAT_AREA)) %>% 
  inner_join(attrdata)

newdat <- as.data.frame(newdat)

compare <- newdat %>% 
  data.frame %>% 
  full_join(lkp) %>% 
  full_join(testsum) %>% 
  mutate(mydiff=records-lkpcount,
         lkpdiff=lkpcount-newlkprecords) %>% 
  arrange(-mydiff)

p1 <- compare %>% 
  ggplot(aes(area,records)) + 
  geom_point()

p2 <- compare %>% 
  ggplot(aes(area,lkpcount)) + 
  geom_point()

grid.arrange(p1,p2,ncol=2)





newdat %>% 
  data.frame %>% 
  full_join(lkp) %>% 
  mutate(mydiff=records-lkpcount) %>% 
  arrange(-mydiff) %>% 
  filter(STAT_AREA==486032)


data %>% 
  filter(STAT_AREA==486032) %>% 
  ggplot(aes(lon,lat)) + 
  geom_point()




WorldData <- map_data('worldhires') %>% fortify()
#WorldData <- fortify(WorldData)


x11();data %>% 
  filter(STAT_AREA==865302) %>%
  ggplot(aes(lon,lat)) +
  geom_point() + 
  geom_map(data=WorldData, map=WorldData,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="grey", colour="#7f7f7f", size=0.5) + 
  xlim(170,180) + 
  ylim(50,55)

