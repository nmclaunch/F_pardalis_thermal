###code that downloads prism daily data for Florida###
library(tidyr)
library(devtools)
#library(Rtools)
library(reshape2)
library(dplyr)
library(raster)
library(sp)
library(rnaturalearth)
library(rnaturalearthhires)
library(rgdal)
library(prism)


getwd()
setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/min")

####download daily prism data (mins)####
options(prism.path = "C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/min")
####Data are only available until October 2021.

get_prism_dailys(type="tmin", minDate = "2001-12-15", maxDate = "2002-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2002-12-15", maxDate = "2003-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2003-12-15", maxDate = "2004-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2004-12-15", maxDate = "2005-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2005-12-15", maxDate = "2006-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2006-12-15", maxDate = "2007-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2007-12-15", maxDate = "2008-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2008-12-15", maxDate = "2009-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2009-12-15", maxDate = "2010-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2010-12-15", maxDate = "2011-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2011-12-15", maxDate = "2012-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2012-12-15", maxDate = "2013-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2013-12-15", maxDate = "2014-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2014-12-15", maxDate = "2015-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2015-12-15", maxDate = "2016-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2016-12-15", maxDate = "2017-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2017-12-15", maxDate = "2018-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2018-12-15", maxDate = "2019-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2019-12-15", maxDate = "2020-02-15", keepZip=FALSE)
get_prism_dailys(type="tmin", minDate = "2020-12-15", maxDate = "2021-02-15", keepZip=FALSE)

####download daily prism data (maxs)####
setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/max")
options(prism.path = "C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/max")
get_prism_dailys(type="tmax", minDate = "2001-12-15", maxDate = "2002-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2002-12-15", maxDate = "2003-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2003-12-15", maxDate = "2004-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2004-12-15", maxDate = "2005-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2005-12-15", maxDate = "2006-02-15", keepZip=FALSE)

get_prism_dailys(type="tmax", minDate = "2006-12-15", maxDate = "2007-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2007-12-15", maxDate = "2008-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2008-12-15", maxDate = "2009-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2009-12-15", maxDate = "2010-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2010-12-15", maxDate = "2011-02-15", keepZip=FALSE)

get_prism_dailys(type="tmax", minDate = "2011-12-15", maxDate = "2012-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2012-12-15", maxDate = "2013-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2013-12-15", maxDate = "2014-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2014-12-15", maxDate = "2015-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2015-12-15", maxDate = "2016-02-15", keepZip=FALSE)

get_prism_dailys(type="tmax", minDate = "2016-12-15", maxDate = "2017-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2017-12-15", maxDate = "2018-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2018-12-15", maxDate = "2018-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2019-12-15", maxDate = "2020-02-15", keepZip=FALSE)
get_prism_dailys(type="tmax", minDate = "2020-12-15", maxDate = "2021-02-15", keepZip=FALSE)



#### move files with ubuntu ####
####extract and aggregate files from individual folders into one
### navigate to the folder where they are stored
# pwd=print working directory. cd= change working directory
# Change to folder: /mnt/c/Users/nmcla/
# can change directory one by one (cd Dropbox) (cd 2PhysioHerpInvasives_Shared)
# navigate to ~/prism/dailytest folder
# ls is list files
###code is:  move files that are all .bil into one place in the same directory we are in
## spacing is VERY IMPORTANT
## mv **/*.bil .
## mv **/*.hdr .
## to go back one directory use " cd -"


###convert multiple Bil files to raster brick and crop to extent of Florida####
setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/min")
current.list<-list.files(path=".", pattern="bil.bil$", full.names=TRUE) #make sure wd is where bil files are
current.list #this is a list of all files we want to convert from our current working directory
raster_list<-lapply(current.list, raster) #convert files in the list to rasters
raster_list[1] #call files by number in list


rbr<-brick(raster_list)

####crop an individual raster or brick
usa_map<-ne_states(country="united states of america", returnclass="sf")
fl <- filter(usa_map, name == "Florida")
fl2 <- fl[1] #only want to use one shapefile for plotting
plot(fl2)

rs_fl_mask <- mask(rbr,fl2) #first entry can be either a brick of rasters or a single raster
#plot(rs_fl_mask)
rs_fl_m_c <- crop(rs_fl_mask,fl2) #crop the map to the size of Florida
#plot(rs_fl_m_c)

##### extract cropped files from the brick and save them/write as tif
#writeRaster(rs_fl_m_c, paste0(names(rs_fl_m_c),".tif"), bylayer=TRUE, format="GTiff")
r1 <- raster("PRISM_tmin_stable_4kmD2_20130121_bil.tif")
#plot(r1)

###crop to extent of Florida for max data too
setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/max")
current.list<-list.files(path=".", pattern="bil.bil$", full.names=TRUE) #make sure wd is where bil files are
current.list #this is a list of all files we want to convert from our current working directory
raster_list<-lapply(current.list, raster) #convert files in the list to rasters
raster_list[1] #call files by number in list


rbr<-brick(raster_list)

####crop an individual raster or brick
usa_map<-ne_states(country="united states of america", returnclass="sf")
fl <- filter(usa_map, name == "Florida")
fl2 <- fl[1] #only want to use one shapefile for plotting
plot(fl2)

rs_fl_mask <- mask(rbr,fl2) #first entry can be either a brick of rasters or a single raster
#plot(rs_fl_mask)
rs_fl_m_c <- crop(rs_fl_mask,fl2) #crop the map to the size of Florida
#plot(rs_fl_m_c)

##### extract cropped files from the brick and save them/write as tif
#writeRaster(rs_fl_m_c, paste0(names(rs_fl_m_c),".tif"), bylayer=TRUE, format="GTiff")
r1 <- raster("PRISM_tmin_stable_4kmD2_20130121_bil.tif")
#plot(r1)






###Read in the cropped Tif files as brick, get dataframe of points for min and max####
setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/min")
current.listmin<-list.files(path=".", pattern="bil.tif$", full.names=TRUE) #make sure wd is where tif files are
current.listmin #this is a list of all files we want to convert from our current working directory
raster_listmin<-lapply(current.listmin, raster) #convert files in the list to rasters
raster_listmin[1] #call files by number in list

#can brick from this list
rbrmin<-brick(raster_listmin)

stackpointmin<-rasterToPoints(rbrmin)

#rename all columns to just the date portion
colnames(stackpointmin) <- sub("PRISM_tmin_stable_4kmD2_", "", colnames(stackpointmin))
colnames(stackpointmin) <- sub("\\_bil", "", colnames(stackpointmin))
#pivot dataframe so dates/lat longs are individual observations
#library(tidyr)
minframe<-as.data.frame(stackpointmin)
ncol(minframe)
minlong <- pivot_longer(minframe, cols=3:1262, names_to = "Date", values_to = "Tmin")

#do the above with tmax data
getwd()
setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years/max")

current.listmax<-list.files(path=".", pattern="bil.tif$", full.names=TRUE) #make sure wd is where tif files are
current.listmax #this is a list of all files we want to convert from our current working directory
raster_listmax<-lapply(current.listmax, raster) #convert files in the list to rasters
raster_listmax[1] #call files by number in list

#can brick from this list
rbrmax<-brick(raster_listmax)

stackpointmax<-rasterToPoints(rbrmax)



colnames(stackpointmax) <- sub("PRISM_tmax_stable_4kmD2_", "", colnames(stackpointmax))
colnames(stackpointmax) <- sub("\\_bil", "", colnames(stackpointmax))

maxframe<-as.data.frame(stackpointmax)
ncol(maxframe)
maxlong <- pivot_longer(maxframe, cols=3:1262, names_to = "Date", values_to = "Tmax")


#merge tmax with tmin based on date, x, y

t1<-merge(x = minlong, y = maxlong, by = c("Date","x","y"), all.x = TRUE)

df <-
  tidyr::separate(
    data = t1,
    col = Date,
    sep = c(4, 6, 8),
    into = c("Year", "Month", "Day"),
    remove = FALSE
  ) 

#format Year Month Day as numeric
df$Year<-as.numeric(df$Year)
df$Month<-as.numeric(df$Month)
df$Day<-as.numeric(df$Day)

setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM/prism/DectoFeb20years")

#write.csv(df, "20yFlorida.csv")
getwd()
