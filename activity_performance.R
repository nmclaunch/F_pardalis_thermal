

##This code requires full hourly data for 2001 to 2020 Dec 15-Feb 15, achieved by running ChillR on PRISM data.

##It filters to return only daylight hours, when Furcifer pardalis is potentially active
##Due to memory limitations, this code is run in batches and dataframes are saved as .csv, 
##then read in and appended together at the end.
##The daylight hours subset is then used to estimate activity and preference rasters.
##Finally, all models are thresholded for comparison.


setwd("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleon_SDM")

#packages for all analyses:
library(dtplyr)
library(data.table)
library(tidyverse)
library(tidyr)
library(dplyr)
library(raster)
library(ggplot2)
library(mgcv)
library(gamair)


####filter out hours that are below sunrise and above sunset####
####Take all the hourly data, filter for daylight hours, then append together in one table
d1<-fread("Full Hourly Data/year0103df220012.csv")
d2<-fread("Full Hourly Data/year0103df220022.csv")
d3<-fread("Full Hourly Data/year0103df220032.csv")
d4<-fread("Full Hourly Data/year0406df220042.csv")
d5<-fread("Full Hourly Data/year0406df220052.csv")
d6<-fread("Full Hourly Data/year0607df220062.csv")
d7<-fread("Full Hourly Data/year20074Nat.csv")
d8<-fread("Full Hourly Data/year2008to20134NC/year0809df220082.csv")
d9<-fread("Full Hourly Data/year2008to20134NC/year0809df220092.csv")
d10<-fread("Full Hourly Data/year2008to20134NC/year1011df220102.csv")

### do the daylight sorting in 2 batches of 10 to save memory.


d11<-fread("Full Hourly Data/year2008to20134NC/year1011df220112.csv")
d12<-fread("Full Hourly Data/year2008to20134NC/year1213df220122.csv")
d13<-fread("Full Hourly Data/year2008to20134NC/year1213df220132.csv")
d14<-fread("Full Hourly Data/year2014to20204NC/year1415df220142.csv")
d15<-fread("Full Hourly Data/year2014to20204NC/year1415df220152.csv")
d16<-fread("Full Hourly Data/year2014to20204NC/year1617df220162.csv")
d17<-fread("Full Hourly Data/year2014to20204NC/year1617df220172.csv")
d18<-fread("Full Hourly Data/year2014to20204NC/year1819df220182.csv")
d19<-fread("Full Hourly Data/year2014to20204NC/year1819df220192.csv")
d20<-fread("Full Hourly Data/year2014to20204NC/year2020df220202.csv")


##where the hourtemps dataframe is long format hourly temperatures for each pixel, YMD
##needs to have the sunrise/sunset columns!

#apply this code to every d# above, append to the allday dataframe.
d2<-d2%>%
  filter(Hour>Sunrise)%>%
  filter(Hour<Sunset)

d2<-as_tibble(d2)

d1<-d1%>%
  filter(Hour>Sunrise)%>%
  filter(Hour<Sunset)

d1<-as_tibble(d1)

allday<-rbind(allday, d20)
unique(allday$Date)

#d19<-0 #reassign names as 0 once finished to save memory without clearing environment completely.

#ftable(allday)
str(allday)
# as ftable, then fwrite
#fwrite(allday, "Full Hourly Data/11to20.csv")


unique(allday$Date)


####Generate activity window from Tpref data####

#We use the min and max data for thermal preference collected in lab trials to generate an activity window
#Using this range, we filter through the hours on the spreadsheet for active temperatures "yes" and inactive temperatures "no"
#We then count number of active/inactive per day,
#then average # yes's per day per winter period (daily average winter activity window)

pref<-read.csv("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleons/thermal preference/thermal_pref_summary_merged.csv") #adjust to your local directory
#filter out diseased/dying animals
pref<-filter(pref,notes_pref!="moribund")
mean(pref$min_prefbody_temp)
min(pref$min_prefbody_temp)

mean(pref$max_prefbody_temp)
max(pref$max_prefbody_temp)
###using averages our window is 27.072 to 37.168, let's use 27 to 37###
#minimum in gradient is 21.9 max is 40.6








###Read in the data thresholded by daylight hours

d1<-fread("Full Hourly Data/01to10.csv")
#do process with first decade, then second, then append.

###probably have to run code separately for each 10 year batch, memory issue.

####apply activity threshold to climate data####
##it is possible that 27 to 37 is too restricted
d1$active_smallwindow<-ifelse(d1$Temp <= 27, "no",
                       ifelse(d1$Temp >=37,"no", "yes"))

#d1<-fread("Full Hourly Data/01to10.csv")
d1<-fread("Full Hourly Data/11to20.csv")

#this is a larger window for all temps measured in gradient 22-40.5
#use this in analyses
d1$active_lrgwindow<-ifelse(d1$Temp <= 22, "no",
                                   ifelse(d1$Temp >=40.5,"no", "yes"))

#count number of yes per day
d1sum <- d1 %>%
  group_by(x2,y2,Year,Month,Day)%>%
  count(active_lrgwindow, name= "nhours")

d1sum<-as_tibble(d1sum)

####There are many instances where all hours are active ("yes") or inactive ("no") variables for a pixel and date.

###This code adds "yes" and "0" where they are missing
nos<-d1sum%>%
  filter(active_lrgwindow=="no")%>%
  filter(nhours==11)
nos<-as_tibble(nos)
nhours11_new <-replace(nos, 7,0)
nhours11_new <-replace(nhours11_new, 6,"yes")
d1_zeroes_added <-rbind(d1sum,nhours11_new)

#Then, add "no" and "0" where they are missing
yess<-d1sum%>%
  filter(active_lrgwindow=="yes")%>%
  filter(nhours==11)
yess<-as_tibble(yess)
nhours11_new <-replace(yess, 7,0)
nhours11_new <-replace(nhours11_new, 6,"no")
d1_zeroes_added <-rbind(d1_zeroes_added,nhours11_new)

#write the end product to a csv
#fwrite(d1_zeroes_added, "Full Hourly Data/11to20activesummary.csv")

#APPEND THE 2 DECADES TOGETHER HERE
d1<-fread("Full Hourly Data/01to10activesummary.csv")
d2<-fread("Full Hourly Data/11to20activesummary.csv")
alldata<-rbind(d1,d2)

###calculate active hours

#average hours per pixel per day active or not across ENTIRE winter period
avg_activity<-aggregate(list(nhours=alldata$nhours), by=list(x=alldata$x2, y=alldata$y2, Category=alldata$active_lrgwindow), FUN=mean) 

#this outputs mean # daylight hours inactive (no) and active (yes)

#then calculate the % of active daylight hours per winter period per pixel.
percactive<-avg_activity%>%
  group_by(x,y)%>%
  mutate(perc= ifelse(Category=="yes", 100*(nhours[Category=="yes"]/(nhours[Category=="yes"]+nhours[Category=="no"])), NA))%>%
  drop_na(perc)

#fwrite(percactive, "Full Hourly Data/01to20activitysummary.csv")
#this final dataframe gives both average daylight active hours and % active daylight hours

###now to convert to a raster###
activity<-fread("Full Hourly Data/01to20activitysummary.csv")

###convert the average hours per pixel back into a raster#
coordinates(activity)<- ~x+y

#read in already-cropped FL raster to copy extent
r<-raster("Thresholded_daily_temps/9C/PanChemT9L6.tif") #this is a raster from CTmin threshold
activity_nhours_raster<-rasterize(activity[,1:2], r, activity$nhours)
activity_percent_raster<-rasterize(activity[,1:2], r, activity$perc)

plot(activity_nhours_raster)
plot(activity_percent_raster)

#writeRaster(activity_nhours_raster, paste0(names(activity_nhours_raster),"activity_nhours_raster.tif"), bylayer=TRUE, format="GTiff")
#writeRaster(activity_percent_raster, paste0(names(activity_percent_raster),"activity_percent_raster.tif"), bylayer=TRUE, format="GTiff")








#####Generate curve from Performance Data####

#extract best performance values from trials from individuals at each temperature/season. 
#fit a GAM to the performance data, generating performance prediction curve
#use the equation to get a performance value by inputting a matrix of temperature values
#apply the equation to all the daytime hourly data for each pixel
#average daily predicted performance per pixel for winter period 
#determine areas that fall below 80% predicted performance threshold

df<-read.csv("performance_summary_withlimits.csv")
str(df)
df$sample<-as.factor(df$sample)
df$temp<-df$trial_temp
df$rate<-df$max_vel_25cm_ms
###testing a single curve

#filter outlier out of it
df<-filter(df, rate<=1)

ggplot(df, aes(temp, rate))+
  geom_point()+
  theme_bw(base_size=12)+
  labs(x='Temperature',
       y='Sprint Speed')
###testing a single curve
d<-filter(df, sample=="465 fall")
ggplot(d, aes(temp, rate))+
  geom_point()+
  theme_bw(base_size=12)+
  labs(x='Temperature',
       y='Sprint Speed')


#######Fit curve for all data ######


#####GAMS#####
####can go up to k=7 bc that is the # of temperatures tested.

m3<-gam(rate ~ s(temp, k=5), data = df, method="REML", sp=0.1)
plot(m3,residuals = TRUE, pch = 1, shade=TRUE, shift = coef(m3)[1], seWithMean = TRUE)

m3$sp

summary(m3)

gam.check(m3)

concurvity(m3, full = TRUE)

####Apply curve to hourly dataset####


#Apply the curve to a set of numbers encompassing all possible temperatures

##Choose the simplest curve, and we don't expect any individual chameleon to be 
#more representative than another.
new_data <- data.frame(temp = seq(-10.2,45, by=0.1))
new_data$temp<-round(new_data$temp, digits=1)
m3_pred <- bind_cols(new_data,
                     as.data.frame(predict(m3, newdata = new_data,
                                           se.fit = TRUE)))

max(m3$fit)


####extract and condense outputs from hourly dataset (subsetted for daylight)####
library(data.table)
#Read in the daylight hourly temperature data for Florida.
d1<-fread("Full Hourly Data/01to10.csv")

##reassigning variable as 0 to save memory in second iteration
#new_data<-0
#hourtemps<-0
#x<-0
gc()

#uncomment and read in after saving the predictions appended to 01to10 data
#d1<-fread("Full Hourly Data/11to20.csv")


##Run the following for 01to10, save as a .csv, then run for 11to20 and save. 
d1$Hour<-as.numeric(d1$Hour)
#need to round all hourtemps so they will match, as precision of prediciton is 0.1
d1$Temp<-round(d1$Temp, digits=1)
head(d1)
#need to convert temps to character for exact joining.
d1$Temp<-as.character(d1$Temp)


#match the case for joining..
m3_pred<-rename(m3_pred, Temp=temp)
m3_pred$Temp<-as.character(m3_pred$Temp)


str(d1)
str(m3_pred)

###join predictions to hourly temperature data

x<-left_join(
  d1,
  m3_pred,
  by = "Temp",
  copy = FALSE,
  keep = FALSE
)

##save as a .csv here.
#fwrite(x, "01to10_day_performance.csv")
#fwrite(x, "11to20_day_performance.csv")



####The following was ran in a computing cluster due to personal computer memory use limitations
###User mileage may vary
####Read in performance data, combine and write to raster.#####

#head(d2)
d1<-fread("01to10_day_performance.csv")
d2<-fread("11to20_day_performance.csv")
alldata<-rbind(d1,d2)

####calculate average performance across all hours all pixels.
avg_performance<-aggregate(list(performance=alldata$fit), by=list(x=alldata$x2, y=alldata$y2), FUN=mean) 


#calculating within 80% max performance
0.2*max(m3$fit)
##anywhere above 0.05071189 is within 80% max performance (predicted)

performance_80<-avg_performance%>%
  group_by(x,y)%>%
  mutate(perf80= ifelse(performance>=0.051, "pred_above80", "pred_below80"))

#this final dataframe gives both average performance per pixel and thresholds (above or below 80% performance)
#fwrite(performance_80, "summary_winter_perf.csv")

###now to convert to a raster###
#make as above
perf<-fread("Activity_Preference_outputs/summary_winter_perf.csv")

#convert above 0% performance to 1 and below to 0 and  for raster output
perf$perf80_log<-ifelse(perf$perf80 == "pred_above80", 1,0)

library(raster)
coordinates(perf)<- ~x+y

#read in already-cropped FL raster to copy geographic extent
r<-raster("Thresholded_daily_temps/9C/PanChemT9L6.tif")
avg_winter_perf_raster<-rasterize(perf[,1:2], r, perf$performance)
threshold_80_perf_raster<-rasterize(perf[,1:2], r, perf$perf80_log)

plot(avg_winter_perf_raster)
plot(threshold_80_perf_raster)
#there are no pixels where it falls below average 80% performance,
#therefore we will not report this map in publication.

#writeRaster(avg_winter_perf_raster, paste0(names(avg_winter_perf_raster),"avg_winter_perf_raster.tif"), bylayer=TRUE, format="GTiff")


###############Threshold all thermal trait inferences for comparison########
####compare the predictions from three CTmin thresholds, 
####activity thresholds, and performance
#### by setting them all to an equivalent scale 0 to 1 as rasters

###CTmin will have to read in raster to points
CTmin3<-raster("Raster_outputs/PanChemT3L6.tif")
CTmin6<-raster("Raster_outputs/PanChemT6L6.tif")
CTmin9<-raster("Raster_outputs/PanChemT9L6.tif")

CTmin3<-rasterToPoints(CTmin3)
CTmin3<-as.data.frame(CTmin3)
max(CTmin3$PanChemT3L6)#divide by 33
#33
min(CTmin3$PanChemT3L6)#no need to subtract, already at 0
#0


CTmin3<-CTmin3%>%
  mutate(ndays_thresholded = (PanChemT3L6/33))
#check for 0-1 scale
min(CTmin3$ndays_thresholded)
max(CTmin3$ndays_thresholded)
#good

CTmin6<-rasterToPoints(CTmin6)
CTmin6<-as.data.frame(CTmin6)
max(CTmin6$PanChemT6L6)#divide by 47.9
#47.9
min(CTmin6$PanChemT6L6)#no need to subtract, already at 0
#0

CTmin6<-CTmin6%>%
  mutate(ndays_thresholded = (PanChemT6L6/47.9))
#check for 0-1 scale
min(CTmin6$ndays_thresholded)
max(CTmin6$ndays_thresholded)
#good

CTmin9<-rasterToPoints(CTmin9)
CTmin9<-as.data.frame(CTmin9)
CTmin9$PanChemT9L6<-round(CTmin9$PanChemT9L6, digits=2)
max(CTmin9$PanChemT9L6)#see max
#63.15
min(CTmin9$PanChemT9L6)#subtract 0.45 from all to set 0
#0.45
63.15-0.45 #divide all by 62.7 to set 1

CTmin9<-CTmin9%>%
  mutate(ndays_thresholded = (PanChemT9L6 -0.45)/62.7)
#check for 0-1 scale
min(CTmin9$ndays_thresholded)
max(CTmin9$ndays_thresholded)
#good.

coordinates(CTmin3)<- ~x+y
coordinates(CTmin6)<- ~x+y
coordinates(CTmin9)<- ~x+y
r<-raster("Thresholded_daily_temps/9C/PanChemT9L6.tif")
PanChemT3L6_thresholded<-rasterize(CTmin3[,1:2], r, CTmin3$ndays_thresholded)
PanChemT6L6_thresholded<-rasterize(CTmin6[,1:2], r, CTmin6$ndays_thresholded)
PanChemT9L6_thresholded<-rasterize(CTmin9[,1:2], r, CTmin9$ndays_thresholded)
#writeRaster(PanChemT3L6_thresholded, paste0(names(PanChemT3L6_thresholded),"PanChemT3L6_thresholded"), bylayer=TRUE, format="GTiff")
#writeRaster(PanChemT6L6_thresholded, paste0(names(PanChemT6L6_thresholded),"PanChemT6L6_thresholded"), bylayer=TRUE, format="GTiff")
#writeRaster(PanChemT9L6_thresholded, paste0(names(PanChemT9L6_thresholded),"PanChemT9L6_thresholded"), bylayer=TRUE, format="GTiff")




###Activity, Preference can read from CSV.
activity<-read.csv("Activity_Preference_outputs/01to20activitysummary.csv") ###this is created above as 'percactive'
activity$nhours<-round(activity$nhours, 4) #first, round for calculations
max(activity$nhours)
#7.1318
min(activity$nhours)#subtract this from all to set 0
#0.2488
7.1318-0.2488 #divide by 6.883 to scale from 0 to 1.

activity<-activity%>%
mutate(nhours_thresholded = (nhours -0.2488)/6.883)
#check for 0-1 scale
min(activity$nhours_thresholded)
max(activity$nhours_thresholded)
#good

activity$perc<-round(activity$perc, 4) #first, round for calculations
max(activity$perc)
#64.8345
min(activity$perc)#subtract this from all to set 0
#2.2615
64.8345-2.2615 #divide by 62.573 to scale from 0 to 1.

activity<-activity%>%
  mutate(perc_thresholded = (perc -2.2615)/62.573)
#check for 0 to 1 scale
max(activity$perc_thresholded)
min(activity$perc_thresholded)
#good

#make thresholded raster
#read example for extents
coordinates(activity)<- ~x+y
r<-raster("Thresholded_daily_temps/9C/PanChemT9L6.tif")
activity_nhours_thresh_raster<-rasterize(activity[,1:2], r, activity$nhours_thresholded)
activity_percent_thresh_raster<-rasterize(activity[,1:2], r, activity$perc_thresholded)

#writeRaster(activity_nhours_thresh_raster, paste0(names(activity_nhours_thresh_raster),"activity_nhours_thresh_raster"), bylayer=TRUE, format="GTiff")
#writeRaster(activity_percent_thresh_raster, paste0(names(activity_percent_thresh_raster),"activity_percent_thresh_raster"), bylayer=TRUE, format="GTiff")
plot(activity_nhours_thresh_raster)
plot(activity_percent_thresh_raster)


###now for performance data
performance<-read.csv("Activity_Preference_outputs/summary_winter_perf.csv") #this is created above as 'performance_80'
performance$performance<-round(performance$performance, 4) #first, round for calculations
max(performance$performance)
#0.17
min(performance$performance)#subtract this from all to set 0
#0.061
0.17-0.061 #divide by 0.109 to scale from 0 to 1.

performance<-performance%>%
  mutate(performance_thresholded = (performance -0.061)/0.109)
#check for 0-1 scale
min(performance$performance_thresholded)
max(performance$performance_thresholded)
#good


coordinates(performance)<- ~x+y
avg_winter_perf_thresholded_raster<-rasterize(performance[,1:2], r, performance$performance_thresholded)
#writeRaster(avg_winter_perf_thresholded_raster, paste0(names(avg_winter_perf_thresholded_raster),"avg_winter_perf_thresholded_raster"), bylayer=TRUE, format="GTiff")
plot(avg_winter_perf_thresholded_raster)


#create percent of maximum predicted performance map

performanceperc<-performance%>%
  mutate(performance_percent = ((performance)/0.17)*100)


performanceperc$performance_percent<-as.numeric(performanceperc$performance_percent)

coordinates(performanceperc)<- ~x+y
avg_winter__percentperf_raster<-rasterize(performanceperc[,1:2], r, performanceperc$performance_percent)
#writeRaster(avg_winter__percentperf_raster, paste0(names(avg_winter__percentperf_raster),"avg_winter__percentperf_raster"), bylayer=TRUE, format="GTiff")
plot(avg_winter__percentperf_raster)
