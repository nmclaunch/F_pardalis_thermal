###This code incorporates thermal limits and thermal preference data for F. pardalis
#It does not currently have thermal performance data


library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(car)
library(lubridate)
library(lme4)
library(nlme)
library(RColorBrewer)
library(emmeans)

#####Read in the data files####
pref<-read.csv("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleons/thermal preference/thermal_pref_summary_merged.csv") #adjust to your local directory
breadth<-read.csv("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleons/thermal preference/breadt_summary_merged.csv")
limit<-read.csv("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Data/chameleons/thermal preference/thermal_limit_summary_merged.csv")


######## filter out sickly/moribund animals ######
limit2<-filter(limit, notes!="moribund")
breadth2<-filter(breadth,notes!="moribund")
pref2<-filter(pref,notes_pref!="moribund")

#####subset based on CTmin or CTmax trial for analyses####
min<-subset(limit2,trial_type=="cold")
max<-subset(limit2,trial_type=="hot")

str(limit)

###check data distributions
hist(min$cham_temp_end)#OK
hist(min$avg_cham_temp_rate_min)#OK
hist(max$cham_temp_end)#OK
hist(max$avg_cham_temp_rate_min)#OK
hist(min$mass_g)


#thermal minimum model: account for repeated measures
min1<-lme(cham_temp_end~season+ which_first + avg_cham_temp_rate_min*mass_g, random=~1|cham_id, data = min, method='REML')
summary(min1)
anova(min1)
tuk<-lsmeans(min1, pairwise~season, adjust="tukey")
tuk

mean(min$avg_cham_temp_rate_min)
sd(min$avg_cham_temp_rate_min)

mean(min$cham_temp_end)
sd(min$cham_temp_end)


#thermal maximum model: account for repeated measures
max1<-lme(cham_temp_end~season + which_first + avg_cham_temp_rate_min*mass_g, random=~1|cham_id, data = max, method='REML')
summary(max1)
anova(max1)

mean(max$avg_cham_temp_rate_min)
sd(max$avg_cham_temp_rate_min)

mean(max$cham_temp_end)
sd(max$cham_temp_end)

#rate of temperature change according to mass (CTmin trial)
temprate<-lme(avg_cham_temp_rate_min~mass_g, random=~1|cham_id, data = min, method='REML')
summary(temprate)
anova(temprate)

#rate of temperature change according to mass (CTmax trial)
temprate<-lme(avg_cham_temp_rate_min~mass_g, random=~1|cham_id, data = max, method='REML')
summary(temprate)
anova(temprate)
#tuk<-lsmeans(m1, pairwise~cort_method*sample_num, adjust="tukey")
#tuk



#thermal breadth: account for repeated measures

bre<-lme(tbreadth~which_first*season + mass_g, random=~1|cham_id, data = breadth, method='REML')
summary(bre)
anova(bre)
tuk<-lsmeans(bre, pairwise~season, adjust="tukey")
tuk

mean(breadth2$tbreadth)
sd(breadth2$tbreadth)

#thermal breadth without sickly/moribund animals
bre<-lme(tbreadth~which_first+ season + mass_g, random=~1|cham_id, data = breadth2, method='REML')
summary(bre)
anova(bre)
tuk<-lsmeans(bre, pairwise~season, adjust="tukey")
tuk


####Thermal preference analyses####
str(pref)

#thermal preference as mode temperature during trial
prefs<-lme(mode_prefbody_temp~season+ mass_g, random=~1|cham_id, data = pref2, method='REML')
summary(prefs)
anova(prefs)
tuk<-lsmeans(prefs, pairwise~season*mass_g, adjust="tukey")#tuk
tuk
mean(pref2$mode_prefbody_temp)
sd(pref2$mode_prefbody_temp)

####thermal preference as average temperature during trial
prefs<-lme(avg_prefbody_temp~season + mass_g, random=~1| cham_id, data = pref2, method='REML')
summary(prefs)
anova(prefs)

sd(pref2$avg_prefbody_temp)


####thermal preference as standard deviation of the trial
prefs<-lme(std_dev_prefbody_temp~season + mass_g, random=~1|cham_id data = pref2, method='REML')
summary(prefs)
anova(prefs)

mean(pref2$std_dev_prefbody_temp)
sd(pref2$std_dev_prefbody_temp)

##Thermal preference minimum temperature during trial
prefs<-lme(min_prefbody_temp~season*mass, random=~1|cham_id, data = pref2, method='REML')
summary(prefs)
anova(prefs)

##Thermal preference maximum temperature during trial
prefs<-lme(max_prefbody_temp~season*mass, random=~1| cham_id, data = pref2, method='REML')
summary(prefs)
anova(prefs)

####Plots####

####boxplot showing thermal limits

limit2$season<-factor(limit2$season,levels=c("fall","winter","summer"))
levels(limit2$season)<-list(Fall="fall", Winter="winter", Summer="summer")
levels(limit2$season)

#jpeg("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Publications/cham_thermal/limitsfig.jpg",
#20, 15,units="cm", res=300)
minpop<-ggplot(data=limit2, aes(x=trial_type, y=cham_temp_end, group=trial_type, fill=trial_type)) +
  geom_boxplot(size=1, alpha=0.8)+
  geom_point(color="black", shape=21, size=2.5)+
  theme_classic()+
  ylab("Critical Thermal Limit (C)")+
  scale_fill_manual( values=c("cold"="paleturquoise3",
                              "hot"="red3"),
                     name="Trial",
                     labels=c("CTmin","CTgape"))+
  theme(
    plot.title = element_text(size=18),
    axis.title.x = element_blank(), axis.text.x=element_blank(),axis.ticks=element_blank(),
    #axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text=element_text(size=14), 
    legend.text=element_text(size=14),
    legend.title=element_text(size=15),
    legend.key.width = unit(1.8,"cm"),
    strip.text.x = element_text(size = 14)
  )+
  facet_grid(~season)
minpop
#dev.off()

###scatter and best fit showing thermal max value(CTgape) relating to body temperature rate change during trial
maxrate<-ggplot(data=max, aes(x=avg_cham_temp_rate_min, y=cham_temp_end)) +
  geom_smooth(size=1, alpha=0.8,color="black")+
  geom_point(color="black", shape=21, size=2.5)
maxrate

#scatter and best fit of temperature rate change per minute and body mass according to CTmin or CTgape trial
sizetemprate<-ggplot(data=limit, aes(x=avg_cham_temp_rate_min, y=mass_g, color=trial_type)) +
  geom_point()+
  geom_smooth()+
  # xlim(30, 165)+
  #  ylim(28, 46)+
  theme_classic()+
  xlab("Body temp change rate(C/min)")+
  ylab("Body mass (g)")+
  #scale_y_continuous(limits=c(10,20),breaks=seq(10,20,2))+
  theme(
    plot.title = element_text(size=18),
    #axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text=element_text(size=14), 
    legend.text=element_text(size=14,face="italic" ),
    legend.title=element_text(size=15),
    legend.key.width = unit(1.8,"cm")
  )
sizetemprate

###boxplot showing thermal breadth
Tbreadth<-ggplot(data=breadth, aes(x=season, y=tbreadth)) +
  geom_boxplot(size=1, alpha=0.8,color="black")+
  geom_point(color="black", shape=21, size=2.5)+
  # xlim(30, 165)+
  #  ylim(28, 46)+
  theme_classic()+
  #xlab("Population")+
  ylab("Thermal Breadth(?C)")+
  #scale_y_continuous(limits=c(10,20),breaks=seq(10,20,2))+
  theme(
    plot.title = element_text(size=18),
    axis.title.x = element_blank(),axis.ticks=element_blank(),
    #axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text=element_text(size=14), 
    legend.text=element_text(size=14,face="italic" ),
    legend.title=element_text(size=15),
    legend.key.width = unit(1.8,"cm")
  )
Tbreadth



###boxplot of thermal preference as mode temperature of trial
pref2$season<-factor(pref2$season,levels=c("fall","winter","summer"))
levels(pref2$season)<-list(Fall="fall", Winter="winter", Summer="summer")
levels(pref2$season)

tprefmode<-ggplot(data=pref2, aes(x=season, y=mode_prefbody_temp)) +
  geom_boxplot(size=1, alpha=0.8,color="black")+
  geom_point(color="black", shape=21, size=2.5)+
  # xlim(30, 165)+
  #  ylim(28, 46)+
  theme_classic()+
  #xlab("Population")+
  ylab("Thermal Preference MODE(?C)")+
  #scale_y_continuous(limits=c(10,20),breaks=seq(10,20,2))+
  theme(
    plot.title = element_text(size=18),
    axis.title.x = element_blank(),axis.ticks=element_blank(),
    #axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text=element_text(size=14), 
    legend.text=element_text(size=14,face="italic" ),
    legend.title=element_text(size=15),
    legend.key.width = unit(1.8,"cm")
  )
tprefmode

####boxplot of thermal preference as average body temperature graph####
#jpeg("C:/Users/nmcla/Dropbox/2PhysioHerpInvasives_Shared/Publications/cham_thermal/preffig.jpg",
#20, 10,units="cm", res=300)
Tprefavg<-ggplot(data=pref2, aes(x=season, y=avg_prefbody_temp, fill=season)) +
  geom_boxplot(size=1, alpha=0.8,color="black")+
  geom_point(color="black", shape=21, size=3.5)+
  theme_classic()+
  scale_fill_manual(values=c("Fall"="#9999CC",
                             "Winter"="#0000CC",
                             "Summer"="#CC3300"))+
  xlab("Season")+
  ylab("Thermal Preference (?C)")+
  theme(
    plot.title = element_text(size=18),
    axis.title.x = element_blank(),axis.ticks=element_blank(),
    #axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text=element_text(size=14), 
    legend.position = "none"
  )
Tprefavg
#dev.off()

##boxplot of thermal preference as standard deviation of body temperatures during trial
tprefsd<-ggplot(data=pref, aes(x=season, y=std_dev_prefbody_temp)) +
  geom_boxplot(size=1, alpha=0.8,color="black")+
  geom_point(color="black", shape=21, size=2.5)+
  # xlim(30, 165)+
  #  ylim(28, 46)+
  theme_classic()+
  #xlab("Population")+
  ylab("Thermal Preference stddev(?C)")+
  #scale_y_continuous(limits=c(10,20),breaks=seq(10,20,2))+
  theme(
    plot.title = element_text(size=18),
    axis.title.x = element_blank(),axis.ticks=element_blank(),
    #axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text=element_text(size=14), 
    legend.text=element_text(size=14,face="italic" ),
    legend.title=element_text(size=15),
    legend.key.width = unit(1.8,"cm")
  )
tprefsd

