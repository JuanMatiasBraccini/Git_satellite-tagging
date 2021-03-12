# Script for reporting SPAT data 2020
#notes:

# .kmz            Will open a Google Earth map so you can see where the tag has popped up. 
# All.csv         Has all the data from Argos such as Location quality (Class) and dates and times of signals received. 
# DailyData.csv   This is useful as it has the min/max depth and temperature each day and also the change in light-level. It tells you if the fish is behaving normally and the light-level should change. If it does not, then it is probably inside a larger fish. 
# Series.csv ; SeriesRange.csv   This gives you the depth and resolution of each reading every 10 minutes during the last 5 days. 
# Status.csv      This will give you the diagnostics about the tag - why it released and at what depth etc. 
# Summary.csv     Simply a summary of the data from the tag. 
# .pxp            A Igor file that we can use to visualize tag data as above. At the end of this email I have copied some information on how to download free Igor to view your data.


#Missing: Sort out 'Hi lactate' level
#         vertical migrations: use depth and time to calculate speed, is it 
#           changing depending o migration direction, etc? see Coffey et al 2020;
#           can also analyse using GAM as for depth... and can also follow 
#           Papastamatiou et al 2018 on a whole range of 
#           behavioural analsysis on the sat tagging data (stand alone paper?)


library(tidyverse)
library(dplyr)
library(lubridate)
library(maptools)
library(RODBC)
library(chron)
library(stringr)
library(data.table)
library(mgcv)

# 1. Data section ---------------------------------------------------------
#Tag data
  #Summarise data
options(stringsAsFactors = FALSE) 
setwd('C:/Matias/Data/Tagging/Satellite/sPAT_2020')
Serial=read.csv('Serial.csv')
toMatch <- c("All.csv", "DailyData.csv", "Series.csv","Summary.csv")
all.files=list.files(paste(getwd(),'/downloads',sep=''))
all.dat=vector('list',length(all.files))
names(all.dat)=all.files
for(i in 1:length(all.files))
{
  temp=paste(getwd(),'/downloads/',all.files[i],sep='')
  matches <- grep(paste(toMatch,collapse="|"), list.files(temp), value=TRUE)
  
  All=DailyData=Series=Summary=NULL
  
  a=paste(all.files[i],"-All.csv",sep='')
  if(a%in%matches)
  {
    All=read.csv(paste(temp,'/',a,sep=''))
    All$datetime=parse_date_time(All$Loc..date,c('%m/%d/%Y %H:%M:%S'),tz = "Australia/Perth") +8*60*60 #to convert UTC to Perth time
    if(All$Platform.ID.No.[1]==200445) All$datetime= as.POSIXct('2020-07-11 03:45:00')+8*60*60
    if(All$Platform.ID.No.[1]==200430) All$datetime= as.POSIXct('2020-12-20 02:22:00')+8*60*60
  }
    
  a=paste(all.files[i],"-DailyData.csv",sep='')
  if(a%in%matches)
  {
    DailyData=read.csv(paste(temp,'/',a,sep=''))
    DailyData$Date=parse_date_time(DailyData$Date,c('%m/%d/%Y'))
  }
  
  a=paste(all.files[i],"-Series.csv",sep='')
  if(a%in%matches)
  {
    Series=read.csv(paste(temp,'/',a,sep=''))
    if(!Series$Ptt[1]==200445) Series$datetime=parse_date_time(paste(Series$Day,Series$Time),c('%d-%m-%Y %H:%M:%S'),
                                tz = "Australia/Perth") +8*60*60
    if(Series$Ptt[1]==200445) Series$datetime=parse_date_time(paste(Series$Day,Series$Time),c('%d-%m-%y %H:%M:%S'),
                    tz = "Australia/Perth") +8*60*60
  }
  
  a=paste(all.files[i],"-Summary.csv",sep='')
  if(a%in%matches)
  {
    Summary=read.csv(paste(temp,'/',a,sep=''))
    Summary$EarliestDataTime=parse_date_time(Summary$EarliestDataTime,c('%H:%M:%S %d-%m-%Y'),
                                    tz = "Australia/Perth") +8*60*60
    Summary$LatestDataTime=parse_date_time(Summary$LatestDataTime,c('%H:%M:%S %d-%m-%Y'),
                                             tz = "Australia/Perth") +8*60*60
    Summary$ReleaseDate=parse_date_time(Summary$ReleaseDate,c('%H:%M:%S %d-%m-%Y'),
                                             tz = "Australia/Perth") +8*60*60
  }

  all.dat[[i]]=list(All=All,DailyData=DailyData,Series=Series,Summary=Summary)
}

  #Archived data from reported tags
Archived_19P1600=read.csv('C:/Matias/Data/Tagging/Satellite/sPAT_2020/19P1600_Archive.csv')

#Bio data
setwd("U:/Shark")  # working directory
channel <- odbcConnectAccess2007("Sharks v20200323.mdb")
Boat_bio=sqlFetch(channel, "Boat_bio", colnames = F) 
Boat_hdr=sqlFetch(channel, "Boat_hdr", colnames = F)   
close(channel)
SPECIES.names=read.csv("C:/Matias/Data/Species.code.csv")

suppressWarnings({DATA=left_join(Boat_bio,Boat_hdr,by="SHEET_NO")%>%
      filter(!is.na(PSATTagNumber) & !SPECIES=='WP')%>%
      rename(Release.condition="RELEASE CONDITION",
             Fintag="FINTAG NO",
             AVE.SET.TIME="AVE SET TIME",
             AVE.HAUL.TIME="AVE HAUL TIME",
             MID.LAT="MID LAT",
             MID.LONG="MID LONG",
             BAG.NO='BAG NO')%>%
      dplyr::select(SHEET_NO,BOAT,BOTDEPTH,SPECIES,DATE,MID.LAT,MID.LONG,
                    Method,FL,TL,SEX,Release.condition,
                    AVE.SET.TIME,AVE.HAUL.TIME,
                    BAG.NO,BloodFlag,Lactate,
                    HookLocation,HookRemoved,HookType,HookSize,BleedingFlag,
                    Fintag,PSATTagNumber,PSATTagSerial,TaggerName,
                    COMMENTS.x,NewComments,COMMENTS.y)%>%
  left_join(SPECIES.names,by=c("SPECIES"="Species"))%>%
  dplyr::select(-c(Taxa,CAES_Code))%>%
  mutate(HookedTime=paste(str_match(NewComments, "Hook timer \\s*(.*?)\\s*-")[,2],"00",sep=":"),
         AVE.SET.TIME=chron(times=sapply(strsplit(as.character(AVE.SET.TIME)," "), '[[', 2)),   
         AVE.HAUL.TIME=chron(times=sapply(strsplit(as.character(AVE.HAUL.TIME)," "), '[[', 2)))
})

#Al's deployments
Al.data=read.csv('C:/Matias/Data/Tagging/Satellite/sPAT_2020/Al_data.csv')
Al.data=Al.data%>%
    rename(HookedTime=hook._timer,
         BOTDEPTH=depth,
         SEX=sex,
         PSATTagNumber=spat_tag,
         Fintag=id,
         BAG.NO=spec_num,
         Lactate=blood_lactate,
         NewComments=comments_post_mortem)%>%
  mutate(SHEET_NO=paste("Al.Harry",session,shot,sep='_'),
         BOAT="NAT",
         Method="LL",
         SPECIES=case_when(common=="Tiger Shark"~"TG",
                           common=="Sandbar Shark"~"TK",
                           TRUE~""),
         DATE=as.POSIXct(date,format="%d/%m/%Y"),
         MID.LAT=lat,
         MID.LONG=lon,
         FL=FL/10,
         TL=TL/10,
         Release.condition=NA,
         AVE.SET.TIME=chron(times=paste(start,":00",sep='')),   
         AVE.HAUL.TIME=chron(times=paste(end,":00",sep='')),
         BloodFlag=case_when(!is.na(Lactate)~'Yes',
                             TRUE~'No'),
         HookLocation=case_when(Fintag%in%c('D1870','D1838')~'Tongue',
                                TRUE~'Corner of the mouth'),
         HookRemoved=case_when(Fintag%in%c('D1870','D1838')~'No',
                               TRUE~'Yes'),
         HookType=NA,
         HookSize=NA,
         BleedingFlag=case_when(Fintag%in%c('D1870','D1838')~'Yes',
                                TRUE~'No'),
         PSATTagSerial=case_when(PSATTagNumber==200431~"19P1513",
                                 PSATTagNumber==200433~"19P1523",
                                 PSATTagNumber==200434~"19P1525",
                                 PSATTagNumber==200438~"19P1614",
                                 TRUE~""),
         TaggerName="Al Harry",
         COMMENTS.x=NA,
         COMMENTS.y=NA)%>%
  left_join(SPECIES.names,by=c("SPECIES"="Species"))%>%
  dplyr::select(names(DATA))

suppressWarnings({DATA=rbind(DATA,Al.data)%>%
    mutate(HookedTime=chron(times=HookedTime),
           SEX=tolower(SEX))
})

#Sort out hook timer popping during gear deployment
DATA=DATA%>%
  mutate(soak.time=case_when(Method=="LL"~AVE.HAUL.TIME-AVE.SET.TIME),
         HookedTime.original=HookedTime,
         HookedTime=ifelse(HookedTime>(soak.time-as.times("00:10:00")),"",HookedTime))
class(DATA$HookedTime)<- "times"

# 2. Data manipulation section ---------------------------------------------------------

#Archived Port Jackson
Archived_19P1600=Archived_19P1600%>%
              mutate(Time.Perth=parse_date_time(Time,c('%H:%M:%S %d-%m-%Y'))+8*60*60)%>% #convert UTC to Perth time
              mutate(Round.Date=round_date(Time.Perth,"hour"))%>%
              group_by(Round.Date)%>%
              summarise(Depth=mean(Depth,na.rm=T),
                        Temperature=mean(Temperature,na.rm=T),
                        Light.Level=mean(Light.Level,na.rm=T))%>%
              mutate(Round.day=date(Round.Date),
                     Round.Date=as.POSIXct(as.character(Round.Date), tz="Australia/Perth"))
Relis <- matrix(c(116.82,-35.05), nrow=1)
Relis <- SpatialPoints(Relis, proj4string=CRS("+proj=longlat +datum=WGS84"))
Sun.rise=sunriset(Relis,  Archived_19P1600$Round.Date,  direction="sunrise", POSIXct.out=TRUE)
Sun.set=sunriset(Relis, Archived_19P1600$Round.Date, direction="sunset", POSIXct.out=TRUE)
Archived_19P1600$Sun.rise=Sun.rise$time
Archived_19P1600$Sun.set=Sun.set$time
Archived_19P1600=Archived_19P1600%>%
       mutate(Time.period=ifelse(Round.Date>=Sun.rise & Round.Date<Sun.set,"Day","Night"))


#Summaries
All=vector('list',length(all.files))
DailyData=Series=Summaries=All

for(i in 1:length(All))
{
  All[[i]]=all.dat[[i]]$All
  DailyData[[i]]=all.dat[[i]]$DailyData
  Series[[i]]=all.dat[[i]]$Series
  Summaries[[i]]=all.dat[[i]]$Summary
}
All=do.call(rbind,All)%>%
  left_join(Serial,by=c('Platform.ID.No.'='PTT'))
DailyData=do.call(rbind,DailyData)%>%
  left_join(Serial,by=c('Ptt'='PTT'))
Series=do.call(rbind,Series)%>%
  left_join(Serial,by=c('Ptt'='PTT'))

Summaries=do.call(rbind,Summaries)%>%
  left_join(Serial,by=c('Ptt'='PTT'))%>%
  mutate(Serial=ifelse(Ptt==200435,"19P1653",
                ifelse(Ptt==200436,"19P1598",
                Serial)))


#add tagging position to consumed dusky 
All=All%>%
  mutate(Latitude=ifelse(Platform.ID.No.==200445,-22.53218,Latitude),
         Longitude=ifelse(Platform.ID.No.==200445,113.6275,Longitude))


Mean.lat=mean(All$Latitude[All$Latitude<0],na.rm=T)
Mean.lon=mean(All$Longitude[All$Longitude>110],na.rm=T)

All=All%>%
      mutate(Latitude=ifelse(Latitude>0 |Latitude<(-37),Mean.lat,Latitude),
             Latitude=-abs(Latitude),
             Longitude=ifelse(Longitude<(110),Mean.lon,Longitude),
             Longitude=abs(Longitude),
             Serial=ifelse(Platform.ID.No.==200435,"19P1653",
                    ifelse(Platform.ID.No.==200436,"19P1598",
                    Serial)))


#Add day or night
mean.pos=All%>%
  group_by(Serial)%>%
  summarise(Lat.detach=mean(Latitude,na.rm=T),
            Lon.detach=mean(Longitude,na.rm=T))%>%
  data.frame%>%
  filter(!is.na(Lat.detach))

Series=Series%>%
  mutate(Serial=ifelse(Ptt==200435,"19P1653",
                ifelse(Ptt==200436,"19P1598",
                Serial)))%>%
  left_join(mean.pos,by='Serial')%>%   
  filter(!is.na(Lon.detach))
  
Sun.rise=sunriset(cbind(Series$Lon.detach,Series$Lat.detach), Series$datetime,  
                  direction="sunrise", POSIXct.out=TRUE)
Sun.set=sunriset(cbind(Series$Lon.detach,Series$Lat.detach), Series$datetime,
                  direction="sunset", POSIXct.out=TRUE)

Series=Series%>%
  mutate(Sun.rise=Sun.rise$time,
         Sun.set=Sun.set$time,
         Time.period=ifelse(datetime>=Sun.rise & datetime<Sun.set,"Day","Night"))

DailyData=DailyData%>%
  mutate(Serial=ifelse(Ptt==200435,"19P1653",
                ifelse(Ptt==200436,"19P1598",
                Serial)))

#Add bio data
DATA=DATA%>%
  mutate(PSATTagNumber=ifelse(PSATTagNumber=="19P1598","200436",
                       ifelse(PSATTagNumber=="19P1653","200435",
                       PSATTagNumber)),
         PSATTagSerial=ifelse(PSATTagSerial=="19P1465","19P1485",
                       ifelse(PSATTagSerial=="200435","19P1653",
                       ifelse(PSATTagSerial=="200436","19P1598",
                       PSATTagSerial))))
All=left_join(All,DATA,by=c('Serial'='PSATTagSerial'))
DailyData=left_join(DailyData,DATA,by=c('Serial'='PSATTagSerial'))
Series=left_join(Series,DATA,by=c('Serial'='PSATTagSerial'))

Summaries=left_join(Summaries,DATA,by=c('Serial'='PSATTagSerial'))

#Order by datetime and create ID tag
Series=Series%>%
          mutate(ID=paste(PSATTagNumber," h.time",HookedTime," Lac",Lactate))%>%
          arrange(Ptt,datetime)
DailyData=DailyData%>%
          mutate(ID=paste(PSATTagNumber," h.time",HookedTime," Lac",Lactate))%>%
          arrange(Ptt,Date)

#Export data for Taylor
hndl.Taylor='C:/Matias/Students/2020_Taylor Grosse/data.for.Taylor'
setwd(hndl.Taylor)
write.csv(All,"All.csv",row.names = FALSE)
write.csv(DailyData,"DailyData.csv",row.names = FALSE)
write.csv(Series,"Series.csv",row.names = FALSE)
write.csv(Summaries,"Summaries.csv",row.names = FALSE)


# 3. General analysis section ---------------------------------------------------------
setwd('C:\\Matias\\Analyses\\Satellite_tagging\\Outputs')

#Archived Port Jackson
Archived_19P1600_release=DATA%>%
  filter(PSATTagSerial=="19P1600")%>%
  mutate(DATE.stamp=as.POSIXct(
         paste(as.character(DATE),as.character(AVE.HAUL.TIME)))+24*60*60) #add release datetime to check post release recovery

Archived_19P1600%>%
  mutate(Time.period=ifelse(Depth<0,"Detached",Time.period))%>%
  ggplot(aes(Round.Date,Depth,colour=Time.period))+
  geom_point(size=1.1) + 
  geom_point(aes(x=Archived_19P1600_release$DATE.stamp,y=0),shape=8, color="grey40", size=4)+
  scale_y_continuous(trans = "reverse")+
  theme(legend.title=element_blank(),
        legend.position="top",
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        strip.text = element_text(size = 7))+
  ylab("Depth (m)")+xlab("Date")+ expand_limits(y=0)
ggsave('Port Jackson/Fine.scale_depth_Archived_19P1600.tiff', width = 12,height = 6, dpi = 300, compression = "lzw")


#Identify species
Species=unique(Series$COMMON_NAME)
n.sp=length(Species)


#Summaries
  #type of tag releases
for(s in 1:n.sp)
{
  Tab.rel=Summaries%>%
    filter(COMMON_NAME==Species[s])%>%
    group_by(Ptt,COMMON_NAME,ReleaseType)%>%summarise(n=n())%>%
    spread(ReleaseType,n,fill='')%>%
    data.frame
  write.csv(Tab.rel,paste(Species[s],"Table.release.types.csv",sep="/"),row.names = F)
}


#3.1 Depth by datetime
#3.1.1 fine scale
for(s in 1:n.sp)
{
  Series%>%
    filter(COMMON_NAME==Species[s])%>%
    ggplot(aes(datetime,Depth,colour=Time.period))+
      geom_point(size=.7) + 
      facet_wrap(vars(ID), scales = "free")+  
      scale_y_continuous(trans = "reverse")+
      theme(legend.title=element_blank(),
            legend.position="top",
            axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            strip.text = element_text(size = 7))+
      ylab("Depth (m)")+xlab("Date")+ expand_limits(y=0)
  ggsave(paste(Species[s],'Fine.scale_depth.tiff',sep="/"), width = 12,height = 6, dpi = 300, compression = "lzw")
  
}
#3.1.2 broad scale
  #min and max depth
for(s in 1:n.sp)
{
  DailyData%>%
    filter(COMMON_NAME==Species[s])%>%
    ggplot(aes(Date,MinDepth))+
    geom_line(aes(color="Min")) +
    geom_line(aes(Date,MaxDepth,color="Max")) +
    facet_wrap(vars(ID), scales = "free")+  
    scale_y_continuous(trans = "reverse")+
    labs(fill="Depth")+ 
    theme(legend.title=element_blank(),
          axis.text.x=element_text(size=8),
          axis.text.y=element_text(size=8),
          strip.text = element_text(size = 7))+
    ylab("Depth (m)")+xlab("Date")+ expand_limits(y=0)
  ggsave(paste(Species[s],'Broad.scale_min.max_depth.tiff',sep="/"), width = 12,height = 6, dpi = 300, compression = "lzw")
  
}
  #max depth only
for(s in 1:n.sp)
{
  DailyData%>%
    filter(COMMON_NAME==Species[s])%>%
    ggplot(aes(Date,MaxDepth))+
      geom_line() +
      facet_wrap(vars(ID), scales = "free")+  
      scale_y_continuous(trans = "reverse")+
      theme(axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            strip.text = element_text(size = 7))+
      ylab("Depth (m)")+xlab("Date")+ expand_limits(y=0)
  ggsave(paste(Species[s],'Broad.scale_max.depth_only.tiff',sep="/"), width = 12,height = 6, dpi = 300, compression = "lzw")
}

#3.2 Temperature by datetime
for(s in 1:n.sp)
{
    DailyData%>%
      filter(COMMON_NAME==Species[s])%>%
      ggplot(aes(Date,MaxTemp))+
        geom_line() +
        facet_wrap(vars(ID), scales = "free")+  
        theme(axis.text.x=element_text(size=8),
              axis.text.y=element_text(size=8),
              strip.text = element_text(size = 7))+
        ylab("Temperature (C)")+xlab("Date")
    ggsave(paste(Species[s],'Broad.scale_max.temperature.only.tiff',sep="/"), width = 12,height = 6, dpi = 300, compression = "lzw")
}

#3.3 Light by datetime
for(s in 1:n.sp)
{
  DailyData%>%
    filter(COMMON_NAME==Species[s])%>%
    ggplot(aes(Date,DeltaLight))+
      geom_line() +
      facet_wrap(vars(ID),scales="free_x")+  
      theme(axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            strip.text = element_text(size = 7))+
      ylab("DeltaLight")+xlab("Date")
  ggsave(paste(Species[s],'Broad.scale_deltaLight.tiff',sep="/"), width = 12,height = 6, dpi = 300, compression = "lzw")
}

#3.4 Check deep diving for sandbar shark with unusual depth
Check.deep.dive=FALSE
if(Check.deep.dive)
{
  library(PBSmapping)
  library(geosphere)
  source('C:/Matias/Analyses/SOURCE_SCRIPTS/Git_other/Plot.Map.R')
  
  Bathymetry_120=read.table("C:/Matias/Data/Mapping/get_data112_120.cgi")
  Bathymetry_138=read.table("C:/Matias/Data/Mapping/get_data120.05_138.cgi")
  Bathymetry=rbind(Bathymetry_120,Bathymetry_138)
  Bathymetry=subset(Bathymetry,V2>=(-25) & V2<=(-21))
  Bathymetry=subset(Bathymetry,V1>=(111) & V1<=(114))
  Bathymetry=Bathymetry[order(Bathymetry$V1,Bathymetry$V2),]
  xbat=sort(unique(Bathymetry$V1))
  ybat=sort(unique(Bathymetry$V2))
  reshaped=as.matrix(reshape(Bathymetry,idvar="V1",timevar="V2",v.names="V3", direction="wide"))
  
  South.WA.lat=c(-26,-21)
  South.WA.long=c(112,117)
  a=South.WA.long[1]:South.WA.long[2]
  b=South.WA.lat[1]:South.WA.lat[2]
  PLATE=c(.01,.9,.075,.9)
  
  tiff(file="Deep.dive.tiff",width = 2400, height = 2400,units = "px", res = 300, compression = "lzw")    
  plotmap(a,b,PLATE,"dark grey",South.WA.long,South.WA.lat)
  d=DailyData%>%filter(Serial=='20P0251')
  points(d$MID.LONG,-d$MID.LAT,pch=19,cex=2,col=2)
  
  with(subset(All,Platform.ID.No.==200453),points(mean(Longitude,na.rm=T),mean(Latitude,na.rm=T),pch=19,cex=2,col=3))
  
  contour(xbat, ybat, reshaped[,2:ncol(reshaped)],
          levels = seq(0,-1200,-200),labcex=0.9,add=T)
  From=c(112.9716,-23.76857)
  To=c(112.4466,-23.19956)
  segments(x0=From[1],y0=From[2], x1=To[1],y1=To[2])
  text(To[1],To[2],paste(round(distCosine(From,To)/1000),"km"),pos=2)
  legend('topleft',c("tagged","detached"),bty='n',pch=19,col=c(2,3),cex=1.5)
  mtext(d$Serial[1],3)
  dev.off()

}

#3.3 Consumed dusky archived data
Check.archived=FALSE
if(Check.archived)
{
  library(ggpubr)
  Archived=read.csv('C:/Matias/Data/Tagging/Satellite/sPAT_2020/downloads/200445/200445-Archive.csv')
  Archived=Archived%>%
    mutate(
      hora=sub("\\s.*","",Time),
      Date=sub("^\\S+\\s+", '',Time),
      datetime=parse_date_time(paste(Date,hora),c('%d-%m-%y %H:%M:%S'),
                               tz = "Australia/Perth") +8*60*60)
  
  p3=ggplot(Archived,aes(datetime,Depth))+geom_line()+ylab("Depth")+xlab("Date")+scale_y_continuous(trans = "reverse")
  p2=ggplot(Archived,aes(datetime,Temperature))+geom_line()+ylab("Temperature")+xlab("Date")
  p1=ggplot(Archived,aes(datetime,Light.Level))+geom_line()+ylab("DeltaLight")+xlab("Date")
  
  ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
  
  ggsave('Dusky shark/Consumed_dusky.tiff', width = 9,height = 6, dpi = 300, compression = "lzw")
  
  write.csv(Archived,paste(hndl.Taylor,'Consumed.dusky.csv',sep='/'),row.names = F)
}

#3.4 Depth frequency distributions by day/night
for(s in 1:n.sp)
{
  ggplot() +
    facet_wrap(vars(ID), scales = "free") +
    geom_histogram(data=subset(Series,COMMON_NAME==Species[s] & Time.period=="Day"),
                   aes(x = Depth, y = ..density.., fill="Day"),bins=30) +
    geom_histogram(data=subset(Series,COMMON_NAME==Species[s] & Time.period=="Night"),
                   aes(x = Depth, y = -..density.., fill="Night"),bins=30)+
    coord_flip() +
    theme(legend.title=element_blank(),
          legend.position="top",
          axis.text.x=element_text(size=8),
          axis.text.y=element_text(size=8),
          strip.text = element_text(size = 7))+
    xlab("Depth (m)")+ylab("Density")+ expand_limits(y=0)
  ggsave(paste(Species[s],'Fine.scale_depth_frequency_dist.tiff',sep="/"), width = 12,height = 6, dpi = 300, compression = "lzw")
  
}


# 5. Post release survival analysis section ---------------------------------------------------------
#note: use Kaplan - Meier survival (Schaefer et al 2021)
# for basic tutorial, see http://www.sthda.com/english/wiki/survival-analysis-basics

#Scenarios on mortality definition
Scenarios=data.frame(Scenario=c("S1","S2"),Death.def=c("dead=only sinkers","dead=all premature"))
n.scen=nrow(Scenarios)

#fit model
fit.formula=as.formula(Surv(DataDays, status) ~ 1)
fun.surv=function(d,Formula,Scenario,Depth)
{
  #add depth last location
  Depth <- Depth %>% 
    group_by(Ptt) %>%
    filter(datetime == max(datetime))%>%
    dplyr::select(Ptt,datetime,Depth)%>%
    mutate(Depth=round(Depth))
  d=d%>%
    left_join(Depth,by="Ptt")%>%
    distinct(Ptt,SEX,ReleaseDate,EarliestDataTime,LatestDataTime,DataDays,
             ReleaseType,Depth,Lactate,HookedTime,Release.condition)
  
  #define mortality
  if(Scenario=="dead=only sinkers")
  {
    d=d%>%
      mutate(status=case_when(ReleaseType=="Premature" & Depth>5~1,   #not working, this is surface!!! should be bottom....
                              TRUE~0))
  }
  if(Scenario=="dead=all premature")
  {
    d=d%>%
      mutate(status=case_when(ReleaseType=='Premature'~1,
                              TRUE~0))
  }
  
  #fit model
  fit <- survfit(Formula, data = d)
  
  return(list(fit=fit))
}
Store.surv.fit=vector('list',n.sp)
names(Store.surv.fit)=Species
for(s in 1:n.sp)
{
 dummy=vector('list',length=n.scen)
 names(dummy)=Scenarios$Scenario
 for(l in 1:n.scen)
 {
   dummy[[l]]=fun.surv(d=Summaries%>%filter(COMMON_NAME==Species[s]),
                Formula=fit.formula,
                Scenario=Scenarios$Death.def[l],
                Depth=Series%>%filter(COMMON_NAME==Species[s]))
 }
 Store.surv.fit[[s]]=dummy
}

#plot model
Scen.col=c("forestgreen","firebrick")
names(Scen.col)=Scenarios$Scenario
plot.surv=function(scenarios,name)
{
  plot(1,ylim=c(0,1),xlim=c(0,33),col="transparent",xlab='',ylab='',main=name)
  for(l in n.scen)
  {
    with(summary(scenarios[[l]]$fit),
    {
      points(time,surv,col=Scen.col[l],pch=19,cex=1.25)
      segments(time,lower,time,upper,col=Scen.col[l])
    })
  }
  mtext("Days",1,cex=1.5,line=2)
  mtext("Survival probability",2,cex=1.5,line=2)
  legend("topright",Scenarios$Scenario,bty='n',pch=19,col=Scen.col,cex=1.25)
}
for(s in 1:n.sp) plot.surv(scenarios=Store.surv.fit[[s]],
                            name=names(Store.surv.fit)[s])

#Test effect of hook time, release condition, and lactate levels on survival  
#note: is there enough contrast in survival data?



# 6. Periodicity in vertical movements analysis section ---------------------------------------------------------
#GAM for relative speed and CWT for depth datetime? but autocorrelation?

#note: use GAM on time of day (Papastamatiou et al 2018)
#      Another option could be Continuous Wavelet Transformations on datetime (Burke et al 2020)

#GAM approach
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
#https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
#follow Papastamatiou et al 2018 (grouping dates)
fn.periodicity.gam=function(d,show.plot=FALSE)
{
  d=d%>%
    mutate(datetime.numeric=as.numeric(d$datetime)/1e6,
           time=as.numeric(as.times(format(d$datetime, format = "%H:%M:%S"))),
           Ptt=factor(Ptt))%>%
    group_by(Ptt)%>%
    mutate(rel.depth=Depth/mean(Depth))%>%
    filter(Depth>2)%>% #remove records at surface (i.e. after tag detached)  
    filter(!Ptt%in%c(200432,200445))%>% #remove PJ shark with default release settings and predated dusky
    ungroup()%>%
    dplyr::select(Ptt,Time.period,datetime,Depth,datetime.numeric,time,rel.depth)
  
  if(show.plot)
  {
    d%>%
      arrange(time)%>%
      ggplot(aes(time,rel.depth,colour=Time.period))+
      geom_point(size=.7) + 
      scale_y_continuous(trans = "reverse")+
      facet_wrap(vars(Ptt), scales = "free")+
      theme(legend.title=element_blank(),
            legend.position="top",
            axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            strip.text = element_text(size = 7))+
      ylab("Relative depth")+xlab("Time")+ expand_limits(y=0)
    
    d%>%
      arrange(datetime.numeric)%>%
      ggplot(aes(datetime.numeric,rel.depth,colour=Time.period))+
      geom_point(size=.7) + 
      scale_y_continuous(trans = "reverse")+
      facet_wrap(vars(Ptt), scales = "free")+
      theme(legend.title=element_blank(),
            legend.position="top",
            axis.text.x=element_text(size=8),
            axis.text.y=element_text(size=8),
            strip.text = element_text(size = 7))+
      ylab("Relative depth")+xlab("datetime.numeric")+ expand_limits(y=0)
  }
  
  #Fit gam with autocorrelation
  ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
  mod <- gamm(rel.depth ~ s(time, bs = "cc"), data = d,
             correlation = corARMA(form = ~ 1|Ptt, p = 1),control = ctrl)
  
  
  #Preliminary stuff, select best model
      #1. No autocorrelation
  #m <- gamm(rel.depth ~ s(time, bs = "cc"), data = d)
  
      #2. Consider autocorrelation
  #m1 <- gamm(rel.depth ~ s(time, bs = "cc"), data = d,
  #           correlation = corARMA(form = ~ 1|Ptt, p = 1),control = ctrl)
  #m2 <- gamm(rel.depth ~ s(time, bs = "cc"), data = d,
  #           correlation = corARMA(form = ~ 1|Ptt, p = 2),control = ctrl)
  #m3 <- gamm(rel.depth ~ s(time, bs = "cc"), data = d,
  #           correlation = corARMA(form = ~ 1|Ptt, p = 3),control = ctrl)
  
  #Compare all models
  #anova(m$lme, m1$lme, m2$lme, m3$lme)  m3 selected best model
  
  #check autocorrelation
  # layout(matrix(1:2, ncol = 2))
  # acf(resid(m$lme), lag.max = 36, main = "ACF")
  # pacf(resid(m$lme), lag.max = 36, main = "pACF")
  # res <- resid(m1$lme, type = "normalized")
  # acf(res, lag.max = 36, main = "ACF - AR(2) errors")
  # pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
  # layout(1)
  
  
  return(list(mod=mod,data=d))
}

Store.gam=vector('list',n.sp)
names(Store.gam)=Species
system.time({for(s in 1:n.sp)
{
  Store.gam[[s]]=fn.periodicity.gam(d=Series%>%filter(COMMON_NAME==Species[s]))
}})


hm <- merge(0:23, 0)
Time.ref=data.frame(Times=chron(time = paste(hm$x, ':', hm$y, ':', 0)))
Time.ref$Times.numeric=as.numeric(Time.ref$Times)

#Display diel pattern
fn.pred.gam=function(mod,data)
{
  p  <- predict(mod$gam,type="terms",se.fit=TRUE)
  data=data%>%
          mutate(pred=p$fit[,"s(time)"],
               se=p$se.fit[,"s(time)"],
               lowerCI=pred-1.96*se,
               upperCI=pred+1.96*se)
  
  data%>%
    ggplot(aes(datetime,pred))+
    #geom_point(aes(time,rel.depth),size=.7) + 
    geom_line(aes(datetime,pred),colour="steelblue",size=2) +
    geom_ribbon(aes(ymin=lowerCI, ymax=upperCI), alpha=0.1)+
    #facet_wrap(vars(Ptt), scales = "free")+ 
    scale_y_continuous(trans = "reverse")+
    theme(legend.title=element_blank(),
          legend.position="top",
          axis.text.x=element_text(size=8),
          axis.text.y=element_text(size=8),
          strip.text = element_text(size = 7))+
    ylab("Relative depth")+xlab("Time of day")+ 
    scale_x_chron(format = "%H:%M:%S", n = 5)   #still not there.....
}

chron(times=sapply(strsplit(as.character(AVE.SET.TIME)," "), '[[', 2))

p  <- predict(m$gam,  newdata = pdat, type = "terms", se.fit = TRUE)
p1 <- predict(m1$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p2 <- predict(m2$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p3 <- predict(m3$gam, newdata = pdat, type = "terms", se.fit = TRUE)

## combine with the predictions data, including fitted and SEs
pdat <- transform(pdat,
                  p  = p$fit[,1],  se  = p$se.fit[,1],
                  p1 = p1$fit[,1], se1 = p1$se.fit[,1],
                  p2 = p2$fit[,1], se2 = p2$se.fit[,1],
                  p3 = p3$fit[,1], se3 = p3$se.fit[,1])

pdat%>%
  ggplot(aes(time,p1))+
  geom_point(size=.7) + 
  facet_wrap(vars(Ptt), scales = "free")+  
  scale_y_continuous(trans = "reverse")+
  theme(legend.title=element_blank(),
        legend.position="top",
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        strip.text = element_text(size = 7))+
  ylab("Relative depth (m)")+xlab("Date")+ expand_limits(y=0)


