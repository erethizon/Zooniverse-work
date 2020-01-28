### Chapter 5 - data analysis
### set the working directory
### source library with functions 
### tested on teamexample.csv file; 08/11/2015 FR

source("TEAM library 1.7.R")
library(chron) 
library(reshape)
library (vegan)
library(plotrix)
library(ggplot2)
library(maptools)

### loading data

team_data<-read.csv(file="teamexample.csv", sep=",",h=T,stringsAsFactors=F)

### add Class, Order, Family attributes using the IUCN database

iucn.full<-read.csv("IUCN.csv", sep=",",h=T)
iucn<-iucn.full[,c("Class","Order","Family","Genus","Species")]
team<-merge(iucn, team_data, all.y=T)

### fixing data formats for analysis

data<-fix.dta(team) 

data<- droplevels(data[data$bin!="Homo sapiens", ]) # remove Homo sapiens from data set

### looking at data only for the year 2009 (sampling sites, species, dates)
names(data)
yr2009<-data[data$Sampling.Event =="2009.01" & data$Class=="MAMMALIA",]
unique(yr2009$Sampling.Unit.Name)
unique(yr2009$bin) # binomial name of species
unique(yr2009$Camera.Start.Date.and.Time)
unique(yr2009$Camera.End.Date.and.Time)

### descriptive analyses

# camera trap days
camera_days<-cam.days(data,2009.01)
summary(camera_days[,2:4])
write.table(camera_days, file="camera_days_2009.txt",quote=F, sep="\t",row.names = F)

# independent events by chosen time interval
events_hh<-event.sp(dtaframe=data, year=2009.01, thresh=60) #  thresh in minutes
events_dd<-event.sp(dtaframe=data, year=2009.01, thresh=1440) 

# saving away tables with events by species and camera site
write.table(events_hh, file="events_hh.txt",quote=F, sep="\t")
write.table(events_dd, file="events_dd.txt",quote=F, sep="\t")

# cumulative events per species 
events_hh_species<-colSums(events_hh)
write.table(events_hh_species, file="events_hh_species.txt", quote=F, sep="\t")

events_dd_species<-colSums(events_dd)
write.table(events_dd_species, file="events_dd_species.txt",quote=F, sep="\t")

# cumulative events per camera sites
cameras<-rowSums(events_hh)
write.table(cameras, file="events_species.txt",quote=F, sep="\t")

### naive occupancy
yr2009<-data[data$Sampling.Event =="2009.01" & data$Class=="MAMMALIA",]
mat<-f.matrix.creator(yr2009) # list of matrices camera x days for each species
naive_occu_2009<-naive(mat) # get naive occupancy for each species
write.table(naive_occu_2009, file="naive_occu_2009.txt",quote=F, sep="\t",row.names = F)


# accumulation curve  
accumulation<-acc.curve(data,2009.01)
write.table(accumulation, file="accsp_2009.txt",quote=F, sep="\t")
ggplot(accumulation, aes(x=Camera.trap.days, y=species)) +
  geom_line(aes(y=species-sd), colour="grey50", linetype="dotted") +
  geom_line(aes(y=species+sd), colour="grey50", linetype="dotted") +
  theme_bw() +
  geom_line()


# activity pattern of species 
activity_24h<-events.hours(yr2009)
write.table(activity_24h, file="events_24hour_2009.txt",quote=F, sep="\t",row.names = F)

activity_24h<-events.hours(data)

# example of plotting activity pattern of selected species (3 forest antelope)
clock<-c(0:23) 
clock24.plot(activity_24h$Cephalophus.harveyi,clock,show.grid=T,lwd=2,line.col="blue", main="Cephalophus.harveyi",cex.lab=0.5)

par(mfrow=c(1,3),cex.lab=0.5, cex.axis=0.5)
clock24.plot(activity_24h$Cephalophus.spadix,clock,show.grid=T,lwd=2,line.col="green", main="Cephalophus.spadix")
clock24.plot(activity_24h$Cephalophus.harveyi,clock,show.grid=T,lwd=2,line.col="blue", main="Cephalophus.harveyi")
clock24.plot(activity_24h$Nesotragus.moschatus,clock,show.grid=T,lwd=2,line.col="red", main="Nesotragus.moschatus")

# map of two species of sengi
library(maptools)
shape <- readShapeSpatial("park.shp", repair=T)

ev.dd.map<-merge(unique(data[,c("Sampling.Unit.Name","Longitude","Latitude")]),events_dd)
coord<-ev.dd.map[,c("Longitude","Latitude")]
xy <- project(as.matrix(coord), "+proj=utm +zone=37 +south +ellps=clrk80 +units=m +no_defs")
ev.dd.map$Longitude<-xy[,1]
ev.dd.map$Latitude<-xy[,2]

par(mfcol=c(1,2), mar=c(0.5,0.5,0.5,0.5), oma=c(1,1,1,1))
plot(shape,axes=F)
mtext("Rhynchocyon cirnei", cex = 1.5,font =3 )
Rc<-ev.dd.map[,c("Rhynchocyon cirnei")]/max(ev.dd.map[,c("Rhynchocyon cirnei")])
points(ev.dd.map[,"Longitude"],ev.dd.map[,"Latitude"],pch = 21,bg=grey(1-Rc))
plot(shape,axes=F)
mtext("Rhynchocyon udzungwensis",cex = 1.5, font =3)
Ru<-ev.dd.map[,c("Rhynchocyon udzungwensis")]/max(ev.dd.map[,c("Rhynchocyon udzungwensis")])
points(ev.dd.map[,"Longitude"],ev.dd.map[,"Latitude"],pch = 21,bg=grey(1-Ru))




