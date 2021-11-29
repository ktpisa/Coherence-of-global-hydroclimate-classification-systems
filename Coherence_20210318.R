################################# Coherence Mar 2021 ##############################################
############################### Data formatting 1980-2018 ##################################
# note: first extract .gz files using 7Zip
library(ncdf4)
library(raster)

# to save
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence")

####### Monthly Precipitation (mm) ########
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PRE/cru_ts4.04.1971.1980.pre.dat.nc")
PRE_stack<-stack('cru_ts4.04.1971.1980.pre.dat.nc',varname='pre') #monthly PRE 1971-1980 (only need 1980)
PRE_stack_1980<-subset(PRE_stack,109:120) #1980 stack of all months

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PRE/cru_ts4.04.1981.1990.pre.dat.nc")
PRE_stack_8190<-stack('cru_ts4.04.1981.1990.pre.dat.nc',varname='pre') #monthly PRE 1981-1990

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PRE/cru_ts4.04.1991.2000.pre.dat.nc")
PRE_stack_9100<-stack('cru_ts4.04.1991.2000.pre.dat.nc',varname='pre') #monthly PRE 1991-2000

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PRE/cru_ts4.04.2001.2010.pre.dat.nc")
PRE_stack_0110<-stack('cru_ts4.04.2001.2010.pre.dat.nc',varname='pre') #monthly PRE 2001-2010

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PRE/cru_ts4.04.2011.2019.pre.dat.nc")
PRE_stack_1119<-stack('cru_ts4.04.2011.2019.pre.dat.nc',varname='pre') #monthly PRE 2011-2019 (only want thru 2018)
#PRE_stack_1118<-subset(PRE_stack_1119,1:96) #monthly PRE 2011-2018
PRE_stack_1114<-subset(PRE_stack_1119,1:48) #monthly PRE 2011-2014 #Because GRUN_Q is limited to 2014

#all months for 1980-2018 (n=468)
#PRE_Mo_8018<-stack(PRE_stack_1980,PRE_stack_8190,PRE_stack_9100,PRE_stack_0110,PRE_stack_1118)
#all months for 1980-2014 (n=420)
PRE_Mo_8014<-stack(PRE_stack_1980,PRE_stack_8190,PRE_stack_9100,PRE_stack_0110,PRE_stack_1114)

#### PRE monthly long term average
PRE_Jan_8014<-calc(subset(PRE_Mo_8014, seq(1,409,12)), mean)
PRE_Feb_8014<-calc(subset(PRE_Mo_8014, seq(2,410,12)),mean)
PRE_Mar_8014<-calc(subset(PRE_Mo_8014, seq(3,411,12)),mean)
PRE_Apr_8014<-calc(subset(PRE_Mo_8014, seq(4,412,12)),mean)
PRE_May_8014<-calc(subset(PRE_Mo_8014, seq(5,413,12)),mean)
PRE_Jun_8014<-calc(subset(PRE_Mo_8014, seq(6,414,12)),mean)
PRE_Jul_8014<-calc(subset(PRE_Mo_8014, seq(7,415,12)),mean)
PRE_Aug_8014<-calc(subset(PRE_Mo_8014, seq(8,416,12)),mean)
PRE_Sep_8014<-calc(subset(PRE_Mo_8014, seq(9,417,12)),mean)
PRE_Oct_8014<-calc(subset(PRE_Mo_8014, seq(10,418,12)),mean)
PRE_Nov_8014<-calc(subset(PRE_Mo_8014, seq(11,419,12)),mean)
PRE_Dec_8014<-calc(subset(PRE_Mo_8014, seq(12,420,12)),mean)
PRE_mmmo_8014<-stack(PRE_Jan_8014,PRE_Feb_8014,PRE_Mar_8014,PRE_Apr_8014,PRE_May_8014,PRE_Jun_8014,
                     PRE_Jul_8014,PRE_Aug_8014,PRE_Sep_8014,PRE_Oct_8014,PRE_Nov_8014,PRE_Dec_8014,RAT=TRUE)
names(PRE_mmmo_8014)<-c("P_Jan","P_Feb","P_Mar","P_Apr","P_May","P_Jun","P_Jul","P_Aug","P_Sep","P_Oct","P_Nov","P_Dec")
PRE_mmyear_8014<-calc(PRE_mmmo_8014,sum)
names(PRE_mmyear_8014)<-c("P_mmyear")


######## Monthly PET (mm) #########
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PET/cru_ts4.04.1971.1980.pet.dat.nc")
PET_stack<-stack('cru_ts4.04.1971.1980.pet.dat.nc',varname='pet') #monthly PET 1971-1980 (only need 1980)
PET_stack_1980<-subset(PET_stack,109:120) #1980 stack of all months

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PET/cru_ts4.04.1981.1990.pet.dat.nc")
PET_stack_8190<-stack('cru_ts4.04.1981.1990.pet.dat.nc',varname='pet') #monthly PET 1981-1990

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PET/cru_ts4.04.1991.2000.pet.dat.nc")
PET_stack_9100<-stack('cru_ts4.04.1991.2000.pet.dat.nc',varname='pet') #monthly PET 1991-2000

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PET/cru_ts4.04.2001.2010.pet.dat.nc")
PET_stack_0110<-stack('cru_ts4.04.2001.2010.pet.dat.nc',varname='pet') #monthly PET 2001-2010

setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/CRU 4.04/PET/cru_ts4.04.2011.2019.pet.dat.nc")
PET_stack_1119<-stack('cru_ts4.04.2011.2019.pet.dat.nc',varname='pet') #monthly PET 2011-2019 (only want thru 2018)
#PET_stack_1118<-subset(PET_stack_1119,1:96) #monthly PET 2011-2018
PET_stack_1114<-subset(PET_stack_1119,1:48) #monthly PET 2011-2014 #Because GRUN_Q is limited to 2014

PET_Mo_8014<-stack(PET_stack_1980,PET_stack_8190,PET_stack_9100,PET_stack_0110,PET_stack_1114)

#### PET monthly long term average
PET_Jan_8014<-calc(subset(PET_Mo_8014, seq(1,409,12)), mean)*31
PET_Feb_8014<-calc(subset(PET_Mo_8014, seq(2,410,12)),mean)*28.25
PET_Mar_8014<-calc(subset(PET_Mo_8014, seq(3,411,12)),mean)*31
PET_Apr_8014<-calc(subset(PET_Mo_8014, seq(4,412,12)),mean)*30
PET_May_8014<-calc(subset(PET_Mo_8014, seq(5,413,12)),mean)*31
PET_Jun_8014<-calc(subset(PET_Mo_8014, seq(6,414,12)),mean)*30
PET_Jul_8014<-calc(subset(PET_Mo_8014, seq(7,415,12)),mean)*31
PET_Aug_8014<-calc(subset(PET_Mo_8014, seq(8,416,12)),mean)*31
PET_Sep_8014<-calc(subset(PET_Mo_8014, seq(9,417,12)),mean)*30
PET_Oct_8014<-calc(subset(PET_Mo_8014, seq(10,418,12)),mean)*31
PET_Nov_8014<-calc(subset(PET_Mo_8014, seq(11,419,12)),mean)*30
PET_Dec_8014<-calc(subset(PET_Mo_8014, seq(12,420,12)),mean)*31
PET_mmmo_8014<-stack(PET_Jan_8014,PET_Feb_8014,PET_Mar_8014,PET_Apr_8014,PET_May_8014,PET_Jun_8014,
                     PET_Jul_8014,PET_Aug_8014,PET_Sep_8014,PET_Oct_8014,PET_Nov_8014,PET_Dec_8014,RAT=TRUE)
names(PET_mmmo_8014)<-c("PET_Jan","PET_Feb","PET_Mar","PET_Apr","PET_May","PET_Jun","PET_Jul","PET_Aug","PET_Sep","PET_Oct","PET_Nov","PET_Dec")
PET_mmyear_8014<-calc(PET_mmmo_8014,sum)
names(PET_mmyear_8014)<-c("PET_mmyear")

#### Original stack for phase difference analysis #### 
###excludes Antarctica
all_stack<-stack(PRE_mmmo_8014,PRE_mmyear_8014,PET_mmmo_8014,PET_mmyear_8014,KGZ_raster_2018) 
#create unique identifier
all_stack$ID=1:259200 #number of total cells, including NA
#Attributes into dataframe
all_na<-as.data.frame((x=all_stack),na.rm=FALSE) #used for mapping
all_df<-as.data.frame((x=all_stack),na.rm=TRUE) #used for analysis

c<-subset(all_df,all_df$PET_mmyear>0&all_df$P_mmyear>0) 
#Pixels that had ann_PET = 0 & ann_P=0 were discarded (Antarctica and some Sahara).

########################## P-PET Phase Difference Analysis ######################################

month<-c(1:12)
xc<-cos(2*pi*month/12) 
xs<-sin(2*pi*month/12) 

# Create dataframe of PET and of P, such that columns are months and rows are individual pixels
PET<-cbind(c$PET_Jan,c$PET_Feb,c$PET_Mar,c$PET_Apr,c$PET_May,
           c$PET_Jun,c$PET_Jul,c$PET_Aug,c$PET_Sep,c$PET_Oct,
           c$PET_Nov,c$PET_Dec)
#P is always CRU
P<-cbind(c$P_Jan,c$P_Feb,c$P_Mar,c$P_Apr,c$P_May,c$P_Jun,
         c$P_Jul,c$P_Aug,c$P_Sep,c$P_Oct,c$P_Nov,c$P_Dec)

PET_sine8014<-data.frame(sim_num=1,adj.r.squared=1,p.value=1,theta=1,amp=1)[-1,] 
P_sine8014<-data.frame(sim_num=1,adj.r.squared=1,p.value=1,theta=1,amp=1)[-1,] 

########## FITTING SINE FUNCTION TO PET #################

for (i in 1:nrow(PET)){
  
  fit.lm<-lm(PET[i,]~xs+xc) #linear model to fit sine wave to data
  
  #find theta phase angle https://stats.stackexchange.com/questions/77543/how-do-i-get-the-amplitude-and-phase-for-sine-wave-from-lm-summary
  b0 <- coef(fit.lm)[1]
  alpha <- coef(fit.lm)[2] 
  beta <- coef(fit.lm)[3] 
  r <- sqrt(alpha^2 + beta^2) #amplitude
  theta <- atan2(beta, alpha) #phase angle
  #period is = 1 https://stats.stackexchange.com/questions/60500/how-to-find-a-good-fit-for-semi-sinusoidal-model-in-r/60504
  #theta = arctan(y/x) = atan2(y,x) when x>0. 
  #When x<0, pi (if in radians) or 180* (if in degrees) must be added or subtracted from theta to put x,y in correct Euclidean plane.
  #atan2 automatically accounts for this. https://en.wikipedia.org/wiki/Atan2 
  
  ### To pull overall model p-value ###
  lmp <- function (fit.lm) {
    if (class(fit.lm) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(fit.lm)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  lmp(fit.lm)
  
  #write results to table
  PET_sine8014[i,]<-c(i, #simulation number
                      summary(fit.lm)[['adj.r.squared']],
                      lmp(fit.lm), #p-value
                      theta, #phase angle
                      r) #amplitude
}

########## FITTING SINE TO P #################

for (i in 1:nrow(P)){
  
  fit.lm<-lm(P[i,]~xs+xc)
  
  #theta phase parameter
  b0 <- coef(fit.lm)[1]
  alpha <- coef(fit.lm)[2]
  beta <- coef(fit.lm)[3]
  r <- sqrt(alpha^2 + beta^2)
  theta <- atan2(beta, alpha)
  
  ### To pull overall model p-value ###
  lmp <- function (fit.lm) {
    if (class(fit.lm) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(fit.lm)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  lmp(fit.lm)
  
  #write results to table
  P_sine8014[i,]<-c(i,
                    summary(fit.lm)[['adj.r.squared']],
                    lmp(fit.lm), #p-value
                    theta, #phase angle
                    r) #amplitude
}

PET_sine8014<-PET_sine8007 #this was a mistake in naming, from earlier analysis.
P_sine8014<-P_sine8007

colnames(PET_sine8014)=c("sim","PET_rsq","PET_pvalue","PET_theta","PET_amp")
colnames(P_sine8014)=c("sim","P_rsq","P_pvalue","P_theta","P_amp")

dtheta<-as.data.frame(PET_sine8014$PET_theta-P_sine8014$P_theta)
colnames(dtheta)=c("dtheta_orig")

#### Putting delta theta in correct quadrant ####
library(dplyr)
dtheta$dtheta_correct<-case_when(
  ((dtheta$dtheta_orig<=pi)&(dtheta$dtheta_orig>=(-pi)))~dtheta$dtheta_orig,
  dtheta$dtheta_orig>pi~dtheta$dtheta_orig-(2*pi),
  dtheta$dtheta_orig<pi~dtheta$dtheta_orig+(2*pi)
)

all2<-as.data.frame(cbind(c,dtheta,PET_sine8014[2:5],P_sine8014[2:5]))

######### ECDF plots of sine fit metrics (+performance) #########
# R2
ecdfPET<-ecdf(PET_sine8014$PET_rsq)
ecdfP<-ecdf(P_sine8014$P_rsq)
plot(ecdfPET,verticals=TRUE,do.points=FALSE,col="red",lwd=2,main="",xlab="R2 [.]")
plot(ecdfP,verticals=TRUE,do.points=FALSE,add=TRUE,col="blue",lwd=2)
# pvalue
ecdfPET<-ecdf(PET_sine8014$PET_pvalue)
ecdfP<-ecdf(P_sine8014$P_pvalue)
plot(ecdfPET,verticals=TRUE,do.points=FALSE,col="red",lwd=2,main="",xlab="pvalue")
plot(ecdfP,verticals=TRUE,do.points=FALSE,add=TRUE,col="blue",lwd=2)

######### Map of phase diff ############

coordinates<-as.data.frame(coordinates(all_stack)) #"all" is the raster stack
all_na_coord<-cbind(all_na,coordinates$x,coordinates$y)
library(dplyr)
df<-full_join(all2,all_na_coord,by="ID")
df2<-as.data.frame(cbind(df$`coordinates$x`,df$`coordinates$y`,df$PET_theta)) #the variable you want to map
colnames(df2)=c("x","y","")
map<-rasterFromXYZ(df2)
#reproject to WGS84
crs(map) <- "+proj=longlat +datum=WGS84"
#plot(map,col=c("lightgrey","lightblue2","lightblue3","blue","darkblue","purple"))
plot(map)

#overlay koppen geiger boundaries to raster above
library(rgeos)
k2<-rasterToPolygons(KPG_raster,n=16,dissolve=TRUE)
#k3 <- spTransform(k2, CRSobj = proj4string(map))
#plot(k3, add=TRUE) # y coordinates are compressed, so this isn't adding to the map correctly

# This one works though...
k<-rasterToPolygons(KGZ_raster_2018,n=16,dissolve = TRUE)
plot(KGZ_raster_2018)
plot(k,add=TRUE)
#or lines(k) would work

library(tidyr)
library(sf)
library(ggplot2)
rasdf<-as.data.frame(phasediff_raster,xy=TRUE)%>%drop_na()
test<-st_as_sf(k2)

breakpoints <- c(-3.15,-2,-1,-0.5,0,0.5,1,2,3.15) #if -0.5<dtheta<0.5 then this is <1 mo diff, so "in phase"
mycolors <- c("palevioletred3","palevioletred1","mistyrose1","mintcream","mintcream","lightsteelblue1","lightsteelblue","navy")

plot(map,breaks=breakpoints,col=mycolors)
plot(k2,add=TRUE)

ggplot()+
  geom_raster(aes(x=x,y=y,fill=cut(phasediff,breaks=breakpoints)),data=rasdf)+
  #geom_sf(fill='transparent',data=test)+
  geom_sf(data=test,aes(fill='transparent',color="black"))+
  scale_fill_gradientn(colors = mycolors,breaks=breakpoints) +
  scale_fill_manual(values = mycolors) +
  theme(plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"))




# export raster
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
# write raster to netcdf 
if (require(ncdf4)) {	
  wr <- writeRaster(map, filename='Phasediff8014_allfits_CRUV4.04_20210422.nc', format="CDF", overwrite=TRUE)   
}

######### Map of p-values for sine fits (P and PET) ############
coordinates<-as.data.frame(coordinates(all)) #"all" is the raster stack
all_na_coord<-cbind(all_na,coordinates$x,coordinates$y)
library(dplyr)
df<-full_join(all2,all_na_coord,by="ID")
df2<-as.data.frame(cbind(df$`coordinates$x`,df$`coordinates$y`,df$P_pvalue)) #the variable you want to map
colnames(df2)=c("x","y","p-value")
map<-rasterFromXYZ(df2)
breakpoints <- c(0,0.0005,0.05,0.1,0.5,1)
colors <- c("lightgray","lightblue","yellow","orange","red")

#for dtheta map
breakpoints <- c(-3.15,-2,-1,-0.5,0,0.5,1,2,3.15) #if -0.5<dtheta<0.5 then this is <1 mo diff, so "in phase"
colors <- c("palevioletred3","palevioletred1","mistyrose1","mintcream","mintcream","lightsteelblue1","lightsteelblue","navy")
plot(map,breaks=breakpoints,col=colors) # change "map" var above
plot(k2,add=TRUE) #k2 is raster to polygon from KPG_raster

########### Maps of other sine metrics, change var as necessary ###############
coordinates<-as.data.frame(coordinates(all_stack)) #"all_stack" is the raster stack for phasediff analysis
all_na_coord<-cbind(all_na,coordinates$x,coordinates$y)
library(dplyr)
df<-full_join(all2,all_na_coord,by="ID")
df2<-as.data.frame(cbind(df$`coordinates$x`,df$`coordinates$y`,df$P_amp)) #the variable you want to map
colnames(df2)=c("x","y","")
map<-rasterFromXYZ(df2)
plot(map)

#plot map
breakpoints <- c(0,0.0005,0.05,0.1,0.5,1) #p value
breakpoints <- c(-0.3,0,0.1,0.20,0.60,0.8,1) #R2
breakpoints <- c(0,5,50,100,200,600) #amplitude
breakpoints <- c(-3.15,-2.5,-1,0,1,2.5,3.15) #theta
breakpoints <- c(0,200,500,1000,2000,7600) #mean annual P
breakpoints <- c(0,500,1000,1500,2000,3000) #mean annual PET
colors <- c("lightgray","lightblue","yellow","orange",
            "orangered", #add "orangered" when six colors needed
            "red")
plot(map,breaks=breakpoints,col=colors)
plot(k2,add=TRUE)

######## Look at where P sine fits are bad in individual cells #######
plot(ecdf(P_sine8014$P_amp))
quantile(P_sine8014$P_amp)
k<-subset(P_sine8014,P_sine8014$P_rsq<0)
k<-subset(df,(df$P_amp/range(df[,1:12]))<0.5)
k<-P_sine8014[(P_sine8014$P_amp < 5) & (P_sine8014$P_rsq < 0), ]
plot(P[45610,])
fit<-lm(P[45610,]~xs+xc)
points(fit$fitted.values,type="l",col="red")
print(subset(PET_sine8014,PET_sine8014$sim==48657))
mean(PET[48657,])
#which Koppen Geiger zone is the pixel in
c$KGZ[28148]

### Fixing theta values for when R2<0
library(dplyr)
P_sine8014$P_theta_correct<-case_when(
  P_sine8014$P_rsq<0~0, #theta = 0 when R2<0
  P_sine8014$P_rsq>0~P_sine8014$P_theta
)

####### Phase Difference raster - updated for theta = 0 when R2<0 ######## 

###### Phase Difference raster - read in, derived from above ######
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
phasediff_raster<-raster('Phasediff8014_allfits_CRUV4.04_20210422.nc')
names(phasediff_raster)=c("phasediff")

##### TerraClimate ET (mm/mo) ########
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/TerraClimate/AET/")
nclist=list.files(getwd(), pattern="nc$", full.names=TRUE) #read in nc files to list
ET_stack_8018<-stack(nclist) #This dataset is originally 1980-2018
#reproject to WGS84
crs(ET_stack_8018) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#resample to 0.5 deg, 1980-2018 (all data)
ET_stack_8018_resample<-resample(x=ET_stack_8018,y=PRE_Mo_8014)

# write raster to netcdf - resampled ET_TerraClimate (0.5 deg) for future use
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data/TerraClimate_19802018")
if (require(ncdf4)) {	
  t <- writeRaster(ET_stack_8018_resample, filename='ET_stack_8018_resample20210322.nc', format="CDF", overwrite=TRUE)   
}

###### TerraClimate ET stack - read in, derived from above ######
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data/TerraClimate_19802018")
ET_stack_8018_resample<-stack("ET_stack_8018_resample20210322.nc")


#### ET monthly long term average for our time period 1980-2014
ET_Mo_8018<-ET_stack_8018_resample

ET_Jan_8014<-calc(subset(ET_Mo_8018, seq(1,409,12)),mean)
ET_Feb_8014<-calc(subset(ET_Mo_8018, seq(2,410,12)),mean)
ET_Mar_8014<-calc(subset(ET_Mo_8018, seq(3,411,12)),mean)
ET_Apr_8014<-calc(subset(ET_Mo_8018, seq(4,412,12)),mean)
ET_May_8014<-calc(subset(ET_Mo_8018, seq(5,413,12)),mean)
ET_Jun_8014<-calc(subset(ET_Mo_8018, seq(6,414,12)),mean)
ET_Jul_8014<-calc(subset(ET_Mo_8018, seq(7,415,12)),mean)
ET_Aug_8014<-calc(subset(ET_Mo_8018, seq(8,416,12)),mean)
ET_Sep_8014<-calc(subset(ET_Mo_8018, seq(9,417,12)),mean)
ET_Oct_8014<-calc(subset(ET_Mo_8018, seq(10,418,12)),mean)
ET_Nov_8014<-calc(subset(ET_Mo_8018, seq(11,419,12)),mean)
ET_Dec_8014<-calc(subset(ET_Mo_8018, seq(12,420,12)),mean) #only to 420, not 468 because 2014, not 2018
ET_mmmo_8014<-stack(ET_Jan_8014,ET_Feb_8014,ET_Mar_8014,ET_Apr_8014,ET_May_8014,ET_Jun_8014,
                    ET_Jul_8014,ET_Aug_8014,ET_Sep_8014,ET_Oct_8014,ET_Nov_8014,ET_Dec_8014,RAT=TRUE)
names(ET_mmmo_8014)<-c("ET_Jan","ET_Feb","ET_Mar","ET_Apr","ET_May","ET_Jun","ET_Jul","ET_Aug","ET_Sep","ET_Oct","ET_Nov","ET_Dec")
ET_mmyear_8014<-calc(ET_mmmo_8014,sum)
names(ET_mmyear_8014)<-c("ET_mmyear")

######### GLEAM ET (mm/mo) ###########
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/GLEAMv3.5a_MO_1980_2020")
ET_GLEAM_stack_8020<-stack("E_1980-2020_GLEAM_v3.5a_MO.nc")
# Must be properly aligned before resampling
ET_GLEAM_stack_8020_tflip<-t(flip(ET_GLEAM_stack_8020,2))

#resample to 0.5 deg
ET_GLEAM_stack_8020_resample<-resample(x=ET_GLEAM_stack_8020_tflip,y=PRE_mmyear_8014$P_mmyear)

# write raster to netcdf - resampled ET_GLEAM (0.5 deg) for future use
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/GLEAMv3.5a_MO_1980_2020")
if (require(ncdf4)) {	
  t2 <- writeRaster(ET_GLEAM_stack_8020_resample, filename='ET_GLEAM_stack_8020_resample20210421.nc', format="CDF", overwrite=TRUE)   
}

###### ET GLEAM stack - read in, derived from above #######
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/GLEAMv3.5a_MO_1980_2020")
ET_GLEAM_stack_8020_resample<-stack("ET_GLEAM_stack_8020_resample20210421.nc")

#### ET_GLEAM long term monthly average
ET_GLEAM_Mo_8020<-ET_GLEAM_stack_8020_resample

ET_GLEAM_Jan_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(1,409,12)),mean)
ET_GLEAM_Feb_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(2,410,12)),mean)
ET_GLEAM_Mar_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(3,411,12)),mean)
ET_GLEAM_Apr_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(4,412,12)),mean)
ET_GLEAM_May_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(5,413,12)),mean)
ET_GLEAM_Jun_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(6,414,12)),mean)
ET_GLEAM_Jul_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(7,415,12)),mean)
ET_GLEAM_Aug_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(8,416,12)),mean)
ET_GLEAM_Sep_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(9,417,12)),mean)
ET_GLEAM_Oct_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(10,418,12)),mean)
ET_GLEAM_Nov_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(11,419,12)),mean)
ET_GLEAM_Dec_8014<-calc(subset(ET_GLEAM_Mo_8020, seq(12,420,12)),mean)
ET_GLEAM_mmmo_8014<-stack(ET_GLEAM_Jan_8014,ET_GLEAM_Feb_8014,ET_GLEAM_Mar_8014,ET_GLEAM_Apr_8014,ET_GLEAM_May_8014,ET_GLEAM_Jun_8014,
                   ET_GLEAM_Jul_8014,ET_GLEAM_Aug_8014,ET_GLEAM_Sep_8014,ET_GLEAM_Oct_8014,ET_GLEAM_Nov_8014,ET_GLEAM_Dec_8014,RAT=TRUE)
names(ET_GLEAM_mmmo_8014)<-c("ET_GLEAM_Jan","ET_GLEAM_Feb","ET_GLEAM_Mar","ET_GLEAM_Apr","ET_GLEAM_May","ET_GLEAM_Jun","ET_GLEAM_Jul","ET_GLEAM_Aug","ET_GLEAM_Sep","ET_GLEAM_Oct","ET_GLEAM_Nov","ET_GLEAM_Dec")
ET_GLEAM_mmyear_8014<-calc(ET_GLEAM_mmmo_8014,sum)
names(ET_GLEAM_mmyear_8014)<-c("ET_GLEAM_mmyear")

##### TerraClimate Q (mm/mo) ########
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida//GIS Data/TerraClimate/Q/")
nclist=list.files(getwd(), pattern="nc$", full.names=TRUE) #read in nc files to list
Q_stack_8018<-stack(nclist) #this is original resolution, 1/24 deg (~4km)
#reproject to WGS84
crs(Q_stack_8018) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#resample to 0.5 deg
Q_stack_8018_resample<-resample(x=Q_stack_8018,y=PRE_Mo_8014)

# write raster to netcdf - resampled Q_TerraClimate (0.5 deg) for future use
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data/TerraClimate_19802018")
if (require(ncdf4)) {	
  t2 <- writeRaster(Q_stack_8018_resample, filename='Q_stack_8018_resample20210322.nc', format="CDF", overwrite=TRUE)   
}

###### TerraClimate Q stack - read in, derived from above #######
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data/TerraClimate_19802018")
Q_stack_8018<-stack("Q_stack_8018_resample20210322.nc")

#### Q long term monthly average
Q_Mo_8018<-Q_stack_8018

Q_Jan_8014<-calc(subset(Q_Mo_8018, seq(1,409,12)),mean)
Q_Feb_8014<-calc(subset(Q_Mo_8018, seq(2,410,12)),mean)
Q_Mar_8014<-calc(subset(Q_Mo_8018, seq(3,411,12)),mean)
Q_Apr_8014<-calc(subset(Q_Mo_8018, seq(4,412,12)),mean)
Q_May_8014<-calc(subset(Q_Mo_8018, seq(5,413,12)),mean)
Q_Jun_8014<-calc(subset(Q_Mo_8018, seq(6,414,12)),mean)
Q_Jul_8014<-calc(subset(Q_Mo_8018, seq(7,415,12)),mean)
Q_Aug_8014<-calc(subset(Q_Mo_8018, seq(8,416,12)),mean)
Q_Sep_8014<-calc(subset(Q_Mo_8018, seq(9,417,12)),mean)
Q_Oct_8014<-calc(subset(Q_Mo_8018, seq(10,418,12)),mean)
Q_Nov_8014<-calc(subset(Q_Mo_8018, seq(11,419,12)),mean)
Q_Dec_8014<-calc(subset(Q_Mo_8018, seq(12,420,12)),mean)
Q_mmmo_8014<-stack(Q_Jan_8014,Q_Feb_8014,Q_Mar_8014,Q_Apr_8014,Q_May_8014,Q_Jun_8014,
                   Q_Jul_8014,Q_Aug_8014,Q_Sep_8014,Q_Oct_8014,Q_Nov_8014,Q_Dec_8014,RAT=TRUE)
names(Q_mmmo_8014)<-c("Q_Jan","Q_Feb","Q_Mar","Q_Apr","Q_May","Q_Jun","Q_Jul","Q_Aug","Q_Sep","Q_Oct","Q_Nov","Q_Dec")
Q_mmyear_8014<-calc(Q_mmmo_8014,sum)
names(Q_mmyear_8014)<-c("Q_mmyear")

##### GRUN Q (mm/day), monthly resolution 1902-2014 ########
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/GRUN_globalrunoff/")
nclist=list.files(getwd(), pattern="nc$", full.names=TRUE) #read in nc files to list
Q_GRUN_stack_0214<-stack(nclist) #already 0.5 deg, 1902-2014
#reproject to WGS84
crs(Q_GRUN_stack_0214) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

Q_GRUN_Jan_8014<-(calc(subset(Q_GRUN_stack_0214, seq(937,1345,12)),mean))*31 #pull each month starting at 1980
Q_GRUN_Feb_8014<-(calc(subset(Q_GRUN_stack_0214, seq(938,1346,12)),mean))*28.25
Q_GRUN_Mar_8014<-(calc(subset(Q_GRUN_stack_0214, seq(939,1347,12)),mean))*31
Q_GRUN_Apr_8014<-(calc(subset(Q_GRUN_stack_0214, seq(940,1348,12)),mean))*30
Q_GRUN_May_8014<-(calc(subset(Q_GRUN_stack_0214, seq(941,1349,12)),mean))*31
Q_GRUN_Jun_8014<-(calc(subset(Q_GRUN_stack_0214, seq(942,1350,12)),mean))*30
Q_GRUN_Jul_8014<-(calc(subset(Q_GRUN_stack_0214, seq(943,1351,12)),mean))*31
Q_GRUN_Aug_8014<-(calc(subset(Q_GRUN_stack_0214, seq(944,1352,12)),mean))*31
Q_GRUN_Sep_8014<-(calc(subset(Q_GRUN_stack_0214, seq(945,1353,12)),mean))*30
Q_GRUN_Oct_8014<-(calc(subset(Q_GRUN_stack_0214, seq(946,1354,12)),mean))*31
Q_GRUN_Nov_8014<-(calc(subset(Q_GRUN_stack_0214, seq(947,1355,12)),mean))*30
Q_GRUN_Dec_8014<-(calc(subset(Q_GRUN_stack_0214, seq(948,1356,12)),mean))*31
Q_GRUN_mmmo_8014<-stack(Q_GRUN_Jan_8014,Q_GRUN_Feb_8014,Q_GRUN_Mar_8014,Q_GRUN_Apr_8014,Q_GRUN_May_8014,Q_GRUN_Jun_8014,
                   Q_GRUN_Jul_8014,Q_GRUN_Aug_8014,Q_GRUN_Sep_8014,Q_GRUN_Oct_8014,Q_GRUN_Nov_8014,Q_GRUN_Dec_8014,RAT=TRUE)
names(Q_GRUN_mmmo_8014)<-c("Q_GRUN_Jan","Q_GRUN_Feb","Q_GRUN_Mar","Q_GRUN_Apr","Q_GRUN_May","Q_GRUN_Jun",
                           "Q_GRUN_Jul","Q_GRUN_Aug","Q_GRUN_Sep","Q_GRUN_Oct","Q_GRUN_Nov","Q_GRUN_Dec")
Q_GRUN_mmyear_8014<-calc(Q_GRUN_mmmo_8014,sum)
names(Q_GRUN_mmyear_8014)<-c("Q_GRUN_mmyear")

########## Established Climate Classification Systems ################

## Koppen-Geiger Raster##
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/KGZ_Beck_V1")
KGZ_raster_2018<-raster('Beck_KG_V1_present_0p5.tif') # this is updated one (1980-2016), n=28
names(KGZ_raster_2018)=c("KGZ")

##### Holdridge Life Zones (bioclimate) ### 
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/holdrid_climate_zones/")
require(rgdal)
Holdridge <- readOGR(dsn = ".",layer="holdrid") #38 zones
#turn polygon into raster
r <- raster(ncol=720, nrow=360)
extent(r) <- extent(Holdridge)
rp <- rasterize(Holdridge, r,field=as.numeric(Holdridge$ZONE))
Holdridge_resample<-resample(x=rp,y=PRE_mmyear_8014,method="ngb") #method="bilinear" for continuous data, ="ngb" for categorical
Holdridge_zones<-Holdridge_resample
names(Holdridge_zones)=c("Holdridge")

#### Knoben et al., 2018 Hydroclimate Zones ###
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/Knoben_hydroclimatezones_2018/Knoben/")
Knoben_raster<-raster('ClimateClassification_mainMap_geoReferenced.tif') #original fuzzy boundaries
#Create climate zones using climate indices from Knoben et al., 2018
#indices
Knoben_aridity<-raster('HydrologicClimateClassification.nc',var="grid_aridity_Im")
Knoben_seasonal_aridity<-raster('HydrologicClimateClassification.nc',var="grid_seasonalityOfAridity_Imr")
Knoben_annualsnowfraction<-raster('HydrologicClimateClassification.nc',var="grid_annualSnowFraction_fs")
#reproject to WGS84
crs(Knoben_aridity) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(Knoben_seasonal_aridity) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
crs(Knoben_annualsnowfraction) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
#stack with unique identifier for mapping purposes
Knoben<-stack(Knoben_aridity,Knoben_seasonal_aridity,Knoben_annualsnowfraction)
Knoben$ID=1:259200
#cluster 
Knoben_df<-as.data.frame(Knoben,na.rm=TRUE)
test<-kmeans(Knoben_df[1:3],centers=18,nstart=30) #18 zones because this is the number in their paper
Knobenclusters_df<-as.data.frame(test$cluster)
colnames(Knobenclusters_df)=c("Knoben_cluster")
Knoben_df2<-cbind(Knoben_df,Knobenclusters_df)
# Plot map #
library(dplyr)
attributes_df_NA<-as.data.frame((x=Knoben),na.rm=FALSE) #Include NA values so cell values match #cells
coordinates<-as.data.frame(coordinates(Knoben)) #get coordinates of raster
df_NA2<-cbind(attributes_df_NA,coordinates$x,coordinates$y) #join coordinates with dataframe
joined_df<-full_join(Knoben_df2,df_NA2,by="ID") #with data to map
map_df<-as.data.frame(cbind(joined_df$`coordinates$x`,joined_df$`coordinates$y`,joined_df$Knoben_cluster))
colnames(map_df)=c("x","y","Knoben")
dfr<-rasterFromXYZ(map_df)
plot(dfr,col=terrain.colors(18),main="Knoben")
#Reorient map -- this works!
test5<-(flip(dfr,direction='x'))
test6<-(t(flip(test5,2)))
plot(test6)
extent(test6)<-extent(-180,180,-90,90) #do this after it's rotated correctly
(plot(test6))
#now resample...
Knoben_cluster_raster<-resample(test6,KGZ_raster_2018)
#reproject
crs(Knoben_cluster_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
plot(Knoben_cluster_raster)
names(Knoben_cluster_raster)=c("Knoben_cluster")

#### Meybeck et al. 2013 Hydroregions ###
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/GIS Data/Meybeck_hydrobelts_2013/gis-files/meybeck_et_al_2013_hydroregions_shp/")
Meybeck<-readOGR(dsn=".",layer="meybeck_et_al_2013_hydroregions")
# make raster of Meybeck
r <- raster(ncol=720, nrow=360)
extent(r) <- extent(Meybeck)
rp <- rasterize(Meybeck, r,field=as.numeric(Meybeck$hydroregio))
Meybeck_resample<-resample(x=rp,y=PRE_mmyear_8014, method="ngb") #usually method="bilinear" for continuous, not categorical, data
Meybeck_zones<-Meybeck_resample #26 zones
names(Meybeck_zones)=c("Meybeck")

########## Stacking proposed zones using framework described below #######
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
library(raster)
ETC_raster<-raster('ETC_raster.nc')
ETV_raster<-raster('ETV_raster.nc')
WEC_raster<-raster('WEC_raster.nc')
ETA_raster<-raster('ETA_raster.nc')
KPG_raster<-raster('KPG_raster.nc') #used in this study, excludes Antarctica

### Phase difference (using method from ET Drivers) 80-07 ###
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
phasediff_allfits<-raster('Phasediff_allfits.nc')

##### Stack Attributes ####
library(ncdf4)
library(sp)
coherence_all_atts<-stack(ET_mmmo_8014,ET_mmyear_8014,ET_GLEAM_mmyear_8014,
                          Q_mmmo_8014,Q_mmyear_8014,Q_GRUN_mmmo_8014,Q_GRUN_mmyear_8014,
                          PET_mmmo_8014,PET_mmyear_8014,PRE_mmmo_8014,PRE_mmyear_8014,
                          KGZ_raster_2018,phasediff_raster,
                          Holdridge_zones,
                          Meybeck_zones,
                          #Knoben_zones,
                          Knoben_cluster_raster,ETA_raster,ETV_raster,ETC_raster,WEC_raster,RAT=TRUE)
#create unique identifier
coherence_all_atts$ID=1:259200
coherence_all_df<-as.data.frame(coherence_all_atts,na.rm=TRUE) #removed NA values
all<-coherence_all_df
# change ET mm/year < 1 to 0
all$ET_mmyear[all$ET_mmyear<1] <- 0

#all$KGletter="example"
all$KGletter[all$KGZ==1]="Af" 
all$KGletter[all$KGZ==2]="Am" 
all$KGletter[all$KGZ==3]="Aw" 
all$KGletter[all$KGZ==4]="BWh" 
all$KGletter[all$KGZ==5]="BWk" 
all$KGletter[all$KGZ==6]="BSh" 
all$KGletter[all$KGZ==7]="BSk" 
all$KGletter[all$KGZ==8]="Csa" 
all$KGletter[all$KGZ==9]="Csb" 
all$KGletter[all$KGZ==10]="Csc" 
all$KGletter[all$KGZ==11]="Cwa" 
all$KGletter[all$KGZ==12]="Cwb" 
all$KGletter[all$KGZ==13]="Cwc" 
all$KGletter[all$KGZ==14]="Cfa" 
all$KGletter[all$KGZ==15]="Cfb"
all$KGletter[all$KGZ==16]="Cfc" 
all$KGletter[all$KGZ==17]="Dsa" 
all$KGletter[all$KGZ==18]="Dsb" 
all$KGletter[all$KGZ==19]="Dsc" 
all$KGletter[all$KGZ==20]="Dsd" 
all$KGletter[all$KGZ==21]="Dwa" 
all$KGletter[all$KGZ==22]="Dwb" 
all$KGletter[all$KGZ==23]="Dwc" 
all$KGletter[all$KGZ==24]="Dwd" 
all$KGletter[all$KGZ==25]="Dfa" 
all$KGletter[all$KGZ==26]="Dfb" 
all$KGletter[all$KGZ==27]="Dfc" 
all$KGletter[all$KGZ==28]="Dfd" 
all$KGletter[all$KGZ==29]="ET" 
all$KGletter[all$KGZ==30]="EF" 

############### Comparing Q_GRUN and Q_TerraClimate ###############
which( colnames(all)=="Q_Dec" )
diff<-(all[,27:38]-all[,14:25])

breakpoints <- c(-400,-100,-10,-1,0,1,10,100,500) # for absolute difference
breakpoints <- c(-1000,-1,-0.25,-0.0001,0,0.0001,0.25,1,1000) # for comparative difference
colors <- c("red","orange","yellow","white","white","lightblue","blue","darkblue")
#for plotting one month
breakpoints <- c(0,2,5,10,50,100,200,1000,3000)
colors <- c("red","orange","yellow","lightblue","blue","darkblue","purple","black")
par(mfrow=c(1,1))
plot(Q_Mo_8018$X7,breaks=breakpoints,col=colors) #TerraClimate
plot(Q_GRUN_stack_0214$X1980.07.01*31,breaks=breakpoints,col=colors) #GRUN
## for mapping all 12 months
par(mar=c(0.5, 0.5, 2, 0.2), mfrow=c(3,4),
    oma = c(4, 2, 0.5, 0.2))
# absolute difference
plot(Q_Jan_8014-Q_GRUN_Jan_8014,breaks=breakpoints,col=colors, main="January",legend=FALSE)
plot(Q_Feb_8014-Q_GRUN_Feb_8014,breaks=breakpoints,col=colors, main="February",legend=FALSE)
plot(Q_Mar_8014-Q_GRUN_Mar_8014,breaks=breakpoints,col=colors, main="March",legend=FALSE)
plot(Q_Apr_8014-Q_GRUN_Apr_8014,breaks=breakpoints,col=colors, main="April",legend=FALSE)
plot(Q_May_8014-Q_GRUN_May_8014,breaks=breakpoints,col=colors, main="May",legend=FALSE)
plot(Q_Jun_8014-Q_GRUN_Jun_8014,breaks=breakpoints,col=colors, main="June",legend=FALSE)
plot(Q_Jul_8014-Q_GRUN_Jul_8014,breaks=breakpoints,col=colors, main="July",legend=FALSE)
plot(Q_Aug_8014-Q_GRUN_Aug_8014,breaks=breakpoints,col=colors, main="August",legend=FALSE)
plot(Q_Sep_8014-Q_GRUN_Sep_8014,breaks=breakpoints,col=colors, main="September",legend=FALSE)
plot(Q_Oct_8014-Q_GRUN_Oct_8014,breaks=breakpoints,col=colors, main="October",legend=FALSE)
plot(Q_Nov_8014-Q_GRUN_Nov_8014,breaks=breakpoints,col=colors, main="November",legend=FALSE)
plot(Q_Dec_8014-Q_GRUN_Dec_8014,breaks=breakpoints,col=colors, main="December",legend=FALSE)

#comparative difference [.]
plot(((Q_Jan_8014-Q_GRUN_Jan_8014)/Q_Jan_8014),breaks=breakpoints,col=colors, main="January",legend=FALSE)
plot(((Q_Feb_8014-Q_GRUN_Feb_8014)/Q_Feb_8014),breaks=breakpoints,col=colors, main="February",legend=FALSE)
plot(((Q_Mar_8014-Q_GRUN_Mar_8014)/Q_Mar_8014),breaks=breakpoints,col=colors, main="March",legend=FALSE)
plot(((Q_Apr_8014-Q_GRUN_Apr_8014)/Q_Apr_8014),breaks=breakpoints,col=colors, main="April",legend=FALSE)
plot(((Q_May_8014-Q_GRUN_May_8014)/Q_May_8014),breaks=breakpoints,col=colors, main="May",legend=FALSE)
plot(((Q_Jun_8014-Q_GRUN_Jun_8014)/Q_Jun_8014),breaks=breakpoints,col=colors, main="June",legend=FALSE)
plot(((Q_Jul_8014-Q_GRUN_Jul_8014)/Q_Jul_8014),breaks=breakpoints,col=colors, main="July",legend=FALSE)
plot(((Q_Aug_8014-Q_GRUN_Aug_8014)/Q_Aug_8014),breaks=breakpoints,col=colors, main="August",legend=FALSE)
plot(((Q_Sep_8014-Q_GRUN_Sep_8014)/Q_Sep_8014),breaks=breakpoints,col=colors, main="September",legend=FALSE)
plot(((Q_Oct_8014-Q_GRUN_Oct_8014)/Q_Oct_8014),breaks=breakpoints,col=colors, main="October",legend=FALSE)
plot(((Q_Nov_8014-Q_GRUN_Nov_8014)/Q_Nov_8014),breaks=breakpoints,col=colors, main="November",legend=FALSE)
plot(((Q_Dec_8014-Q_GRUN_Dec_8014)/Q_Dec_8014),breaks=breakpoints,col=colors, main="December",legend=FALSE)

library(scales)
par(mfrow=c(3,4))
plot(x=all$Q_Jan,y=all$Q_GRUN_Jan,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Jan",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Jan~all$Q_Jan),col="red")
modsum<-summary(lm(all$Q_GRUN_Jan~all$Q_Jan))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Feb,y=all$Q_GRUN_Feb,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Feb",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Feb~all$Q_Feb),col="red")
modsum<-summary(lm(all$Q_GRUN_Feb~all$Q_Feb))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Mar,y=all$Q_GRUN_Mar,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Mar",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Mar~all$Q_Mar),col="red")
modsum<-summary(lm(all$Q_GRUN_Mar~all$Q_Mar))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Apr,y=all$Q_GRUN_Apr,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Apr",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Apr~all$Q_Apr),col="red")
modsum<-summary(lm(all$Q_GRUN_Apr~all$Q_Apr))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_May,y=all$Q_GRUN_May,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="May",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_May~all$Q_May),col="red")
modsum<-summary(lm(all$Q_GRUN_May~all$Q_May))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Jun,y=all$Q_GRUN_Jun,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Jun",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Jun~all$Q_Jun),col="red")
modsum<-summary(lm(all$Q_GRUN_Jun~all$Q_Jun))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Jul,y=all$Q_GRUN_Jul,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Jul",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Jul~all$Q_Jul),col="red")
modsum<-summary(lm(all$Q_GRUN_Jul~all$Q_Jul))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Aug,y=all$Q_GRUN_Aug,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Aug",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Aug~all$Q_Aug),col="red")
modsum<-summary(lm(all$Q_GRUN_Aug~all$Q_Aug))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Sep,y=all$Q_GRUN_Sep,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Sep",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Sep~all$Q_Sep),col="red")
modsum<-summary(lm(all$Q_GRUN_Sep~all$Q_Sep))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Oct,y=all$Q_GRUN_Oct,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Oct",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Oct~all$Q_Oct),col="red")
modsum<-summary(lm(all$Q_GRUN_Oct~all$Q_Oct))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Nov,y=all$Q_GRUN_Nov,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Nov",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Nov~all$Q_Nov),col="red")
modsum<-summary(lm(all$Q_GRUN_Nov~all$Q_Nov))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=all$Q_Dec,y=all$Q_GRUN_Dec,xlab="TerraClimate Q (mm)",ylab="GRUN Q (mm)",main="Dec",xlim=c(0,1800),ylim=c(0,900), col = alpha("black", 0.4),cex=1.5)
abline(lm(all$Q_GRUN_Dec~all$Q_Dec),col="red")
modsum<-summary(lm(all$Q_GRUN_Dec~all$Q_Dec))
r2 = modsum$adj.r.squared
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)

## to make a gradient of other values but gray for zero values (!!! DID NOT WORK !!!)
# Load libraries
#library('rasterVis')
#library('RColorBrewer')
# Set color palette
#zeroCol <-"#B3B3B3" # (gray color, same as your figure example)
#reds <- rev(brewer.pal('YlOrRd', n = 7))
#blues <- brewer.pal('Blues', n = 7)
#myTheme <- rasterTheme(region = c(reds, blues)) #put zeroCol in the middle for "gray"
# Plot
#levelplot(Q_mmyear_8014-Q_GRUN_mmyear_8014, par.settings = myTheme, margin = FALSE)

# Comparing mean GRUN_Q and TerraClimate_Q
test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$WEC), mean)
test2<-as.data.frame((colMeans(test[,-1])/100))


############### Comparing ET_GLEAM and ET_TerraClimate ###############
ET_TC_GLEAM<-as.data.frame(stack(ET_mmmo_8014,ET_GLEAM_mmmo_8014),na.rm=TRUE)
which( colnames(ET_TC_GLEAM)=="ET_GLEAM_Dec" )
diff<-(all[,27:38]-all[,14:25])

breakpoints <- c(-400,-100,-10,-1,0,1,10,100,500) # for absolute difference
breakpoints <- c(-1000,-1,-0.25,-0.0001,0,0.0001,0.25,1,1000) # for comparative difference
colors <- c("red","orange","yellow","white","white","lightblue","blue","darkblue")
#for plotting one month
breakpoints <- c(0,2,5,10,50,100,200,1000,3000)
colors <- c("red","orange","yellow","lightblue","blue","darkblue","purple","black")
par(mfrow=c(1,1))
plot(Q_Mo_8018$X7,breaks=breakpoints,col=colors) #TerraClimate
plot(Q_GRUN_stack_0214$X1980.07.01*31,breaks=breakpoints,col=colors) #GRUN
## for mapping all 12 months
par(mar=c(0.5, 0.5, 2, 0.2), mfrow=c(3,4),
    oma = c(4, 2, 0.5, 0.2))
# absolute difference
plot(ET_Jan_8014-ET_GLEAM_Jan_8014,breaks=breakpoints,col=colors, main="January",legend=FALSE)
plot(ET_Feb_8014-ET_GLEAM_Feb_8014,breaks=breakpoints,col=colors, main="February",legend=FALSE)
plot(ET_Mar_8014-ET_GLEAM_Mar_8014,breaks=breakpoints,col=colors, main="March",legend=FALSE)
plot(ET_Apr_8014-ET_GLEAM_Apr_8014,breaks=breakpoints,col=colors, main="April",legend=FALSE)
plot(ET_May_8014-ET_GLEAM_May_8014,breaks=breakpoints,col=colors, main="May",legend=FALSE)
plot(ET_Jun_8014-ET_GLEAM_Jun_8014,breaks=breakpoints,col=colors, main="June",legend=FALSE)
plot(ET_Jul_8014-ET_GLEAM_Jul_8014,breaks=breakpoints,col=colors, main="July",legend=FALSE)
plot(ET_Aug_8014-ET_GLEAM_Aug_8014,breaks=breakpoints,col=colors, main="August",legend=FALSE)
plot(ET_Sep_8014-ET_GLEAM_Sep_8014,breaks=breakpoints,col=colors, main="September",legend=FALSE)
plot(ET_Oct_8014-ET_GLEAM_Oct_8014,breaks=breakpoints,col=colors, main="October",legend=FALSE)
plot(ET_Nov_8014-ET_GLEAM_Nov_8014,breaks=breakpoints,col=colors, main="November",legend=FALSE)
plot(ET_Dec_8014-ET_GLEAM_Dec_8014,breaks=breakpoints,col=colors, main="December",legend=FALSE)

#comparative difference [.]
plot(((ET_Jan_8014-ET_GLEAM_Jan_8014)/ET_Jan_8014),breaks=breakpoints,col=colors, main="January",legend=FALSE)
plot(((ET_Feb_8014-ET_GLEAM_Feb_8014)/ET_Feb_8014),breaks=breakpoints,col=colors, main="February",legend=FALSE)
plot(((ET_Mar_8014-ET_GLEAM_Mar_8014)/ET_Mar_8014),breaks=breakpoints,col=colors, main="March",legend=FALSE)
plot(((ET_Apr_8014-ET_GLEAM_Apr_8014)/ET_Apr_8014),breaks=breakpoints,col=colors, main="April",legend=FALSE)
plot(((ET_May_8014-ET_GLEAM_May_8014)/ET_May_8014),breaks=breakpoints,col=colors, main="May",legend=FALSE)
plot(((ET_Jun_8014-ET_GLEAM_Jun_8014)/ET_Jun_8014),breaks=breakpoints,col=colors, main="June",legend=FALSE)
plot(((ET_Jul_8014-ET_GLEAM_Jul_8014)/ET_Jul_8014),breaks=breakpoints,col=colors, main="July",legend=FALSE)
plot(((ET_Aug_8014-ET_GLEAM_Aug_8014)/ET_Aug_8014),breaks=breakpoints,col=colors, main="August",legend=FALSE)
plot(((ET_Sep_8014-ET_GLEAM_Sep_8014)/ET_Sep_8014),breaks=breakpoints,col=colors, main="September",legend=FALSE)
plot(((ET_Oct_8014-ET_GLEAM_Oct_8014)/ET_Oct_8014),breaks=breakpoints,col=colors, main="October",legend=FALSE)
plot(((ET_Nov_8014-ET_GLEAM_Nov_8014)/ET_Nov_8014),breaks=breakpoints,col=colors, main="November",legend=FALSE)
plot(((ET_Dec_8014-ET_GLEAM_Dec_8014)/ET_Dec_8014),breaks=breakpoints,col=colors, main="December",legend=FALSE)

library(scales)
par(mfrow=c(3,4))
plot(x=ET_TC_GLEAM$ET_Jan,y=ET_TC_GLEAM$ET_GLEAM_Jan,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Jan",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Jan~ET_TC_GLEAM$ET_Jan),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Jan~ET_TC_GLEAM$ET_Jan))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Feb,y=ET_TC_GLEAM$ET_GLEAM_Feb,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Feb",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Feb~ET_TC_GLEAM$ET_Feb),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Feb~ET_TC_GLEAM$ET_Feb))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Mar,y=ET_TC_GLEAM$ET_GLEAM_Mar,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Mar",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Mar~ET_TC_GLEAM$ET_Mar),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Mar~ET_TC_GLEAM$ET_Mar))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Apr,y=ET_TC_GLEAM$ET_GLEAM_Apr,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Apr",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Apr~ET_TC_GLEAM$ET_Apr),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Apr~ET_TC_GLEAM$ET_Apr))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_May,y=ET_TC_GLEAM$ET_GLEAM_May,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="May",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_May~ET_TC_GLEAM$ET_May),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_May~ET_TC_GLEAM$ET_May))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Jun,y=ET_TC_GLEAM$ET_GLEAM_Jun,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Jun",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Jun~ET_TC_GLEAM$ET_Jun),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Jun~ET_TC_GLEAM$ET_Jun))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Jul,y=ET_TC_GLEAM$ET_GLEAM_Jul,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Jul",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Jul~ET_TC_GLEAM$ET_Jul),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Jul~ET_TC_GLEAM$ET_Jul))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Aug,y=ET_TC_GLEAM$ET_GLEAM_Aug,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Aug",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Aug~ET_TC_GLEAM$ET_Aug),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Aug~ET_TC_GLEAM$ET_Aug))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Sep,y=ET_TC_GLEAM$ET_GLEAM_Sep,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Sep",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Sep~ET_TC_GLEAM$ET_Sep),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Sep~ET_TC_GLEAM$ET_Sep))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Oct,y=ET_TC_GLEAM$ET_GLEAM_Oct,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Oct",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Oct~ET_TC_GLEAM$ET_Oct),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Oct~ET_TC_GLEAM$ET_Oct))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Nov,y=ET_TC_GLEAM$ET_GLEAM_Nov,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Nov",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Nov~ET_TC_GLEAM$ET_Nov),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Nov~ET_TC_GLEAM$ET_Nov))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)
plot(x=ET_TC_GLEAM$ET_Dec,y=ET_TC_GLEAM$ET_GLEAM_Dec,xlab="TerraClimate ET (mm)",ylab="GLEAM ET (mm)",main="Dec",xlim=c(0,250),ylim=c(0,250), col = alpha("black", 0.4),cex=1.5)
abline(lm(ET_TC_GLEAM$ET_GLEAM_Dec~ET_TC_GLEAM$ET_Dec),col="red")
modsum<-summary(lm(ET_TC_GLEAM$ET_GLEAM_Dec~ET_TC_GLEAM$ET_Dec))
r2 = modsum$adj.r.sETuared
mylabel = bETuote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 1650, y = 50, labels = mylabel,cex=1.5)

## to make a gradient of other values but gray for zero values (!!! DID NOT WORK !!!)
# Load libraries
#library('rasterVis')
#library('RColorBrewer')
# Set color palette
#zeroCol <-"#B3B3B3" # (gray color, same as your figure example)
#reds <- rev(brewer.pal('YlOrRd', n = 7))
#blues <- brewer.pal('Blues', n = 7)
#myTheme <- rasterTheme(region = c(reds, blues)) #put zeroCol in the middle for "gray"
# Plot
#levelplot(Q_mmyear_8014-Q_GRUN_mmyear_8014, par.settings = myTheme, margin = FALSE)

# Comparing mean GRUN_Q and TerraClimate_Q
test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$WEC), mean)
test2<-as.data.frame((colMeans(test[,-1])/100))


################## Figure - CDF of mean annual ET ########################
plot(ecdf(all$ET_mmyear),xaxt="n",main="",
     xlab="Mean annual ET",ylab="Cumulative density",las=1)
abline(a=0.9338,b=0,col="grey")
abline(a=0.8671,b=0,col="grey")
abline(a=0.8004,b=0,col="grey")
abline(a=0.7337,b=0,col="grey")
abline(a=0.667,b=0,col="grey")
abline(a=0.6003,b=0,col="grey")
abline(a=0.5336,b=0,col="grey")
abline(a=0.4669,b=0,col="grey")
abline(a=0.4002,b=0,col="grey")
abline(a=0.3335,b=0,col="grey")
abline(a=0.2668,b=0,col="grey")
abline(a=0.2001,b=0,col="grey")
abline(a=0.1334,b=0,col="grey")
abline(a=0.0667,b=0,col="grey")
axis(side=1,at=c(27,93,151,202,245,285,328,371,421,476,582,770,940,1066),cex.axis=0.75)
abline(v=c(27,93,151,202,245,285,328,371,421,476,582,770,940,1066),col="grey")

###### Water budget coherence across known systems ######
which( colnames(all)=="Q_GRUN_mmyear" ) #ET_mmyear=13,Q_TC=26, Q_GRUN=27, PET_mmyear=40, P_mmyear=53, Phasediff=55
test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$WEC), cv)
k<-as.data.frame((colMeans(test[,-1])/100))

test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$KGZ), cv)
k2<-as.data.frame((colMeans(test[,-1])/100))

test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$Meybeck), cv)
k3<-as.data.frame((colMeans(test[,-1])/100))

test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$Knoben_cluster), cv)
k4<-as.data.frame((colMeans(test[,-1])/100))

test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$ETA), cv)
k5<-as.data.frame((colMeans(test[,-1])/100))

test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$ETV), cv)
k6<-as.data.frame((colMeans(test[,-1])/100))

test<-aggregate(all[, c(13,26,27,40,53,55)], list(all$ETC), cv)
k7<-as.data.frame((colMeans(test[,-1])/100))

# Notes for Coherence 20210318: 
#redo phase diff for this (or at least use data closer derived - maybe?)
#Holdridge didn't work

k_all<-cbind(k,k2,k3,k4,k5,k6,k7)
colnames(k_all)=c("WEC","KGZ","Meybeck","Knoben","ETA","ETV","ETC")
plot(k_all$WEC,type="p",col="black",ylim=c(0,3),xaxt="n",cex=2.5,pch=19,ylab="Mean(CV)")
axis(1, at=1:6, labels=c("ET_mmyear","Q_TC","Q_GRUN","PET_mmyear","P_mmyear","Phasediff"))
points(k_all$KGZ,col="red",cex=2.5,pch=19)
points(k_all$Meybeck,col="blue",cex=2.5,pch=19)
points(k_all$Knoben,col="purple",cex=2.5,pch=19)
points(k_all$ETA,col="orange3",cex=2.5,pch=19)
points(k_all$ETV,col="orange2",cex=2.5,pch=19)
points(k_all$ETC,col="yellow",cex=2.5,pch=19)
legend("topright", legend=c("WEC","KGZ","Meybeck","Knoben","ETA","ETV","ETC"),
       col=c("black","red", "blue","purple","orange3","orange2","yellow"), lty=1, cex=0.8,
       title="Classification Systems", text.font=4)
######## Tier 1 Koppen Geiger Zones ######
#Koppen-Geiger
all$KGZTier1[all$KGZ==1|all$KGZ==2|all$KGZ==3]="Tropical"
all$KGZTier1[all$KGZ==4|all$KGZ==5|all$KGZ==6|all$KGZ==7]="Arid" 
all$KGZTier1[all$KGZ==8|all$KGZ==9|all$KGZ==10|all$KGZ==11|all$KGZ==12|all$KGZ==13|all$KGZ==14|all$KGZ==16]="Temperate"
all$KGZTier1[all$KGZ==17|all$KGZ==18|all$KGZ==19|all$KGZ==20|all$KGZ==21|all$KGZ==22|all$KGZ==24|all$KGZ==25|all$KGZ==26|all$KGZ==27|all$KGZ==28]="Boreal"
all$KGZTier1[all$KGZ==29|all$KGZ==30]="Polar"
################ Budyko -- Plotting pixels in Budyko Space ####################
library(ggplot2)
library(RColorBrewer)
#### Budyko - Koppen-Geiger Zones ####
####### Budyko -- Choudhury equation fit ########
library(stats)
## global data
x=all$PET_mmyear/all$P_mmyear
y=all$ET_mmyear/all$P_mmyear
fu <- function(x,n){(1+(x)^(-n))^(-1/n)} #Fu equation (from Yu Fang - incorrect?)
fu<-function(x,n){1+(x)-(1+(x)^n)^(1/n)} #Fu equation (from paper - correct)
Choud<-function(x,n){x/((1+((x)^n))^(1/n))} #Choudhury, 1999
#plot(x,y,xlim=c(0,10),ylim=c(0,2))
#curve(fu(x,1.8),add=TRUE,col="yellow") #plot orig estimate
nfit <- nls(y~fu(x,n),start=list(n=1.8))
nfit <- nls(y~Choud(x,n),start=list(n=1.8))
coef(nfit) #to get correct n parameter value
test<-predict(nfit,data=x)
summary(lm(y~test))
plot(x,y,xlim=c(0,10),ylim=c(0,2))
curve(fu(x,2.07),add=TRUE,col="red") #plot curve with fitted parameter value, found below
## finding n value for each Tier 1 zone
nparameters_KPGTier1=data.frame(sim=1,Tier1=1,n=1,R2=1,CV_PETP=1,CV_ETP=1)[-1,]
climateclass<-na.omit(unique(all$KGZTier1))
for (i in 1:length(climateclass)){ 
  temp<-subset(all,all$KGZTier1==climateclass[i])
  class=climateclass[i]
  x<-temp$PET_mmyear/temp$P_mmyear
  y<-temp$ET_mmyear/temp$P_mmyear
  nfit <- nls(y~Choud(x,n),start=list(n=1.8))
  test<-predict(nfit,data=x)
  nparameters_KPGTier1[i,]<-cbind(i,
                                  class,
                                  coef(nfit),
                                  summary(lm(y~test))[['adj.r.squared']],
                                  cv(x)/100,
                                  cv(y)/100)#to get correct n parameter value
} 

#put colors in alphabetical order of listed Tier1 names (e.g., "arid" is first)
myColors<-c("yellow2","purple","royalblue","darkorange","forestgreen") 
names(myColors) <- levels(all$KGZTier1)
colScale <- scale_colour_manual(name = "KGZTier1",values = myColors)
x=all$PET_mmyear/all$P_mmyear
y=all$ET_mmyear/all$P_mmyear
df<-as.data.frame(cbind(x,y))
#replace n value with values found above for each Tier1 zone, Choudhury
c1 <- function(x){x/((1+((x)^2.07))^(1/2.07))} #Choud equation, global (black)
c2 <- function(x){x/((1+((x)^1.33))^(1/1.33))} #Choud equation, Polar (royalblue)
c3 <- function(x){x/((1+((x)^2.53))^(1/2.53))} #Choud equation, Boreal (purple)
c4 <- function(x){x/((1+((x)^1.28))^(1/1.28))} #Choud equation, Arid (yellow2)
c5 <- function(x){x/((1+((x)^2.39))^(1/2.39))} #Choud equation, Temperate (darkorange)
c6 <- function(x){x/((1+((x)^2.37))^(1/2.37))} #Choud equation, Tropical (forestgreen)
d <- ggplot(df,aes(x=x,y=y,col=all$KGZTier1))
d +  
  xlim(0, 10) + ylim(0,1.25) +
  geom_point(alpha = 1/10,size=2)+theme_bw()+ 
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "black",size=1)+
  geom_abline(intercept=1, slope=0, linetype="dashed", color = "black",size=1)+
  stat_function(fun=c2,col="royalblue",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=c3,col="purple",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=c4,col="yellow2",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=c5,col="darkorange",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=c6,col="forestgreen",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=c1,col="black",size=2.5,linetype="dashed",alpha=1)+
  colScale



#### Budyko - ETA zones ####
cols <- c("15" = "midnightblue", "14" = "blue4", "13" = "blue3", "12" = "dodgerblue1","11"="deepskyblue",
          "10" = "darkgreen", "9" = "forestgreen", "8" = "palegreen2", "7" = "palegreen1","6"="khaki3",
          "5" = "khaki2", "4" = "khaki1", "3" = "lightyellow2", "2" = "lightyellow","1"="snow3")
x=all$PET_mmyear/all$P_mmyear
y=all$ET_mmyear/all$P_mmyear
df<-as.data.frame(cbind(x,y))
d <- ggplot(df,aes(x=x,y=y,col=factor(all$ETA)))
d +  
  xlim(0, 5) + ylim(0,1.25) +
  geom_point(alpha = 1/2)+theme_bw()+ 
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "black",size=1)+
  geom_abline(intercept=1, slope=0, linetype="dashed", color = "black",size=1) +
  scale_colour_manual(values = cols)


############### how to find # of clusters - elbow #################
library(stats)

#scaled_data = as.matrix(scale(all$ET_mmyear_8007))
data=as.matrix(all$P_mmyear,all$PET_mmyear)

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 30
#data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=80,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters, K",
     ylab="Total within-clusters sum of squares", main="(P, PET)")
####### Complexity in evaluated systems (for data overlap) ########
library(landscapemetrics)
#make into raster layer first
all_df_NA<-as.data.frame((x=coherence_all_atts),na.rm=FALSE) #Include NA values so cell values match #cells
coordinates<-as.data.frame(coordinates(coherence_all_atts))
df_NA2<-cbind(all_df_NA,coordinates$x,coordinates$y)
joined_df<-full_join(all,df_NA2,by="ID") #with current data
map_df<-as.data.frame(cbind(joined_df$`coordinates$x`,joined_df$`coordinates$y`,joined_df$ETV.x))
colnames(map_df)=c("x","y","ETV")
dfr<-rasterFromXYZ(map_df)
plot(dfr,col=terrain.colors(30),main="ETV")
ETV_used<-dfr #imported back as "ETV_raster"

#patchiness
test<-lsm_c_np(ETV_raster)
mean(test$value)
#CV of patch area in ha
test<-lsm_c_area_cv(Meybeck_raster, directions = 8) #queen's case (connectedness directionality)
mean(test$value)
#Mean of patch area in ha
test<-lsm_c_area_mn(KPG_raster, directions = 8)
mean(test$value)
