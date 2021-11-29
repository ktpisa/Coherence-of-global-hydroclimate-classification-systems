######################## Coherence -- Making New P,PET Clustering System #########################

library(dplyr)
library(landscapemetrics)
library(raster)

stack_new<-stack(ET_mmyear_8014,ET_GLEAM_mmyear_8014,Q_mmyear_8014,Q_GRUN_mmyear_8014,
             PET_mmyear_8014,PRE_mmyear_8014,phasediff_raster, KPG_raster)
#create unique identifier
stack_new$ID=1:259200 #number of total cells, including NA
#Attributes into dataframe
stack_na<-as.data.frame((x=stack_new),na.rm=FALSE) #used for mapping
stack_df<-as.data.frame((x=stack_new),na.rm=TRUE) #used for analysis

coordinates<-as.data.frame(coordinates(stack_new)) 
stack_na_coord<-cbind(stack_na,coordinates$x,coordinates$y)

############### Finding # of clusters - elbow #################

PPET_clust_mean_cvs<-data.frame(sim_num=1,n_zones=1,ET_TC=1,ET_GLEAM=1,Q_TC=1,Q_GRUN=1,
                                PET=1,P=1)[-1,] 
PPET_clust_dtheta_mean_cvs<-data.frame(sim_num=1,n_zones=1,dtheta=1)[-1,] 
PPET_clust_complexity<-data.frame(sim_num=1,n_zones=1,mean_n_patches=1,cv_area=1)[-1,] 

#### dtheta
# mode function (R does not have mode built-in)
getMode <- function(x) {
  keys <- na.omit(unique(x))
  keys[which.max(tabulate(match(x, keys)))]
}

center=c()
cv=c()
me=c()
me.cv=c()
dev=c()
i_num=c()

#which( colnames(all)=="PET_mmyear" )

#Modified Elbow Method for finding the optimal number of clusters
set.seed(123)
k.max <- 1:30 #number of zones

for (i in 1:length(k.max)){
  k_clust<-kmeans(stack_df[,c(5,6)],nstart=80,centers=k.max[i]) #stack_df[,c(5,6)] = PET, P
  test_df<-cbind(stack_df,k_clust$cluster)
  test_cvs<-aggregate(test_df[, c(1,2,3,4,5,6,7)], list(test_df$`k_clust$cluster`), cv)
  
  k_clust_df<-as.data.frame(k_clust$cluster)
  colnames(k_clust_df)=c("WECID")
  stack_df$WEC<-k_clust_df$WECID 
  
  dt<-stack_df[stack_df$WEC == i,]  
  dt<-round(dt$phasediff/pi*6,2)  # months, and round for finding mode
  
  # centering around mode
  center[i]<-getMode(dt)
  
  # if NA values, skip (exclamation reverses)
  if (!is.na(center[i])){
    
    # mode far from zero: center the positive values
    if (abs(center[i])>=3) {
      # centering around max value
      neg.correct<-dt[dt<0]+12  # no negative values
      pos.values<-c(neg.correct,dt[dt>=0]) # merge positive with negative corrected
      dt.center<-pos.values+(6-mean(pos.values))
      # record actual mean, bounded by -6, 6
      if (mean(pos.values)>6) {
        me[i]<-mean(pos.values)-12
      } else {
        me[i]<-mean(pos.values) 
      }
    } else {
      # mode near zero: center without making positive
      me[i]<-mean(dt)
      dt.center<-dt+(6-mean(dt))
    }
    
    # CV is computed based on consistently centered values
    # mean of dt.center shoould be 6  
    me.cv[i]<-mean(dt.center)
    dev[i]<-sd(dt.center)  
    cv[i]<-dev[i]/me.cv[i]
    i_num[i]<-i
    
  } # end of if not NAdt<-stack_final_df[stack_final_df$WEC == i,]  
  
  # make raster for landscapemetrics(), patches and area
  df<-full_join(stack_df,stack_na_coord,by="ID")
  df2<-as.data.frame(cbind(df$`coordinates$x`,df$`coordinates$y`,df$WEC)) #the variable you want to map
  colnames(df2)=c("x","y","var")
  raster<-rasterFromXYZ(df2)
  
  #number of patches per zone
  n_patches<-lsm_c_np(raster)
  
  # area of each zone in km2
  test<-tapply(area(raster), raster[], sum)
  
  PPET_clust_mean_cvs[i,]<-c(i,
                             k.max[i],
                             mean(test_cvs$ET_mmyear),
                             mean(test_cvs$ET_GLEAM_mmyear),
                             mean(test_cvs$Q_mmyear),
                             mean(test_cvs$Q_GRUN_mmyear),
                             mean(test_cvs$PET_mmyear),
                             mean(test_cvs$P_mmyear))
  PPET_clust_complexity[i,]<-c(i,
                             k.max[i],
                             mean(n_patches$value),
                             cv(test)/100)
  PPET_clust_dtheta_mean_cvs[i,]<-c(i,
                               k.max[i],
                               mean(cv))                           
                             
                             
  }

########### Elbow Plots Compared to Koppen Geiger Value ############
#plot(x=1:30,y=PPET_clust_mean_cvs$ET_TC,ylim=c(30,90),main="ET",type="b",pch=19,
    # col="black",cex=2,ylab="Mean CV",xlab="Number of zones") #This is in %, not [.]
#test<-lm(PPET_clust_mean_cvs$ET_TC~poly(PPET_clust_mean_cvs$n_zones,3))
#lines(test$fitted.values, col="black",lwd=3)
#abline(h=(32*1.5),col="black",lty=2,lwd=3)

#points(PPET_clust_mean_cvs$ET_GLEAM,col="forestgreen", cex=2, type="b")
#test<-lm(PPET_clust_mean_cvs$ET_GLEAM~poly(PPET_clust_mean_cvs$n_zones,3))
#lines(test$fitted.values,lwd=3,col="forestgreen")
#abline(h=(37*1.5),col="forestgreen",lty=2,lwd=3)

## Koppen-Geiger values - based on new stack below
test<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$KGZ), cv)
k2<-as.data.frame((colMeans(test[,-1])/100)) #KPG mean
k3<-apply(((test[,-1])/100),2,sd) #KPG sd

sd1<-28-21
sd2<-28+21
breakpoints<-c(0,28,49,100) #ET_TC
ggplot() +
  geom_point(aes(x = n_zones,y = ET_TC, color=cut(ET_TC,breaks=breakpoints)),data=PPET_clust_mean_cvs,size=7) +
  #geom_point(aes(x = n_zones,y = ET_GLEAM, colour = cut(ET_GLEAM,breaks=breakpoints2)),data=PPET_clust_mean_cvs,size=5,shape = 17) +
 scale_color_manual(values=c("cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean CV')+
  geom_hline(yintercept=c(28),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")

sd<-36-31
sd2<-36+31
breakpoints2<-c(0,36,67,100) #ET_GLEAM
ggplot() +
  #geom_point(aes(x = n_zones,y = ET_TC, color=cut(ET_TC,breaks=breakpoints)),data=PPET_clust_mean_cvs,size=5) +
  geom_point(aes(x = n_zones,y = ET_GLEAM, color = cut(ET_GLEAM,breaks=breakpoints2)),
             data=PPET_clust_mean_cvs,size=7,shape = 17) +
  scale_color_manual(values=c("cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean CV')+
  geom_hline(yintercept=c(36),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")


breakpoints<-c(0,86,116,170) #TC
breakpoints2<-c(0,73,109,170) #GRUN
sd<-86+30
sd<-73+36

ggplot() +
  geom_point(aes(x = n_zones,y = Q_TC, color=cut(Q_TC,breaks=breakpoints)),data=PPET_clust_mean_cvs,size=7) +
  #geom_point(aes(x = n_zones,y = Q_GRUN, colour = cut(Q_GRUN,breaks=breakpoints2)),data=PPET_clust_mean_cvs,size=5,shape = 17) +
  scale_color_manual(values=c("cadetblue4","cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean CV')+
  geom_hline(yintercept=c(86),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")


ggplot() +
  #geom_point(aes(x = n_zones,y = Q_TC, color=cut(Q_TC,breaks=breakpoints)),data=PPET_clust_mean_cvs,size=5) +
  geom_point(aes(x = n_zones,y = Q_GRUN, colour = cut(Q_GRUN,breaks=breakpoints2)),data=PPET_clust_mean_cvs,size=7,shape = 17) +
  scale_color_manual(values=c("cadetblue4","cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean CV')+
  geom_hline(yintercept=c(73),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")

breakpoints<-c(0,20,33,170) #PET
breakpoints2<-c(0,37,55,170) #P
sd<-20+13
sd<-37+18

ggplot() +
  geom_point(aes(x = n_zones,y = PET, color=cut(PET,breaks=breakpoints)),data=PPET_clust_mean_cvs,size=7) +
  #geom_point(aes(x = n_zones,y = P, colour = cut(P,breaks=breakpoints2)),data=PPET_clust_mean_cvs,size=5,shape = 17) +
  scale_color_manual(values=c("cadetblue4","cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean CV')+
  geom_hline(yintercept=c(20),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")

ggplot() +
  #geom_point(aes(x = n_zones,y = PET, color=cut(PET,breaks=breakpoints)),data=PPET_clust_mean_cvs,size=5) +
  geom_point(aes(x = n_zones,y = P, colour = cut(P,breaks=breakpoints2)),data=PPET_clust_mean_cvs,size=7) +
  scale_color_manual(values=c("cadetblue4","cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean CV')+
  geom_hline(yintercept=c(37),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")


breakpoints<-c(0,0.16,0.26,1.70) #phasediff, based on "water budget coherence for dtheta" below, "KPG_dtheta_cv"
sd<-16+10

ggplot() +
  geom_point(aes(x = n_zones,y = dtheta, color=cut(dtheta,breaks=breakpoints)),data=PPET_clust_dtheta_mean_cvs,size=7) +
  scale_color_manual(values=c("cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean CV')+
  geom_hline(yintercept=c(0.16),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")

breakpoints<-c(0,46,82,170) #mean n.patches
sd<-46+36

ggplot() +
  geom_point(aes(x = n_zones,y = mean_n_patches,
                 color=cut(mean_n_patches,breaks=breakpoints)),data=PPET_clust_complexity,size=5) +
  scale_color_manual(values=c("cadetblue3","azure3"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'Mean number of patches')+
  geom_hline(yintercept=c(46),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")
  #theme_minimal()

breakpoints<-c(0,1.20,170) #cv(area), area in km2
ggplot() +
  geom_point(aes(x = n_zones,y = cv_area,
                 color=cut(cv_area,breaks=breakpoints)),data=PPET_clust_complexity,size=5) +
  scale_color_manual(values=c("cadetblue4"))+
  xlab(label = 'Number of Zones') +
  ylab(label = 'CV area')+
  geom_hline(yintercept=c(1.20),linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")

#### Plots not used ! ####
#plot(x=1:30,y=PPET_clust_mean_cvs$Q_TC,ylim=c(60,170),main="Q",type="b",
 #    col="darkblue", cex=2,ylab="Mean CV",xlab="Number of zones")
#test<-lm(PPET_clust_mean_cvs$Q_TC~poly(PPET_clust_mean_cvs$n_zones,3))
#lines(test$fitted.values,lwd=3,col="darkblue")
#abline(h=(90*1.5),col="darkblue",lty=2,lwd=3)

#points(PPET_clust_mean_cvs$Q_GRUN,col="blue", cex=2)
#test<-lm(PPET_clust_mean_cvs$Q_GRUN~poly(PPET_clust_mean_cvs$n_zones,3))
#lines(test$fitted.values,lwd=3, col="blue")
#abline(h=74,col="blue",lty=2,lwd=3)

#plot(x=1:30,y=PPET_clust_mean_cvs$PET,main="P, PET",
 #    col="orange3", cex=2, ylim=c(0,100),ylab="Mean CV",xlab="Number of zones")
#test<-lm(PPET_clust_mean_cvs$PET~poly(PPET_clust_mean_cvs$n_zones,3))
#lines(test$fitted.values,lwd=3, col="orange3")
#abline(h=19,col="orange3",lty=2,lwd=3)

#points(PPET_clust_mean_cvs$P,col="orange1", cex=2)
#test<-lm(PPET_clust_mean_cvs$P~poly(PPET_clust_mean_cvs$n_zones,3))
#lines(test$fitted.values,lwd=3, col="orange1")
#abline(h=37,col="orange1",lty=2,lwd=3)

#plot(x=1:30,y=PPET_clust_mean_cvs$phasediff,col="red", cex=2,ylim=c(40,80))
#test<-lm(PPET_clust_mean_cvs$phasediff~poly(PPET_clust_mean_cvs$n_zones,3))
#lines(test$fitted.values,lwd=3, col="red")
#abline(h=43,col="red",lty=2,lwd=3)

## Complexity
#plot(x=1:30,y=PPET_clust_complexity$mean_n_patches, ylim=c(40,130),main="mean n_patches",cex=2)
#test<-lm(PPET_clust_complexity$mean_n_patches~poly(PPET_clust_complexity$n_zones,3))
#lines(test$fitted.values,lwd=3,col="black")
#abline(h=46,col="black",lty=2,lwd=3)

#plot(y=PPET_clust_complexity$cv_area[-1],x=2:30,ylim=c(0,1.3),cex=2,main="cv_area") #remove first value, because CV of one zone = NA
#test<-lm(PPET_clust_complexity$cv_area[-1]~poly(PPET_clust_complexity$n_zones[-1],3))
#lines(test$fitted.values,lwd=3,col="black")
#abline(h=1.20,col="black",lty=2,lwd=3)

###### Identifying Optimal Number of Clusters -- not used! ######

library(pspline)
plot(predict(sm.spline(PPET_clust_complexity$n_zones[-1], PPET_clust_complexity$cv_area[-1]), PPET_clust_complexity$n_zones[-1], 1))

plot(diff(PPET_clust_complexity$cv_area)/diff(PPET_clust_complexity$n_zones))

########## Stacking proposed zones using framework described below #######
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
library(raster)
ETC_raster<-raster('ETC_raster.nc')
ETV_raster<-raster('ETV_raster.nc')
WEC_raster<-raster('WEC_raster.nc')
ETA_raster<-raster('ETA_raster.nc')
KPG_raster<-raster('KPG_raster.nc') #used in this study, excludes Anarctica

stack_new2<-stack(stack_new,KPG_raster)
stack_new2_df<-as.data.frame(stack_new2,na.rm=TRUE)

#### Comparing to Koppen Geiger
KPG_waterbudget_cvs<-aggregate(stack_new2_df[, c(1,2,3,4,5,6,7)], list(stack_new2_df$KGZ), cv)
test2<-as.data.frame((colMeans(KPG_waterbudget_cvs[,-1])/100))

#number of patches per zone
n_patches<-lsm_c_np(KPG_raster)
mean(n_patches$value) #46

# area of each zone in km2
test<-tapply(area(KPG_raster), KPG_raster[], sum)
cv(test) #1.20

##### New WEC zoning - n =15 and 19 zones #######

WEC_clust<-kmeans(stack_df[,c(5,6)],nstart=80,centers=15) #stack_df[,c(5,6)] = PET, P
WEC_clust20<-kmeans(stack_df[,c(5,6)],nstart=80,centers=20) #stack_df[,c(5,6)] = PET, P
test_df<-cbind(stack_df,WEC_clust$cluster)
test_df20<-cbind(stack_df,WEC_clust20$cluster)
WEC_cvs<-aggregate(test_df[, c(1,2,3,4,5,6,7)], list(test_df$`WEC_clust$cluster`), cv)
WEC_mean_cvs<-as.data.frame((colMeans(test_cvs)/100))

k_clust_df<-as.data.frame(WEC_clust$cluster)
k_clust_df20<-as.data.frame(WEC_clust20$cluster)
colnames(k_clust_df)=c("WECID")
colnames(k_clust_df20)=c("WECID")
stack_df$WEC<-k_clust_df$WECID
stack_df$WEC20<-k_clust_df20$WECID 

df<-full_join(stack_df,stack_na_coord,by="ID")
df2<-as.data.frame(cbind(df$`coordinates$x`,df$`coordinates$y`,df$WEC20)) #the variable you want to map
colnames(df2)=c("x","y","WEC20")
raster<-rasterFromXYZ(df2)
plot(raster)

# export raster
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
# write raster to netcdf 
if (require(ncdf4)) {	
  wr <- writeRaster(raster, filename='WEC20_20210503.nc', format="CDF", overwrite=TRUE)   
}

###### WEC raster 20210429 and 20210503 - read in, derived from above ######
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
#15 zones
WEC_raster_20210429<-raster('WEC_20210429.nc')
names(WEC_raster_20210429)=c("WEC")
#20 zones
WEC_raster_20210503<-raster('WEC20_20210503.nc')
names(WEC_raster_20210503)=c("WEC20")

library(sf)
pp_sf<-st_as_sf(pp)

### make sure to change these values and colors to reflect aridity index
#breakpoints <- c(0,3,5,7,10,12,14,16) #if -0.5<dtheta<0.5 then this is <1 mo diff, so "in phase"
#mycolors <- c("palevioletred3","palevioletred1","mistyrose1","mintcream","mintcream","lightsteelblue1","lightsteelblue","navy")
#rasdf<-as.data.frame(raster,xy=TRUE)%>%drop_na()

#pp <- rasterToPolygons(raster, dissolve=TRUE)
## Convert SpatialPolygons to a format usable by ggplot2
#outline <- fortify(pp)

## Put it all together:
#ggplot(rasdf, aes(x, y)) +
 # geom_raster(aes(fill = cut(WEC,breaks=breakpoints))) +
#  scale_fill_manual(values = mycolors) +
 # coord_fixed() +
#  geom_path(aes(x = long, y = lat, group = group), data = outline, 
 #           size=0.5, col="black")+
#theme(plot.background = element_rect(fill = "white"),
 #     panel.background = element_rect(fill = "white"))

##### Stack again for analysis #####
stack_final<-stack(ET_mmyear_8014,ET_GLEAM_mmyear_8014,
                   Q_mmyear_8014,Q_GRUN_mmyear_8014,
                   PET_mmyear_8014,PRE_mmyear_8014,phasediff_raster,
                   WEC_raster_20210429,WEC_raster_20210503,KPG_raster,ETV_raster,ETC_raster,
                   ETA_raster,Holdridge_zones,Meybeck_zones,Knoben_cluster_raster)

#create unique identifier
stack_final$ID=1:259200 #number of total cells, including NA
#Attributes into dataframe
stack__final_na<-as.data.frame((x=stack_final),na.rm=FALSE) #used for mapping
stack_final_df<-as.data.frame((x=stack_final),na.rm=TRUE) #used for analysis
##### Meybeck re-naming #####
unique(stack_final_df$Meybeck)
stack_final_df$Meybeck2[stack_final_df$Meybeck==11]=1
stack_final_df$Meybeck2[stack_final_df$Meybeck==0]=0
stack_final_df$Meybeck2[stack_final_df$Meybeck==31]=2
stack_final_df$Meybeck2[stack_final_df$Meybeck==512]=3
stack_final_df$Meybeck2[stack_final_df$Meybeck==511]=4
stack_final_df$Meybeck2[stack_final_df$Meybeck==32]=5
stack_final_df$Meybeck2[stack_final_df$Meybeck==12]=6
stack_final_df$Meybeck2[stack_final_df$Meybeck==532]=7
stack_final_df$Meybeck2[stack_final_df$Meybeck==52]=8
stack_final_df$Meybeck2[stack_final_df$Meybeck==13]=9
stack_final_df$Meybeck2[stack_final_df$Meybeck==531]=10
stack_final_df$Meybeck2[stack_final_df$Meybeck==14]=11
stack_final_df$Meybeck2[stack_final_df$Meybeck==43]=12
stack_final_df$Meybeck2[stack_final_df$Meybeck==54]=13
stack_final_df$Meybeck2[stack_final_df$Meybeck==44]=14
stack_final_df$Meybeck2[stack_final_df$Meybeck==55]=15
stack_final_df$Meybeck2[stack_final_df$Meybeck==25]=16
stack_final_df$Meybeck2[stack_final_df$Meybeck==45]=17
stack_final_df$Meybeck2[stack_final_df$Meybeck==46]=18
stack_final_df$Meybeck2[stack_final_df$Meybeck==26]=19
stack_final_df$Meybeck2[stack_final_df$Meybeck==27]=20
stack_final_df$Meybeck2[stack_final_df$Meybeck==66]=21
stack_final_df$Meybeck2[stack_final_df$Meybeck==47]=22
stack_final_df$Meybeck2[stack_final_df$Meybeck==67]=23
stack_final_df$Meybeck2[stack_final_df$Meybeck==28]=24
stack_final_df$Meybeck2[stack_final_df$Meybeck==68]=25
stack_final_df$Meybeck2[stack_final_df$Meybeck==48]=26


##### Group WEC zones by aridity index and map (as proposed in Coherence paper) ######
# two ways of doing this, first way:
WEC_pet_p<-data.frame(simulation=1,class=1,P_avg=1,PET_avg=1,PET_P=1)[-1,]

climateclass<-unique(stack_final_df$WEC)

for (i in 1:length(climateclass)){ 
  temp<-subset(stack_final_df,stack_final_df$WEC==climateclass[i])
  class<-climateclass[i]
  WEC_pet_p[i,]<-cbind(i, class, mean(temp$P_mmyear),mean(temp$PET_mmyear),
                      # (mean((temp$PET_mmyear/temp$P_mmyear), na.rm=TRUE)))
                       (mean(temp$PET_mmyear,na.rm=TRUE)/mean(temp$P_mmyear, na.rm=TRUE)))
  
}

# label zones by increasing aridity index (PET/P, more arid)
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==1]=1 #zone numbers were arbitrary cluster numbers before
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==4]=2
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==15]=3
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==7]=4
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==13]=5
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==9]=6
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==5]=7
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==12]=8
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==11]=9
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==3]=10
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==10]=11
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==2]=12
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==14]=13
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==8]=14
stack_final_df$WEC_ranked_zone[stack_final_df$WEC==6]=15

stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==1]="superhumid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==2]="superhumid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==3]="superhumid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==4]="humid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==5]="humid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==6]="humid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==7]="temperate"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==8]="temperate"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==9]="temperate"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==10]="arid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==11]="arid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==12]="arid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==13]="hyperarid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==14]="hyperarid"
stack_final_df$WEC15_Tier1[stack_final_df$WEC_ranked_zone==15]="hyperarid"

k<-sort(WEC_pet_p$PET_P,decreasing=FALSE)
mean(k[13:15])

## mapping WEC15 with appropriately ranked zones (increasing mean aridity index)
library(dplyr)
WEC_raster_20210429$ID=1:259200 #add ID to raster, number of total cells, including NA
WEC15map<-as.data.frame(WEC_raster_20210429)
WEC15map$WEC_ranked_zone[WEC15map$WEC==1]=1 #zone numbers were arbitrary cluster numbers before
WEC15map$WEC_ranked_zone[WEC15map$WEC==4]=2
WEC15map$WEC_ranked_zone[WEC15map$WEC==15]=3
WEC15map$WEC_ranked_zone[WEC15map$WEC==7]=4
WEC15map$WEC_ranked_zone[WEC15map$WEC==13]=5
WEC15map$WEC_ranked_zone[WEC15map$WEC==9]=6
WEC15map$WEC_ranked_zone[WEC15map$WEC==5]=7
WEC15map$WEC_ranked_zone[WEC15map$WEC==12]=8
WEC15map$WEC_ranked_zone[WEC15map$WEC==11]=9
WEC15map$WEC_ranked_zone[WEC15map$WEC==3]=10
WEC15map$WEC_ranked_zone[WEC15map$WEC==10]=11
WEC15map$WEC_ranked_zone[WEC15map$WEC==2]=12
WEC15map$WEC_ranked_zone[WEC15map$WEC==14]=13
WEC15map$WEC_ranked_zone[WEC15map$WEC==8]=14
WEC15map$WEC_ranked_zone[WEC15map$WEC==6]=15

coordinates<-as.data.frame(coordinates(WEC_raster_20210429)) #"all_stack" is the raster stack for phasediff analysis
WEC15_na_coord<-cbind(WEC15map,coordinates$x,coordinates$y)
df<-full_join(WEC15map,WEC15_na_coord,by="ID")
df2<-as.data.frame(cbind(df$`coordinates$x`,df$`coordinates$y`,df$WEC_ranked_zone.x)) #the variable you want to map
colnames(df2)=c("x","y","WEC15")
raster<-rasterFromXYZ(df2)
breakpoints <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) #if -0.5<dtheta<0.5 then this is <1 mo diff, so "in phase"
colors <- c("midnightblue","blue4","blue2", #Superhumid 
            "deepskyblue4","cyan4","cyan2", #humid
            "lightblue3","lightblue2","lightblue1", #temperate
            "navajowhite1","navajowhite2","navajowhite3",#arid
            "salmon","salmon3","salmon4") #hyperarid
poly<-rasterToPolygons(raster,n=16,dissolve=TRUE)
plot(raster,breaks=breakpoints,col=colors) 
plot(poly,add=TRUE) 

# export raster
setwd("C:/Users/katie.pisarello/Desktop/Univeristy of Florida/Hydroclimate Coherence/Data")
# write raster to netcdf 
if (require(ncdf4)) {	
  wr <- writeRaster(raster, filename='WEC15_20210809.nc', format="CDF", overwrite=TRUE)   
}

#### Budyko - WEC Zones ####
####### Budyko -- Fu equation fit ########
library(stats)
## global data
x=stack_final_df$PET_mmyear/stack_final_df$P_mmyear
y=stack_final_df$ET_mmyear/stack_final_df$P_mmyear
#fu <- function(x,n){(1+(x)^(-n))^(-1/n)} #Fu equation
#fu<-function(x,n){1+(x)-(1+(x)^n)^(1/n)} #Fu equation
Choud<-function(x,n){x/((1+((x)^n))^(1/n))} #Choudhury, 1999
#plot(x,y,xlim=c(0,10),ylim=c(0,2))
#curve(fu(x,1.8),add=TRUE,col="yellow") #plot orig estimate
#nfit <- nls(y~fu(x,n),start=list(n=1.8))
nfit <- nls(y~Choud(x,n),start=list(n=1.8))
coef(nfit) #to get correct n parameter value
test<-predict(nfit,data=x)
summary(lm(y~test))
plot(x,y,xlim=c(0,10),ylim=c(0,2))
curve(Choud(x,2.07),add=TRUE,col="red") #plot curve with fitted parameter value, found below
## finding n value for each Tier 1 zone
nparameters_WEC15Tier1=data.frame(sim=1,Tier1=1,n=1,R2=1,CV_PETP=1,CV_ETP=1)[-1,]
climateclass<-na.omit(unique(stack_final_df$WEC15_Tier1))
for (i in 1:length(climateclass)){ 
  temp<-subset(stack_final_df,stack_final_df$WEC15_Tier1==climateclass[i])
  class=climateclass[i]
  x<-temp$PET_mmyear/temp$P_mmyear
  y<-temp$ET_mmyear/temp$P_mmyear
  nfit <- nls(y~Choud(x,n),start=list(n=1.8))
  test<-predict(nfit,data=x)
  nparameters_WEC15Tier1[i,]<-cbind(i,
                                  class,
                                  coef(nfit),
                                  summary(lm(y~test))[['adj.r.squared']],
                                  cv(x)/100,
                                  cv(y)/100)#to get correct n parameter value
} 

k<-ks.test(as.numeric(nparameters_KPGTier1$R2), 
        as.numeric(nparameters_WEC15Tier1$R2))
k
k<-t.test(as.numeric(nparameters_KPGTier1$R2), 
          as.numeric(nparameters_WEC15Tier1$R2)) #sig p value = different means (i.e., true diff in means =/=0)
k

##### Plotting WEC in Budyko space #######
library(ggplot2)
library(RColorBrewer)
#put colors in alphabetical order of listed Tier1 names (e.g., "arid" is first)
#WEC = 4arid, 2humid, 5hyperarid, 1superhumid, 3temperate
myColors<-c("lightsalmon","deepskyblue3","indianred3","midnightblue","lightblue1") 
names(myColors) <- levels(stack_final_df$WEC15_Tier1)
colScale <- scale_colour_manual(name = "WEC15_Tier1",values = myColors)
x=stack_final_df$PET_mmyear/stack_final_df$P_mmyear
y=stack_final_df$ET_mmyear/stack_final_df$P_mmyear
df<-as.data.frame(cbind(x,y))
#replace n value with values found above for each Tier1 zone
ChoudGlob <- function(x){x/((1+((x)^2.07))^(1/2.07))} #Choudhury equation, global (black)
Choud1 <- function(x){x/((1+((x)^2.50))^(1/2.50))} #Choudhury equation
Choud2 <- function(x){x/((1+((x)^2.37))^(1/2.37))} #Choudhury equation
Choud3 <- function(x){x/((1+((x)^2.28))^(1/2.28))} #Choudhury equation
Choud4 <- function(x){x/((1+((x)^1.91))^(1/1.91))} #Choudhury equation
Choud5 <- function(x){x/((1+((x)^1.16))^(1/1.16))} #Choudhury equation
d <- ggplot(df,aes(x=x,y=y,col=stack_final_df$WEC15_Tier1))
d +  
  xlim(0, 10) + ylim(0,1.25) +
  geom_point(alpha = 1/10,size=2)+theme_bw()+ 
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "black",size=1)+
  geom_abline(intercept=1, slope=0, linetype="dashed", color = "black",size=1)+
  stat_function(fun=Choud1,col="midnightblue",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=Choud2,col="deepskyblue3",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=Choud3,col="lightblue1",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=Choud4,col="lightsalmon",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=Choud5,col="indianred3",size=2.5,linetype="dashed",alpha=1)+
  stat_function(fun=ChoudGlob,col="black",size=2.5,linetype="dashed",alpha=1)+
  colScale

#### second way - not used!! ####
#figure out column number of given variable name
#which( colnames(stack_final_df)=="P_mmyear" )
#test<-aggregate(stack_final_df[, c(13,26,43)], list(stack_final_df$WEC), mean)

#which( colnames(stack_final_df)=="crop" )
#test<-aggregate(stack_final_df[, c(28,30,31,32,33)], list(stack_final_df$KGZ), cv)
#test2<-aggregate(stack_final_df[, c(28,30,31,32,33)], list(stack_final_df$ETA), cv)
#test3<-aggregate(stack_final_df[, c(28,30,31,32,33)], list(stack_final_df$WEC), cv)
#KGZ_lc<-colMeans(test[-1],na.rm=TRUE)
#ETA_lc<-colMeans(test2[-1],na.rm=TRUE)
#WEC_lc<-colMeans(test3[-1],na.rm=TRUE)
#plot(KGZ_lc/100,col="red",ylim=c(0,10))
#points(ETA_lc/100,col="darkgreen")
#points(WEC_lc/100,col="blue")

###### Water budget coherence ######
#### Phase Difference - centering dtheta, in months, for means of dtheta (used only informationally, 
####     for finding cv = sd/mean) #####

# mode function (R does not have mode built-in)
getMode <- function(x) {
  keys <- na.omit(unique(x))
  keys[which.max(tabulate(match(x, keys)))]
}

# number of zones
z <- 1:20

center=c()
cv=c()
me=c()
me.cv=c()
dev=c()
i_num=c()

for (i in 1:length(z)) {
  dt<-stack_final_df[stack_final_df$WEC20 == i,]  
  dt<-round(dt$phasediff/pi*6,2)  # months, and round for finding mode
  
  # centering around mode
  center[i]<-getMode(dt)
  
  # if NA values, skip (exclamation reverses)
  if (!is.na(center[i])){
    
    # mode far from zero: center the positive values
    if (abs(center[i])>=3) {
      # centering around max value
      neg.correct<-dt[dt<0]+12  # no negative values
      pos.values<-c(neg.correct,dt[dt>=0]) # merge positive with negative corrected
      dt.center<-pos.values+(6-mean(pos.values))
      # record actual mean, bounded by -6, 6
      if (mean(pos.values)>6) {
        me[i]<-mean(pos.values)-12
      } else {
        me[i]<-mean(pos.values)  
      }
    } else {
      # mode near zero: center without making positive
      me[i]<-mean(dt)
      dt.center<-dt+(6-mean(dt))
    }
    
    # CV is computed based on consistently centered values
    # mean of dt.center should be 6  
    me.cv[i]<-mean(dt.center)
    dev[i]<-sd(dt.center)  
    cv[i]<-dev[i]/me.cv[i]
    i_num[i]<-i
    
  } # end of if not NA
  
}  # end of for loop

# record actual mean, bounded by -6, 6
if (mean(pos.values)>6) {
  me[i]<-mean(pos.values)-12
} else {
  me[i]<-mean(pos.values) 
}

WEC_dtheta_cv<-as.data.frame(cbind(1:15,cv))
colnames(WEC_dtheta_cv)=c("n_zones","cv")

WEC20_dtheta_cv<-as.data.frame(cbind(1:20,cv))
colnames(WEC_dtheta_cv)=c("n_zones","cv")

KPG_dtheta_cv<-as.data.frame(cbind(1:30,cv))
colnames(KPG_dtheta_cv)=c("n_zones","cv")

Holdridge_dtheta_cv<-as.data.frame(cbind(1:38,cv))
colnames(Holdridge_dtheta_cv)=c("n_zones","cv")

Knoben_dtheta_cv<-as.data.frame(cbind(1:18,cv))
colnames(Knoben_dtheta_cv)=c("n_zones","cv")

Meybeck_dtheta_cv<-as.data.frame(cbind(1:26,cv))
colnames(Meybeck_dtheta_cv)=c("n_zones","cv")

ETV_dtheta_cv<-as.data.frame(cbind(1:29,cv))
colnames(ETV_dtheta_cv)=c("n_zones","cv")

ETA_dtheta_cv<-as.data.frame(cbind(1:15,cv))
colnames(ETA_dtheta_cv)=c("n_zones","cv")

ETC_dtheta_cv<-as.data.frame(cbind(1:20,cv))
colnames(ETC_dtheta_cv)=c("n_zones","cv")

######## Coherence: Dtheta boxplots - dataframe of unequal row length ############
max_length <- max(c(length(WEC_dtheta_cv$cv), 
                    #length(WEC20_dtheta_cv$cv),
                    length(KPG_dtheta_cv$cv),
                    length(Holdridge_dtheta_cv$cv),length(Knoben_dtheta_cv$cv),length(Meybeck_dtheta_cv$cv),
                    length(ETV_dtheta_cv$cv),length(ETA_dtheta_cv$cv),length(ETC_dtheta_cv$cv)))
data <- data.frame(col1 = c(WEC_dtheta_cv$cv,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(WEC_dtheta_cv$cv))),
                   #col2 = c(WEC20_dtheta_cv$cv,
                    #        rep(NA, max_length - length(WEC20_dtheta_cv$cv))),
                   col2 = c(KPG_dtheta_cv$cv,
                            rep(NA, max_length - length(KPG_dtheta_cv$cv))),
                   col3 = c(Holdridge_dtheta_cv$cv,
                            rep(NA, max_length - length(Holdridge_dtheta_cv$cv))),
                   col4 = c(Knoben_dtheta_cv$cv,
                            rep(NA, max_length - length(Knoben_dtheta_cv$cv))),
                   col5 = c(Meybeck_dtheta_cv$cv,
                            rep(NA, max_length - length(Meybeck_dtheta_cv$cv))),
                   col6 = c(ETV_dtheta_cv$cv,
                            rep(NA, max_length - length(ETV_dtheta_cv$cv))),
                   col7 = c(ETA_dtheta_cv$cv,
                            rep(NA, max_length - length(ETA_dtheta_cv$cv))),
                   col8 = c(ETC_dtheta_cv$cv,
                            rep(NA, max_length - length(ETC_dtheta_cv$cv))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Knoben","Meybeck","ETV","ETA","ETC")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=TRUE,main="dt",
        col=c("lightgray","lightgray","lightgray","lightgray",
              #"cornsilk2",
              "cornsilk2","lightgray","lightgray","darkgoldenrod1"))
#k-s test across matrix using WEC15 (col 1) as a reference
r <- data[,1]
res <- sapply(data[,2:8], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res

####### Plotting overall mean coherence values for each system ###########
which( colnames(stack_final_df)=="Q_GRUN_mmyear" ) 
# from above "which" line, ET_TC=1, ET_GLEAM=2, Q_TC=3, Q_GRUN=4, PET=5,P=6 
test<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$WEC), cv)
k<-as.data.frame((colMeans(test[,-1])/100))

test2<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$WEC20), cv)
k2<-as.data.frame((colMeans(test2[,-1])/100))

test3<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$KGZ), cv)
k3<-as.data.frame((colMeans(test3[,-1])/100))

test4<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$Holdridge), cv)
k4<-as.data.frame((colMeans(test4[,-1])/100))

test5<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$Meybeck2), cv)
k5<-as.data.frame((colMeans(test5[,-1])/100))

test6<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$Knoben_cluster), cv)
k6<-as.data.frame((colMeans(test6[,-1])/100))

test7<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$ETA), cv)
k7<-as.data.frame((colMeans(test7[,-1])/100))

test8<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$ETC), cv)
k8<-as.data.frame((colMeans(test8[,-1])/100))

test9<-aggregate(stack_final_df[, c(1,2,3,4,5,6)], list(stack_final_df$ETV), cv)
k9<-as.data.frame((colMeans(test9[,-1])/100))

k_all<-cbind(k,k2,k3,k4,k5,k6,k7,k8,k9)
colnames(k_all)=c("WEC","WEC20","KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV")
plot(k_all$WEC,type="p",col="black",ylim=c(0,3),xaxt="n",cex=2.5,pch=19,ylab="Mean(CV)")
axis(1, at=1:6, labels=c("ET_mmyear","ET_GLEAM","Q_TC","Q_GRUN","PET_mmyear","P_mmyear"))
points(k_all$WEC20,col="green",cex=2.5,pch=19)
points(k_all$KGZ,col="red",cex=2.5,pch=19)
points(k_all$Meybeck,col="blue",cex=2.5,pch=19)
points(k_all$Knoben,col="purple",cex=2.5,pch=19)
points(k_all$ETA,col="orange3",cex=2.5,pch=19)
points(k_all$ETV,col="orange2",cex=2.5,pch=19)
points(k_all$ETC,col="yellow",cex=2.5,pch=19)
legend("topright", legend=c("WEC","WEC20","KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV"),
       col=c("black","green","red", "blue","purple","orange3","orange2","yellow"), lty=1, cex=0.8,
       title="Classification Systems", text.font=4)

#########  Coherence Boxplots - need dataframe of unequal row length ###########
max_length <- max(c(length(test$ET_mmyear), 
                    #length(test2$ET_mmyear), 
                    length(test3$ET_mmyear),
                    length(test4$ET_mmyear), length(test5$ET_mmyear), length(test6$ET_mmyear),
                    length(test7$ET_mmyear), length(test8$ET_mmyear), length(test9$ET_mmyear))) 
data <- data.frame(col1 = c(test$ET_mmyear,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(test$ET_mmyear))),
                 #  col2 = c(test2$ET_mmyear,
                  #          rep(NA, max_length - length(test2$ET_mmyear))),
                   col2 = c(test3$ET_mmyear,
                            rep(NA, max_length - length(test3$ET_mmyear))),
                   col3 = c(test4$ET_mmyear,
                            rep(NA, max_length - length(test4$ET_mmyear))),
                   col4 = c(test5$ET_mmyear,
                            rep(NA, max_length - length(test5$ET_mmyear))),
                   col5 = c(test6$ET_mmyear,
                            rep(NA, max_length - length(test6$ET_mmyear))),
                   col6 = c(test7$ET_mmyear,
                            rep(NA, max_length - length(test7$ET_mmyear))),
                   col7 = c(test8$ET_mmyear,
                            rep(NA, max_length - length(test8$ET_mmyear))),
                   col8 = c(test9$ET_mmyear,
                            rep(NA, max_length - length(test9$ET_mmyear))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=TRUE,main="ET_TC",
        col=c("lightgray","lightgray","lightgray","cornsilk2",
              #"cornsilk2",
              "darkgoldenrod1","lightgray","lightgray","lightgray"))
#k-s test across matrix using WEC15 (col 1) as a reference
r <- data[,1]
res <- sapply(data[,2:8], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res
k<-t.test(data$WEC20,data$ETV) #sig p value = different means (i.e., true diff in means =/=0)
k

max_length <- max(c(length(test$ET_GLEAM), 
                    #length(test2$ET_GLEAM), 
                    length(test3$ET_GLEAM),
                    length(test4$ET_GLEAM), length(test5$ET_GLEAM), length(test6$ET_GLEAM),
                    length(test7$ET_GLEAM), length(test8$ET_GLEAM), length(test9$ET_GLEAM))) 
data <- data.frame(col1 = c(test$ET_GLEAM,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(test$ET_GLEAM))),
                #   col2 = c(test2$ET_GLEAM,
                 #           rep(NA, max_length - length(test2$ET_GLEAM))),
                   col2 = c(test3$ET_GLEAM,
                            rep(NA, max_length - length(test3$ET_GLEAM))),
                   col3 = c(test4$ET_GLEAM,
                            rep(NA, max_length - length(test4$ET_GLEAM))),
                   col4 = c(test5$ET_GLEAM,
                            rep(NA, max_length - length(test5$ET_GLEAM))),
                   col5 = c(test6$ET_GLEAM,
                            rep(NA, max_length - length(test6$ET_GLEAM))),
                   col6 = c(test7$ET_GLEAM,
                            rep(NA, max_length - length(test7$ET_GLEAM))),
                   col7 = c(test8$ET_GLEAM,
                            rep(NA, max_length - length(test8$ET_GLEAM))),
                   col8 = c(test9$ET_GLEAM,
                            rep(NA, max_length - length(test9$ET_GLEAM))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=TRUE,main="ET_GLEAM",
        col=c("lightgray","lightgray","lightgray","lightgray","darkgoldenrod1",
              #"cornsilk2",
              "cornsilk2","lightgray","lightgray"))
#k-s test across matrix using WEC15 (col 1) as a reference
r <- data[,1]
res <- sapply(data[,2:8], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res

max_length <- max(c(length(test$Q_mmyear), 
                    #length(test2$Q_mmyear), 
                    length(test3$Q_mmyear),
                    length(test4$Q_mmyear), length(test5$Q_mmyear), length(test6$Q_mmyear),
                    length(test7$Q_mmyear), length(test8$Q_mmyear), length(test9$Q_mmyear))) 
data <- data.frame(col1 = c(test$Q_mmyear,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(test$Q_mmyear))),
                  # col2 = c(test2$Q_mmyear,
                    #        rep(NA, max_length - length(test2$Q_mmyear))),
                   col2 = c(test3$Q_mmyear,
                            rep(NA, max_length - length(test3$Q_mmyear))),
                   col3 = c(test4$Q_mmyear,
                            rep(NA, max_length - length(test4$Q_mmyear))),
                   col4 = c(test5$Q_mmyear,
                            rep(NA, max_length - length(test5$Q_mmyear))),
                   col5 = c(test6$Q_mmyear,
                            rep(NA, max_length - length(test6$Q_mmyear))),
                   col6 = c(test7$Q_mmyear,
                            rep(NA, max_length - length(test7$Q_mmyear))),
                   col7 = c(test8$Q_mmyear,
                            rep(NA, max_length - length(test8$Q_mmyear))),
                   col8 = c(test9$Q_mmyear,
                            rep(NA, max_length - length(test9$Q_mmyear))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=TRUE,main="Q_TC",
        col=c("lightgray","lightgray","lightgray","lightgray","lightgray",
              "darkgoldenrod1","lightgray","cornsilk2","cornsilk2"))
#k-s test across matrix using WEC20 (col 2) as a reference
r <- data[,1]
res <- sapply(data[,2:8], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res


max_length <- max(c(length(test$Q_GRUN), 
                    #length(test2$Q_GRUN), 
                    length(test3$Q_GRUN),
                    length(test4$Q_GRUN), length(test5$Q_GRUN), length(test6$Q_GRUN),
                    length(test7$Q_GRUN), length(test8$Q_GRUN), length(test9$Q_GRUN))) 
data <- data.frame(col1 = c(test$Q_GRUN,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(test$Q_GRUN))),
                   #col2 = c(test2$Q_GRUN,
                    #        rep(NA, max_length - length(test2$Q_GRUN))),
                   col2 = c(test3$Q_GRUN,
                            rep(NA, max_length - length(test3$Q_GRUN))),
                   col3 = c(test4$Q_GRUN,
                            rep(NA, max_length - length(test4$Q_GRUN))),
                   col4 = c(test5$Q_GRUN,
                            rep(NA, max_length - length(test5$Q_GRUN))),
                   col5 = c(test6$Q_GRUN,
                            rep(NA, max_length - length(test6$Q_GRUN))),
                   col6 = c(test7$Q_GRUN,
                            rep(NA, max_length - length(test7$Q_GRUN))),
                   col7 = c(test8$Q_GRUN,
                            rep(NA, max_length - length(test8$Q_GRUN))),
                   col8 = c(test9$Q_GRUN,
                            rep(NA, max_length - length(test9$Q_GRUN))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=TRUE,main="Q_GRUN",
        col=c("lightgray","lightgray","lightgray","lightgray","darkgoldenrod1",
              "lightgray","lightgray","cornsilk2","cornsilk2"))
#k-s test across matrix using WEC15 (col 1) as a reference
r <- data[,1]
res <- sapply(data[,2:8], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res


max_length <- max(c(length(test$PET_mmyear), 
                    #length(test2$PET_mmyear), 
                    length(test3$PET_mmyear),
                    length(test4$PET_mmyear), length(test5$PET_mmyear), length(test6$PET_mmyear),
                    length(test7$PET_mmyear), length(test8$PET_mmyear), length(test9$PET_mmyear))) 
data <- data.frame(col1 = c(test$PET_mmyear,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(test$PET_mmyear))),
                  # col2 = c(test2$PET_mmyear,
                   #         rep(NA, max_length - length(test2$PET_mmyear))),
                   col2 = c(test3$PET_mmyear,
                            rep(NA, max_length - length(test3$PET_mmyear))),
                   col3 = c(test4$PET_mmyear,
                            rep(NA, max_length - length(test4$PET_mmyear))),
                   col4 = c(test5$PET_mmyear,
                            rep(NA, max_length - length(test5$PET_mmyear))),
                   col5 = c(test6$PET_mmyear,
                            rep(NA, max_length - length(test6$PET_mmyear))),
                   col6 = c(test7$PET_mmyear,
                            rep(NA, max_length - length(test7$PET_mmyear))),
                   col7 = c(test8$PET_mmyear,
                            rep(NA, max_length - length(test8$PET_mmyear))),
                   col8 = c(test9$PET_mmyear,
                            rep(NA, max_length - length(test9$PET_mmyear))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=TRUE, main="PET",
        col=c("lightgray","lightgray","lightgray","lightgray","lightgray",
              "lightgray","darkgoldenrod1","cornsilk2","cornsilk2"))
#k-s test across matrix using WEC15 (col 1) as a reference
r <- data[,1]
res <- sapply(data[,2:8], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res


max_length <- max(c(length(test$P_mmyear), 
                    #length(test2$P_mmyear), 
                    length(test3$P_mmyear),
                    length(test4$P_mmyear), length(test5$P_mmyear), length(test6$P_mmyear),
                    length(test7$P_mmyear), length(test8$P_mmyear), length(test9$P_mmyear))) 
data <- data.frame(col1 = c(test$P_mmyear,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(test$P_mmyear))),
                   #col2 = c(test2$P_mmyear,
                    #        rep(NA, max_length - length(test2$P_mmyear))),
                   col2 = c(test3$P_mmyear,
                            rep(NA, max_length - length(test3$P_mmyear))),
                   col3 = c(test4$P_mmyear,
                            rep(NA, max_length - length(test4$P_mmyear))),
                   col4 = c(test5$P_mmyear,
                            rep(NA, max_length - length(test5$P_mmyear))),
                   col5 = c(test6$P_mmyear,
                            rep(NA, max_length - length(test6$P_mmyear))),
                   col6 = c(test7$P_mmyear,
                            rep(NA, max_length - length(test7$P_mmyear))),
                   col7 = c(test8$P_mmyear,
                            rep(NA, max_length - length(test8$P_mmyear))),
                   col8 = c(test9$P_mmyear,
                            rep(NA, max_length - length(test9$P_mmyear))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Meybeck","Knoben","ETA","ETC","ETV")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=TRUE,main="P",
        col=c("lightgray","lightgray","lightgray","lightgray","lightgray",
              "lightgray","darkgoldenrod1","cornsilk2","cornsilk2"))
#k-s test across matrix using WEC20 (col 2) as a reference
r <- data[,1]
res <- sapply(data[,2:8], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res


######## Complexity: CV(area) and number of patches ##########

#patchiness
test<-lsm_c_np(ETC_raster)
ETV_patchiness<-test
ETA_patchiness<-test
ETC_patchiness<-test
WEC15_patchiness<-test
WEC20_patchiness<-test
KPG_patchiness<-test
Hold_patchiness<-test
Knoben_patchiness<-test
Meybeck_patchiness<-test

# area of each zone in km2
test<-tapply(area(Meybeck_zones), Meybeck_zones[], sum)
ETV_area<-test
ETA_area<-test
ETC_area<-test
WEC15_area<-test
WEC20_area<-test
KPG_area<-test
Hold_area<-test
Knoben_area<-test
Meybeck_area<-test

###### Complexity boxplots ########
max_length <- max(c(length(WEC15_patchiness$value), 
                    #length(WEC20_patchiness$value),
                    length(KPG_patchiness$value),
                    length(Hold_patchiness$value),length(Knoben_patchiness$value),length(Meybeck_patchiness$value),
                    length(ETV_patchiness$value),length(ETA_patchiness$value),length(ETC_patchiness$value)))
data <- data.frame(col1 = c(WEC15_patchiness$value,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(WEC15_patchiness$value))),
                   #col2 = c(WEC20_patchiness$value,
                    #        rep(NA, max_length - length(WEC20_patchiness$value))),
                   col2 = c(KPG_patchiness$value,
                            rep(NA, max_length - length(KPG_patchiness$value))),
                   col3 = c(Hold_patchiness$value,
                            rep(NA, max_length - length(Hold_patchiness$value))),
                   col4 = c(Knoben_patchiness$value,
                            rep(NA, max_length - length(Knoben_patchiness$value))),
                   col5 = c(Meybeck_patchiness$value,
                            rep(NA, max_length - length(Meybeck_patchiness$value))),
                   col6 = c(ETV_patchiness$value,
                            rep(NA, max_length - length(ETV_patchiness$value))),
                   col7 = c(ETA_patchiness$value,
                            rep(NA, max_length - length(ETA_patchiness$value))),
                   col8 = c(ETC_patchiness$value,
                            rep(NA, max_length - length(ETC_patchiness$value))))
colnames(data)=c("WEC15",
                 #"WEC20",
                 "KGZ","Holdridge","Knoben","Meybeck","ETV","ETA","ETC")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = median, na.rm = TRUE)
o<-order(m,decreasing=TRUE)
boxplot(data[,o],medcol="red",outline=FALSE,main="",
        col=c("lightgray","lightgray","lightgray","lightgray","lightgray",
              #"cornsilk2",
              "cornsilk2","darkgoldenrod1","lightgray"))
#k-s test across matrix using WEC20 (col 2) as a reference
r <- data[,1]
res <- sapply(data[,c(2:8)], function(y) {
  ks <- ks.test(r, y)
  c(statistic=ks$statistic, p.value=ks$p.value)
  setNames(c(ks$statistic, ks$p.value), c("statistic", "p.value"))
})
res


max_length <- max(c(length(WEC15_area), length(WEC20_area),length(KPG_area),
                    length(Hold_area),length(Knoben_area),length(Meybeck_area),
                    length(ETV_area),length(ETA_area),length(ETC_area)))
data <- data.frame(col1 = c(WEC15_area,                 # Create data frame with unequal vectors
                            rep(NA, max_length - length(WEC15_area))),
                   col2 = c(WEC20_area,
                            rep(NA, max_length - length(WEC20_area))),
                   col3 = c(KPG_area,
                            rep(NA, max_length - length(KPG_area))),
                   col4 = c(Hold_area,
                            rep(NA, max_length - length(Hold_area))),
                   col5 = c(Knoben_area,
                            rep(NA, max_length - length(Knoben_area))),
                   col6 = c(Meybeck_area,
                            rep(NA, max_length - length(Meybeck_area))),
                   col7 = c(ETV_area,
                            rep(NA, max_length - length(ETV_area))),
                   col8 = c(ETA_area,
                            rep(NA, max_length - length(ETA_area))),
                   col9 = c(ETC_area,
                            rep(NA, max_length - length(ETC_area))))
colnames(data)=c("WEC15","WEC20","KGZ","Holdridge","Knoben","Meybeck","ETV","ETA","ETC")
par(mfrow=c(1,1))
m <- apply(data, MARGIN = 2, FUN = cv, na.rm = TRUE)
o<-sort(m,decreasing=TRUE)
barplot(o)

########### WEC20 boxplot -- coherence and complexity values #############

boxplot(test2[,-1], medcol="red")
mean(test2$Q_GRUN_mmyear)
sd(test2$Q_GRUN_mmyear)
mean(WEC20_dtheta_cv$cv)
sd(WEC20_dtheta_cv$cv)

mean(WEC20_patchiness$value)
sd(WEC20_patchiness$value)
