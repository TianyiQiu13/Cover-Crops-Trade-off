library(devtools)
library(tidyverse)
library(ggplot2)
library(raster)
library(RColorBrewer)
library(grid)
library(ggplot2)
library(dplyr)
library(rgdal)
library(cowplot)
library(scales)
library(viridis)
library(swatches)
library(ggalt)
library(nord)
library(ggpolypath)
library(Cairo)
library(colorspace)
library(cptcity) # http://soliton.vm.bytemark.co.uk/pub/cpt-city/index.html
library(rgeos)
require(Cairo)
library(ggspatial)
library(rcartocolor)
library(colorspace)
library(tibble)
library(mapproj)
library(ggsignif)
library(rworldmap)
library(rnaturalearth)
library(terra)
library(tidyterra)
library(rcolors)
library(sf)

setwd("E:/Data/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches
make_pct <- function(x) (exp(x) - 1) * 100

worldMap <- fortify(map_data("world"),region="subregion")
worldMap <- worldMap[worldMap$region!="Antarctica",]

wmap <- ne_download(type = "admin_0_countries", category = "cultural",scale = 110,
                    returnclass = "sf") 
wmap <- st_transform(wmap, CRS("+proj=robin"))
wmap <- wmap[wmap$CONTINENT!="Antarctica",]

ocean <- ne_download(type="ocean", category = "physical",scale = 50,
                     returnclass = "sf")
ocean <- st_transform(ocean, CRS("+proj=robin"))

bbox <- ne_download(type = "wgs84_bounding_box", category = "physical",
                    returnclass = "sf") 
bbox <- st_transform(bbox, CRS("+proj=robin"))

crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" 
crs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ----------------------Wheat--------------------------------------------
### N rate ###
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
N.rate <- raster("E:/Data/Raster/CC-raster/N.rate.wheat.1961-2020.tif")
wheat <- stack(AI,pH,N.rate)
wheat <- resample(wheat,ref)
names(wheat) <- c("AI","pH","N.rate1")
wheat.df <- as.data.frame(wheat,xy=T)

wheat.df$AI2 <- ifelse(wheat.df$AI<=0.2,"Arid",
                       ifelse(wheat.df$AI<=0.5,"Semi-arid",
                              ifelse(wheat.df$AI<=0.75,"Semi-humid","Humid")))
wheat.df$pH2 <- ifelse(wheat.df$pH<=5.5,"Strongly acidic",
                       ifelse(wheat.df$pH<6.5,"Moderately acidic",
                              ifelse(wheat.df$pH<=7.5,"Neutral","Alkaline")))
wheat.df$AI2 <- factor(wheat.df$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
wheat.df$pH2 <- factor(wheat.df$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
wheat.df$ecoregion <- ifelse(wheat.df$AI2=="Arid"&wheat.df$pH2=="Strongly acidic",1,
                             ifelse(wheat.df$AI2=="Arid"&wheat.df$pH2=="Moderately acidic",2,
                                    ifelse(wheat.df$AI2=="Arid"&wheat.df$pH2=="Neutral",3,
                                           ifelse(wheat.df$AI2=="Arid"&wheat.df$pH2=="Alkaline",4,
                                                  ifelse(wheat.df$AI2=="Semi-arid"&wheat.df$pH2=="Strongly acidic",5,
                                                         ifelse(wheat.df$AI2=="Semi-arid"&wheat.df$pH2=="Moderately acidic",6,
                                                                ifelse(wheat.df$AI2=="Semi-arid"&wheat.df$pH2=="Neutral",7,
                                                                       ifelse(wheat.df$AI2=="Semi-arid"&wheat.df$pH2=="Alkaline",8,
                                                                              ifelse(wheat.df$AI2=="Semi-humid"&wheat.df$pH2=="Strongly acidic",9,
                                                                                     ifelse(wheat.df$AI2=="Semi-humid"&wheat.df$pH2=="Moderately acidic",10,
                                                                                            ifelse(wheat.df$AI2=="Semi-humid"&wheat.df$pH2=="Neutral",11,
                                                                                                   ifelse(wheat.df$AI2=="Semi-humid"&wheat.df$pH2=="Alkaline",12,
                                                                                                          ifelse(wheat.df$AI2=="Humid"&wheat.df$pH2=="Strongly acidic",13,
                                                                                                                 ifelse(wheat.df$AI2=="Humid"&wheat.df$pH2=="Moderately acidic",14,15))))))))))))))
wheat.df$N.rate_legume <- ifelse(wheat.df$ecoregion==1,wheat.df$N.rate1,
                                 ifelse(wheat.df$ecoregion==2,wheat.df$N.rate1,
                                        ifelse(wheat.df$ecoregion==3,81.8,
                                               ifelse(wheat.df$ecoregion==4,54.9,
                                                      ifelse(wheat.df$ecoregion==5,1.94,
                                                             ifelse(wheat.df$ecoregion==6,1.67,
                                                                    ifelse(wheat.df$ecoregion==7,96.3,
                                                                           ifelse(wheat.df$ecoregion==8,20.2,
                                                                                  ifelse(wheat.df$ecoregion==9,12.6,
                                                                                         ifelse(wheat.df$ecoregion==10,43.9,
                                                                                                ifelse(wheat.df$ecoregion==11,50.5,
                                                                                                       ifelse(wheat.df$ecoregion==12,wheat.df$N.rate1,
                                                                                                              ifelse(wheat.df$ecoregion==13,66.8,
                                                                                                                     ifelse(wheat.df$ecoregion==14,33.1,4.51))))))))))))))

wheat.df$N.rate_mixture <- ifelse(wheat.df$ecoregion==1,wheat.df$N.rate1,
                                  ifelse(wheat.df$ecoregion==2,wheat.df$N.rate1,
                                         ifelse(wheat.df$ecoregion==3,81.8,
                                                ifelse(wheat.df$ecoregion==4,54.9,
                                                       ifelse(wheat.df$ecoregion==5,1.94,
                                                              ifelse(wheat.df$ecoregion==6,1.67,
                                                                     ifelse(wheat.df$ecoregion==7,96.3,
                                                                            ifelse(wheat.df$ecoregion==8,20.2,
                                                                                   ifelse(wheat.df$ecoregion==9,12.6,
                                                                                          ifelse(wheat.df$ecoregion==10,43.9,
                                                                                                 ifelse(wheat.df$ecoregion==11,50.5,
                                                                                                        ifelse(wheat.df$ecoregion==12,wheat.df$N.rate1,
                                                                                                               ifelse(wheat.df$ecoregion==13,63.4,
                                                                                                                      ifelse(wheat.df$ecoregion==14,40.0,4.51))))))))))))))
wheat.df$N.rate_non_legume <- ifelse(wheat.df$ecoregion==1,wheat.df$N.rate1,
                                     ifelse(wheat.df$ecoregion==2,wheat.df$N.rate1,
                                            ifelse(wheat.df$ecoregion==3,81.8,
                                                   ifelse(wheat.df$ecoregion==4,50.9,
                                                          ifelse(wheat.df$ecoregion==5,1.94,
                                                                 ifelse(wheat.df$ecoregion==6,1.67,
                                                                        ifelse(wheat.df$ecoregion==7,96.3,
                                                                               ifelse(wheat.df$ecoregion==8,20.2,
                                                                                      ifelse(wheat.df$ecoregion==9,wheat.df$N.rate1,
                                                                                             ifelse(wheat.df$ecoregion==10,51.5,
                                                                                                    ifelse(wheat.df$ecoregion==11,10.1,
                                                                                                           ifelse(wheat.df$ecoregion==12,wheat.df$N.rate1,
                                                                                                                  ifelse(wheat.df$ecoregion==13,46.3,
                                                                                                                         ifelse(wheat.df$ecoregion==14,40.0,4.51))))))))))))))
wheat.df$N.rate2 <- (wheat.df$N.rate_legume+wheat.df$N.rate_mixture+wheat.df$N.rate_non_legume)/3-wheat.df$N.rate1+wheat.df$N.rate1
N.rate_opt.wheat <- rasterFromXYZ(wheat.df[,c("x", "y", "N.rate2")],
                         crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

N.rate_opt.wheat_robin <- raster::projectRaster(N.rate_opt.wheat, crs = crs_robin, method = "bilinear")
N.rate_opt.wheat.df = as.data.frame(N.rate_opt.wheat_robin,xy=TRUE)
ggplot(N.rate_opt.wheat.df)+
  geom_density(aes(N.rate2))

p.N.rate_surplus_wheat <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=N.rate_opt.wheat.df,aes(x,y,fill=N.rate2)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = c(0.65,0.125),legend.text = element_text(size = 12))+
  scale_fill_stepsn(
    colors = hcl.colors(10,"RdBu",rev=T),
    na.value = NA,
    name = "Optimized N\napplication rate (kg/ha)",
    limits = c(0, 100),
    breaks = seq(0, 100, 20/2),
    labels = seq(0, 100, 20/2) %>% sapply(function(x){
      if(x %% 20 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      label.position = "bottom",
      barwidth = 7.5,
      barheight = 0.75,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.N.rate_surplus_wheat

### regional applicability ###
yield.scaled.N2O_legume_wheat1 <- raster("yield.scaled.N2O-wheat-legume.relMean.tif")
yield.scaled.N2O_legume_wheat2 <- raster("opt.yield.scaled.N2O-wheat-legume.relMean.tif")
yield.scaled.N2O_legume_wheat <- yield.scaled.N2O_legume_wheat1-yield.scaled.N2O_legume_wheat2
yield.scaled.N2O_mixture_wheat1 <- raster("yield.scaled.N2O-wheat-mixture.relMean.tif")
yield.scaled.N2O_mixture_wheat2 <- raster("opt.yield.scaled.N2O-wheat-mixture.relMean.tif")
yield.scaled.N2O_mixture_wheat <- yield.scaled.N2O_mixture_wheat1-yield.scaled.N2O_mixture_wheat2
yield.scaled.N2O_non_legume_wheat1 <- raster("yield.scaled.N2O-wheat-non-legume.relMean.tif")
yield.scaled.N2O_non_legume_wheat2 <- raster("opt.yield.scaled.N2O-wheat-non-legume.relMean.tif")
yield.scaled.N2O_non_legume_wheat <- yield.scaled.N2O_non_legume_wheat1-yield.scaled.N2O_non_legume_wheat2
yield.scaled.N2O_wheat <- mean(yield.scaled.N2O_legume_wheat,yield.scaled.N2O_mixture_wheat,yield.scaled.N2O_non_legume_wheat)

yield.scaled.N2O_wheat_robin <- raster::projectRaster(yield.scaled.N2O_wheat, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_wheat.df = as.data.frame(yield.scaled.N2O_wheat_robin,xy=TRUE)
yield.scaled.N2O_wheat.df$value <- make_pct(yield.scaled.N2O_wheat.df$layer)
ggplot(yield.scaled.N2O_wheat.df)+
  geom_density(aes(value))

p.yield.scaled.N2O_wheat <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_wheat.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = c(0.65,0.125),legend.text = element_text(size = 12))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"PiYG",rev=F)[c(1,3,5,7,9:16)],
    na.value = NA,
      name = "Reduced trade-off\nby N optimization (%)",
    limits = c(-3, 6),
    breaks = seq(-3, 6, 3/4),
    labels = seq(-3, 6, 3/4) %>% sapply(function(x){
      if(x %% 1.5 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      label.position = "bottom",
      barwidth = 7.5,
      barheight = 0.75,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_wheat

### Actual reduced N2O emissions ###
N2O_legume_wheat1 <- raster("N2O-wheat-legume.relMean.tif")
N2O_legume_wheat2 <- raster("opt.N2O-wheat-legume.relMean.tif")
N2O_legume_wheat <- N2O_legume_wheat1-N2O_legume_wheat2
N2O_mixture_wheat1 <- raster("N2O-wheat-mixture.relMean.tif")
N2O_mixture_wheat2 <- raster("opt.N2O-wheat-mixture.relMean.tif")
N2O_mixture_wheat <- N2O_mixture_wheat1-N2O_mixture_wheat2
N2O_non_legume_wheat1 <- raster("N2O-wheat-non-legume.relMean.tif")
N2O_non_legume_wheat2 <- raster("opt.N2O-wheat-non-legume.relMean.tif")
N2O_non_legume_wheat <- N2O_non_legume_wheat1-N2O_non_legume_wheat2
N2O_wheat <- mean(N2O_legume_wheat,N2O_mixture_wheat,N2O_non_legume_wheat)
N2O_baseline <- raster("E:/Data/Raster/CC-raster/N2O_1970-2019-0.1.tif")
N2O_baseline <- resample(N2O_baseline,ref)
dN2O_wheat <- make_pct(N2O_wheat)*N2O_baseline/100
cropland.ha <- raster("E:/Data/Raster/CC-raster/cropland.ha-0.1.tif")
cropland.ha <- resample(cropland.ha,ref)
dN2O_wheat_actual <- dN2O_wheat*cropland.ha/1000##Unit: t N2O/yr

dN2O_wheat_actual_robin <- raster::projectRaster(dN2O_wheat_actual, crs = crs_robin, method = "bilinear")
dN2O_wheat_actual.df = as.data.frame(dN2O_wheat_actual_robin,xy=TRUE)
ggplot(dN2O_wheat_actual.df)+
  geom_density(aes(layer))

p.N2O_wheat <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=dN2O_wheat_actual.df,aes(x,y,fill=layer)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = c(0.65,0.125),legend.text = element_text(size = 12))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"PuOr",rev=T)[c(1,5,9:16)],
    na.value = NA,
    name = "Reduced N2O production\nby N optimization (t/yr)",
    limits = c(-1, 4),
    breaks = seq(-1, 4, 0.5),
    labels = seq(-1, 4, 0.5) %>% sapply(function(x){
      if(x %% 1 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      label.position = "bottom",
      barwidth = 7.5,
      barheight = 0.75,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.N2O_wheat

# ----------------------Maize--------------------------------------------
### N rate ###
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
N.rate <- raster("E:/Data/Raster/CC-raster/N.rate.maize.1961-2020.tif")
maize <- stack(AI,pH,N.rate)
maize <- resample(maize,ref)
names(maize) <- c("AI","pH","N.rate1")
maize.df <- as.data.frame(maize,xy=T)

maize.df$AI2 <- ifelse(maize.df$AI<=0.2,"Arid",
                       ifelse(maize.df$AI<=0.5,"Semi-arid",
                              ifelse(maize.df$AI<=0.75,"Semi-humid","Humid")))
maize.df$pH2 <- ifelse(maize.df$pH<=5.5,"Strongly acidic",
                       ifelse(maize.df$pH<6.5,"Moderately acidic",
                              ifelse(maize.df$pH<=7.5,"Neutral","Alkaline")))
maize.df$AI2 <- factor(maize.df$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
maize.df$pH2 <- factor(maize.df$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
maize.df$ecoregion <- ifelse(maize.df$AI2=="Arid"&maize.df$pH2=="Strongly acidic",1,
                             ifelse(maize.df$AI2=="Arid"&maize.df$pH2=="Moderately acidic",2,
                                    ifelse(maize.df$AI2=="Arid"&maize.df$pH2=="Neutral",3,
                                           ifelse(maize.df$AI2=="Arid"&maize.df$pH2=="Alkaline",4,
                                                  ifelse(maize.df$AI2=="Semi-arid"&maize.df$pH2=="Strongly acidic",5,
                                                         ifelse(maize.df$AI2=="Semi-arid"&maize.df$pH2=="Moderately acidic",6,
                                                                ifelse(maize.df$AI2=="Semi-arid"&maize.df$pH2=="Neutral",7,
                                                                       ifelse(maize.df$AI2=="Semi-arid"&maize.df$pH2=="Alkaline",8,
                                                                              ifelse(maize.df$AI2=="Semi-humid"&maize.df$pH2=="Strongly acidic",9,
                                                                                     ifelse(maize.df$AI2=="Semi-humid"&maize.df$pH2=="Moderately acidic",10,
                                                                                            ifelse(maize.df$AI2=="Semi-humid"&maize.df$pH2=="Neutral",11,
                                                                                                   ifelse(maize.df$AI2=="Semi-humid"&maize.df$pH2=="Alkaline",12,
                                                                                                          ifelse(maize.df$AI2=="Humid"&maize.df$pH2=="Strongly acidic",13,
                                                                                                                 ifelse(maize.df$AI2=="Humid"&maize.df$pH2=="Moderately acidic",14,15))))))))))))))
maize.df$N.rate_legume <- ifelse(maize.df$ecoregion==1,maize.df$N.rate1,
                      ifelse(maize.df$ecoregion==2,11.6,
                             ifelse(maize.df$ecoregion==3,maize.df$N.rate1,
                                    ifelse(maize.df$ecoregion==4,75.0,
                                           ifelse(maize.df$ecoregion==5,101,
                                                  ifelse(maize.df$ecoregion==6,23.7,
                                                         ifelse(maize.df$ecoregion==7,21.6,
                                                                ifelse(maize.df$ecoregion==8,105,
                                                                       ifelse(maize.df$ecoregion==9,38.9,
                                                                              ifelse(maize.df$ecoregion==10,30.0,
                                                                                     ifelse(maize.df$ecoregion==11,24.0,
                                                                                            ifelse(maize.df$ecoregion==12,maize.df$N.rate1,
                                                                                                   ifelse(maize.df$ecoregion==13,49.3,
                                                                                                          ifelse(maize.df$ecoregion==14,31.3,9.32))))))))))))))
maize.df$N.rate_mixture <- ifelse(maize.df$ecoregion==1,maize.df$N.rate1,
                      ifelse(maize.df$ecoregion==2,11.6,
                             ifelse(maize.df$ecoregion==3,maize.df$N.rate1,
                                    ifelse(maize.df$ecoregion==4,75.0,
                                           ifelse(maize.df$ecoregion==5,101,
                                                  ifelse(maize.df$ecoregion==6,23.7,
                                                         ifelse(maize.df$ecoregion==7,21.6,
                                                                ifelse(maize.df$ecoregion==8,105,
                                                                       ifelse(maize.df$ecoregion==9,41.1,
                                                                              ifelse(maize.df$ecoregion==10,30.0,
                                                                                     ifelse(maize.df$ecoregion==11,27.7,
                                                                                            ifelse(maize.df$ecoregion==12,maize.df$N.rate1,
                                                                                                   ifelse(maize.df$ecoregion==13,49.3,
                                                                                                          ifelse(maize.df$ecoregion==14,31.3,9.32))))))))))))))
maize.df$N.rate_non_legume <- ifelse(maize.df$ecoregion==1,maize.df$N.rate1,
                      ifelse(maize.df$ecoregion==2,11.6,
                             ifelse(maize.df$ecoregion==3,maize.df$N.rate1,
                                    ifelse(maize.df$ecoregion==4,62.1,
                                           ifelse(maize.df$ecoregion==5,101,
                                                  ifelse(maize.df$ecoregion==6,23.7,
                                                         ifelse(maize.df$ecoregion==7,21.6,
                                                                ifelse(maize.df$ecoregion==8,105,
                                                                       ifelse(maize.df$ecoregion==9,41.1,
                                                                              ifelse(maize.df$ecoregion==10,30.0,
                                                                                     ifelse(maize.df$ecoregion==11,27.7,
                                                                                            ifelse(maize.df$ecoregion==12,maize.df$N.rate1,
                                                                                                   ifelse(maize.df$ecoregion==13,38.4,
                                                                                                          ifelse(maize.df$ecoregion==14,178,9.32))))))))))))))
maize.df$N.rate2 <- (maize.df$N.rate_legume+maize.df$N.rate_mixture+maize.df$N.rate_non_legume)/3-maize.df$N.rate1+maize.df$N.rate1
N.rate_opt.maize <- rasterFromXYZ(maize.df[,c("x", "y", "N.rate2")],
                                  crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

N.rate_opt.maize_robin <- raster::projectRaster(N.rate_opt.maize, crs = crs_robin, method = "bilinear")
N.rate_opt.maize.df = as.data.frame(N.rate_opt.maize_robin,xy=TRUE)
ggplot(N.rate_opt.maize.df)+
  geom_density(aes(N.rate2))

p.N.rate_surplus_maize <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=N.rate_opt.maize.df,aes(x,y,fill=N.rate2)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = c(0.65,0.125),legend.text = element_text(size = 12))+
  scale_fill_stepsn(
    colors = hcl.colors(10,"RdBu",rev=T),
    na.value = NA,
    name = "Optimized N\napplication rate (kg/ha)",
    limits = c(0, 100),
    breaks = seq(0, 100, 20/2),
    labels = seq(0, 100, 20/2) %>% sapply(function(x){
      if(x %% 20 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      label.position = "bottom",
      barwidth = 7.5,
      barheight = 0.75,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.N.rate_surplus_maize

### regional applicability ###
yield.scaled.N2O_legume_maize1 <- raster("yield.scaled.N2O-maize-legume.relMean.tif")
yield.scaled.N2O_legume_maize2 <- raster("opt.yield.scaled.N2O-maize-legume.relMean.tif")
yield.scaled.N2O_legume_maize <- yield.scaled.N2O_legume_maize1-yield.scaled.N2O_legume_maize2
yield.scaled.N2O_mixture_maize1 <- raster("yield.scaled.N2O-maize-mixture.relMean.tif")
yield.scaled.N2O_mixture_maize2 <- raster("opt.yield.scaled.N2O-maize-mixture.relMean.tif")
yield.scaled.N2O_mixture_maize <- yield.scaled.N2O_mixture_maize1-yield.scaled.N2O_mixture_maize2
yield.scaled.N2O_non_legume_maize1 <- raster("yield.scaled.N2O-maize-non-legume.relMean.tif")
yield.scaled.N2O_non_legume_maize2 <- raster("opt.yield.scaled.N2O-maize-non-legume.relMean.tif")
yield.scaled.N2O_non_legume_maize <- yield.scaled.N2O_non_legume_maize1-yield.scaled.N2O_non_legume_maize2
yield.scaled.N2O_maize <- mean(yield.scaled.N2O_legume_maize,yield.scaled.N2O_mixture_maize,yield.scaled.N2O_non_legume_maize)

yield.scaled.N2O_maize_robin <- raster::projectRaster(yield.scaled.N2O_maize, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_maize.df = as.data.frame(yield.scaled.N2O_maize_robin,xy=TRUE)
yield.scaled.N2O_maize.df$value <- make_pct(yield.scaled.N2O_maize.df$layer)
ggplot(yield.scaled.N2O_maize.df)+
  geom_density(aes(value))

p.yield.scaled.N2O_maize <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_maize.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = c(0.65,0.125),legend.text = element_text(size = 12))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"PiYG",rev=F)[c(1,3,5,7,9:16)],
    na.value = NA,
    name = "Reduced trade-off\nby N optimization (%)",
    limits = c(-3, 6),
    breaks = seq(-3, 6, 3/4),
    labels = seq(-3, 6, 3/4) %>% sapply(function(x){
      if(x %% 1.5 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      label.position = "bottom",
      barwidth = 7.5,
      barheight = 0.75,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_maize

### Actual reduced N2O emissions ###
N2O_legume_maize1 <- raster("N2O-maize-legume.relMean.tif")
N2O_legume_maize2 <- raster("opt.N2O-maize-legume.relMean.tif")
N2O_legume_maize <- N2O_legume_maize1-N2O_legume_maize2
N2O_mixture_maize1 <- raster("N2O-maize-mixture.relMean.tif")
N2O_mixture_maize2 <- raster("opt.N2O-maize-mixture.relMean.tif")
N2O_mixture_maize <- N2O_mixture_maize1-N2O_mixture_maize2
N2O_non_legume_maize1 <- raster("N2O-maize-non-legume.relMean.tif")
N2O_non_legume_maize2 <- raster("opt.N2O-maize-non-legume.relMean.tif")
N2O_non_legume_maize <- N2O_non_legume_maize1-N2O_non_legume_maize2
N2O_maize <- mean(N2O_legume_maize,N2O_mixture_maize,N2O_non_legume_maize)
N2O_baseline <- raster("E:/Data/Raster/CC-raster/N2O_1970-2019-0.1.tif")
N2O_baseline <- resample(N2O_baseline,ref)
dN2O_maize <- make_pct(N2O_maize)*N2O_baseline/100
cropland.ha <- raster("E:/Data/Raster/CC-raster/cropland.ha-0.1.tif")
cropland.ha <- resample(cropland.ha,ref)
dN2O_maize_actual <- dN2O_maize*cropland.ha/1000##Unit: t N2O/yr

dN2O_maize_actual_robin <- raster::projectRaster(dN2O_maize_actual, crs = crs_robin, method = "bilinear")
dN2O_maize_actual.df = as.data.frame(dN2O_maize_actual_robin,xy=TRUE)
ggplot(dN2O_maize_actual.df)+
  geom_density(aes(layer))

p.N2O_maize <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=dN2O_maize_actual.df,aes(x,y,fill=layer)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = c(0.65,0.125),legend.text = element_text(size = 12))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"PuOr",rev=T)[c(1,5,9:16)],
    na.value = NA,
    name = "Reduced N2Oproduction\nby N optimization (t/yr)",
    limits = c(-1, 4),
    breaks = seq(-1, 4, 0.5),
    labels = seq(-1, 4, 0.5) %>% sapply(function(x){
      if(x %% 1 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "horizontal",
      nrow = 1,
      title.position = "top",
      label.position = "bottom",
      barwidth = 7.5,
      barheight = 0.75,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.N2O_maize

# ----------------------Regional applicability--------------------------------------------
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
env <- stack(AI,pH)
env <- resample(env,ref)
names(env) <- c("AI","pH")
env_robin <- raster::projectRaster(env, crs = crs_robin, method = "bilinear")
env.df <- as.data.frame(env_robin,xy=T)

wheat <- na.omit(left_join(N.rate_opt.wheat.df, env.df)%>%select(N.rate2,AI,pH))%>%dplyr::mutate(crop="Wheat")
maize <- na.omit(left_join(N.rate_opt.maize.df, env.df)%>%select(N.rate2,AI,pH))%>%dplyr::mutate(crop="Maize")

wheat$AI2 <- ifelse(wheat$AI<=0.2,"Arid",
                    ifelse(wheat$AI<=0.5,"Semi-arid",
                           ifelse(wheat$AI<=0.75,"Semi-humid","Humid")))
wheat$pH2 <- ifelse(wheat$pH<=5.5,"Strongly acidic",
                    ifelse(wheat$pH<6.5,"Moderately acidic",
                           ifelse(wheat$pH<=7.5,"Neutral","Alkaline")))
maize$AI2 <- ifelse(maize$AI<=0.2,"Arid",
                    ifelse(maize$AI<=0.5,"Semi-arid",
                           ifelse(maize$AI<=0.75,"Semi-humid","Humid")))
maize$pH2 <- ifelse(maize$pH<=5.5,"Strongly acidic",
                    ifelse(maize$pH<6.5,"Moderately acidic",
                           ifelse(maize$pH<=7.5,"Neutral","Alkaline")))

N.opt <- rbind.data.frame(wheat,maize)
N.opt$AI2 <- factor(N.opt$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
N.opt$pH2 <- factor(N.opt$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
N.opt$crop <- factor(N.opt$crop,levels = c("Wheat","Maize"))

box.N.opt <- ggplot(data=N.opt,aes(pH2, N.rate2)) + 
  geom_boxplot(show.legend = F,na.rm=T,outlier.alpha=0.1,aes(fill=crop),alpha=0.75) +
  #geom_point(data=AMS.upland.continent.mean,aes(continent,mean),color="red",size=1.6)+
  scale_fill_manual(values = c("wheat","orange")) + 
  ylab("Optimized N application rate (kg/ha)") +
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #geom_text(data=AMS.upland.continent.mean,aes(x=continent,y=-50,label=round(mean,2)),
  #          color="red")+
  #theme_classic(base_size = 8) +
  facet_wrap(~AI2, nrow = 4, ncol = 1,scales = "free_y",strip.position = "right")+
  scale_y_continuous(limits = c(0,150))+
  #geom_signif(comparisons=list(c("upland","paddy")),
  #            test = t.test,tip_length = 0,
  #            na.rm=T,map_signif_level = c("***"=0.001,"**"=0.01,"*"=0.05))+
  theme_bw()+font+
  coord_flip()+
  theme(axis.title.y = element_blank(),legend.position = c(0.75,0.015),
        legend.background = element_blank(),legend.key = element_blank(),
        legend.box = "horizontal", panel.grid = element_blank(),
        axis.text.y = element_text(angle=30,vjust = 0.25,hjust=1),
        panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))
box.N.opt

wheat <- na.omit(left_join(yield.scaled.N2O_wheat.df, env.df)%>%select(value,AI,pH))%>%dplyr::mutate(crop="Wheat")
maize <- na.omit(left_join(yield.scaled.N2O_maize.df, env.df)%>%select(value,AI,pH))%>%dplyr::mutate(crop="Maize")

wheat$AI2 <- ifelse(wheat$AI<=0.2,"Arid",
                    ifelse(wheat$AI<=0.5,"Semi-arid",
                           ifelse(wheat$AI<=0.75,"Semi-humid","Humid")))
wheat$pH2 <- ifelse(wheat$pH<=5.5,"Strongly acidic",
                    ifelse(wheat$pH<6.5,"Moderately acidic",
                           ifelse(wheat$pH<=7.5,"Neutral","Alkaline")))
maize$AI2 <- ifelse(maize$AI<=0.2,"Arid",
                    ifelse(maize$AI<=0.5,"Semi-arid",
                           ifelse(maize$AI<=0.75,"Semi-humid","Humid")))
maize$pH2 <- ifelse(maize$pH<=5.5,"Strongly acidic",
                    ifelse(maize$pH<6.5,"Moderately acidic",
                           ifelse(maize$pH<=7.5,"Neutral","Alkaline")))

yield.scaled.N2O <- rbind.data.frame(wheat,maize)
yield.scaled.N2O$AI2 <- factor(yield.scaled.N2O$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
yield.scaled.N2O$pH2 <- factor(yield.scaled.N2O$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
yield.scaled.N2O$crop <- factor(yield.scaled.N2O$crop,levels = c("Wheat","Maize"))

box.yield.scaled.N2O <- ggplot(data=yield.scaled.N2O,aes(pH2, value)) + 
  geom_boxplot(show.legend = F,na.rm=T,outlier.alpha=0.1,aes(fill=crop),alpha=0.75) +
  #geom_point(data=AMS.upland.continent.mean,aes(continent,mean),color="red",size=1.6)+
  scale_fill_manual(values = c("wheat","orange")) + 
  ylab(expression(paste("Reduced trade-off by N optimization (%)",sep = ""))) +
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #geom_text(data=AMS.upland.continent.mean,aes(x=continent,y=-50,label=round(mean,2)),
  #          color="red")+
  #theme_classic(base_size = 8) +
  facet_wrap(~AI2, nrow = 2, ncol = 2,scales = "free_y")+
  #scale_y_continuous(limits = c(0,200))+
  #geom_signif(comparisons=list(c("upland","paddy")),
  #            test = t.test,tip_length = 0,
  #            na.rm=T,map_signif_level = c("***"=0.001,"**"=0.01,"*"=0.05))+
  theme_bw()+font+
  theme(axis.title.x = element_blank(),legend.position = c(0.75,0.015),
        legend.background = element_blank(),legend.key = element_blank(),
        legend.box = "horizontal", panel.grid = element_blank(),
        axis.text.x = element_text(angle=30,vjust = 1,hjust=1),
        panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))
box.yield.scaled.N2O

wheat <- na.omit(left_join(dN2O_wheat_actual.df, env.df)%>%select(layer,AI,pH))%>%dplyr::mutate(crop="Wheat")
maize <- na.omit(left_join(dN2O_maize_actual.df, env.df)%>%select(layer,AI,pH))%>%dplyr::mutate(crop="Maize")

wheat$AI2 <- ifelse(wheat$AI<=0.2,"Arid",
                    ifelse(wheat$AI<=0.5,"Semi-arid",
                           ifelse(wheat$AI<=0.75,"Semi-humid","Humid")))
wheat$pH2 <- ifelse(wheat$pH<=5.5,"Strongly acidic",
                    ifelse(wheat$pH<6.5,"Moderately acidic",
                           ifelse(wheat$pH<=7.5,"Neutral","Alkaline")))
maize$AI2 <- ifelse(maize$AI<=0.2,"Arid",
                    ifelse(maize$AI<=0.5,"Semi-arid",
                           ifelse(maize$AI<=0.75,"Semi-humid","Humid")))
maize$pH2 <- ifelse(maize$pH<=5.5,"Strongly acidic",
                    ifelse(maize$pH<6.5,"Moderately acidic",
                           ifelse(maize$pH<=7.5,"Neutral","Alkaline")))

N2O <- rbind.data.frame(wheat,maize)
N2O$AI2 <- factor(N2O$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
N2O$pH2 <- factor(N2O$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
N2O$crop <- factor(N2O$crop,levels = c("Wheat","Maize"))

box.N2O <- ggplot(data=N2O,aes(pH2, layer)) + 
  geom_bar(position = position_dodge(width = 0.8), stat = "summary",aes(fill=crop),color="black",width = 0.75) + 
  geom_errorbar(position = position_dodge(width = 0.8),stat="summary",aes(fill=crop),color="black", width=0.25)+
  scale_fill_manual(values = c("wheat","orange")) + 
  scale_color_manual(values = c("wheat","orange")) + 
  ylab(expression(paste("Reduced ",N[2],"O production by N optimization (t/yr)",sep = "")))+
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #geom_text(data=AMS.upland.continent.mean,aes(x=continent,y=-50,label=round(mean,2)),
  #          color="red")+
  #theme_classic(base_size = 8) +
  facet_wrap(~AI2, nrow = 2, ncol = 2,scales = "free_y")+
  #scale_y_continuous(limits = c(-1,5))+
  #geom_signif(comparisons=list(c("upland","paddy")),
  #            test = t.test,tip_length = 0,
  #            na.rm=T,map_signif_level = c("***"=0.001,"**"=0.01,"*"=0.05))+
  theme_bw()+font+
  theme(axis.title.x = element_blank(),legend.position = c(0.75,0.875),
        legend.background = element_blank(),legend.key = element_blank(),
        legend.box = "horizontal", panel.grid = element_blank(),
        axis.text.x = element_text(angle=30,vjust = 1,hjust=1),
        panel.border = element_rect(color = "black", size = 0.75),legend.title = element_blank(),
        strip.background = element_rect(color = "black", size = 0.75))
box.N2O

p1 <- plot_grid(p.N.rate_surplus_wheat,p.N.rate_surplus_maize,nrow = 1,ncol=2)
p2 <- plot_grid(p.yield.scaled.N2O_wheat,p.yield.scaled.N2O_maize,nrow = 1,ncol=2)
p3 <- plot_grid(p.N2O_wheat,p.N2O_maize,nrow = 1,ncol=2)

ggdraw()+ 
  annotation_custom(
    grob = rectGrob(gp = gpar(fill = "wheat", alpha = 0.2, col = NA)),
    xmin = 0, xmax = 1/3, ymin = 2/5, ymax = 1
  ) +
  annotation_custom(
    grob = rectGrob(gp = gpar(fill = "orange", alpha = 0.2, col = NA)),
    xmin = 1/3, xmax = 2/3, ymin = 2/5, ymax = 1
  ) +
  draw_plot(p1, x=0, y=4/5, width = 2/3, height = 1/5)+
  draw_plot(p2, x=0, y=3/5, width = 2/3, height = 1/5)+
  draw_plot(p3, x=0, y=2/5, width = 2/3, height = 1/5)+
  draw_plot(box.N.opt, x=2/3, y=2/5, width = 1/3, height = 3/5)+
  draw_plot(box.yield.scaled.N2O, x=0, y=0, width = 1/2-0.01, height = 2/5)+
  draw_plot(box.N2O, x=1/2, y=0, width = 1/2-0.01, height = 2/5)+
  draw_plot_label(label = c("a","b","c","d","e"," f","g","h","i"), size = 15,
                  x=c(0,1/3,0,1/3,0,1/3,2/3,0,1/2-0.01),
                  y=c(1,1,4/5,4/5,3/5,3/5,1,2/5,2/5))##11.6*11.0inches
