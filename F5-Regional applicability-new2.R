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
### legume ###
yield.scaled.N2O_legume_wheat <- raster("yield.scaled.N2O-wheat-legume.relMean.tif")
yield.scaled.N2O_legume_wheat_robin <- raster::projectRaster(yield.scaled.N2O_legume_wheat, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_legume_wheat.df = as.data.frame(yield.scaled.N2O_legume_wheat_robin,xy=TRUE)
yield.scaled.N2O_legume_wheat.df$value <- make_pct(yield.scaled.N2O_legume_wheat.df$yield.scaled.N2O.wheat.legume.relMean)
ggplot(yield.scaled.N2O_legume_wheat.df)+
  geom_density(aes(value))
yield.scaled.N2O_legume_wheat_se <- raster("yield.scaled.N2O-wheat-legume.relSE.tif")
yield.scaled.N2O_legume_wheat_se_robin <- raster::projectRaster(yield.scaled.N2O_legume_wheat_se, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_legume_wheat_se.df = as.data.frame(yield.scaled.N2O_legume_wheat_se_robin,xy=TRUE)
yield.scaled.N2O_legume_wheat.df$se <- make_pct(yield.scaled.N2O_legume_wheat_se.df$yield.scaled.N2O.wheat.legume.relSE)
yield.scaled.N2O_legume_wheat.df$upper <- yield.scaled.N2O_legume_wheat.df$value+1.96*yield.scaled.N2O_legume_wheat.df$se
ggplot(yield.scaled.N2O_legume_wheat.df)+
  geom_density(aes(upper))
yield.scaled.N2O_legume_wheat.df$lower <- yield.scaled.N2O_legume_wheat.df$value-1.96*yield.scaled.N2O_legume_wheat.df$se
ggplot(yield.scaled.N2O_legume_wheat.df)+
  geom_density(aes(lower))
yield.scaled.N2O_legume_wheat.df$sig1 <- ifelse(yield.scaled.N2O_legume_wheat.df$upper<0,"*",NA)
yield.scaled.N2O_legume_wheat.df$sig2 <- ifelse(yield.scaled.N2O_legume_wheat.df$lower>0,"*",NA)
  
p.yield.scaled.N2O_legume_wheat <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_legume_wheat.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = "right",legend.text = element_text(size = 12),legend.title = element_text(angle = 90,size=13))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"BrBG",rev=T)[c(1,3,5,7,9:16)],
    na.value = NA,
    name = expression(paste("Yield-scaled ",N[2],"O emissions for wheat (%)",sep = "")),
    limits = c(-30, 60),
    breaks = seq(-30, 60, 15/2),
    labels = seq(-30, 60, 15/2) %>% sapply(function(x){
      if(x %% 15 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "vertical",
      nrow = 1,
      title.position = "left",
      label.position = "right",
      barheight = 15,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_legume_wheat

### mixture ###
yield.scaled.N2O_mixture_wheat <- raster("yield.scaled.N2O-wheat-mixture.relMean.tif")
yield.scaled.N2O_mixture_wheat_robin <- raster::projectRaster(yield.scaled.N2O_mixture_wheat, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_mixture_wheat.df = as.data.frame(yield.scaled.N2O_mixture_wheat_robin,xy=TRUE)
yield.scaled.N2O_mixture_wheat.df$value <- make_pct(yield.scaled.N2O_mixture_wheat.df$yield.scaled.N2O.wheat.mixture.relMean)
ggplot(yield.scaled.N2O_mixture_wheat.df)+
  geom_density(aes(value))
yield.scaled.N2O_mixture_wheat_se <- raster("yield.scaled.N2O-wheat-mixture.relSE.tif")
yield.scaled.N2O_mixture_wheat_se_robin <- raster::projectRaster(yield.scaled.N2O_mixture_wheat_se, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_mixture_wheat_se.df = as.data.frame(yield.scaled.N2O_mixture_wheat_se_robin,xy=TRUE)
yield.scaled.N2O_mixture_wheat.df$se <- make_pct(yield.scaled.N2O_mixture_wheat_se.df$yield.scaled.N2O.wheat.mixture.relSE)
yield.scaled.N2O_mixture_wheat.df$upper <- yield.scaled.N2O_mixture_wheat.df$value+1.96*yield.scaled.N2O_mixture_wheat.df$se
ggplot(yield.scaled.N2O_mixture_wheat.df)+
  geom_density(aes(upper))
yield.scaled.N2O_mixture_wheat.df$lower <- yield.scaled.N2O_mixture_wheat.df$value-1.96*yield.scaled.N2O_mixture_wheat.df$se
ggplot(yield.scaled.N2O_mixture_wheat.df)+
  geom_density(aes(lower))
yield.scaled.N2O_mixture_wheat.df$sig1 <- ifelse(yield.scaled.N2O_mixture_wheat.df$upper<0,"*",NA)
yield.scaled.N2O_mixture_wheat.df$sig2 <- ifelse(yield.scaled.N2O_mixture_wheat.df$lower>0,"*",NA)

p.yield.scaled.N2O_mixture_wheat <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_mixture_wheat.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = "right",legend.text = element_text(size = 12),legend.title = element_text(angle = 90,size=13))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"BrBG",rev=T)[c(1,3,5,7,9:16)],
    na.value = NA,
    name = expression(paste("Yield-scaled ",N[2],"O emissions for wheat (%)",sep = "")),
    limits = c(-30, 60),
    breaks = seq(-30, 60, 15/2),
    labels = seq(-30, 60, 15/2) %>% sapply(function(x){
      if(x %% 15 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "vertical",
      nrow = 1,
      title.position = "left",
      label.position = "right",
      barheight = 15,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_mixture_wheat

### non-legume ###
yield.scaled.N2O_non_legume_wheat <- raster("yield.scaled.N2O-wheat-non-legume.relMean.tif")
yield.scaled.N2O_non_legume_wheat_robin <- raster::projectRaster(yield.scaled.N2O_non_legume_wheat, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_non_legume_wheat.df = as.data.frame(yield.scaled.N2O_non_legume_wheat_robin,xy=TRUE)
yield.scaled.N2O_non_legume_wheat.df$value <- make_pct(yield.scaled.N2O_non_legume_wheat.df$yield.scaled.N2O.wheat.non.legume.relMean)
ggplot(yield.scaled.N2O_non_legume_wheat.df)+
  geom_density(aes(value))
yield.scaled.N2O_non_legume_wheat_se <- raster("yield.scaled.N2O-wheat-non-legume.relSE.tif")
yield.scaled.N2O_non_legume_wheat_se_robin <- raster::projectRaster(yield.scaled.N2O_non_legume_wheat_se, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_non_legume_wheat_se.df = as.data.frame(yield.scaled.N2O_non_legume_wheat_se_robin,xy=TRUE)
yield.scaled.N2O_non_legume_wheat.df$se <- make_pct(yield.scaled.N2O_non_legume_wheat_se.df$yield.scaled.N2O.wheat.non.legume.relSE)
yield.scaled.N2O_non_legume_wheat.df$upper <- yield.scaled.N2O_non_legume_wheat.df$value+1.96*yield.scaled.N2O_non_legume_wheat.df$se
ggplot(yield.scaled.N2O_non_legume_wheat.df)+
  geom_density(aes(upper))
yield.scaled.N2O_non_legume_wheat.df$lower <- yield.scaled.N2O_non_legume_wheat.df$value-1.96*yield.scaled.N2O_non_legume_wheat.df$se
ggplot(yield.scaled.N2O_non_legume_wheat.df)+
  geom_density(aes(lower))
yield.scaled.N2O_non_legume_wheat.df$sig1 <- ifelse(yield.scaled.N2O_non_legume_wheat.df$upper<0,"*",NA)
yield.scaled.N2O_non_legume_wheat.df$sig2 <- ifelse(yield.scaled.N2O_non_legume_wheat.df$lower>0,"*",NA)

p.yield.scaled.N2O_non_legume_wheat <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_non_legume_wheat.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = "right",legend.text = element_text(size = 12),legend.title = element_text(angle = 90,size=13))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"BrBG",rev=T)[c(1,3,5,7,9:16)],
    na.value = NA,
    name = expression(paste("Yield-scaled ",N[2],"O emissions for wheat (%)",sep = "")),
    limits = c(-30, 60),
    breaks = seq(-30, 60, 15/2),
    labels = seq(-30, 60, 15/2) %>% sapply(function(x){
      if(x %% 15 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "vertical",
      nrow = 1,
      title.position = "left",
      label.position = "right",
      barheight = 15,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_non_legume_wheat

# ----------------------Maize--------------------------------------------
### legume ###
yield.scaled.N2O_legume_maize <- raster("yield.scaled.N2O-maize-legume.relMean.tif")
yield.scaled.N2O_legume_maize_robin <- raster::projectRaster(yield.scaled.N2O_legume_maize, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_legume_maize.df = as.data.frame(yield.scaled.N2O_legume_maize_robin,xy=TRUE)
yield.scaled.N2O_legume_maize.df$value <- make_pct(yield.scaled.N2O_legume_maize.df$yield.scaled.N2O.maize.legume.relMean)
ggplot(yield.scaled.N2O_legume_maize.df)+
  geom_density(aes(value))
yield.scaled.N2O_legume_maize_se <- raster("yield.scaled.N2O-maize-legume.relSE.tif")
yield.scaled.N2O_legume_maize_se_robin <- raster::projectRaster(yield.scaled.N2O_legume_maize_se, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_legume_maize_se.df = as.data.frame(yield.scaled.N2O_legume_maize_se_robin,xy=TRUE)
yield.scaled.N2O_legume_maize.df$se <- make_pct(yield.scaled.N2O_legume_maize_se.df$yield.scaled.N2O.maize.legume.relSE)
yield.scaled.N2O_legume_maize.df$upper <- yield.scaled.N2O_legume_maize.df$value+1.96*yield.scaled.N2O_legume_maize.df$se
ggplot(yield.scaled.N2O_legume_maize.df)+
  geom_density(aes(upper))
yield.scaled.N2O_legume_maize.df$lower <- yield.scaled.N2O_legume_maize.df$value-1.96*yield.scaled.N2O_legume_maize.df$se
ggplot(yield.scaled.N2O_legume_maize.df)+
  geom_density(aes(lower))
yield.scaled.N2O_legume_maize.df$sig1 <- ifelse(yield.scaled.N2O_legume_maize.df$upper<0,"*",NA)
yield.scaled.N2O_legume_maize.df$sig2 <- ifelse(yield.scaled.N2O_legume_maize.df$lower>0,"*",NA)


p.yield.scaled.N2O_legume_maize <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_legume_maize.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = "right",legend.text = element_text(size = 12),legend.title = element_text(angle = 90,size=13))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"RdYlBu",rev=T)[c(1,3,5,7,9:16)],
    na.value = NA,
    name = expression(paste("Yield-scaled ",N[2],"O emissions for maize (%)",sep = "")),
    limits = c(-30, 60),
    breaks = seq(-30, 60, 15/2),
    labels = seq(-30, 60, 15/2) %>% sapply(function(x){
      if(x %% 15 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "vertical",
      nrow = 1,
      title.position = "left",
      label.position = "right",
      barheight = 15,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_legume_maize

### mixture ###
yield.scaled.N2O_mixture_maize <- raster("yield.scaled.N2O-maize-mixture.relMean.tif")
yield.scaled.N2O_mixture_maize_robin <- raster::projectRaster(yield.scaled.N2O_mixture_maize, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_mixture_maize.df = as.data.frame(yield.scaled.N2O_mixture_maize_robin,xy=TRUE)
yield.scaled.N2O_mixture_maize.df$value <- make_pct(yield.scaled.N2O_mixture_maize.df$yield.scaled.N2O.maize.mixture.relMean)
ggplot(yield.scaled.N2O_mixture_maize.df)+
  geom_density(aes(value))
yield.scaled.N2O_mixture_maize_se <- raster("yield.scaled.N2O-maize-mixture.relSE.tif")
yield.scaled.N2O_mixture_maize_se_robin <- raster::projectRaster(yield.scaled.N2O_mixture_maize_se, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_mixture_maize_se.df = as.data.frame(yield.scaled.N2O_mixture_maize_se_robin,xy=TRUE)
yield.scaled.N2O_mixture_maize.df$se <- make_pct(yield.scaled.N2O_mixture_maize_se.df$yield.scaled.N2O.maize.mixture.relSE)
yield.scaled.N2O_mixture_maize.df$upper <- yield.scaled.N2O_mixture_maize.df$value+1.96*yield.scaled.N2O_mixture_maize.df$se
ggplot(yield.scaled.N2O_mixture_maize.df)+
  geom_density(aes(upper))
yield.scaled.N2O_mixture_maize.df$lower <- yield.scaled.N2O_mixture_maize.df$value-1.96*yield.scaled.N2O_mixture_maize.df$se
ggplot(yield.scaled.N2O_mixture_maize.df)+
  geom_density(aes(lower))
yield.scaled.N2O_mixture_maize.df$sig1 <- ifelse(yield.scaled.N2O_mixture_maize.df$upper<0,"*",NA)
yield.scaled.N2O_mixture_maize.df$sig2 <- ifelse(yield.scaled.N2O_mixture_maize.df$lower>0,"*",NA)

p.yield.scaled.N2O_mixture_maize <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_mixture_maize.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = "right",legend.text = element_text(size = 12),legend.title = element_text(angle = 90,size=13))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"RdYlBu",rev=T)[c(1,3,5,7,9:16)],
    na.value = NA,
    name = expression(paste("Yield-scaled ",N[2],"O emissions for maize (%)",sep = "")),
    limits = c(-30, 60),
    breaks = seq(-30, 60, 15/2),
    labels = seq(-30, 60, 15/2) %>% sapply(function(x){
      if(x %% 15 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "vertical",
      nrow = 1,
      title.position = "left",
      label.position = "right",
      barheight = 15,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_mixture_maize

### non-legume ###
yield.scaled.N2O_non_legume_maize <- raster("yield.scaled.N2O-maize-non-legume.relMean.tif")
yield.scaled.N2O_non_legume_maize_robin <- raster::projectRaster(yield.scaled.N2O_non_legume_maize, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_non_legume_maize.df = as.data.frame(yield.scaled.N2O_non_legume_maize_robin,xy=TRUE)
yield.scaled.N2O_non_legume_maize.df$value <- make_pct(yield.scaled.N2O_non_legume_maize.df$yield.scaled.N2O.maize.non.legume.relMean)
ggplot(yield.scaled.N2O_non_legume_maize.df)+
  geom_density(aes(value))
yield.scaled.N2O_non_legume_maize_se <- raster("yield.scaled.N2O-maize-non-legume.relSE.tif")
yield.scaled.N2O_non_legume_maize_se_robin <- raster::projectRaster(yield.scaled.N2O_non_legume_maize_se, crs = crs_robin, method = "bilinear")
yield.scaled.N2O_non_legume_maize_se.df = as.data.frame(yield.scaled.N2O_non_legume_maize_se_robin,xy=TRUE)
yield.scaled.N2O_non_legume_maize.df$se <- make_pct(yield.scaled.N2O_non_legume_maize_se.df$yield.scaled.N2O.maize.non.legume.relSE)
yield.scaled.N2O_non_legume_maize.df$upper <- yield.scaled.N2O_non_legume_maize.df$value+1.96*yield.scaled.N2O_non_legume_maize.df$se
ggplot(yield.scaled.N2O_non_legume_maize.df)+
  geom_density(aes(upper))
yield.scaled.N2O_non_legume_maize.df$lower <- yield.scaled.N2O_non_legume_maize.df$value-1.96*yield.scaled.N2O_non_legume_maize.df$se
ggplot(yield.scaled.N2O_non_legume_maize.df)+
  geom_density(aes(lower))
yield.scaled.N2O_non_legume_maize.df$sig1 <- ifelse(yield.scaled.N2O_non_legume_maize.df$upper<0,"*",NA)
yield.scaled.N2O_non_legume_maize.df$sig2 <- ifelse(yield.scaled.N2O_non_legume_maize.df$lower>0,"*",NA)

p.yield.scaled.N2O_non_legume_maize <- ggplot()+ 
  geom_sf(data=ocean,fill= "#e5e9f0", size = 0.1) +
  geom_sf(data=bbox,colour="black", fill="transparent", size = 0.3) +
  geom_sf(data=wmap,fill="white", color="black", size = 0.3) +
  #geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),fill="#ededed",color="black",size=0.1)+
  geom_raster(data=yield.scaled.N2O_non_legume_maize.df,aes(x,y,fill=value)) +
  coord_sf() +font+
  theme_void()+
  theme(legend.position = "right",legend.text = element_text(size = 12),legend.title = element_text(angle = 90,size=13))+
  scale_fill_stepsn(
    colors = hcl.colors(16,"RdYlBu",rev=T)[c(1,3,5,7,9:16)],
    na.value = NA,
    name = expression(paste("Yield-scaled ",N[2],"O emissions for maize (%)",sep = "")),
    limits = c(-30, 60),
    breaks = seq(-30, 60, 15/2),
    labels = seq(-30, 60, 15/2) %>% sapply(function(x){
      if(x %% 15 == 0) x
      else ''
    }),
    guide = guide_colorbar(
      direction = "vertical",
      nrow = 1,
      title.position = "left",
      label.position = "right",
      barheight = 15,
      ticks.colour = NA,
      ticks.linewidth = 0.3
    ))
p.yield.scaled.N2O_non_legume_maize


# ----------------------Regional applicability--------------------------------------------
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
env <- stack(AI,pH)
env <- resample(env,ref)
names(env) <- c("AI","pH")
env_robin <- raster::projectRaster(env, crs = crs_robin, method = "bilinear")
env.df <- as.data.frame(env_robin,xy=T)

wheat_legume <- left_join(yield.scaled.N2O_legume_wheat.df, env.df)%>%mutate(type="Legume")%>%select(value,AI,pH,type)
wheat_mixture <- left_join(yield.scaled.N2O_mixture_wheat.df, env.df)%>%mutate(type="Mixture")%>%select(value,AI,pH,type)
wheat_non_legume <- left_join(yield.scaled.N2O_non_legume_wheat.df, env.df)%>%mutate(type="Non-legume")%>%select(value,AI,pH,type)
maize_legume <- left_join(yield.scaled.N2O_legume_maize.df, env.df)%>%mutate(type="Legume")%>%select(value,AI,pH,type)
maize_mixture <- left_join(yield.scaled.N2O_mixture_maize.df, env.df)%>%mutate(type="Mixture")%>%select(value,AI,pH,type)
maize_non_legume <- left_join(yield.scaled.N2O_non_legume_maize.df, env.df)%>%mutate(type="Non-legume")%>%select(value,AI,pH,type)

wheat <- na.omit(rbind.data.frame(wheat_legume,wheat_mixture,wheat_non_legume))
maize <- na.omit(rbind.data.frame(maize_legume,maize_mixture,maize_non_legume))

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

wheat$AI2 <- factor(wheat$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
wheat$pH2 <- factor(wheat$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
  
box.wheat <- ggplot(data=wheat,aes(pH2, value)) + 
  geom_boxplot(show.legend = F,na.rm=T,outlier.alpha=0.1,aes(fill=type),alpha=0.75) +
  #geom_point(data=AMS.upland.continent.mean,aes(continent,mean),color="red",size=1.6)+
  scale_fill_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  ylab(expression(paste("Yield-scaled ",N[2],"O emissions (%)",sep = ""))) +
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #geom_text(data=AMS.upland.continent.mean,aes(x=continent,y=-50,label=round(mean,2)),
  #          color="red")+
  #theme_classic(base_size = 8) +
  facet_wrap(~AI2, nrow = 2, ncol = 2,scales = "free_y")+
  scale_y_continuous(breaks = c(-30,0,30,60,90))+
  #geom_signif(comparisons=list(c("upland","paddy")),
  #            test = t.test,tip_length = 0,
  #            na.rm=T,map_signif_level = c("***"=0.001,"**"=0.01,"*"=0.05))+
  theme_bw()+font+
  theme(axis.title.x = element_blank(),legend.position = c(0.9,1.25),
        legend.background = element_blank(),legend.key = element_blank(),legend.title = element_blank(),
        legend.box = "horizontal", panel.grid = element_blank(),
        axis.text.x = element_text(angle=30,vjust = 1,hjust=1),
        panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))
box.wheat

maize$AI2 <- factor(maize$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
maize$pH2 <- factor(maize$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))

box.maize <- ggplot(data=maize,aes(pH2, value)) + 
  geom_boxplot(show.legend = T,na.rm=T,outlier.alpha=0.1,aes(fill=type),alpha=0.75) +
  #geom_point(data=AMS.upland.continent.mean,aes(continent,mean),color="red",size=1.6)+
  scale_fill_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  ylab(expression(paste("Yield-scaled ",N[2],"O emissions (%)",sep = ""))) +
  geom_hline(yintercept = 0,linetype="dashed",color="black")+
  #geom_text(data=AMS.upland.continent.mean,aes(x=continent,y=-50,label=round(mean,2)),
  #          color="red")+
  #theme_classic(base_size = 8) +
  facet_wrap(~AI2, nrow = 2, ncol = 2,scales = "free_y")+
  scale_y_continuous(breaks = c(-30,0,30,60,90))+
  #geom_signif(comparisons=list(c("upland","paddy")),
  #            test = t.test,tip_length = 0,
  #            na.rm=T,map_signif_level = c("***"=0.001,"**"=0.01,"*"=0.05))+
  theme_bw()+font+
  theme(axis.title.x = element_blank(),legend.position = c(0.9,1.25),
        legend.background = element_blank(),legend.key = element_blank(),legend.title = element_blank(),
        legend.box = "horizontal", panel.grid = element_blank(),
        axis.text.x = element_text(angle=30,vjust = 1,hjust=1),
        panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))
box.maize


p1 <- lemon::grid_arrange_shared_legend(p.yield.scaled.N2O_legume_wheat,
                                        p.yield.scaled.N2O_mixture_wheat,
                                        p.yield.scaled.N2O_non_legume_wheat,
                                        nrow = 3,ncol=1,position = "right")

p2 <- lemon::grid_arrange_shared_legend(p.yield.scaled.N2O_legume_maize,
                                        p.yield.scaled.N2O_mixture_maize,
                                        p.yield.scaled.N2O_non_legume_maize,
                                        nrow = 3,ncol=1,position = "right")

ggdraw()+ 
  draw_plot(p1, x=0+0.015, y=2/5, width = 1/2-0.03, height = 3/5-0.02)+
  draw_plot(p2, x=1/2+0.015, y=2/5, width = 1/2-0.03, height = 3/5-0.02)+
  draw_plot(box.wheat, x=0, y=0, width = 1/2-0.01, height = 2/5)+
  draw_plot(box.maize, x=1/2, y=0, width = 1/2-0.01, height = 2/5)+
  draw_label("Legume CCs: 19.4%",size=12,fontface = "bold",x=0.25,y=0.845-0.01)+
  draw_label("CC mixtures: 19.6%",size=12,fontface = "bold",x=0.25,y=0.645-0.005)+
  draw_label("Non-legume CCs: 11.6%",size=12,fontface = "bold",x=0.255,y=0.445)+
  draw_label("Legume CCs: 15.5%",size=12,fontface = "bold",x=0.75,y=0.845-0.01)+
  draw_label("CC mixtures: 16.1%",size=12,fontface = "bold",x=0.75,y=0.645-0.005)+
  draw_label("Non-legume CCs: 11.9%",size=12,fontface = "bold",x=0.755,y=0.445)+
  draw_label("Wheat",size=16,fontface = "bold",x=0.22,y=0.985)+
  draw_label("Maize",size=16,fontface = "bold",x=0.72,y=0.985)+
  draw_plot_label(label = c("a","b","c","d","e"," f","g","h"), size = 15,
                  x=c(0,1/2,0,1/2,0,1/2,0,1/2),
                  y=c(1-0.02,1-0.02,4/5-0.015,4/5-0.015,3/5-0.01,3/5-0.01,2/5,2/5))##11.6*11.0inches
