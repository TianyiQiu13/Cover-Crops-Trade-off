library(metafor)
library(pbkrtest)
library(car)
library(Matrix)
library(ggplot2)
library(gridExtra)
library(reshape)
library(gam)
library(jsonlite)
library(MASS)
library(boot)
library(dplyr)
library(segmented)
library(chngpt)
library(ggpubr)
library(rlang)
library(DMwR)
library(raster)
library(mgcv)
library(patchwork)
library(cowplot)
library(gratia)

font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12))#11.6inches

# ----------------------Wheat--------------------------------------------
yield_legume_wheat <- raster("yield-wheat-legume.relMean.tif")
yield_mixture_wheat <- raster("yield-wheat-mixture.relMean.tif")
yield_non_legume_wheat <- raster("yield-wheat-non-legume.relMean.tif")
yield_legume_wheat.df = as.data.frame(yield_legume_wheat,xy=TRUE)%>%`colnames<-`(c("x","y","value1"))
yield_mixture_wheat.df = as.data.frame(yield_mixture_wheat,xy=TRUE)%>%`colnames<-`(c("x","y","value1"))
yield_non_legume_wheat.df = as.data.frame(yield_non_legume_wheat,xy=TRUE)%>%`colnames<-`(c("x","y","value1"))
yield.scaled.N2O_legume_wheat <- raster("yield.scaled.N2O-wheat-legume.relMean.tif")
yield.scaled.N2O_mixture_wheat <- raster("yield.scaled.N2O-wheat-mixture.relMean.tif")
yield.scaled.N2O_non_legume_wheat <- raster("yield.scaled.N2O-wheat-non-legume.relMean.tif")
yield.scaled.N2O_legume_wheat.df = as.data.frame(yield.scaled.N2O_legume_wheat,xy=TRUE)%>%`colnames<-`(c("x","y","value2"))
yield.scaled.N2O_mixture_wheat.df = as.data.frame(yield.scaled.N2O_mixture_wheat,xy=TRUE)%>%`colnames<-`(c("x","y","value2"))
yield.scaled.N2O_non_legume_wheat.df = as.data.frame(yield.scaled.N2O_non_legume_wheat,xy=TRUE)%>%`colnames<-`(c("x","y","value2"))

###Yield gap###
Ypot.wheat <- raster("E:/Data/Raster/CC-raster/Ypot.wheat.tif")
Y.wheat <- raster("E:/Data/Raster/CC-raster/Yield.wheat.tif")
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
N.rate <- raster("E:/Data/Raster/CC-raster/N.rate.wheat.1961-2020.tif")
env <- stack(AI,pH,N.rate,Ypot.wheat,Y.wheat)
env <- resample(env,ref)
names(env) <- c("AI","pH","N.rate","Ypot","Yield")
env.df <- as.data.frame(env,xy=T)

wheat_legume <- left_join(yield.scaled.N2O_legume_wheat.df, yield_legume_wheat.df)
wheat_mixture <- left_join(yield.scaled.N2O_mixture_wheat.df, yield_mixture_wheat.df)
wheat_non_legume <- left_join(yield.scaled.N2O_non_legume_wheat.df, yield_non_legume_wheat.df)
wheat_legume <- left_join(wheat_legume, env.df)%>%mutate(type="Legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,Ypot,Yield,type)
wheat_mixture <- left_join(wheat_mixture, env.df)%>%mutate(type="Mixture")%>%dplyr::select(value1,value2,AI,pH,N.rate,Ypot,Yield,type)
wheat_non_legume <- left_join(wheat_non_legume, env.df)%>%mutate(type="Non-legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,Ypot,Yield,type)
wheat <- na.omit(rbind.data.frame(wheat_legume,wheat_mixture,wheat_non_legume))

wheat$AI2 <- ifelse(wheat$AI<=0.2,"Arid",
                    ifelse(wheat$AI<=0.5,"Semi-arid",
                           ifelse(wheat$AI<=0.75,"Semi-humid","Humid")))
wheat$pH2 <- ifelse(wheat$pH<=5.5,"Strongly acidic",
                    ifelse(wheat$pH<6.5,"Moderately acidic",
                           ifelse(wheat$pH<=7.5,"Neutral","Alkaline")))
wheat$AI2 <- factor(wheat$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
wheat$pH2 <- factor(wheat$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
wheat$ecoregion <- ifelse(wheat$AI2=="Arid"&wheat$pH2=="Strongly acidic",1,
                       ifelse(wheat$AI2=="Arid"&wheat$pH2=="Moderately acidic",2,
                              ifelse(wheat$AI2=="Arid"&wheat$pH2=="Neutral",3,
                                     ifelse(wheat$AI2=="Arid"&wheat$pH2=="Alkaline",4,
                                            ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Strongly acidic",5,
                                                   ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Moderately acidic",6,
                                                          ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Neutral",7,
                                                                 ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Alkaline",8,
                                                                        ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Strongly acidic",9,
                                                                               ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Moderately acidic",10,
                                                                                      ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Neutral",11,
                                                                                             ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Alkaline",12,
                                                                                                    ifelse(wheat$AI2=="Humid"&wheat$pH2=="Strongly acidic",13,
                                                                                                           ifelse(wheat$AI2=="Humid"&wheat$pH2=="Moderately acidic",14,15))))))))))))))

wheat$ecoregion <- as.factor(wheat$ecoregion)
wheat$type <- as.factor(wheat$type)

ggplot(wheat)+
  theme_bw()+
  geom_point(aes(x=N.rate,y=value1-log(Ypot*0.75)+log(Yield),color=type,shape=type),size=2.2,fill=NA)+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  facet_wrap(~ecoregion,scales = "free_x")+
  #geom_smooth(aes(x=N.rate,y=value1-log(Ypot*0.65)+log(Yield),color=type),size=1,linetype=2,method = "lm",formula = y~poly(x,2))+
  #geom_smooth(aes(x=N.rate,y=value2,color=type),size=1,linetype=1,method = "gam",formula = y~s(x))+
  #geom_vline(xintercept = 0.46,linetype=2,color="black",size=0.6)+
  scale_y_continuous(limits = c(-9,2))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  scale_shape_manual(values = c(22,23,21)) + 
  labs(x="N application rate (kg N/ha)",y=("LnRR of yield - ln(75% attainable yield/actual yield)"),
       color="CC type",shape="CC type")+font+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))+
  theme(legend.position = c(0.875,0.125),legend.background = element_blank(),legend.key = element_blank())###11.6*11.0inches

wheat$value3 <- wheat$value1-log(wheat$Ypot*0.75)+log(wheat$Yield)
w1 <- wheat%>%filter(ecoregion=="1");write.csv(w1,"ecoregion1-N rate.csv")
w2 <- wheat%>%filter(ecoregion=="2");write.csv(w2,"ecoregion2-N rate.csv")
w3 <- wheat%>%filter(ecoregion=="3");write.csv(w3,"ecoregion3-N rate.csv")
w4 <- wheat%>%filter(ecoregion=="4");write.csv(w4,"ecoregion4-N rate.csv")
w5 <- wheat%>%filter(ecoregion=="5");write.csv(w5,"ecoregion5-N rate.csv")
w6 <- wheat%>%filter(ecoregion=="6");write.csv(w6,"ecoregion6-N rate.csv")
w7 <- wheat%>%filter(ecoregion=="7");write.csv(w7,"ecoregion7-N rate.csv")
w8 <- wheat%>%filter(ecoregion=="8");write.csv(w8,"ecoregion8-N rate.csv")
w9 <- wheat%>%filter(ecoregion=="9");write.csv(w9,"ecoregion9-N rate.csv")
w10 <- wheat%>%filter(ecoregion=="10");write.csv(w10,"ecoregion10-N rate.csv")
w11 <- wheat%>%filter(ecoregion=="11");write.csv(w11,"ecoregion11-N rate.csv")
w12 <- wheat%>%filter(ecoregion=="12");write.csv(w12,"ecoregion12-N rate.csv")
w13 <- wheat%>%filter(ecoregion=="13");write.csv(w13,"ecoregion13-N rate.csv")
w14 <- wheat%>%filter(ecoregion=="14");write.csv(w14,"ecoregion14-N rate.csv")
w15 <- wheat%>%filter(ecoregion=="15");write.csv(w15,"ecoregion15-N rate.csv")

###Yield-scaled N2O###
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
N.rate <- raster("E:/Data/Raster/CC-raster/N.rate.wheat.1961-2020.tif")
env <- stack(AI,pH,N.rate)
env <- resample(env,ref)
names(env) <- c("AI","pH","N.rate")
env.df <- as.data.frame(env,xy=T)

wheat_legume <- left_join(yield.scaled.N2O_legume_wheat.df, yield_legume_wheat.df)
wheat_mixture <- left_join(yield.scaled.N2O_mixture_wheat.df, yield_mixture_wheat.df)
wheat_non_legume <- left_join(yield.scaled.N2O_non_legume_wheat.df, yield_non_legume_wheat.df)
wheat_legume <- left_join(wheat_legume, env.df)%>%mutate(type="Legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,type)
wheat_mixture <- left_join(wheat_mixture, env.df)%>%mutate(type="Mixture")%>%dplyr::select(value1,value2,AI,pH,N.rate,type)
wheat_non_legume <- left_join(wheat_non_legume, env.df)%>%mutate(type="Non-legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,type)
wheat <- na.omit(rbind.data.frame(wheat_legume,wheat_mixture,wheat_non_legume))

wheat$AI2 <- ifelse(wheat$AI<=0.2,"Arid",
                    ifelse(wheat$AI<=0.5,"Semi-arid",
                           ifelse(wheat$AI<=0.75,"Semi-humid","Humid")))
wheat$pH2 <- ifelse(wheat$pH<=5.5,"Strongly acidic",
                    ifelse(wheat$pH<6.5,"Moderately acidic",
                           ifelse(wheat$pH<=7.5,"Neutral","Alkaline")))
wheat$AI2 <- factor(wheat$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
wheat$pH2 <- factor(wheat$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
wheat$ecoregion <- ifelse(wheat$AI2=="Arid"&wheat$pH2=="Strongly acidic",1,
                          ifelse(wheat$AI2=="Arid"&wheat$pH2=="Moderately acidic",2,
                                 ifelse(wheat$AI2=="Arid"&wheat$pH2=="Neutral",3,
                                        ifelse(wheat$AI2=="Arid"&wheat$pH2=="Alkaline",4,
                                               ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Strongly acidic",5,
                                                      ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Moderately acidic",6,
                                                             ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Neutral",7,
                                                                    ifelse(wheat$AI2=="Semi-arid"&wheat$pH2=="Alkaline",8,
                                                                           ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Strongly acidic",9,
                                                                                  ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Moderately acidic",10,
                                                                                         ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Neutral",11,
                                                                                                ifelse(wheat$AI2=="Semi-humid"&wheat$pH2=="Alkaline",12,
                                                                                                       ifelse(wheat$AI2=="Humid"&wheat$pH2=="Strongly acidic",13,
                                                                                                              ifelse(wheat$AI2=="Humid"&wheat$pH2=="Moderately acidic",14,15))))))))))))))

wheat$ecoregion <- as.factor(wheat$ecoregion)
wheat$type <- as.factor(wheat$type)

###Ecoregion1###
wheat1 <- filter(wheat,ecoregion=="1")

model1 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat1)

ecoregion1 <- data.frame(N.rate = seq(min(wheat1$N.rate), max(wheat1$N.rate), length.out = 1000))
ecoregion1_legume <- ecoregion1%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred1_legume <- predict(model1, newdata = ecoregion1_legume, type = "response")
pred1_legume_df <- data.frame(N.rate=ecoregion1_legume$N.rate,value=pred1_legume)%>%dplyr::mutate(type="Legume")
ecoregion1_mixture <- ecoregion1%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred1_mixture <- predict(model1, newdata = ecoregion1_mixture, type = "response")
pred1_mixture_df <- data.frame(N.rate=ecoregion1_mixture$N.rate,value=pred1_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion1_non_legume <- ecoregion1%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred1_non_legume <- predict(model1, newdata = ecoregion1_non_legume, type = "response")
pred1_non_legume_df <- data.frame(N.rate=ecoregion1_non_legume$N.rate,value=pred1_non_legume)%>%dplyr::mutate(type="Non-legume")

pred1_df <- rbind.data.frame(pred1_legume_df,pred1_mixture_df,pred1_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")

p1 <- ggplot(pred1_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=7.58,xmax=22.0,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  #geom_vline(xintercept=7.58,linetype="dashed",color="#48C9B0")+
  #geom_vline(xintercept=7.58,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=22.0,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.16,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-1.1,1.1))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p1


###Ecoregion2###
wheat2 <- filter(wheat,ecoregion=="2")

model2 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat2)

ecoregion2 <- data.frame(N.rate = seq(min(wheat2$N.rate), max(wheat2$N.rate), length.out = 1000))
ecoregion2_legume <- ecoregion2%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred2_legume <- predict(model2, newdata = ecoregion2_legume, type = "response")
pred2_legume_df <- data.frame(N.rate=ecoregion2_legume$N.rate,value=pred2_legume)%>%dplyr::mutate(type="Legume")
ecoregion2_mixture <- ecoregion2%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred2_mixture <- predict(model2, newdata = ecoregion2_mixture, type = "response")
pred2_mixture_df <- data.frame(N.rate=ecoregion2_mixture$N.rate,value=pred2_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion2_non_legume <- ecoregion2%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred2_non_legume <- predict(model2, newdata = ecoregion2_non_legume, type = "response")
pred2_non_legume_df <- data.frame(N.rate=ecoregion2_non_legume$N.rate,value=pred2_non_legume)%>%dplyr::mutate(type="Non-legume")

pred2_df <- rbind.data.frame(pred2_legume_df,pred2_mixture_df,pred2_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p2 <- ggplot(pred2_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=9.46,xmax=29.4,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  #geom_vline(xintercept=9.46,linetype="dashed",color="#48C9B0")+
  #geom_vline(xintercept=9.46,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=9.46,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.26,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-2.2,2.2))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p2


###Ecoregion3###
wheat3 <- filter(wheat,ecoregion=="3")

model3 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat3)
deriv3 <- derivatives(model3)
print(deriv3,n=300)

ecoregion3 <- data.frame(N.rate = seq(min(wheat3$N.rate), max(wheat3$N.rate), length.out = 1000))
ecoregion3_legume <- ecoregion3%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred3_legume <- predict(model3, newdata = ecoregion3_legume, type = "response")
pred3_legume_df <- data.frame(N.rate=ecoregion3_legume$N.rate,value=pred3_legume)%>%dplyr::mutate(type="Legume")
ecoregion3_mixture <- ecoregion3%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred3_mixture <- predict(model3, newdata = ecoregion3_mixture, type = "response")
pred3_mixture_df <- data.frame(N.rate=ecoregion3_mixture$N.rate,value=pred3_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion3_non_legume <- ecoregion3%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred3_non_legume <- predict(model3, newdata = ecoregion3_non_legume, type = "response")
pred3_non_legume_df <- data.frame(N.rate=ecoregion3_non_legume$N.rate,value=pred3_non_legume)%>%dplyr::mutate(type="Non-legume")

pred3_df <- rbind.data.frame(pred3_legume_df,pred3_mixture_df,pred3_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p3 <- ggplot(pred3_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0.59,xmax=81.8,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=81.8,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=81.8,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=81.8,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.36,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-3.3,3.3))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p3


###Ecoregion4###
wheat4 <- filter(wheat,ecoregion=="4")

model4 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat4)
deriv4 <- derivatives(model4)
print(deriv4,n=300)

ecoregion4 <- data.frame(N.rate = seq(min(wheat4$N.rate), max(wheat4$N.rate), length.out = 1000))
ecoregion4_legume <- ecoregion4%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred4_legume <- predict(model4, newdata = ecoregion4_legume, type = "response")
pred4_legume_df <- data.frame(N.rate=ecoregion4_legume$N.rate,value=pred4_legume)%>%dplyr::mutate(type="Legume")
ecoregion4_mixture <- ecoregion4%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred4_mixture <- predict(model4, newdata = ecoregion4_mixture, type = "response")
pred4_mixture_df <- data.frame(N.rate=ecoregion4_mixture$N.rate,value=pred4_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion4_non_legume <- ecoregion4%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred4_non_legume <- predict(model4, newdata = ecoregion4_non_legume, type = "response")
pred4_non_legume_df <- data.frame(N.rate=ecoregion4_non_legume$N.rate,value=pred4_non_legume)%>%dplyr::mutate(type="Non-legume")

pred4_df <- rbind.data.frame(pred4_legume_df,pred4_mixture_df,pred4_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p4 <- ggplot(pred4_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0,xmax=124,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=54.9,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=54.9,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=50.9,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.46,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-4.4,4.4))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p4


###Ecoregion5###
wheat5 <- filter(wheat,ecoregion=="5")

model5 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat5)

ecoregion5 <- data.frame(N.rate = seq(min(wheat5$N.rate), max(wheat5$N.rate), length.out = 1000))
ecoregion5_legume <- ecoregion5%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred5_legume <- predict(model5, newdata = ecoregion5_legume, type = "response")
pred5_legume_df <- data.frame(N.rate=ecoregion5_legume$N.rate,value=pred5_legume)%>%dplyr::mutate(type="Legume")
ecoregion5_mixture <- ecoregion5%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred5_mixture <- predict(model5, newdata = ecoregion5_mixture, type = "response")
pred5_mixture_df <- data.frame(N.rate=ecoregion5_mixture$N.rate,value=pred5_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion5_non_legume <- ecoregion5%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred5_non_legume <- predict(model5, newdata = ecoregion5_non_legume, type = "response")
pred5_non_legume_df <- data.frame(N.rate=ecoregion5_non_legume$N.rate,value=pred5_non_legume)%>%dplyr::mutate(type="Non-legume")

pred5_df <- rbind.data.frame(pred5_legume_df,pred5_mixture_df,pred5_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p5 <- ggplot(pred5_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=1.94,xmax=17.2,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=1.94,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=1.94,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=1.94,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.56,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-5.5,5.5))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p5


###Ecoregion6###
wheat6 <- filter(wheat,ecoregion=="6")

model6 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat6)

ecoregion6 <- data.frame(N.rate = seq(min(wheat6$N.rate), max(wheat6$N.rate), length.out = 1000))
ecoregion6_legume <- ecoregion6%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred6_legume <- predict(model6, newdata = ecoregion6_legume, type = "response")
pred6_legume_df <- data.frame(N.rate=ecoregion6_legume$N.rate,value=pred6_legume)%>%dplyr::mutate(type="Legume")
ecoregion6_mixture <- ecoregion6%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred6_mixture <- predict(model6, newdata = ecoregion6_mixture, type = "response")
pred6_mixture_df <- data.frame(N.rate=ecoregion6_mixture$N.rate,value=pred6_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion6_non_legume <- ecoregion6%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred6_non_legume <- predict(model6, newdata = ecoregion6_non_legume, type = "response")
pred6_non_legume_df <- data.frame(N.rate=ecoregion6_non_legume$N.rate,value=pred6_non_legume)%>%dplyr::mutate(type="Non-legume")

pred6_df <- rbind.data.frame(pred6_legume_df,pred6_mixture_df,pred6_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p6 <- ggplot(pred6_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=1.67,xmax=45.7,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=1.67,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=1.67,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=1.67,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.66,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-6.6,6.6))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p6


###Ecoregion7###
wheat7 <- filter(wheat,ecoregion=="7")

model7 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat7)

ecoregion7 <- data.frame(N.rate = seq(min(wheat7$N.rate), max(wheat7$N.rate), length.out = 1000))
ecoregion7_legume <- ecoregion7%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred7_legume <- predict(model7, newdata = ecoregion7_legume, type = "response")
pred7_legume_df <- data.frame(N.rate=ecoregion7_legume$N.rate,value=pred7_legume)%>%dplyr::mutate(type="Legume")
ecoregion7_mixture <- ecoregion7%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred7_mixture <- predict(model7, newdata = ecoregion7_mixture, type = "response")
pred7_mixture_df <- data.frame(N.rate=ecoregion7_mixture$N.rate,value=pred7_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion7_non_legume <- ecoregion7%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred7_non_legume <- predict(model7, newdata = ecoregion7_non_legume, type = "response")
pred7_non_legume_df <- data.frame(N.rate=ecoregion7_non_legume$N.rate,value=pred7_non_legume)%>%dplyr::mutate(type="Non-legume")

pred7_df <- rbind.data.frame(pred7_legume_df,pred7_mixture_df,pred7_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p7 <- ggplot(pred7_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0.51,xmax=96.3,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=96.3,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=96.3,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=96.3,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.76,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-7.7,7.7))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p7


###Ecoregion8###
wheat8 <- filter(wheat,ecoregion=="8")

model8 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat8)
deriv8 <- derivatives(model8)
print(deriv8,n=300)

ecoregion8 <- data.frame(N.rate = seq(min(wheat8$N.rate), max(wheat8$N.rate), length.out = 1000))
ecoregion8_legume <- ecoregion8%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred8_legume <- predict(model8, newdata = ecoregion8_legume, type = "response")
pred8_legume_df <- data.frame(N.rate=ecoregion8_legume$N.rate,value=pred8_legume)%>%dplyr::mutate(type="Legume")
ecoregion8_mixture <- ecoregion8%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred8_mixture <- predict(model8, newdata = ecoregion8_mixture, type = "response")
pred8_mixture_df <- data.frame(N.rate=ecoregion8_mixture$N.rate,value=pred8_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion8_non_legume <- ecoregion8%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred8_non_legume <- predict(model8, newdata = ecoregion8_non_legume, type = "response")
pred8_non_legume_df <- data.frame(N.rate=ecoregion8_non_legume$N.rate,value=pred8_non_legume)%>%dplyr::mutate(type="Non-legume")

pred8_df <- rbind.data.frame(pred8_legume_df,pred8_mixture_df,pred8_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p8 <- ggplot(pred8_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=1.24,xmax=74.5,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=20.2,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=20.2,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=20.2,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.86,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-8.8,8.8))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p8


###Ecoregion9###
wheat9 <- filter(wheat,ecoregion=="9")

model9 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat9)

ecoregion9 <- data.frame(N.rate = seq(min(wheat9$N.rate), max(wheat9$N.rate), length.out = 1000))
ecoregion9_legume <- ecoregion9%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred9_legume <- predict(model9, newdata = ecoregion9_legume, type = "response")
pred9_legume_df <- data.frame(N.rate=ecoregion9_legume$N.rate,value=pred9_legume)%>%dplyr::mutate(type="Legume")
ecoregion9_mixture <- ecoregion9%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred9_mixture <- predict(model9, newdata = ecoregion9_mixture, type = "response")
pred9_mixture_df <- data.frame(N.rate=ecoregion9_mixture$N.rate,value=pred9_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion9_non_legume <- ecoregion9%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred9_non_legume <- predict(model9, newdata = ecoregion9_non_legume, type = "response")
pred9_non_legume_df <- data.frame(N.rate=ecoregion9_non_legume$N.rate,value=pred9_non_legume)%>%dplyr::mutate(type="Non-legume")

pred9_df <- rbind.data.frame(pred9_legume_df,pred9_mixture_df,pred9_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p9 <- ggplot(pred9_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=12.6,xmax=38.1,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=12.6,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=12.6,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=38.1,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.96,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-9.9,9.9))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p9


###Ecoregion10###
wheat10 <- filter(wheat,ecoregion=="10")

model10 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat10)
deriv10 <- derivatives(model10)
print(deriv10,n=300)

ecoregion10 <- data.frame(N.rate = seq(min(wheat10$N.rate), max(wheat10$N.rate), length.out = 1000))
ecoregion10_legume <- ecoregion10%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred10_legume <- predict(model10, newdata = ecoregion10_legume, type = "response")
pred10_legume_df <- data.frame(N.rate=ecoregion10_legume$N.rate,value=pred10_legume)%>%dplyr::mutate(type="Legume")
ecoregion10_mixture <- ecoregion10%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred10_mixture <- predict(model10, newdata = ecoregion10_mixture, type = "response")
pred10_mixture_df <- data.frame(N.rate=ecoregion10_mixture$N.rate,value=pred10_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion10_non_legume <- ecoregion10%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred10_non_legume <- predict(model10, newdata = ecoregion10_non_legume, type = "response")
pred10_non_legume_df <- data.frame(N.rate=ecoregion10_non_legume$N.rate,value=pred10_non_legume)%>%dplyr::mutate(type="Non-legume")

pred10_df <- rbind.data.frame(pred10_legume_df,pred10_mixture_df,pred10_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p10 <- ggplot(pred10_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=1.61,xmax=100,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=43.9,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=43.9,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=51.5,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.106,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-10.10,10.10))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p10


###Ecoregion11###
wheat11 <- filter(wheat,ecoregion=="11")

model11 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat11)
deriv11 <- derivatives(model11)
print(deriv11,n=300)

ecoregion11 <- data.frame(N.rate = seq(min(wheat11$N.rate), max(wheat11$N.rate), length.out = 1000))
ecoregion11_legume <- ecoregion11%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred11_legume <- predict(model11, newdata = ecoregion11_legume, type = "response")
pred11_legume_df <- data.frame(N.rate=ecoregion11_legume$N.rate,value=pred11_legume)%>%dplyr::mutate(type="Legume")
ecoregion11_mixture <- ecoregion11%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred11_mixture <- predict(model11, newdata = ecoregion11_mixture, type = "response")
pred11_mixture_df <- data.frame(N.rate=ecoregion11_mixture$N.rate,value=pred11_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion11_non_legume <- ecoregion11%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred11_non_legume <- predict(model11, newdata = ecoregion11_non_legume, type = "response")
pred11_non_legume_df <- data.frame(N.rate=ecoregion11_non_legume$N.rate,value=pred11_non_legume)%>%dplyr::mutate(type="Non-legume")

pred11_df <- rbind.data.frame(pred11_legume_df,pred11_mixture_df,pred11_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p11 <- ggplot(pred11_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=2.46,xmax=58.6,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=50.5,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=50.5,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=10.1,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.116,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-11.11,11.11))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p11


###Ecoregion12###
wheat12 <- filter(wheat,ecoregion=="12")

model12 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat12)
deriv12 <- derivatives(model12)
print(deriv12,n=300)

ecoregion12 <- data.frame(N.rate = seq(min(wheat12$N.rate), max(wheat12$N.rate), length.out = 1000))
ecoregion12_legume <- ecoregion12%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred12_legume <- predict(model12, newdata = ecoregion12_legume, type = "response")
pred12_legume_df <- data.frame(N.rate=ecoregion12_legume$N.rate,value=pred12_legume)%>%dplyr::mutate(type="Legume")
ecoregion12_mixture <- ecoregion12%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred12_mixture <- predict(model12, newdata = ecoregion12_mixture, type = "response")
pred12_mixture_df <- data.frame(N.rate=ecoregion12_mixture$N.rate,value=pred12_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion12_non_legume <- ecoregion12%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred12_non_legume <- predict(model12, newdata = ecoregion12_non_legume, type = "response")
pred12_non_legume_df <- data.frame(N.rate=ecoregion12_non_legume$N.rate,value=pred12_non_legume)%>%dplyr::mutate(type="Non-legume")

pred12_df <- rbind.data.frame(pred12_legume_df,pred12_mixture_df,pred12_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p12 <- ggplot(pred12_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=41.8,xmax=65.1,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  #geom_vline(xintercept=59.0,linetype="dashed",color="#48C9B0")+
  #geom_vline(xintercept=59.0,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=59.7,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.126,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-12.12,12.12))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p12


###Ecoregion13###
wheat13 <- filter(wheat,ecoregion=="13")

model13 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat13)
deriv13 <- derivatives(model13)
print(deriv13,n=300)

ecoregion13 <- data.frame(N.rate = seq(min(wheat13$N.rate), max(wheat13$N.rate), length.out = 1000))
ecoregion13_legume <- ecoregion13%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred13_legume <- predict(model13, newdata = ecoregion13_legume, type = "response")
pred13_legume_df <- data.frame(N.rate=ecoregion13_legume$N.rate,value=pred13_legume)%>%dplyr::mutate(type="Legume")
ecoregion13_mixture <- ecoregion13%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred13_mixture <- predict(model13, newdata = ecoregion13_mixture, type = "response")
pred13_mixture_df <- data.frame(N.rate=ecoregion13_mixture$N.rate,value=pred13_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion13_non_legume <- ecoregion13%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred13_non_legume <- predict(model13, newdata = ecoregion13_non_legume, type = "response")
pred13_non_legume_df <- data.frame(N.rate=ecoregion13_non_legume$N.rate,value=pred13_non_legume)%>%dplyr::mutate(type="Non-legume")

pred13_df <- rbind.data.frame(pred13_legume_df,pred13_mixture_df,pred13_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p13 <- ggplot(pred13_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0.53,xmax=142,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=66.8,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=63.4,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=46.3,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.136,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-13.13,13.13))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p13


###Ecoregion14###
wheat14 <- filter(wheat,ecoregion=="14")

model14 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat14)
deriv14 <- derivatives(model14)
print(deriv14,n=300)

ecoregion14 <- data.frame(N.rate = seq(min(wheat14$N.rate), max(wheat14$N.rate), length.out = 1000))
ecoregion14_legume <- ecoregion14%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred14_legume <- predict(model14, newdata = ecoregion14_legume, type = "response")
pred14_legume_df <- data.frame(N.rate=ecoregion14_legume$N.rate,value=pred14_legume)%>%dplyr::mutate(type="Legume")
ecoregion14_mixture <- ecoregion14%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred14_mixture <- predict(model14, newdata = ecoregion14_mixture, type = "response")
pred14_mixture_df <- data.frame(N.rate=ecoregion14_mixture$N.rate,value=pred14_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion14_non_legume <- ecoregion14%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred14_non_legume <- predict(model14, newdata = ecoregion14_non_legume, type = "response")
pred14_non_legume_df <- data.frame(N.rate=ecoregion14_non_legume$N.rate,value=pred14_non_legume)%>%dplyr::mutate(type="Non-legume")

pred14_df <- rbind.data.frame(pred14_legume_df,pred14_mixture_df,pred14_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p14 <- ggplot(pred14_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=1.32,xmax=163,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=33.1,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=40.0,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=40.0,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.146,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-14.14,14.14))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p14


###Ecoregion15###
wheat15 <- filter(wheat,ecoregion=="15")

model15 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = wheat15)

ecoregion15 <- data.frame(N.rate = seq(min(wheat15$N.rate), max(wheat15$N.rate), length.out = 1000))
ecoregion15_legume <- ecoregion15%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred15_legume <- predict(model15, newdata = ecoregion15_legume, type = "response")
pred15_legume_df <- data.frame(N.rate=ecoregion15_legume$N.rate,value=pred15_legume)%>%dplyr::mutate(type="Legume")
ecoregion15_mixture <- ecoregion15%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred15_mixture <- predict(model15, newdata = ecoregion15_mixture, type = "response")
pred15_mixture_df <- data.frame(N.rate=ecoregion15_mixture$N.rate,value=pred15_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion15_non_legume <- ecoregion15%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred15_non_legume <- predict(model15, newdata = ecoregion15_non_legume, type = "response")
pred15_non_legume_df <- data.frame(N.rate=ecoregion15_non_legume$N.rate,value=pred15_non_legume)%>%dplyr::mutate(type="Non-legume")

pred15_df <- rbind.data.frame(pred15_legume_df,pred15_mixture_df,pred15_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p15 <- ggplot(pred15_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=4.51,xmax=48.8,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=4.51,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=4.51,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=4.51,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.156,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-15.15,15.15))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p15


ggdraw()+ 
  draw_plot(p1, x=0, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p2, x=1/4, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p3, x=2/4, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p4, x=3/4, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p5, x=0, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p6, x=1/4, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p7, x=2/4, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p8, x=3/4, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p9, x=0, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p10, x=1/4, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p11, x=2/4, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p12, x=3/4, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p13, x=0, y=0, width = 1/4, height = 1/4)+
  draw_plot(p14, x=1/4, y=0, width = 1/4, height = 1/4)+
  draw_plot(p15, x=2/4, y=0, width = 1/4, height = 1/4)##11.6*11.0inches


# ----------------------Maize--------------------------------------------
yield_legume_maize <- raster("yield-maize-legume.relMean.tif")
yield_mixture_maize <- raster("yield-maize-mixture.relMean.tif")
yield_non_legume_maize <- raster("yield-maize-non-legume.relMean.tif")
yield_legume_maize.df = as.data.frame(yield_legume_maize,xy=TRUE)%>%`colnames<-`(c("x","y","value1"))
yield_mixture_maize.df = as.data.frame(yield_mixture_maize,xy=TRUE)%>%`colnames<-`(c("x","y","value1"))
yield_non_legume_maize.df = as.data.frame(yield_non_legume_maize,xy=TRUE)%>%`colnames<-`(c("x","y","value1"))
yield.scaled.N2O_legume_maize <- raster("yield.scaled.N2O-maize-legume.relMean.tif")
yield.scaled.N2O_mixture_maize <- raster("yield.scaled.N2O-maize-mixture.relMean.tif")
yield.scaled.N2O_non_legume_maize <- raster("yield.scaled.N2O-maize-non-legume.relMean.tif")
yield.scaled.N2O_legume_maize.df = as.data.frame(yield.scaled.N2O_legume_maize,xy=TRUE)%>%`colnames<-`(c("x","y","value2"))
yield.scaled.N2O_mixture_maize.df = as.data.frame(yield.scaled.N2O_mixture_maize,xy=TRUE)%>%`colnames<-`(c("x","y","value2"))
yield.scaled.N2O_non_legume_maize.df = as.data.frame(yield.scaled.N2O_non_legume_maize,xy=TRUE)%>%`colnames<-`(c("x","y","value2"))

###Yield gap###
Ypot.maize <- raster("E:/Data/Raster/CC-raster/Ypot.maize.tif")
Y.maize <- raster("E:/Data/Raster/CC-raster/Yield.maize.tif")
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
N.rate <- raster("E:/Data/Raster/CC-raster/N.rate.maize.1961-2020.tif")
env <- stack(AI,pH,N.rate,Ypot.maize,Y.maize)
env <- resample(env,ref)
names(env) <- c("AI","pH","N.rate","Ypot","Yield")
env.df <- as.data.frame(env,xy=T)

maize_legume <- left_join(yield.scaled.N2O_legume_maize.df, yield_legume_maize.df)
maize_mixture <- left_join(yield.scaled.N2O_mixture_maize.df, yield_mixture_maize.df)
maize_non_legume <- left_join(yield.scaled.N2O_non_legume_maize.df, yield_non_legume_maize.df)
maize_legume <- left_join(maize_legume, env.df)%>%mutate(type="Legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,Ypot,Yield,type)
maize_mixture <- left_join(maize_mixture, env.df)%>%mutate(type="Mixture")%>%dplyr::select(value1,value2,AI,pH,N.rate,Ypot,Yield,type)
maize_non_legume <- left_join(maize_non_legume, env.df)%>%mutate(type="Non-legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,Ypot,Yield,type)
maize <- na.omit(rbind.data.frame(maize_legume,maize_mixture,maize_non_legume))

maize$AI2 <- ifelse(maize$AI<=0.2,"Arid",
                    ifelse(maize$AI<=0.5,"Semi-arid",
                           ifelse(maize$AI<=0.75,"Semi-humid","Humid")))
maize$pH2 <- ifelse(maize$pH<=5.5,"Strongly acidic",
                    ifelse(maize$pH<6.5,"Moderately acidic",
                           ifelse(maize$pH<=7.5,"Neutral","Alkaline")))
maize$AI2 <- factor(maize$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
maize$pH2 <- factor(maize$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
maize$ecoregion <- ifelse(maize$AI2=="Arid"&maize$pH2=="Strongly acidic",1,
                          ifelse(maize$AI2=="Arid"&maize$pH2=="Moderately acidic",2,
                                 ifelse(maize$AI2=="Arid"&maize$pH2=="Neutral",3,
                                        ifelse(maize$AI2=="Arid"&maize$pH2=="Alkaline",4,
                                               ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Strongly acidic",5,
                                                      ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Moderately acidic",6,
                                                             ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Neutral",7,
                                                                    ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Alkaline",8,
                                                                           ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Strongly acidic",9,
                                                                                  ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Moderately acidic",10,
                                                                                         ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Neutral",11,
                                                                                                ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Alkaline",12,
                                                                                                       ifelse(maize$AI2=="Humid"&maize$pH2=="Strongly acidic",13,
                                                                                                              ifelse(maize$AI2=="Humid"&maize$pH2=="Moderately acidic",14,15))))))))))))))

maize$ecoregion <- as.factor(maize$ecoregion)
maize$type <- as.factor(maize$type)

ggplot(maize)+
  theme_bw()+
  geom_point(aes(x=N.rate,y=value1-log(Ypot*0.75)+log(Yield),color=type,shape=type),size=2.2,fill=NA)+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  facet_wrap(~ecoregion,scales = "free_x")+
  #geom_smooth(aes(x=N.rate,y=value1-log(Ypot*0.65)+log(Yield),color=type),size=1,linetype=2,method = "lm",formula = y~poly(x,2))+
  #geom_smooth(aes(x=N.rate,y=value2,color=type),size=1,linetype=1,method = "gam",formula = y~s(x))+
  #geom_vline(xintercept = 0.46,linetype=2,color="black",size=0.6)+
  scale_y_continuous(limits = c(-9,2))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  scale_shape_manual(values = c(22,23,21)) + 
  labs(x="N application rate (kg N/ha)",y=("LnRR of yield - ln(75% attainable yield/actual yield)"),
       color="CC type",shape="CC type")+font+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))+
  theme(legend.position = c(0.875,0.125),legend.background = element_blank(),legend.key = element_blank())###11.6*11.0inches

maize$value3 <- maize$value1-log(maize$Ypot*0.75)+log(maize$Yield)
w1 <- maize%>%filter(ecoregion=="1");write.csv(w1,"ecoregion1-N rate-maize.csv")
w2 <- maize%>%filter(ecoregion=="2");write.csv(w2,"ecoregion2-N rate-maize.csv")
w3 <- maize%>%filter(ecoregion=="3");write.csv(w3,"ecoregion3-N rate-maize.csv")
w4 <- maize%>%filter(ecoregion=="4");write.csv(w4,"ecoregion4-N rate-maize.csv")
w5 <- maize%>%filter(ecoregion=="5");write.csv(w5,"ecoregion5-N rate-maize.csv")
w6 <- maize%>%filter(ecoregion=="6");write.csv(w6,"ecoregion6-N rate-maize.csv")
w7 <- maize%>%filter(ecoregion=="7");write.csv(w7,"ecoregion7-N rate-maize.csv")
w8 <- maize%>%filter(ecoregion=="8");write.csv(w8,"ecoregion8-N rate-maize.csv")
w9 <- maize%>%filter(ecoregion=="9");write.csv(w9,"ecoregion9-N rate-maize.csv")
w10 <- maize%>%filter(ecoregion=="10");write.csv(w10,"ecoregion10-N rate-maize.csv")
w11 <- maize%>%filter(ecoregion=="11");write.csv(w11,"ecoregion11-N rate-maize.csv")
w12 <- maize%>%filter(ecoregion=="12");write.csv(w12,"ecoregion12-N rate-maize.csv")
w13 <- maize%>%filter(ecoregion=="13");write.csv(w13,"ecoregion13-N rate-maize.csv")
w14 <- maize%>%filter(ecoregion=="14");write.csv(w14,"ecoregion14-N rate-maize.csv")
w15 <- maize%>%filter(ecoregion=="15");write.csv(w15,"ecoregion15-N rate-maize.csv")

###Yield-scaled N2O###
ref <- raster("E:/Data/Raster/CMIP6/ISIMIP/gfdl-esm4_r1i1p1f1_w5e5_ssp126_pr_global_daily_2021_2030.nc")
AI<- raster("E:/Data/Raster/CC-raster/AI-0.1.tif")
pH <- raster("E:/Data/Raster/CC-raster/pH0..30-0.1.tif")
N.rate <- raster("E:/Data/Raster/CC-raster/N.rate.maize.1961-2020.tif")
env <- stack(AI,pH,N.rate)
env <- resample(env,ref)
names(env) <- c("AI","pH","N.rate")
env.df <- as.data.frame(env,xy=T)

maize_legume <- left_join(yield.scaled.N2O_legume_maize.df, yield_legume_maize.df)
maize_mixture <- left_join(yield.scaled.N2O_mixture_maize.df, yield_mixture_maize.df)
maize_non_legume <- left_join(yield.scaled.N2O_non_legume_maize.df, yield_non_legume_maize.df)
maize_legume <- left_join(maize_legume, env.df)%>%mutate(type="Legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,type)
maize_mixture <- left_join(maize_mixture, env.df)%>%mutate(type="Mixture")%>%dplyr::select(value1,value2,AI,pH,N.rate,type)
maize_non_legume <- left_join(maize_non_legume, env.df)%>%mutate(type="Non-legume")%>%dplyr::select(value1,value2,AI,pH,N.rate,type)
maize <- na.omit(rbind.data.frame(maize_legume,maize_mixture,maize_non_legume))

maize$AI2 <- ifelse(maize$AI<=0.2,"Arid",
                    ifelse(maize$AI<=0.5,"Semi-arid",
                           ifelse(maize$AI<=0.75,"Semi-humid","Humid")))
maize$pH2 <- ifelse(maize$pH<=5.5,"Strongly acidic",
                    ifelse(maize$pH<6.5,"Moderately acidic",
                           ifelse(maize$pH<=7.5,"Neutral","Alkaline")))
maize$AI2 <- factor(maize$AI2,levels = c("Arid","Semi-arid","Semi-humid","Humid"))
maize$pH2 <- factor(maize$pH2,levels = c("Strongly acidic","Moderately acidic","Neutral","Alkaline"))
maize$ecoregion <- ifelse(maize$AI2=="Arid"&maize$pH2=="Strongly acidic",1,
                          ifelse(maize$AI2=="Arid"&maize$pH2=="Moderately acidic",2,
                                 ifelse(maize$AI2=="Arid"&maize$pH2=="Neutral",3,
                                        ifelse(maize$AI2=="Arid"&maize$pH2=="Alkaline",4,
                                               ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Strongly acidic",5,
                                                      ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Moderately acidic",6,
                                                             ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Neutral",7,
                                                                    ifelse(maize$AI2=="Semi-arid"&maize$pH2=="Alkaline",8,
                                                                           ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Strongly acidic",9,
                                                                                  ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Moderately acidic",10,
                                                                                         ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Neutral",11,
                                                                                                ifelse(maize$AI2=="Semi-humid"&maize$pH2=="Alkaline",12,
                                                                                                       ifelse(maize$AI2=="Humid"&maize$pH2=="Strongly acidic",13,
                                                                                                              ifelse(maize$AI2=="Humid"&maize$pH2=="Moderately acidic",14,15))))))))))))))

maize$ecoregion <- as.factor(maize$ecoregion)
maize$type <- as.factor(maize$type)

###Ecoregion1###
maize1 <- filter(maize,ecoregion=="1")

model1 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize1)

ecoregion1 <- data.frame(N.rate = seq(min(maize1$N.rate), max(maize1$N.rate), length.out = 1000))
ecoregion1_legume <- ecoregion1%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred1_legume <- predict(model1, newdata = ecoregion1_legume, type = "response")
pred1_legume_df <- data.frame(N.rate=ecoregion1_legume$N.rate,value=pred1_legume)%>%dplyr::mutate(type="Legume")
ecoregion1_mixture <- ecoregion1%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred1_mixture <- predict(model1, newdata = ecoregion1_mixture, type = "response")
pred1_mixture_df <- data.frame(N.rate=ecoregion1_mixture$N.rate,value=pred1_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion1_non_legume <- ecoregion1%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred1_non_legume <- predict(model1, newdata = ecoregion1_non_legume, type = "response")
pred1_non_legume_df <- data.frame(N.rate=ecoregion1_non_legume$N.rate,value=pred1_non_legume)%>%dplyr::mutate(type="Non-legume")

pred1_df <- rbind.data.frame(pred1_legume_df,pred1_mixture_df,pred1_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")

p1 <- ggplot(pred1_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=19.0,xmax=53.2,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  #geom_vline(xintercept=53.2,linetype="dashed",color="#48C9B0")+
  #geom_vline(xintercept=53.2,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=53.2,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.16,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-1.1,1.1))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p1


###Ecoregion2###
maize2 <- filter(maize,ecoregion=="2")

model2 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize2)

ecoregion2 <- data.frame(N.rate = seq(min(maize2$N.rate), max(maize2$N.rate), length.out = 1000))
ecoregion2_legume <- ecoregion2%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred2_legume <- predict(model2, newdata = ecoregion2_legume, type = "response")
pred2_legume_df <- data.frame(N.rate=ecoregion2_legume$N.rate,value=pred2_legume)%>%dplyr::mutate(type="Legume")
ecoregion2_mixture <- ecoregion2%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred2_mixture <- predict(model2, newdata = ecoregion2_mixture, type = "response")
pred2_mixture_df <- data.frame(N.rate=ecoregion2_mixture$N.rate,value=pred2_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion2_non_legume <- ecoregion2%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred2_non_legume <- predict(model2, newdata = ecoregion2_non_legume, type = "response")
pred2_non_legume_df <- data.frame(N.rate=ecoregion2_non_legume$N.rate,value=pred2_non_legume)%>%dplyr::mutate(type="Non-legume")

pred2_df <- rbind.data.frame(pred2_legume_df,pred2_mixture_df,pred2_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p2 <- ggplot(pred2_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=6.09,xmax=17.1,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=11.6,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=11.6,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=11.6,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.26,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-2.2,2.2))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p2


###Ecoregion3###
maize3 <- filter(maize,ecoregion=="3")

model3 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize3)
deriv3 <- derivatives(model3)
print(deriv3,n=300)

ecoregion3 <- data.frame(N.rate = seq(min(maize3$N.rate), max(maize3$N.rate), length.out = 1000))
ecoregion3_legume <- ecoregion3%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred3_legume <- predict(model3, newdata = ecoregion3_legume, type = "response")
pred3_legume_df <- data.frame(N.rate=ecoregion3_legume$N.rate,value=pred3_legume)%>%dplyr::mutate(type="Legume")
ecoregion3_mixture <- ecoregion3%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred3_mixture <- predict(model3, newdata = ecoregion3_mixture, type = "response")
pred3_mixture_df <- data.frame(N.rate=ecoregion3_mixture$N.rate,value=pred3_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion3_non_legume <- ecoregion3%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred3_non_legume <- predict(model3, newdata = ecoregion3_non_legume, type = "response")
pred3_non_legume_df <- data.frame(N.rate=ecoregion3_non_legume$N.rate,value=pred3_non_legume)%>%dplyr::mutate(type="Non-legume")

pred3_df <- rbind.data.frame(pred3_legume_df,pred3_mixture_df,pred3_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p3 <- ggplot(pred3_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=8.98,xmax=62.2,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  #geom_vline(xintercept=22.0,linetype="dashed",color="#48C9B0")+
  #geom_vline(xintercept=22.0,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=24.0,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.36,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-3.3,3.3))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p3


###Ecoregion4###
maize4 <- filter(maize,ecoregion=="4")

model4 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize4)
deriv4 <- derivatives(model4)
print(deriv4,n=300)

ecoregion4 <- data.frame(N.rate = seq(min(maize4$N.rate), max(maize4$N.rate), length.out = 1000))
ecoregion4_legume <- ecoregion4%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred4_legume <- predict(model4, newdata = ecoregion4_legume, type = "response")
pred4_legume_df <- data.frame(N.rate=ecoregion4_legume$N.rate,value=pred4_legume)%>%dplyr::mutate(type="Legume")
ecoregion4_mixture <- ecoregion4%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred4_mixture <- predict(model4, newdata = ecoregion4_mixture, type = "response")
pred4_mixture_df <- data.frame(N.rate=ecoregion4_mixture$N.rate,value=pred4_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion4_non_legume <- ecoregion4%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred4_non_legume <- predict(model4, newdata = ecoregion4_non_legume, type = "response")
pred4_non_legume_df <- data.frame(N.rate=ecoregion4_non_legume$N.rate,value=pred4_non_legume)%>%dplyr::mutate(type="Non-legume")

pred4_df <- rbind.data.frame(pred4_legume_df,pred4_mixture_df,pred4_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p4 <- ggplot(pred4_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=21.4,xmax=299,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=75.0,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=75.0,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=62.1,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.46,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-4.4,4.4))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p4


###Ecoregion5###
maize5 <- filter(maize,ecoregion=="5")

model5 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize5)
deriv5 <- derivatives(model5)
print(deriv5,n=300)

ecoregion5 <- data.frame(N.rate = seq(min(maize5$N.rate), max(maize5$N.rate), length.out = 1000))
ecoregion5_legume <- ecoregion5%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred5_legume <- predict(model5, newdata = ecoregion5_legume, type = "response")
pred5_legume_df <- data.frame(N.rate=ecoregion5_legume$N.rate,value=pred5_legume)%>%dplyr::mutate(type="Legume")
ecoregion5_mixture <- ecoregion5%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred5_mixture <- predict(model5, newdata = ecoregion5_mixture, type = "response")
pred5_mixture_df <- data.frame(N.rate=ecoregion5_mixture$N.rate,value=pred5_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion5_non_legume <- ecoregion5%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred5_non_legume <- predict(model5, newdata = ecoregion5_non_legume, type = "response")
pred5_non_legume_df <- data.frame(N.rate=ecoregion5_non_legume$N.rate,value=pred5_non_legume)%>%dplyr::mutate(type="Non-legume")

pred5_df <- rbind.data.frame(pred5_legume_df,pred5_mixture_df,pred5_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p5 <- ggplot(pred5_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=20.6,xmax=101,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=101,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=101,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=101,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.56,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-5.5,5.5))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p5


###Ecoregion6###
maize6 <- filter(maize,ecoregion=="6")

model6 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize6)
deriv6 <- derivatives(model6)
print(deriv6,n=300)

ecoregion6 <- data.frame(N.rate = seq(min(maize6$N.rate), max(maize6$N.rate), length.out = 1000))
ecoregion6_legume <- ecoregion6%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred6_legume <- predict(model6, newdata = ecoregion6_legume, type = "response")
pred6_legume_df <- data.frame(N.rate=ecoregion6_legume$N.rate,value=pred6_legume)%>%dplyr::mutate(type="Legume")
ecoregion6_mixture <- ecoregion6%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred6_mixture <- predict(model6, newdata = ecoregion6_mixture, type = "response")
pred6_mixture_df <- data.frame(N.rate=ecoregion6_mixture$N.rate,value=pred6_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion6_non_legume <- ecoregion6%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred6_non_legume <- predict(model6, newdata = ecoregion6_non_legume, type = "response")
pred6_non_legume_df <- data.frame(N.rate=ecoregion6_non_legume$N.rate,value=pred6_non_legume)%>%dplyr::mutate(type="Non-legume")

pred6_df <- rbind.data.frame(pred6_legume_df,pred6_mixture_df,pred6_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p6 <- ggplot(pred6_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=13.9,xmax=31.4,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=23.7,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=23.7,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=23.7,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.66,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-6.6,6.6))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p6


###Ecoregion7###
maize7 <- filter(maize,ecoregion=="7")

model7 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize7)
deriv7 <- derivatives(model7)
print(deriv7,n=300)

ecoregion7 <- data.frame(N.rate = seq(min(maize7$N.rate), max(maize7$N.rate), length.out = 1000))
ecoregion7_legume <- ecoregion7%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred7_legume <- predict(model7, newdata = ecoregion7_legume, type = "response")
pred7_legume_df <- data.frame(N.rate=ecoregion7_legume$N.rate,value=pred7_legume)%>%dplyr::mutate(type="Legume")
ecoregion7_mixture <- ecoregion7%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred7_mixture <- predict(model7, newdata = ecoregion7_mixture, type = "response")
pred7_mixture_df <- data.frame(N.rate=ecoregion7_mixture$N.rate,value=pred7_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion7_non_legume <- ecoregion7%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred7_non_legume <- predict(model7, newdata = ecoregion7_non_legume, type = "response")
pred7_non_legume_df <- data.frame(N.rate=ecoregion7_non_legume$N.rate,value=pred7_non_legume)%>%dplyr::mutate(type="Non-legume")

pred7_df <- rbind.data.frame(pred7_legume_df,pred7_mixture_df,pred7_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p7 <- ggplot(pred7_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=1.45,xmax=104,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=21.6,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=21.6,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=21.6,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.76,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-7.7,7.7))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p7


###Ecoregion8###
maize8 <- filter(maize,ecoregion=="8")

model8 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize8)
deriv8 <- derivatives(model8)
print(deriv8,n=300)

ecoregion8 <- data.frame(N.rate = seq(min(maize8$N.rate), max(maize8$N.rate), length.out = 1000))
ecoregion8_legume <- ecoregion8%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred8_legume <- predict(model8, newdata = ecoregion8_legume, type = "response")
pred8_legume_df <- data.frame(N.rate=ecoregion8_legume$N.rate,value=pred8_legume)%>%dplyr::mutate(type="Legume")
ecoregion8_mixture <- ecoregion8%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred8_mixture <- predict(model8, newdata = ecoregion8_mixture, type = "response")
pred8_mixture_df <- data.frame(N.rate=ecoregion8_mixture$N.rate,value=pred8_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion8_non_legume <- ecoregion8%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred8_non_legume <- predict(model8, newdata = ecoregion8_non_legume, type = "response")
pred8_non_legume_df <- data.frame(N.rate=ecoregion8_non_legume$N.rate,value=pred8_non_legume)%>%dplyr::mutate(type="Non-legume")

pred8_df <- rbind.data.frame(pred8_legume_df,pred8_mixture_df,pred8_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p8 <- ggplot(pred8_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=5.16,xmax=105,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=105,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=105,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=105,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.86,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-8.8,8.8))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p8


###Ecoregion9###
maize9 <- filter(maize,ecoregion=="9")

model9 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize9)
deriv9 <- derivatives(model9)
print(deriv9,n=300)

ecoregion9 <- data.frame(N.rate = seq(min(maize9$N.rate), max(maize9$N.rate), length.out = 1000))
ecoregion9_legume <- ecoregion9%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred9_legume <- predict(model9, newdata = ecoregion9_legume, type = "response")
pred9_legume_df <- data.frame(N.rate=ecoregion9_legume$N.rate,value=pred9_legume)%>%dplyr::mutate(type="Legume")
ecoregion9_mixture <- ecoregion9%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred9_mixture <- predict(model9, newdata = ecoregion9_mixture, type = "response")
pred9_mixture_df <- data.frame(N.rate=ecoregion9_mixture$N.rate,value=pred9_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion9_non_legume <- ecoregion9%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred9_non_legume <- predict(model9, newdata = ecoregion9_non_legume, type = "response")
pred9_non_legume_df <- data.frame(N.rate=ecoregion9_non_legume$N.rate,value=pred9_non_legume)%>%dplyr::mutate(type="Non-legume")

pred9_df <- rbind.data.frame(pred9_legume_df,pred9_mixture_df,pred9_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p9 <- ggplot(pred9_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0.26,xmax=44.6,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=38.9,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=41.1,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=41.1,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.96,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-9.9,9.9))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p9


###Ecoregion10###
maize10 <- filter(maize,ecoregion=="10")

model10 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize10)
deriv10 <- derivatives(model10)
print(deriv10,n=300)

ecoregion10 <- data.frame(N.rate = seq(min(maize10$N.rate), max(maize10$N.rate), length.out = 1000))
ecoregion10_legume <- ecoregion10%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred10_legume <- predict(model10, newdata = ecoregion10_legume, type = "response")
pred10_legume_df <- data.frame(N.rate=ecoregion10_legume$N.rate,value=pred10_legume)%>%dplyr::mutate(type="Legume")
ecoregion10_mixture <- ecoregion10%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred10_mixture <- predict(model10, newdata = ecoregion10_mixture, type = "response")
pred10_mixture_df <- data.frame(N.rate=ecoregion10_mixture$N.rate,value=pred10_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion10_non_legume <- ecoregion10%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred10_non_legume <- predict(model10, newdata = ecoregion10_non_legume, type = "response")
pred10_non_legume_df <- data.frame(N.rate=ecoregion10_non_legume$N.rate,value=pred10_non_legume)%>%dplyr::mutate(type="Non-legume")

pred10_df <- rbind.data.frame(pred10_legume_df,pred10_mixture_df,pred10_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p10 <- ggplot(pred10_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0.28,xmax=56.7,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=30.0,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=30.0,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=30.0,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.106,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-10.10,10.10))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p10


###Ecoregion11###
maize11 <- filter(maize,ecoregion=="11")

model11 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize11)
deriv11 <- derivatives(model11)
print(deriv11,n=300)

ecoregion11 <- data.frame(N.rate = seq(min(maize11$N.rate), max(maize11$N.rate), length.out = 1000))
ecoregion11_legume <- ecoregion11%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred11_legume <- predict(model11, newdata = ecoregion11_legume, type = "response")
pred11_legume_df <- data.frame(N.rate=ecoregion11_legume$N.rate,value=pred11_legume)%>%dplyr::mutate(type="Legume")
ecoregion11_mixture <- ecoregion11%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred11_mixture <- predict(model11, newdata = ecoregion11_mixture, type = "response")
pred11_mixture_df <- data.frame(N.rate=ecoregion11_mixture$N.rate,value=pred11_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion11_non_legume <- ecoregion11%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred11_non_legume <- predict(model11, newdata = ecoregion11_non_legume, type = "response")
pred11_non_legume_df <- data.frame(N.rate=ecoregion11_non_legume$N.rate,value=pred11_non_legume)%>%dplyr::mutate(type="Non-legume")

pred11_df <- rbind.data.frame(pred11_legume_df,pred11_mixture_df,pred11_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p11 <- ggplot(pred11_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=2.46,xmax=58.6,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=24.0,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=27.7,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=27.7,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.116,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-11.11,11.11))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p11


###Ecoregion12###
maize12 <- filter(maize,ecoregion=="12")

model12 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize12)
deriv12 <- derivatives(model12)
print(deriv12,n=300)

ecoregion12 <- data.frame(N.rate = seq(min(maize12$N.rate), max(maize12$N.rate), length.out = 1000))
ecoregion12_legume <- ecoregion12%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred12_legume <- predict(model12, newdata = ecoregion12_legume, type = "response")
pred12_legume_df <- data.frame(N.rate=ecoregion12_legume$N.rate,value=pred12_legume)%>%dplyr::mutate(type="Legume")
ecoregion12_mixture <- ecoregion12%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred12_mixture <- predict(model12, newdata = ecoregion12_mixture, type = "response")
pred12_mixture_df <- data.frame(N.rate=ecoregion12_mixture$N.rate,value=pred12_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion12_non_legume <- ecoregion12%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred12_non_legume <- predict(model12, newdata = ecoregion12_non_legume, type = "response")
pred12_non_legume_df <- data.frame(N.rate=ecoregion12_non_legume$N.rate,value=pred12_non_legume)%>%dplyr::mutate(type="Non-legume")

pred12_df <- rbind.data.frame(pred12_legume_df,pred12_mixture_df,pred12_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p12 <- ggplot(pred12_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  #geom_rect(xmin=34.0,xmax=95.4,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  #geom_vline(xintercept=34.0,linetype="dashed",color="#48C9B0")+
  #geom_vline(xintercept=34.0,linetype="dashed",color="#F95738")+
  #geom_vline(xintercept=34.0,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.126,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-12.12,12.12))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p12


###Ecoregion13###
maize13 <- filter(maize,ecoregion=="13")

model13 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize13)
deriv13 <- derivatives(model13)
print(deriv13,n=300)

ecoregion13 <- data.frame(N.rate = seq(min(maize13$N.rate), max(maize13$N.rate), length.out = 1000))
ecoregion13_legume <- ecoregion13%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred13_legume <- predict(model13, newdata = ecoregion13_legume, type = "response")
pred13_legume_df <- data.frame(N.rate=ecoregion13_legume$N.rate,value=pred13_legume)%>%dplyr::mutate(type="Legume")
ecoregion13_mixture <- ecoregion13%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred13_mixture <- predict(model13, newdata = ecoregion13_mixture, type = "response")
pred13_mixture_df <- data.frame(N.rate=ecoregion13_mixture$N.rate,value=pred13_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion13_non_legume <- ecoregion13%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred13_non_legume <- predict(model13, newdata = ecoregion13_non_legume, type = "response")
pred13_non_legume_df <- data.frame(N.rate=ecoregion13_non_legume$N.rate,value=pred13_non_legume)%>%dplyr::mutate(type="Non-legume")

pred13_df <- rbind.data.frame(pred13_legume_df,pred13_mixture_df,pred13_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p13 <- ggplot(pred13_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0.17,xmax=96.1,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=49.3,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=49.3,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=38.4,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.136,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-13.13,13.13))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p13


###Ecoregion14###
maize14 <- filter(maize,ecoregion=="14")

model14 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize14)
deriv14 <- derivatives(model14)
print(deriv14,n=300)

ecoregion14 <- data.frame(N.rate = seq(min(maize14$N.rate), max(maize14$N.rate), length.out = 1000))
ecoregion14_legume <- ecoregion14%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred14_legume <- predict(model14, newdata = ecoregion14_legume, type = "response")
pred14_legume_df <- data.frame(N.rate=ecoregion14_legume$N.rate,value=pred14_legume)%>%dplyr::mutate(type="Legume")
ecoregion14_mixture <- ecoregion14%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred14_mixture <- predict(model14, newdata = ecoregion14_mixture, type = "response")
pred14_mixture_df <- data.frame(N.rate=ecoregion14_mixture$N.rate,value=pred14_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion14_non_legume <- ecoregion14%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred14_non_legume <- predict(model14, newdata = ecoregion14_non_legume, type = "response")
pred14_non_legume_df <- data.frame(N.rate=ecoregion14_non_legume$N.rate,value=pred14_non_legume)%>%dplyr::mutate(type="Non-legume")

pred14_df <- rbind.data.frame(pred14_legume_df,pred14_mixture_df,pred14_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p14 <- ggplot(pred14_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=0.19,xmax=221,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=31.3,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=31.3,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=178,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.146,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-14.14,14.14))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p14


###Ecoregion15###
maize15 <- filter(maize,ecoregion=="15")

model15 <- mgcv::gam(value2 ~ s(N.rate, by = type) + type, data = maize15)

ecoregion15 <- data.frame(N.rate = seq(min(maize15$N.rate), max(maize15$N.rate), length.out = 1000))
ecoregion15_legume <- ecoregion15%>%dplyr::mutate(type=factor("Legume",levels=c("Legume","Mixture","Non-legume")))
pred15_legume <- predict(model15, newdata = ecoregion15_legume, type = "response")
pred15_legume_df <- data.frame(N.rate=ecoregion15_legume$N.rate,value=pred15_legume)%>%dplyr::mutate(type="Legume")
ecoregion15_mixture <- ecoregion15%>%dplyr::mutate(type=factor("Mixture",levels=c("Legume","Mixture","Non-legume")))
pred15_mixture <- predict(model15, newdata = ecoregion15_mixture, type = "response")
pred15_mixture_df <- data.frame(N.rate=ecoregion15_mixture$N.rate,value=pred15_mixture)%>%dplyr::mutate(type="Mixture")
ecoregion15_non_legume <- ecoregion15%>%dplyr::mutate(type=factor("Non-legume",levels=c("Legume","Mixture","Non-legume")))
pred15_non_legume <- predict(model15, newdata = ecoregion15_non_legume, type = "response")
pred15_non_legume_df <- data.frame(N.rate=ecoregion15_non_legume$N.rate,value=pred15_non_legume)%>%dplyr::mutate(type="Non-legume")

pred15_df <- rbind.data.frame(pred15_legume_df,pred15_mixture_df,pred15_non_legume_df)%>%mutate(AI2="Humid",pH2="Neutral")


p15 <- ggplot(pred15_df,aes(x=N.rate,y=value,color=type))+
  theme_cowplot()+
  geom_rect(xmin=9.32,xmax=140,ymin=-1,ymax=1,fill=alpha("#ededed",0.05),color=alpha("#ededed",0.05))+
  geom_vline(xintercept=9.32,linetype="dashed",color="#48C9B0")+
  geom_vline(xintercept=9.32,linetype="dashed",color="#F95738")+
  geom_vline(xintercept=9.32,linetype="dashed",color="#FFC857")+
  geom_hline(yintercept=0,linetype="dashed",color="black")+
  geom_line(size=1,linetype=1)+
  #geom_vline(xintercept = 0.156,linetype=2,color="black",size=0.6)+
  #scale_y_continuous(limits = c(-15.15,15.15))+
  scale_color_manual(values = c("#48C9B0","#F95738","#FFC857")) + 
  labs(x="N application rate (kg N/ha)",y=expression(paste("LnRR of yield-scaled ",N[2],"O",sep = "")))+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+font+
  theme(legend.position = "none")
p15


ggdraw()+ 
  draw_plot(p1, x=0, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p2, x=1/4, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p3, x=2/4, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p4, x=3/4, y=3/4, width = 1/4, height = 1/4)+
  draw_plot(p5, x=0, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p6, x=1/4, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p7, x=2/4, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p8, x=3/4, y=2/4, width = 1/4, height = 1/4)+
  draw_plot(p9, x=0, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p10, x=1/4, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p11, x=2/4, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p12, x=3/4, y=1/4, width = 1/4, height = 1/4)+
  draw_plot(p13, x=0, y=0, width = 1/4, height = 1/4)+
  draw_plot(p14, x=1/4, y=0, width = 1/4, height = 1/4)+
  draw_plot(p15, x=2/4, y=0, width = 1/4, height = 1/4)##11.6*11.0inches
