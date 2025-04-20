library(ggplot2)
library(rworldmap)
library(scatterpie)
library(ggrepel)
library(ggspatial)
library(plotbiomes)
library(ggforce)
library(cowplot)
library(ggalluvial)
library(raster)
library(orchaRd)
library(ggbeeswarm)

setwd("D:/Document/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches

#####======Study site=======#####
a <- read.csv("study site.csv")%>%filter(study!=29)

worldMap <- fortify(map_data("world"), region = "subregion")
worldMap <- worldMap[worldMap$region != "Antarctica",]

cropland <- raster("D:/Raster/Cropland area/Cropland2000_5m.tif")
cropland.df <- as.data.frame(cropland,xy=TRUE)
cropland.df$Cropland2000_5m[which(cropland.df$Cropland2000_5m=="0")] <- NA

p1 <- ggplot()+
  geom_polygon(data=worldMap,aes(x=long,y=lat,group=group),
               fill="darkgray",color="white",size=0.2)+
  geom_raster(data=cropland.df,aes(x,y,fill=Cropland2000_5m*100)) +
  geom_point(data=a,aes(x=longitude,y=latitude,size=n),color="red",alpha=0.5)+
  theme_cowplot()+
  scale_fill_gradientn(colors = hcl.colors(9,"YlGn",rev = T)[1:7],na.value = NA)+
  scale_color_gradientn(colors = hcl.colors(11,"RdYlBu"))+
  scale_x_continuous(limits= c(-175,195))+
  scale_y_continuous(limits= c(-60,85))+
  #scale_size_continuous(limits = c(0.2,6))+
  xlab(expression(paste("Longitude (",degree,")",sep="")))+
  ylab(expression(paste("Latitude (",degree,")",sep="")))+font+
  guides(size = guide_legend("Number of observations",order = 2),
         fill = guide_colorbar("Cropland area (%)",order = 1))+
  theme(legend.direction = "horizontal",legend.position = c(0.5,0.1))

a$CC.type <- factor(a$CC.type,labels = c("Legume","Mixture","Non-legume"))
a$cash.crop <- factor(a$cash.crop,levels = c("wheat","maize","rice","soybean","others"),
                      labels = c("Wheat","Maize","Rice","Soybean","Others"))

p2 <- ggplot(data=a,aes(axis1=CC.type,axis2=cash.crop))+
  geom_alluvium(aes(fill=CC.type))+
  geom_stratum(aes(fill=CC.type),width = 1/3, color = "white")+
  geom_text(stat = "stratum",aes(label = after_stat(stratum)),size=4)+
  scale_fill_manual(values=c("#48C9B0","#F95738","#FFC857"))+
  font+
  theme_cowplot()+
  theme(axis.text = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),
        axis.line = element_blank(),legend.position = "none")

p3 <- ggdraw()+ 
  draw_plot(p1, x=0, y=0, width = 1, height = 1)+
  draw_plot(p2, x=0.05, y=0.075, width = 0.275, height = 0.475)##11.6*7.0inches


#####======Trade-off=======#####
b <- read.csv("Original data-first order.csv")%>%filter(number!=37)
b$ID <- as.factor(b$ID)
b$number <- as.factor(b$number)
b$study <- as.factor(b$study)
#a$seN2O <- sqrt(a$varN2O/a$N2O.tn)
#a$seyield <- sqrt(a$varyield/a$yield.tn)

trade_off1 <- b%>%filter(rryield>0)%>%filter(rrN2O>0)
win_win <- b%>%filter(rryield>0)%>%filter(rrN2O<=0)
trade_off2 <- b%>%filter(rryield<=0)%>%filter(rrN2O<=0)
lose_lose <- b%>%filter(rryield<=0)%>%filter(rrN2O>0)

modyield.N2O = rma.mv(rrN2O,varN2O,data=b,mods=~rryield,random=~1|study/ID)

p4 <- ggplot(data=b,aes(x=rryield,y=rrN2O))+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(aes(size = 1/(varN2O + modyield.N2O$tau2),color=AI),alpha=0.65) +
  #geom_errorbar(aes(x = rryield,ymin = rrN2O-1.96*seN2O, ymax = rrN2O+1.96*seN2O,color=AI),
  #              alpha=0.65,width = 0, show.legend = FALSE) +
  #geom_errorbarh(aes(y = rrN2O,xmin = rryield-1.96*seyield, xmax = rryield+1.96*seyield,color=AI),
  #               alpha=0.65,height = 0, show.legend = FALSE) +
  geom_smooth(aes(weight = 1/(varN2O + modyield.N2O$tau2)),method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  stat_poly_eq(aes(label=paste(..eq.label..,..rr.label..,..p.value.label..,sep="*\", \"*"),
                   weight = 1/(varN2O + modyield.N2O$tau2)),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "bottom")+ 
  scale_color_gradientn(colors = hcl.colors(11,"RdYlBu"))+
  theme_cowplot()+
  xlab(expression(paste("LnRR of yield")))+
  ylab(expression(paste("LnRR of ",N[2]*O,sep="")))+
  guides(size = guide_legend("Precision"),color = guide_colorbar("Aridity index"))+
  theme(legend.position = "right")+font



#####======Overall effect=======#####
c <- read.csv("Original data-first order.csv")%>%filter(number!=37)
c$ID <- as.factor(c$ID)
c$number <- as.factor(c$number)
c$study <- as.factor(c$study)
c$yi <- c$rrN2O-c$rryield
c$vi <- c$varN2O+c$varyield

yield <- c%>%dplyr::select(ID,number,study,rryield,varyield,biomass.N)%>%
  `colnames<-`(c("ID","number","study","yi","vi","biomass.N"))%>%
  mutate(response_variable="yield")
N2O <- c%>%dplyr::select(ID,number,study,rrN2O,varN2O,biomass.N)%>%
  `colnames<-`(c("ID","number","study","yi","vi","biomass.N"))%>%
  mutate(response_variable="N2O")
yield.scaled.N2O <- c%>%dplyr::select(ID,number,study,yi,vi,biomass.N)%>%
  mutate(response_variable="yield.scaled.N2O")

##### Overall first-order meta-analysis #####
data <- rbind.data.frame(yield,N2O,yield.scaled.N2O)
data.sen <- na.omit(data)
data.sen$yi.sen <- (exp(data.sen$yi)-1)*100/(data.sen$biomass.N)
data.sen$vi.sen <- (exp(data.sen$vi)-1)*100/(data.sen$biomass.N)

overall <- rma.mv(yi,vi,data=data,mods=~response_variable-1,random=~1|study/ID)
overall.sen <- rma.mv(yi.sen,vi.sen,data=data.sen,mods=~response_variable-1,random=~1|study/ID)

overall_output <- orchaRd::mod_results(overall,mod="response_variable",group="study")
overall_result <- data.frame(overall_output$mod_table)
overall_data <- data.frame(overall_output$data)%>%mutate(precision=1/sqrt(vi))

overall_result$name <- factor(overall_result$name,
                              levels = c("Yield","N2O","Yield.scaled.N2O"),
                              labels = c("Yield","N2O","Yield-scaled N2O"))
overall_result$sig <- c("a","b","b")
overall_data$moderator <- factor(overall_data$moderator,
                                 levels = c("Yield","N2O","Yield.scaled.N2O"),
                                 labels = c("Yield","N2O","Yield-scaled N2O"))

p5 <- ggplot(overall_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=overall_data,aes(x = yi, y = moderator, size = precision,color=moderator),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=name),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=name),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=name,shape=sig),size = 3.6) +
  scale_colour_manual(values=c("#F95738","#48C9B0","#9C27B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))),
                               c(expression(paste("Yield-scaled ",N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))),
                            c(expression(paste("Yield-scaled ",N[2],"O",sep = "")))))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "Effect size (lnRR)") +
  guides(size = guide_legend("Precision (1/SE)"),color="none",
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = "right",strip.text = element_blank(),
        axis.text.x = element_text(angle=12,vjust = 1,hjust=1))

overall.sen_output <- orchaRd::mod_results(overall.sen,mod="response_variable",group="study")
overall.sen_result <- data.frame(overall.sen_output$mod_table)
overall.sen_data <- data.frame(overall.sen_output$data)%>%mutate(precision=1/sqrt(vi))

overall.sen_result$name <- factor(overall.sen_result$name,
                                  levels = c("Yield","N2O","Yield.scaled.N2O"),
                                  labels = c("Yield","N2O","Yield-scaled N2O"))
overall.sen_result$sig <- c("a","b","b")
overall.sen_data$moderator <- factor(overall.sen_data$moderator,
                                     levels = c("Yield","N2O","Yield.scaled.N2O"),
                                     labels = c("Yield","N2O","Yield-scaled N2O"))

p6 <- ggplot(overall.sen_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=overall.sen_data,aes(x = yi, y = moderator, size = precision,color=moderator),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=name),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=name),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=name,shape=sig),size = 3.6) +
  scale_colour_manual(values=c("#F95738","#48C9B0","#9C27B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))),
                               c(expression(paste("Yield-scaled ",N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))),
                            c(expression(paste("Yield-scaled ",N[2],"O",sep = "")))),
                   position = "left")+
  scale_x_continuous(limits = c(-5,8),breaks = c(-4,0,4,8))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "Sensitivity (% per unit CC biomass N)") +
  guides(size = guide_legend("Precision (1/SE)"),color="none",
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = "right",strip.text = element_blank(),
        axis.text.x = element_text(angle=12,vjust = 1,hjust=1))

p7 <- lemon::grid_arrange_shared_legend(p4,p5,p6,nrow=1,ncol=3,position = "right")


ggdraw()+ 
  draw_plot(p3, x=0, y=0.45, width = 1, height = 0.55)+
  draw_plot(p7, x=0.01, y=0, width = 0.98, height = 0.45)+
  draw_plot_label(label = c("a","b","c"), size = 15,
                  x=c(0.01,0.01,1/3-0.01),
                  y=c(1,0.45,0.45))+
  draw_label("Trade-off (46%)",size=13,fontface = "bold",x=0.25,y=0.375)+
  draw_label("Win-win (12%)",size=13,fontface = "bold",x=0.25,y=0.125)+
  draw_label("Trade-off (19%)",size=13,fontface = "bold",x=0.125,y=0.125)+
  draw_label("Lose-lose (23%)",size=13,fontface = "bold",x=0.125,y=0.375)##11.6*10.2

