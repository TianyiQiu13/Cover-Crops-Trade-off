library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(grid)
library(ggpmisc)
library(metafor)
library(orchaRd)
library(ggbeeswarm)

setwd("E:/Data/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches

a <- read.csv("Original data-first order.csv")%>%filter(number!=37)
a$ID <- as.factor(a$ID)
a$number <- as.factor(a$number)
a$study <- as.factor(a$study)
a$yi <- a$rrN2O-a$rryield
a$vi <- a$varN2O+a$varyield

yield <- a%>%dplyr::select(ID,number,study,rryield,varyield,N.rate,CC.type,biomass.N,CN.ratio)%>%
  `colnames<-`(c("ID","number","study","yi","vi","N.rate","CC.type","biomass.N","CN.ratio"))%>%
  mutate(response_variable="yield")
N2O <- a%>%dplyr::select(ID,number,study,rrN2O,varN2O,N.rate,CC.type,biomass.N,CN.ratio)%>%
  `colnames<-`(c("ID","number","study","yi","vi","N.rate","CC.type","biomass.N","CN.ratio"))%>%
  mutate(response_variable="N2O")
yield.scaled.N2O <- a%>%dplyr::select(ID,number,study,yi,vi,N.rate,CC.type,biomass.N,CN.ratio)%>%
  mutate(response_variable="yield.scaled.N2O")

##### Relationship with biomass N #####
###yield###
modbiomassN.yield = rma.mv(yi,vi,data=yield,mods=~biomass.N,random=~1|study/ID)

p1 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield,aes(biomass.N,yi, size = 1/(vi + modbiomassN.yield$tau2)),
             color="#F95738",alpha=0.5) +
  geom_smooth(data = yield,aes(biomass.N,yi, weight = 1/(vi + modbiomassN.yield$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  stat_poly_eq(data=yield,aes(biomass.N,yi, weight = 1/(vi + modbiomassN.yield$tau2),
                              label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("CC biomass N (kg ",ha^-1,")",sep="")))+
  ylab(expression(paste("LnRR of yield",sep="")))+
  theme(legend.position = "none")+font
p1

###N2O###
modbiomassN.N2O = rma.mv(yi,vi,data=N2O,mods=~biomass.N,random=~1|study/ID)

p2 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = N2O,aes(biomass.N,yi, size = 1/(vi + modbiomassN.N2O$tau2)),
             color="#48C9B0",alpha=0.5) +
  geom_smooth(data = N2O,aes(biomass.N,yi, weight = 1/(vi + modbiomassN.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  stat_poly_eq(data=N2O,aes(biomass.N,yi, weight = 1/(vi + modbiomassN.N2O$tau2),
                            label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("CC biomass N (kg ",ha^-1,")",sep="")))+
  ylab(expression(paste("LnRR of ",N[2]*O,sep="")))+
  theme(legend.position = "none")+font
p2

###yield.scaled.N2O###
modbiomassN.yield.scaled.N2O = rma.mv(yi,vi,data=yield.scaled.N2O,mods=~biomass.N,random=~1|study/ID)

p3 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield.scaled.N2O,aes(biomass.N,yi, 
                                         size = 1/(vi + modbiomassN.yield.scaled.N2O$tau2)),
             color="#9C27B0",alpha=0.5) +
  geom_smooth(data = yield.scaled.N2O,aes(biomass.N,yi, 
                                          weight = 1/(vi + modbiomassN.yield.scaled.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  stat_poly_eq(data=yield.scaled.N2O,aes(biomass.N,yi, 
                                         weight = 1/(vi + modbiomassN.yield.scaled.N2O$tau2),
                                         label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("CC biomass N (kg ",ha^-1,")",sep="")))+
  ylab(expression(paste("LnRR of yield-scaled ",N[2]*O,sep="")))+
  theme(legend.position = "none")+font
p3

##### Relationship with C/N ratio #####
###yield###
modCNratio.yield = rma.mv(yi,vi,data=yield,mods=~CN.ratio,random=~1|study/ID)

p4 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield,aes(CN.ratio,yi, size = 1/(vi + modCNratio.yield$tau2)),
             color="#F95738",alpha=0.5) +
  geom_smooth(data = yield,aes(CN.ratio,yi, weight = 1/(vi + modCNratio.yield$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  stat_poly_eq(data=yield,aes(CN.ratio,yi, weight = 1/(vi + modCNratio.yield$tau2),
                              label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("CC biomass C/N",sep="")))+
  ylab(expression(paste("LnRR of yield",sep="")))+
  theme(legend.position = "none")+font
p4

###N2O###
modCNratio.N2O = rma.mv(yi,vi,data=N2O,mods=~CN.ratio,random=~1|study/ID)

p5 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = N2O,aes(CN.ratio,yi, size = 1/(vi + modCNratio.N2O$tau2)),
             color="#48C9B0",alpha=0.5) +
  geom_smooth(data = N2O,aes(CN.ratio,yi, weight = 1/(vi + modCNratio.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  stat_poly_eq(data=N2O,aes(CN.ratio,yi, weight = 1/(vi + modCNratio.N2O$tau2),
                            label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("CC biomass C/N",sep="")))+
  ylab(expression(paste("LnRR of ",N[2]*O,sep="")))+
  theme(legend.position = "none")+font
p5

###yield.scaled.N2O###
modCNratio.yield.scaled.N2O = rma.mv(yi,vi,data=yield.scaled.N2O,mods=~CN.ratio,random=~1|study/ID)

p6 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield.scaled.N2O,aes(CN.ratio,yi, 
                                         size = 1/(vi + modCNratio.yield.scaled.N2O$tau2)),
             color="#9C27B0",alpha=0.5) +
  geom_smooth(data = yield.scaled.N2O,aes(CN.ratio,yi, 
                                          weight = 1/(vi + modCNratio.yield.scaled.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  stat_poly_eq(data=yield.scaled.N2O,aes(CN.ratio,yi, 
                                         weight = 1/(vi + modCNratio.yield.scaled.N2O$tau2),
                                         label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("CC biomass C/N",sep="")))+
  ylab(expression(paste("LnRR of yield-scaled ",N[2]*O,sep="")))+
  theme(legend.position = "none")+font
p6 

###C/N ratio in different CC type###
anova(lm(CN.ratio~CC.type,data=a))
TukeyHSD(aov(CN.ratio~CC.type,data=a))

legume <- a%>%filter(CC.type=="legume")
mixture <- a%>%filter(CC.type=="mixture")
non_legume <- a%>%filter(CC.type=="non-legume")

CN_legume_mean <- mean(legume$CN.ratio,na.rm=T)
CN_mixture_mean <- mean(mixture$CN.ratio,na.rm=T)
CN_non_legume_mean <- mean(non_legume$CN.ratio,na.rm=T)
CN_legume_se <- confintr::se_mean(legume$CN.ratio,na.rm=T)
CN_mixture_se <- confintr::se_mean(mixture$CN.ratio,na.rm=T)
CN_non_legume_se <- confintr::se_mean(non_legume$CN.ratio,na.rm=T)

mean <- rbind.data.frame(CN_legume_mean,CN_mixture_mean,CN_non_legume_mean)
se <- rbind.data.frame(CN_legume_se,CN_mixture_se,CN_non_legume_se)
df <- cbind.data.frame(mean,se)%>%`colnames<-`(c("mean","se"))%>%
  mutate(CC.type=c("legume","mixture","non-legume"),n=c("n = 33","n = 38","n = 18"),sig=c("b","b","a"))

p7 <- ggplot() +
  geom_bar(data=df,aes(x=CC.type,y=mean,fill=CC.type,color=CC.type),size = 1.1,alpha=0.7,
           position="dodge", stat="identity",width = 0.65)+  
  geom_errorbar(data=df,aes(x = CC.type,ymin = mean-se, ymax = mean+se),color="black",size=0.7,
                 width = 0.25, show.legend = FALSE) +
  geom_text(data=df,aes(x=CC.type,label=n,y=45), col="black", size=4.2)+ 
  geom_text(data=df,aes(x=CC.type,label=sig,y=mean+se+2), col="black", size=4.2)+ 
  scale_fill_manual(values=c("#48C9B0","#F95738","#FFC857")) +
  scale_colour_manual(values=c("#48C9B0","#F95738","#FFC857")) +
  scale_x_discrete(labels=c("Legume","Mixture","Non-legume"))+
  theme_cowplot()+
  ylab(expression(paste("CC biomass C/N",sep="")))+
  guides(size = guide_legend("Precision (1/SE)"),color="none",
         shape=guide_legend("Significance"))+font+
  theme(legend.position = "none",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1),axis.title.x = element_blank())
p7

ggdraw()+ 
  draw_plot(p1, x=0, y=2/3, width = 1/3, height = 1/3)+
  draw_plot(p2, x=1/3, y=2/3, width = 1/3, height = 1/3)+
  draw_plot(p3, x=2/3, y=2/3, width = 1/3, height = 1/3)+
  draw_plot(p4, x=0, y=1/3, width = 1/3, height = 1/3)+
  draw_plot(p5, x=1/3, y=1/3, width = 1/3, height = 1/3)+
  draw_plot(p6, x=2/3, y=1/3, width = 1/3, height = 1/3)+
  draw_plot(p7, x=1/3, y=0, width = 1/3, height = 1/3)+
  draw_plot_label(label = c("a","b","c","d","e","f","g"), size = 15,
                  x=c(0,1/3,2/3,0,1/3,2/3,1/3), y=c(1,1,1,2/3,2/3,2/3,1/3))##11.6*11.0inches


