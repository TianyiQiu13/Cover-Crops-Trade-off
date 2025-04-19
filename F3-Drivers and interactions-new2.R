library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(grid)
library(ggpmisc)
library(metafor)
library(orchaRd)
library(ggbeeswarm)
library(metaforest)
library(caret)

setwd("D:/Document/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches

##### Variable importance from MF models #####
####yield####
mf_cv <- readRDS("yield_mf.RData")
importance.export.rel <- varImp(mf_cv)$importance
importance.export.rel$variable <- c("MAT","MAP","Aridity index", 
                                    "Bulk density","Sand", "Clay",
                                    "SOC", "Soil C:N", "pH",
                                    "Cash crop type","N application rate",
                                    "CC type","CC residue management","CC termination time")
yield <- importance.export.rel
yield$total <- "Yield"

####N2O####
mf_cv <- readRDS("N2O_mf.RData")
importance.export.rel <- varImp(mf_cv)$importance
importance.export.rel$variable <- c("MAT","MAP","Aridity index", 
                                    "Bulk density","Sand", "Clay",
                                    "SOC", "Soil C:N", "pH",
                                    "Cash crop type","N application rate",
                                    "CC type","CC residue management","CC termination time")
N2O <- importance.export.rel
N2O$total <- "N2O"

####yield.scaled.N2O####
mf_cv <- readRDS("yield.scaled.N2O_mf.RData")
importance.export.rel <- varImp(mf_cv)$importance
importance.export.rel$variable <- c("MAT","MAP","Aridity index", 
                                    "Bulk density","Sand", "Clay",
                                    "SOC", "Soil C:N", "pH",
                                    "Cash crop type","N application rate",
                                    "CC type","CC residue management","CC termination time")
yield.scaled.N2O <- importance.export.rel
yield.scaled.N2O$total <- "Yield-scaled N2O"

importance <- rbind.data.frame(yield,N2O,yield.scaled.N2O)
importance$variable <- factor(importance$variable,
                              levels = rev(c("Cash crop type","CC type",
                                         "MAT","MAP","Aridity index", 
                                         "Bulk density","Sand", "Clay",
                                         "SOC", "Soil C:N", "pH",
                                         "N application rate","CC termination time",
                                         "CC residue management")),
                              labels = rev(c("Cash crop type","CC type",
                                             "MAT","MAP","Aridity index", 
                                             "Bulk density","Sand", "Clay",
                                             "SOC", "Soil C:N", "pH",
                                             "N application\nrate","CC termination\ntime",
                                             "CC residue\nmanagement")))
importance$total <- factor(importance$total,
                           levels = c("Yield","N2O","Yield-scaled N2O"))
importance$Overall2 <- ifelse(importance$Overall<50,importance$Overall,NA)

p1 <- ggplot(importance, aes(y = variable, x = Overall)) +
  geom_bar(stat='identity',width=0.75,color="black",aes(fill=total)) +
  geom_bar(aes(y = variable, x = Overall2),stat='identity',width=0.75,color="black",fill="lightgray") +
  theme_cowplot()+
  facet_wrap(~total,ncol = 3)+
  scale_fill_manual(values=c("#F95738","#48C9B0","#9C27B0"),
                    labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))),
                             c(expression(paste("Yield-scaled ",N[2],"O",sep = ""))))) +
  theme(panel.grid=element_blank())+
  labs(x="Relative importance (%)")+
  font+
  theme(axis.title.y = element_blank(),panel.spacing = unit(0,"lines"),
        strip.text = element_blank(),strip.background =element_blank(),legend.position = "none")
p1

##### Relationship with important predictors #####
a <- read.csv("Original data-first order.csv")%>%filter(number!=37)
a$ID <- as.factor(a$ID)
a$number <- as.factor(a$number)
a$study <- as.factor(a$study)
a$yi <- a$rrN2O-a$rryield
a$vi <- a$varN2O+a$varyield

yield <- a%>%dplyr::select(ID,number,study,rryield,varyield,AI,N.rate)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","N.rate"))%>%
  mutate(response_variable="yield")
N2O <- a%>%dplyr::select(ID,number,study,rrN2O,varN2O,CC.type,pH)%>%
  `colnames<-`(c("ID","number","study","yi","vi","CC.type","pH"))%>%
  mutate(response_variable="N2O")
yield.scaled.N2O <- a%>%dplyr::select(ID,number,study,yi,vi,pH,CN)%>%
  mutate(response_variable="yield.scaled.N2O")

###yield###
modN.rate.yield = rma.mv(yi,vi,data=yield,mods=~N.rate,random=~1|study/ID)

p2 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield,aes(N.rate,yi, size = 1/(vi + modN.rate.yield$tau2)),
             color="#F95738",alpha=0.5) +
  geom_smooth(data = yield,aes(N.rate,yi, weight = 1/(vi + modN.rate.yield$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  scale_y_continuous(breaks = c(-1,0,1))+
  stat_poly_eq(data=yield,aes(N.rate,yi, weight = 1/(vi + modN.rate.yield$tau2),
                              label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("N application rate (kg ",ha^-1,")",sep="")))+
  ylab(expression(paste("LnRR of yield")))+
  theme(legend.position = "none")+font
p2

modAI.yield = rma.mv(yi,vi,data=yield,mods=~AI,random=~1|study/ID)

p3 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield,aes(AI,yi, size = 1/(vi + modAI.yield$tau2)),
             color="#F95738",alpha=0.5) +
  geom_smooth(data = yield,aes(AI,yi, weight = 1/(vi + modAI.yield$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  scale_x_continuous(breaks = c(0.2,0.6,1,1.4))+
  scale_y_continuous(breaks = c(-1,0,1))+
  stat_poly_eq(data=yield,aes(AI,yi, weight = 1/(vi + modAI.yield$tau2),
                              label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("Aridity index",sep="")))+
  ylab(expression(paste("LnRR of yield")))+
  theme(legend.position = "none",axis.title.y = element_text(color = "transparent"))+font
p3

###N2O###
modCC.type.N2O = rma.mv(yi,vi,data=N2O,mods=~CC.type-1,random=~1|study/ID)

CC.type.N2O_output <- orchaRd::mod_results(modCC.type.N2O,mod="CC.type",group="study")
CC.type.N2O_result <- data.frame(CC.type.N2O_output$mod_table)
CC.type.N2O_data <- data.frame(CC.type.N2O_output$data)%>%mutate(precision=1/sqrt(vi))
CC.type.N2O_result$n <- c("(62)","(55)","(78)")
CC.type.N2O_result$sig <- c("a","a","b")

p4 <- ggplot(CC.type.N2O_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=CC.type.N2O_data,aes(x = yi, y = moderator, size = precision,color=moderator),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=name),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=name),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=name,shape=sig),size = 3.6) +
  geom_text(aes(label=n,x=upperPR+0.1), col="black", size=3.8)+ 
  scale_colour_manual(values=c("#48C9B0","#48C9B0","#48C9B0")) +
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  xlab(expression(paste("LnRR of ",N[2]*O,sep="")))+
  ylab(expression(paste("CC type",sep="")))+
  guides(size = guide_legend("Precision (1/SE)"),color="none",
         shape=guide_legend("Significance"))+font+
  theme(legend.position = "none",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1),axis.title.x = element_blank())
p4

modpH.N2O = rma.mv(yi,vi,data=N2O,mods=~pH,random=~1|study/ID)

p5 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = N2O,aes(pH,yi, size = 1/(vi + modpH.N2O$tau2)),
             color="#48C9B0",alpha=0.5) +
  geom_smooth(data = N2O,aes(pH,yi, weight = 1/(vi + modpH.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE), color = "black")+
  stat_poly_eq(data=N2O,aes(pH,yi, weight = 1/(vi + modpH.N2O$tau2),
                            label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 2, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("pH",sep="")))+
  ylab(expression(paste("LnRR of ",N[2]*O,sep="")))+
  theme(legend.position = "none",axis.title.y = element_text(color = "transparent"))+font
p5

###yield.scaled.N2O###
modpH.yield.scaled.N2O = rma.mv(yi,vi,data=yield.scaled.N2O,mods=~pH,random=~1|study/ID)

p6 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield.scaled.N2O,aes(pH,yi, 
                                         size = 1/(vi + modpH.yield.scaled.N2O$tau2)),
             color="#9C27B0",alpha=0.5) +
  geom_smooth(data = yield.scaled.N2O,aes(pH,yi, weight = 1/(vi + modpH.yield.scaled.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 2, raw = TRUE), color = "black")+
  stat_poly_eq(data=yield.scaled.N2O,
               aes(pH,yi, weight = 1/(vi + modpH.yield.scaled.N2O$tau2),
                   label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 2, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("pH",sep="")))+
  ylab(expression(paste("LnRR of yield-scaled ",N[2]*O,sep="")))+
  theme(legend.position = "none")+font
p6

modCN.yield.scaled.N2O = rma.mv(yi,vi,data=yield.scaled.N2O,mods=~CN,random=~1|study/ID)

p7 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield.scaled.N2O,aes(CN,yi, 
                                         size = 1/(vi + modCN.yield.scaled.N2O$tau2)),
             color="#9C27B0",alpha=0.5) +
  geom_smooth(data = yield.scaled.N2O,aes(CN,yi, weight = 1/(vi + modCN.yield.scaled.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black",linetype="longdash")+
  stat_poly_eq(data=yield.scaled.N2O,
               aes(CN,yi, weight = 1/(vi + modCN.yield.scaled.N2O$tau2),
                   label=paste(..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("Soil C:N",sep="")))+
  ylab(expression(paste("LnRR of yield-scaled ",N[2]*O,sep="")))+
  theme(legend.position = "none",axis.title.y = element_text(color = "transparent"))+font
p7

ggdraw()+ 
  draw_plot(p1, x=0, y=0, width = 0.58, height = 1)+
  draw_plot(p2, x=0.58-0.0065, y=2/3-0.005, width = 0.235, height = 1/3+0.005)+
  draw_plot(p3, x=0.765, y=2/3, width = 0.235, height = 1/3)+
  draw_plot(p4, x=0.58-0.0065, y=1/3, width = 0.235, height = 1/3)+
  draw_plot(p5, x=0.765, y=1/3, width = 0.235, height = 1/3)+
  draw_plot(p6, x=0.58-0.0065, y=0-0.002, width = 0.235, height = 1/3+0.002)+
  draw_plot(p7, x=0.765, y=0, width = 0.235, height = 1/3)+
  draw_label(expression(paste("Yield: ",italic(R)^2," = 0.80",sep = "")),x=0.2,y=0.15,size=12)+
  draw_label(expression(paste(N[2],"O: ",italic(R)^2," = 0.54",sep = "")),
             x=0.35,y=0.15,size=12)+
  #draw_label(expression(paste("Yield-scaled ",N[2],"O: ",italic(R)^2," = 0.34",sep = "")),
  #           x=0.5,y=0.15,size=12)+
  draw_plot_label(label = c("a","b","c","d"), size = 15,
                  x=c(0,0.58-0.008,0.58-0.008,0.58-0.008), y=c(1,1,2/3,1/3))##11.6*8.6inches
