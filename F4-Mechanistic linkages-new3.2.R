library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(grid)
library(ggpmisc)
library(metafor)
library(orchaRd)
library(ggbeeswarm)
library(GGally)

setwd("D:/Document/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches

#####======Overall effect=======#####
c <- read.csv("Original data-first order2-new.csv")
c$ID <- as.factor(c$ID)
c$number <- as.factor(c$number)
c$study <- as.factor(c$study)

nirK <- c%>%dplyr::select(ID,number,study,rrnirK,varnirK,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="nirK")
nirS <- c%>%dplyr::select(ID,number,study,rrnirS,varnirS,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="nirS")
nosZ <- c%>%dplyr::select(ID,number,study,rrnosZ,varnosZ,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="nosZ")
nirKS.nosZ <- c%>%dplyr::select(ID,number,study,rrnirKS.nosZ,varnirKS.nosZ,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="nirKS.nosZ")
pH <- c%>%dplyr::select(ID,number,study,rrpH,varpH,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="pH")
yield <- c%>%dplyr::select(ID,number,study,rryield,varyield,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="yield")
N2O <- c%>%dplyr::select(ID,number,study,rrN2O,varN2O,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="N2O")
yield.scaled.N2O <- c%>%dplyr::select(ID,number,study,rryield.scaled.N2O,varyield.scaled.N2O,AI,pH,CC.type,biomass.N..kg.ha.)%>%
  `colnames<-`(c("ID","number","study","yi","vi","AI","pH","CC.type","biomass.N"))%>%
  mutate(response_variable="yield.scaled.N2O")

##### Effect sizes #####
data <- rbind.data.frame(nirK,nirS,nosZ,nirKS.nosZ,pH,yield,N2O,yield.scaled.N2O)
data.sen <- na.omit(data)
data.sen$yi.sen <- (exp(data.sen$yi)-1)*100/(data.sen$biomass.N)
data.sen$vi.sen <- (exp(data.sen$vi)-1)*100/(data.sen$biomass.N)

semiarid.sen <- rma.mv(yi.sen,vi.sen,data=filter(data.sen,AI>0.2&AI<0.5),mods=~response_variable-1,random=~1|study/ID,
                       control = list(optimizer = "bobyqa"))
semihumid.sen <- rma.mv(yi.sen,vi.sen,data=filter(data.sen,AI>0.5&AI<0.75),mods=~response_variable-1,random=~1|study/ID,
                        control = list(optimizer = "bobyqa"))
humid.sen <- rma.mv(yi.sen,vi.sen,data=filter(data.sen,AI>0.75),mods=~response_variable-1,random=~1|study/ID,
                    control = list(optimizer = "bobyqa"))

Sacid.sen <- rma.mv(yi.sen,vi.sen,data=filter(data.sen,pH<5.5),mods=~response_variable-1,random=~1|study/ID,
                    control = list(optimizer = "bobyqa"))
Macid.sen <- rma.mv(yi.sen,vi.sen,data=filter(data.sen,pH>5.5&pH<6.5),mods=~response_variable-1,random=~1|study/ID,
                    control = list(optimizer = "bobyqa"))
neutral.sen <- rma.mv(yi.sen,vi.sen,data=filter(data.sen,pH>=6.5&pH<7.5),mods=~response_variable-1,random=~1|study/ID,
                      control = list(optimizer = "bobyqa"))
alka.sen <- rma.mv(yi.sen,vi.sen,data=filter(data.sen,pH>7.5),mods=~response_variable-1,random=~1|study/ID,
                   control = list(optimizer = "bobyqa"))

semiarid.sen_output <- orchaRd::mod_results(semiarid.sen,mod="response_variable",group="study")
semiarid.sen_result <- data.frame(semiarid.sen_output$mod_table)%>%mutate(group="Semi-arid")
semiarid.sen_data <- data.frame(semiarid.sen_output$data)%>%mutate(precision=1/sqrt(vi))%>%mutate(group="Semi-arid")
semihumid.sen_output <- orchaRd::mod_results(semihumid.sen,mod="response_variable",group="study")
semihumid.sen_result <- data.frame(semihumid.sen_output$mod_table)%>%mutate(group="Semi-humid")
semihumid.sen_data <- data.frame(semihumid.sen_output$data)%>%mutate(precision=1/sqrt(vi))%>%mutate(group="Semi-humid")
humid.sen_output <- orchaRd::mod_results(humid.sen,mod="response_variable",group="study")
humid.sen_result <- data.frame(humid.sen_output$mod_table)%>%mutate(group="Humid")
humid.sen_data <- data.frame(humid.sen_output$data)%>%mutate(precision=1/sqrt(vi))%>%mutate(group="Humid")
Sacid.sen_output <- orchaRd::mod_results(Sacid.sen,mod="response_variable",group="study")
Sacid.sen_result <- data.frame(Sacid.sen_output$mod_table)%>%mutate(group="Strongly acidic")
Sacid.sen_data <- data.frame(Sacid.sen_output$data)%>%mutate(precision=1/sqrt(vi))%>%mutate(group="Strongly acidic")
Macid.sen_output <- orchaRd::mod_results(Macid.sen,mod="response_variable",group="study")
Macid.sen_result <- data.frame(Macid.sen_output$mod_table)%>%mutate(group="Moderately acidic")
Macid.sen_data <- data.frame(Macid.sen_output$data)%>%mutate(precision=1/sqrt(vi))%>%mutate(group="Moderately acidic")
neutral.sen_output <- orchaRd::mod_results(neutral.sen,mod="response_variable",group="study")
neutral.sen_result <- data.frame(neutral.sen_output$mod_table)%>%mutate(group="Neutral")
neutral.sen_data <- data.frame(neutral.sen_output$data)%>%mutate(precision=1/sqrt(vi))%>%mutate(group="Neutral")
alka.sen_output <- orchaRd::mod_results(alka.sen,mod="response_variable",group="study")
alka.sen_result <- data.frame(alka.sen_output$mod_table)%>%mutate(group="Alkaline")
alka.sen_data <- data.frame(alka.sen_output$data)%>%mutate(precision=1/sqrt(vi))%>%mutate(group="Alkaline")

overall.sen_result <- rbind.data.frame(semiarid.sen_result,semihumid.sen_result,humid.sen_result,
                                       Sacid.sen_result,Macid.sen_result,neutral.sen_result,alka.sen_result)
overall.sen_result$name <- factor(overall.sen_result$name,
                                  levels = c("NirK","NirS","NosZ","NirKS.nosZ",
                                             "PH","Yield","N2O","Yield.scaled.N2O"),
                                  labels = c("nirK","nirS","nosZ","(nirK+nirS)/nosZ",
                                             "pH","Yield","N2O","Yield-scaled N2O"))
overall.sen_result$group <- factor(overall.sen_result$group,
                                   levels = c("Strongly acidic","Semi-arid",
                                              "Moderately acidic","Semi-humid",
                                              "Neutral","Humid",
                                              "Alkaline"))
overall.sen_result$sig <- c("a","a","a","a","a","b","a","b","b","b","a","b","a","b","b","b","a","b","b","b","b","b",
                            "a","a","b","b","a","b","a","a","a","b","a","b","b","a","b","a","a","b","b","a","b","b")


overall.sen_data <- rbind.data.frame(semiarid.sen_data,semihumid.sen_data,humid.sen_data,
                                       Sacid.sen_data,Macid.sen_data,neutral.sen_data,alka.sen_data)
overall.sen_data$moderator <- factor(overall.sen_data$moderator,
                                     levels = c("NirK","NirS","NosZ","NirKS.nosZ",
                                                "PH","Yield","N2O","Yield.scaled.N2O"),
                                     labels = c("nirK","nirS","nosZ","(nirK+nirS)/nosZ",
                                                "pH","Yield","N2O","Yield-scaled N2O"))
overall.sen_data$group <- factor(overall.sen_data$group,
                                 levels = c("Strongly acidic","Semi-arid",
                                            "Moderately acidic","Semi-humid",
                                            "Neutral","Humid",
                                            "Alkaline"))

ggplot(overall.sen_result, aes(x = estimate, y = name)) +
  #geom_quasirandom(data=overall.sen_data,aes(x = yi, y = moderator, size = precision,color=moderator),
  #                 groupOnX = FALSE, alpha = 0.1,width = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=name),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=name),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=name,shape=sig),size = 3) +
  facet_wrap(~group, nrow = 4, ncol = 2,scales = "free_y")+
  scale_colour_manual(values=c("#5988c4","#5988c4","#5988c4","#5988c4","black","#F95738","#48C9B0","#9C27B0"),
                      labels=c("nirK","nirS","nosZ","(nirK+nirS)/nosZ",
                               "pH","Yield",c(expression(paste(N[2],"O",sep = ""))),
                               c(expression(paste("Yield-scaled ",N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("nirK","nirS","nosZ","(nirK+nirS)/nosZ",
                            "pH","Yield",c(expression(paste(N[2],"O",sep = ""))),
                            c(expression(paste("Yield-scaled ",N[2],"O",sep = "")))),
                   position = "left")+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_bw()+
  labs(x = "Sensitivity (% per unit CC biomass N)") +
  guides(size = guide_legend("Precision"),color="none",
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = c(0.75,0.015),
        legend.background = element_blank(),legend.key = element_blank(),
        legend.box = "horizontal", panel.grid = element_blank(),
        axis.text.x = element_text(angle=30,vjust = 1,hjust=1),
        panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))##5.8*8.6

##### Correlation #####
a <- c%>%dplyr::select(rrnosZ,rrnirKS.nosZ,rrpH,rrN2O,rryield.scaled.N2O,AI,pH)
names(a) <- c("nosZ","(nirK+nirS)/nosZ","pH",paste0("N[2]*O"),paste0("Yield~scaled~N[2]*O"),"AI","pHi")
yi28 <- na.omit(a[,c(4,5,6:7)])
summary(yi28)

ggpairs(a[,1:5],
        lower = list(continuous=wrap("smooth",size=3,color="#5988c4",alpha=0.5),na='blank'),
        upper = list(continuous=wrap("cor",size=3.8,color="black"),na='blank'),
        labeller=label_parsed)+
  theme_bw()+
  font+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.border = element_rect(color = "black", size = 0.75),
        strip.background = element_rect(color = "black", size = 0.75))##5.8*8.6


  