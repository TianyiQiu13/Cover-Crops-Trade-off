library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(grid)
library(ggpmisc)
library(metafor)
library(orchaRd)
library(ggbeeswarm)

doParallel::registerDoParallel(20)
setwd("E:/Data/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches

a <- read.csv("Subgroup-new.csv")
a$ID <- as.factor(a$ID)
a$number <- as.factor(a$number)
a$var <- (a$se2)^2
a <- dplyr::filter(a,quality=="high quality")
yield <- dplyr::filter(a,response_variable=="yield")
N2O <- dplyr::filter(a,response_variable=="N2O")

###Climate zone###
yield1 <- filter(yield,climate.zone!="")
N2O1 <- filter(N2O,climate.zone!="")

yield_climate <- rma.mv(effect_size2,var,data=yield1,mods=~climate.zone-1,random=~1|number/ID)
N2O_climate<- rma.mv(effect_size2,var,data=N2O1,mods=~climate.zone-1,random=~1|number/ID)

climate_y <- orchaRd::mod_results(yield_climate,mod="climate.zone",group="number")
climate_y_1 <- data.frame(climate_y$mod_table)%>%mutate(variable="yield")
climate_y_2 <- data.frame(climate_y$data)%>%mutate(precision=1/sqrt(vi),variable="yield")
climate_N <- orchaRd::mod_results(N2O_climate,mod="climate.zone",group="number")
climate_N_1 <- data.frame(climate_N$mod_table)%>%mutate(variable="N2O")
climate_N_2 <- data.frame(climate_N$data)%>%mutate(precision=1/sqrt(vi),variable="N2O")

climate_result <- rbind.data.frame(climate_y_1,climate_N_1)
climate_data <- rbind.data.frame(climate_y_2,climate_N_2)
climate_result$variable <- factor(climate_result$variable,
                                  levels = c("yield","N2O"),labels = c("Yield","N2O"))
climate_result$name <- factor(climate_result$name,
                              levels = c("Arid","Semi-arid","Semi-humid","Humid"))
climate_result$n <- c("(6)","(23)","(19)","(19)","(4)","(6)","(3)","(2)")
climate_result$sig <- c("b","a","b","b","a","a","b","a")
climate_data$variable <- factor(climate_data$variable,
                                levels = c("yield","N2O"),labels = c("Yield","N2O"))
climate_data$moderator <- factor(climate_data$moderator,
                                 levels = c("Arid","Semi-arid","Semi-humid","Humid"))

p1 <- ggplot(climate_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=climate_data,aes(x = yi, y = moderator, size = precision,color=variable),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=variable),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=variable),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=variable,shape=sig),size = 3) +
  geom_text(aes(label=n,x=upperCL+0.2), col="black", size=3.6)+ 
  facet_wrap(~variable,scales = "free_y",nrow=2)+
  scale_colour_manual(values=c("#F95738","#48C9B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("Arid","Semi-arid","Semi-humid","Humid"))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "LnRR",subtitle="Climate zone") +
  guides(size = guide_legend("Precision (1/SE)"),color=guide_legend("Response variable"),
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = "bottom",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1))
p1

###Soil texture###
yield2 <- filter(yield,texture!="")
N2O2 <- filter(N2O,texture!="")

yield_texture <- rma.mv(effect_size2,var,data=yield2,mods=~texture-1,random=~1|number/ID)
N2O_texture<- rma.mv(effect_size2,var,data=N2O2,mods=~texture-1,random=~1|number/ID)

texture_y <- orchaRd::mod_results(yield_texture,mod="texture",group="number")
texture_y_1 <- data.frame(texture_y$mod_table)%>%mutate(variable="yield")
texture_y_2 <- data.frame(texture_y$data)%>%mutate(precision=1/sqrt(vi),variable="yield")
texture_N <- orchaRd::mod_results(N2O_texture,mod="texture",group="number")
texture_N_1 <- data.frame(texture_N$mod_table)%>%mutate(variable="N2O")
texture_N_2 <- data.frame(texture_N$data)%>%mutate(precision=1/sqrt(vi),variable="N2O")

texture_result <- rbind.data.frame(texture_y_1,texture_N_1)
texture_data <- rbind.data.frame(texture_y_2,texture_N_2)
texture_result$variable <- factor(texture_result$variable,levels = c("yield","N2O"),labels = c("Yield","N2O"))
texture_result$name <- factor(texture_result$name,
                              levels = c("Sandy","Loam","Clay loam","Clay"))
texture_result$n <- c("(10)","(6)","(13)","(13)","(1)","(1)","(3)","(3)")
texture_result$sig <- c("b","b","b","a","b","b","b","b")
texture_data$variable <- factor(texture_data$variable,levels = c("yield","N2O"),labels = c("Yield","N2O"))
texture_data$moderator <- factor(texture_data$moderator,
                                 levels = c("Sandy","Loam","Clay loam","Clay"))

p2 <- ggplot(texture_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=texture_data,aes(x = yi, y = moderator, size = precision,color=variable),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=variable),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=variable),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=variable,shape=sig),size = 3) +
  geom_text(aes(label=n,x=upperCL+0.2), col="black", size=3.6)+ 
  facet_wrap(~variable,scales = "free_y",nrow=2)+
  scale_x_continuous(breaks = c(-0.8,-0.4,0,0.4,0.8,1.2))+
  scale_colour_manual(values=c("#F95738","#48C9B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("Sandy","Loam","   Clay loam","Clay"))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "LnRR",subtitle="Soil texture") +
  guides(size = guide_legend("Precision (1/SE)"),color=guide_legend("Response variable"),
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = "bottom",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1))
p2

###Main crop###
yield3 <- filter(yield,main.crop!="")

yield_main.crop <- rma.mv(effect_size2,var,data=yield3,mods=~main.crop-1,random=~1|number/ID)

main.crop_y <- orchaRd::mod_results(yield_main.crop,mod="main.crop",group="number")
main.crop_y_1 <- data.frame(main.crop_y$mod_table)%>%mutate(variable="yield")
main.crop_y_2 <- data.frame(main.crop_y$data)%>%mutate(precision=1/sqrt(vi),variable="yield")

main.crop_result <- rbind.data.frame(main.crop_y_1)
main.crop_data <- rbind.data.frame(main.crop_y_2)
main.crop_result$variable <- factor(main.crop_result$variable,
                                    levels = c("yield"),labels = c("Yield"))
main.crop_result$name <- factor(main.crop_result$name,
                                levels = rev(c("Wheat","Maize","Rice","Soybean","Others")),
                                labels = rev(c("Wheat (3)","Maize (15)","Rice (9)",
                                               "Soybean (1)","Others (5)")))
main.crop_result$sig <- c("a","b","a","b","b")
main.crop_data$variable <- factor(main.crop_data$variable,
                                  levels = c("yield"),labels = c("Yield"))
main.crop_data$moderator <- factor(main.crop_data$moderator,
                                   levels = rev(c("Wheat","Maize","Rice","Soybean","Others")),
                                   labels = rev(c("Wheat (3)","Maize (15)","Rice (9)",
                                                  "Soybean (1)","Others (5)")))

p3 <- ggplot(main.crop_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=main.crop_data,aes(x = yi, y = moderator, size = precision,color=variable),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=variable),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=variable),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=variable,shape=sig),size = 4) +
  scale_colour_manual(values=c("#F95738")) +
  scale_shape_manual(values = c(16,21))+
  theme_cowplot()+
  labs(x = "Effect size (lnRR)",subtitle="Main crop type") +
  guides(size = guide_legend("Precision (1/SE)"),color=guide_legend("Response variable"))+font+
  theme(axis.title.y = element_blank(),legend.position = "none",strip.text = element_blank())
p3

###Residue management###
yield4 <- filter(yield,application!="")
N2O4 <- filter(N2O,application!="")

yield_application <- rma.mv(effect_size2,var,data=yield4,mods=~application-1,random=~1|number/ID)
N2O_application<- rma.mv(effect_size2,var,data=N2O4,mods=~application-1,random=~1|number/ID)

application_y <- orchaRd::mod_results(yield_application,mod="application",group="number")
application_y_1 <- data.frame(application_y$mod_table)%>%mutate(variable="yield")
application_y_2 <- data.frame(application_y$data)%>%mutate(precision=1/sqrt(vi),variable="yield")
application_N <- orchaRd::mod_results(N2O_application,mod="application",group="number")
application_N_1 <- data.frame(application_N$mod_table)%>%mutate(variable="N2O")
application_N_2 <- data.frame(application_N$data)%>%mutate(precision=1/sqrt(vi),variable="N2O")

application_result <- rbind.data.frame(application_y_1,application_N_1)
application_data <- rbind.data.frame(application_y_2,application_N_2)
application_result$variable <- factor(application_result$variable,
                                      levels = c("yield","N2O"),labels = c("Yield","N2O"))
application_result$name <- factor(application_result$name,
                                  levels = c("Incorporated","Surface","Removed"))
application_result$n <- c("(12)","(5)","(6)","(2)","(2)","(2)")
application_result$sig <- c("a","b","b","b","b","b")
application_data$variable <- factor(application_data$variable,
                                    levels = c("yield","N2O"),labels = c("Yield","N2O"))
application_data$moderator <- factor(application_data$moderator,
                                     levels = c("Incorporated","Surface","Removed"))

p4 <- ggplot(application_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=application_data,aes(x = yi, y = moderator, size = precision,color=variable),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=variable),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=variable),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=variable,shape=sig),size = 3) +
  geom_text(aes(label=n,x=upperCL+0.2), col="black", size=3.6)+ 
  facet_wrap(~variable,scales = "free_y",nrow=2)+
  scale_colour_manual(values=c("#F95738","#48C9B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))))) +
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "LnRR",subtitle="CC residue management") +
  guides(size = guide_legend("Precision (1/SE)"),color=guide_legend("Response variable"),
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = "bottom",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1))
p4

###CC type###
yield5 <- filter(yield,CC.type!="")
N2O5 <- filter(N2O,CC.type!="")
yield5 <- filter(yield5,CC.type!="overall")
N2O5 <- filter(N2O5,CC.type!="overall")

yield_CC.type <- rma.mv(effect_size2,var,data=yield5,mods=~CC.type-1,random=~1|number/ID)
N2O_CC.type<- rma.mv(effect_size2,var,data=N2O5,mods=~CC.type-1,random=~1|number/ID)

CC.type_y <- orchaRd::mod_results(yield_CC.type,mod="CC.type",group="number")
CC.type_y_1 <- data.frame(CC.type_y$mod_table)%>%mutate(variable="yield")
CC.type_y_2 <- data.frame(CC.type_y$data)%>%mutate(precision=1/sqrt(vi),variable="yield")
CC.type_N <- orchaRd::mod_results(N2O_CC.type,mod="CC.type",group="number")
CC.type_N_1 <- data.frame(CC.type_N$mod_table)%>%mutate(variable="N2O")
CC.type_N_2 <- data.frame(CC.type_N$data)%>%mutate(precision=1/sqrt(vi),variable="N2O")

CC.type_result <- rbind.data.frame(CC.type_y_1,CC.type_N_1)
CC.type_data <- rbind.data.frame(CC.type_y_2,CC.type_N_2)
CC.type_result$variable <- factor(CC.type_result$variable,
                                  levels = c("yield","N2O"),labels = c("Yield","N2O"))
CC.type_result$name <- factor(CC.type_result$name,
                              levels = c("Legume","Mixture","Non-legume"))
CC.type_result$n <- c("(38)","(20)","(42)","(4)","(3)","(4)")
CC.type_result$sig <- c("a","a","b","a","b","b")
CC.type_data$variable <- factor(CC.type_data$variable,
                                levels = c("yield","N2O"),labels = c("Yield","N2O"))
CC.type_data$moderator <- factor(CC.type_data$moderator,
                                 levels = c("Legume","Mixture","Non-legume"))

p5 <- ggplot(CC.type_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=CC.type_data,aes(x = yi, y = moderator, size = precision,color=variable),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=variable),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=variable),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=variable,shape=sig),size = 3) +
  geom_text(aes(label=n,x=upperCL+0.2), col="black", size=3.6)+ 
  facet_wrap(~variable,scales = "free_y",nrow=2)+
  scale_colour_manual(values=c("#F95738","#48C9B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("Legume","Mixture","     Non-legume"))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "LnRR",subtitle="CC type") +
  guides(color=guide_legend("Response variable",order = 1),
         size = guide_legend("Precision (1/SE)",order = 3),
         shape=guide_legend("Significance",order=2))+font+
  theme(axis.title.x = element_blank(),legend.position = "right",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1))
p5

###N rate###
yield6 <- filter(yield,N.rate!="")

yield_N.rate <- rma.mv(effect_size2,var,data=yield6,mods=~N.rate-1,random=~1|number/ID)

N.rate_y <- orchaRd::mod_results(yield_N.rate,mod="N.rate",group="number")
N.rate_y_1 <- data.frame(N.rate_y$mod_table)%>%mutate(variable="yield")
N.rate_y_2 <- data.frame(N.rate_y$data)%>%mutate(precision=1/sqrt(vi),variable="yield")

N.rate_result <- rbind.data.frame(N.rate_y_1)
N.rate_data <- rbind.data.frame(N.rate_y_2)
N.rate_result$variable <- factor(N.rate_result$variable,labels = c("Yield"))
N.rate_result$name <- factor(N.rate_result$name,
                             levels = c("Low","Recommended","High"))
N.rate_result$n <- c("(12)","(12)","(8)")
N.rate_result$sig <- c("b","a","a")
N.rate_data$variable <- factor(N.rate_data$variable,labels = c("Yield"))
N.rate_data$moderator <- factor(N.rate_data$moderator,
                                levels = c("Low","Recommended","High"))

p6 <- ggplot(N.rate_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=N.rate_data,aes(x = yi, y = moderator, size = precision,color=variable),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=variable),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=variable),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=variable,shape=sig),size = 3) +
  geom_text(aes(label=n,x=upperCL+0.2), col="black", size=3.6)+ 
  facet_wrap(~variable,scales = "free_y",nrow=2)+
  scale_colour_manual(values=c("#F95738","#48C9B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("Low","Recommended","High"))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "LnRR",subtitle="N application rate") +
  guides(size = guide_legend("Precision (1/SE)"),color=guide_legend("Response variable"),
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = "bottom",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1))
p6

###Termination time###
yield7 <- filter(yield,termination.time!="")
N2O7 <- filter(N2O,termination.time!="")

yield_termination.time <- rma.mv(effect_size2,var,data=yield7,
                                 mods=~termination.time-1,random=~1|number/ID)
N2O_termination.time<- rma.mv(effect_size2,var,data=N2O7,
                              mods=~termination.time-1,random=~1|ID)

termination.time_y <- orchaRd::mod_results(yield_termination.time,
                                           mod="termination.time",group="number")
termination.time_y_1 <- data.frame(termination.time_y$mod_table)%>%mutate(variable="yield")
termination.time_y_2 <- data.frame(termination.time_y$data)%>%
  mutate(precision=1/sqrt(vi),variable="yield")
termination.time_N <- orchaRd::mod_results(N2O_termination.time,
                                           mod="termination.time",group="number")
termination.time_N_1 <- data.frame(termination.time_N$mod_table)%>%mutate(variable="N2O")
termination.time_N_2 <- data.frame(termination.time_N$data)%>%
  mutate(precision=1/sqrt(vi),variable="N2O")

termination.time_result <- rbind.data.frame(termination.time_y_1,termination.time_N_1)
termination.time_data <- rbind.data.frame(termination.time_y_2,termination.time_N_2)
termination.time_result$variable <- factor(termination.time_result$variable,
                                           levels = c("yield","N2O"),labels = c("Yield","N2O"))
termination.time_result$name <- factor(termination.time_result$name,
                                       levels = c("Early","Mid","Late"))
termination.time_result$n <- c("(4)","(3)","(3)","(1)","(1)","(1)")
termination.time_result$sig <- c("b","b","b","a","a","b")
termination.time_data$variable <- factor(termination.time_data$variable,
                                         levels = c("yield","N2O"),labels = c("Yield","N2O"))
termination.time_data$moderator <- factor(termination.time_data$moderator,
                                          levels = c("Early","Mid","Late"))

p7 <- ggplot(termination.time_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=termination.time_data,aes(x = yi, y = moderator, size = precision,color=variable),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=variable),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=variable),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=variable,shape=sig),size = 3) +
  geom_text(aes(label=n,x=upperCL+0.2), col="black", size=3.6)+ 
  facet_wrap(~variable,scales = "free_y",nrow=2)+
  scale_colour_manual(values=c("#F95738","#48C9B0"),
                      labels=c("Yield",c(expression(paste(N[2],"O",sep = ""))))) +
  scale_y_discrete(labels=c("Early","Mid","           Late"))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  coord_flip()+
  theme_cowplot()+
  labs(x = "LnRR",subtitle="CC termination time") +
  guides(size = guide_legend("Precision (1/SE)"),color=guide_legend("Response variable"),
         shape=guide_legend("Significance"))+font+
  theme(axis.title.x = element_blank(),legend.position = "bottom",strip.text = element_blank(),
        axis.text.x = element_text(angle=15,vjust = 1,hjust=1))
p7

p8 <- lemon::grid_arrange_shared_legend(p5,p6,p1,p7,p2,p4,nrow=3,ncol=2,position = "right")

ggdraw()+ 
  draw_plot(p3, x=0, y=0, width = 1/3, height = 1)+
  draw_plot(p8, x=1/3-0.005, y=0, width = 2/3, height = 1)+
  draw_plot_label(label = c("a","b","c","d","e","f","g"), size = 15,
                  x=c(0.01,1/3-0.01,1/3-0.01,1/3-0.01,2/3-0.07,2/3-0.07,2/3-0.07),
                  y=c(1,1,2/3,1/3,1,2/3,1/3))##11.6*8.6

