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

a <- read.csv("Subgroup-new.csv")
a$ID <- as.factor(a$ID)
a$number <- as.factor(a$number)
a$var <- (a$se2)^2
yield <- dplyr::filter(a,response_variable=="yield")
N2O <- dplyr::filter(a,response_variable=="N2O")

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
                                labels = rev(c("Wheat (17)","Maize (7)","Rice (10)",
                                               "Soybean (2)","Others (5)")))
main.crop_result$sig <- c("a","b","a","b","b")
main.crop_data$variable <- factor(main.crop_data$variable,
                                  levels = c("yield"),labels = c("Yield"))
main.crop_data$moderator <- factor(main.crop_data$moderator,
                                   levels = rev(c("Wheat","Maize","Rice","Soybean","Others")),
                                   labels = rev(c("Wheat (17)","Maize (7)","Rice (9)",
                                                  "Soybean (2)","Others (5)")))

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
  labs(x = "LnRR of yield",subtitle="Cash crop type") +
  guides(size = guide_legend("Precision (1/SE)"),color=guide_legend("Response variable"))+font+
  theme(axis.title.y = element_blank(),legend.position = "none",strip.text = element_blank())
p3##6.2*7.0
