library(dplyr)
library(janitor)
library(metafor)
library(orchaRd)
library(ggplot2)
library(patchwork)
library(ggbeeswarm)

doParallel::registerDoParallel(20)
setwd("E:/Data/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches

a <- read.csv("Overall-second order-new.csv")
a$ID <- as.factor(a$ID)
a$number <- as.factor(a$number)
a$var <- (a$se2)^2

# bias corrected t-test
a_result1 <- rma.mv(effect_size2,var,data=a,mods=~response_variable-1,random=~1|number/ID,test="t")
a_result2 <- rma.mv(effect_size2,var,mods=~var+response_variable-1,data=a,random=~1|number/ID,test="t")

t.test <- orchaRd::mod_results(a_result1,mod="response_variable",group="number")
t.test_result <- data.frame(t.test$mod_table)
BCT_result <- data.frame(a_result2$beta,a_result2$ci.lb,a_result2$ci.ub)
names(BCT_result) <- c("estimate","lowerCL","upperCL")
BCT_result <- BCT_result[-1,]
BCT_result <- BCT_result%>%mutate(name=c("N2O","Yield"),y=c(1.2,1.8))
t.test_data <- data.frame(t.test$data)%>%mutate(precision=1/sqrt(vi))
t.test_result$name <- factor(t.test_result$name,levels = rev(c("Yield","N2O")))
t.test_result <- t.test_result%>%mutate(n=c("(11, 8)","(54, 24)"))
t.test_data$moderator <- factor(t.test_data$moderator,levels = rev(c("Yield","N2O")))

t.test_plot <- ggplot(t.test_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=t.test_data,aes(x = yi, y = moderator, size = precision,color=moderator),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=name),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=name),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=name),shape=16,size = 3) +
  geom_text(aes(label=n,x=upperPR+0.2), col="black", size=4.2)+ 
  annotate("text", x = -0.5, y = 2.2, size = 4.2,
           label = "t-test: 0.087 (95% CI: 0.022, 0.15)") +
  annotate("text", x = -0.5, y = 0.8, size = 4.2,
           label = "t-test: 0.41 (95% CI: 0.24, 0.58)") +
  geom_point(data=BCT_result,aes(x=estimate,y=y,color=name),shape=18,size = 4) +
  geom_errorbarh(data=BCT_result,aes(y = y,xmin = lowerCL, xmax = upperCL,color=name),
                 height = 0, show.legend = FALSE, size = 1.2) +
  annotate("text", x = 0.5, y = 1.6, size = 4.2,
           label = "Bias-corrected estimate: 0.10 (95% CI: 0.038, 0.17)") +
  annotate("text", x = 0.5, y = 1.4, size = 4.2,
           label = "Bias-corrected estimate: 0.48 (95% CI: 0.30, 0.66)") +
  scale_colour_manual(values=rev(c("#F95738","#48C9B0")),
                      labels=rev(c("Yield",c(expression(paste(N[2],"O",sep = "")))))) +
  scale_y_discrete(labels=rev(c("Yield",c(expression(paste(N[2],"O",sep = ""))))))+
  theme_cowplot()+
  labs(x = "Effect size (lnRR)",subtitle="Second-order meta-analysis based on t-test") +
  guides(size = guide_legend("Precision (1/SE)"),color="none",
         shape="none")+font+
  theme(axis.title.y = element_blank(),legend.position = "right")
t.test_plot

# based on original weights and quality
a$quality_weight <- a$quality_score/max(a$quality_score)
a_result_quality<- rma.mv(effect_size2,var,mods=~response_variable-1,
                          W=weights(a_result)*quality_weight,data=a,random=~1|number/ID)

quality <- orchaRd::mod_results(a_result_quality,mod="response_variable",group="number")
quality_result <- data.frame(quality$mod_table)
quality_data <- data.frame(quality$data)%>%mutate(precision=1/sqrt(vi))
quality_result$name <- factor(quality_result$name,levels = rev(c("Yield","N2O")))
quality_result$n <- c("(11, 8)","(54, 24)")
quality_result$sig <- c("a","a")
quality_data$moderator <- factor(quality_data$moderator,levels = rev(c("Yield","N2O")))

quality_plot <- ggplot(quality_result, aes(x = estimate, y = name)) +
  geom_quasirandom(data=quality_data,aes(x = yi, y = moderator, size = precision,color=moderator),
                   groupOnX = FALSE, alpha = 0.1) +
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR,color=name),
                 height = 0, show.legend = FALSE, size = 0.5, alpha = 0.6) +
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL,color=name),
                 height = 0, show.legend = FALSE, size = 1.2) +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", alpha = 0.5) +
  geom_point(aes(color=name,shape=sig),size = 3) +
  geom_text(aes(label=n,x=upperPR+0.2), col="black", size=4.2)+ 
  annotate("text", x = -0.25, y = 2.2, size = 4.2,
           label = "Quality-fitted estimate: 0.088 (95% CI: 0.024, 0.15)") +
  annotate("text", x = 0.75, y = 0.8, size = 4.2,
           label = "Quality-fitted estimate: 0.43 (95% CI: 0.26, 0.60)") +
  scale_colour_manual(values=rev(c("#F95738","#48C9B0")),
                      labels=rev(c("Yield",c(expression(paste(N[2],"O",sep = "")))))) +
  scale_y_discrete(labels=rev(c("Yield",c(expression(paste(N[2],"O",sep = ""))))))+
  scale_shape_manual(values = c(16,21),labels=c("Significant","Non-significant"))+
  theme_cowplot()+
  labs(x = "Effect size (lnRR)",subtitle="Second-order meta-analysis based on quality") +
  guides(size = guide_legend("Precision (1/SE)"),color="none",
         shape="none")+font+
  theme(axis.title.y = element_blank(),legend.position = "right")
quality_plot

p <- lemon::grid_arrange_shared_legend(t.test_plot,quality_plot,nrow=2,ncol=1,position = "top")

ggdraw()+ 
  draw_plot(p, x=0.01, y=0, width = 0.98, height = 1)+
  draw_plot_label(label = c("a","b"), size = 15,
                  x=c(0.01,0.01),
                  y=c(0.975,0.4875))##6.2*9.4
