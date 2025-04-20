library(metaforest)
library(caret)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rcartocolor)
library(parallel)
library(doParallel)
library(ggpmisc)
library(ggpointdensity)
library(viridis)

setwd("E:/Data/Second-order meta-analysis of CC trade-offs between yield and N2O emission")
font=theme(axis.title=element_text(size=13),axis.text = element_text(size=12,colour = 'black'),
           strip.text = element_text(size=12),legend.title = element_text(size = 12),
           legend.text = element_text(size = 12),plot.subtitle = element_text(size=13))#11.6inches

data <- read.csv("Original data-first order.csv",stringsAsFactors = T)
data$study <- as.factor(data$study)
data$yi <- data$rrN2O-data$rryield
data$vi <- data$varN2O+data$varyield

###yield###
mf_cv <- readRDS("yield_mf.RData")
final_yield = mf_cv$finalModel
data$Pred = predict(final_yield, data = data)$predictions
yield <- data
modpred.yield = rma(rryield,varyield,mods = ~Pred, data= yield)

p1 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield,aes(Pred,rryield, size = 1/(varyield + modpred.yield$tau2)),
                              color="#F95738",alpha=0.5) +
  geom_smooth(data = yield,aes(Pred,rryield, weight = 1/(varyield + modpred.yield$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  scale_size_continuous(range=c(1,3.6))+
  scale_y_continuous(limits = c(-1.3,1.2))+
  scale_x_continuous(limits = c(-1.3,1.2))+
  stat_poly_eq(data=yield,aes(Pred,rryield,
                              label=paste(..eq.label..,..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab("Predicted lnRR of yield")+
  ylab("Observed lnRR of yield")+
  theme(legend.position = "none")+font
p1

###N2O###
mf_cv <- readRDS("N2O_mf.RData")
final_N2O = mf_cv$finalModel
data$Pred = predict(final_N2O, data = data)$predictions
N2O <- data
modpred.N2O = rma(rrN2O,varN2O,mods = ~Pred, data= N2O)

p2 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed") + 
  geom_point(data = N2O,aes(Pred,rrN2O, size = 1/(varN2O + modpred.N2O$tau2)),
                              color="#48C9B0",alpha=0.5) +
  geom_smooth(data = N2O,aes(Pred,rrN2O, weight = 1/(varN2O + modpred.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  scale_size_continuous(range=c(1,3.6))+
  scale_y_continuous(limits = c(-1.4,2.2))+
  scale_x_continuous(limits = c(-1.4,2.2))+
  stat_poly_eq(data=N2O,aes(Pred,rrN2O,
                              label=paste(..eq.label..,..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("Predicted lnRR of ",N[2]*O,sep="")))+
  ylab(expression(paste("Observed lnRR of ",N[2]*O,sep="")))+
  theme(legend.position = "none")+font
p2

###yield.scaled.N2O###
mf_cv <- readRDS("yield.scaled.N2O_mf.RData")
final_yield.scaled.N2O = mf_cv$finalModel
data$Pred = predict(final_yield.scaled.N2O, data = data)$predictions
yield.scaled.N2O <- data
modpred.yield.scaled.N2O = rma(yi,vi,yield.scaled.N2O,mods = ~Pred, data= yield.scaled.N2O)

p3 <- ggplot()+
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") + 
  geom_abline(intercept = 0, slope = 1, color = "gray50", linetype = "dashed") + 
  geom_point(data = yield.scaled.N2O,aes(Pred,yi, size = 1/(vi + modpred.yield.scaled.N2O$tau2)),
             color="#9C27B0",alpha=0.5) +
  geom_smooth(data = yield.scaled.N2O,aes(Pred,yi, weight = 1/(vi + modpred.yield.scaled.N2O$tau2)),
              method = "lm", 
              formula = y ~ poly(x, 1, raw = TRUE), color = "black")+
  scale_size_continuous(range=c(1,3.6))+
  scale_y_continuous(limits = c(-1.3,1.9))+
  scale_x_continuous(limits = c(-1.3,1.9))+
  stat_poly_eq(data=yield.scaled.N2O,aes(Pred,yi,
                              label=paste(..eq.label..,..rr.label..,..p.value.label..,sep="*\", \"*")),
               formula = y ~ poly(x, 1, raw = TRUE), parse = T,label.x = "left",label.y = "top")+ 
  theme_cowplot()+
  xlab(expression(paste("Predicted lnRR of yield-scaled ",N[2]*O,sep="")))+
  ylab(expression(paste("Observed lnRR of yield-scaled ",N[2]*O,sep="")))+
  theme(legend.position = "none")+font
p3

ggdraw()+ 
  draw_plot(p1, x=0, y=0.5, width = 0.5, height = 0.5)+
  draw_plot(p2, x=0.5, y=0.5, width = 0.5, height = 0.5)+
  draw_plot(p3, x=0.25, y=0, width = 0.5, height = 0.5)+
  draw_plot_label(label = c("a","b","c"), size = 15,
                  x=c(0,0.5,0.25), y=c(1,1,0.5))##8.6*8.6inches

