setwd("~/R files")
#Graphing ordinal data from chestnut nut rot analyses.
#Not all of these library packages may be necessary.

library(Rmisc)
library(tidyr)
library(MASS)
library(nnet)
library(car)
library(splines)
library(effects)
library(multcomp)
library(multcompView)
library(ggplot2)
library(dplyr)
library(viridis)
#install.packages("hrbrthemes")
library(hrbrthemes)
#install.packages("doBy")
library(doBy)
library(ggpubr)

#Importing chestnut nut rot test 1 full data
cnut1 <- read.csv("chestnutordered.csv")

#Ordering the levels
cnut1$Treatment <- ordered(cnut1$Treatment, levels = c("Control","Erie","Maryland","UP"))
cnut1$Species <- ordered(cnut1$Species, levels = c("American","Chinese","Hybrid"))

cnutplot1 <- ggplot(cnut1, aes(x=Treatment, y=Rank, fill=Treatment, 
                           color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+facet_wrap(~Species) #create separate displays for each species 
+geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1)) #Add data points to boxplot 
  +ggtitle("Chestnut Rot Test 1 Full")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5) #Add significance asterisks +
  theme_bw()+theme(strip.background = element_blank()) #simple black and white theme and remove borders around facet titles
  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #get rid of background grid
  +theme(legend.position = "none") #remove legend
cnutplot1
ggsave(cnutplot1, filename = 'ChestnutRotTest1Full.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Importing chestnut nut rot test 1 without nut species. Considering treatments/strains only
cnutTrt1 <- read.csv("chestnuttrtsonly1.csv")
cnutTrt1$Treatment <- ordered(cnutTrt1$Treatment, levels = c("Control","Erie","Maryland","UP"))
Trt1plot <- ggplot(cnutTrt1, aes(x=Treatment, y=Rank, fill=Treatment, 
                               color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5) #No facet_wrap in this one, because there are no species to consider
+geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Chestnut Rot by Treatment 1")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
Trt1plot
ggsave(Trt1plot, filename = 'ChestnutRotTest1Trt.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Importing chestnut nut rot test 1 with pooled strains. Only considering treated vs control for each nut species
cnutpool1 <- read.csv("chestnutpooled1.csv")
cnutpool1$Species <- ordered(cnutpool1$Species, levels = c("American","Chinese","Hybrid"))
pooled1 <- ggplot(cnutpool1, aes(x=Treatment, y=Rank, fill=Treatment, 
                                color=Treatment))+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  facet_wrap(~Species)+labs(y="Rot Rank",x="Treatment")+
  ggtitle("Chestnut Rot Test 1 Pooled")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
pooled1
ggsave(pooled1, filename = 'ChestnutRotTest1Pool.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

cnut2 <- read.csv("chestnutordered2.csv")

cnut2$Treatment <- ordered(cnut2$Treatment, levels = c("Control","Erie","Maryland","UP"))
cnut2$Species <- ordered(cnut2$Species, levels = c("American","Chinese","Hybrid"))

cnutplot2 <- ggplot(cnut2, aes(x=Treatment, y=Rank, fill=Treatment, 
                               color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+facet_wrap(~Species)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Chestnut Rot Test 2 Full")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
cnutplot2
ggsave(cnutplot2, filename = 'ChestnutRotTest2Full.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()


cnutTrt2 <- read.csv("chestnuttrtsonly2.csv")
cnutTrt2$Treatment <- ordered(cnutTrt2$Treatment, levels = c("Control","Erie","Maryland","UP"))
Trt2plot <- ggplot(cnutTrt2, aes(x=Treatment, y=Rank, fill=Treatment, 
                                 color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Chestnut Rot by Treatment 2")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
Trt2plot
ggsave(Trt2plot, filename = 'ChestnutRotTest2Trt.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

cnutpool2 <- read.csv("chestnutpooled2.csv")
cnutpool2$Species <- ordered(cnutpool2$Species, levels = c("American","Chinese","Hybrid"))
pooled2 <- ggplot(cnutpool2, aes(x=Treatment, y=Rank, fill=Treatment, 
                                 color=Treatment))+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  facet_wrap(~Species)+labs(y="Rot Rank",x="Treatment")+
  ggtitle("Chestnut Rot Test 2 Pooled")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
pooled2
ggsave(pooled2, filename = 'ChestnutRotTest2Pool.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

cnutAll <- read.csv("chestnutorderedAll.csv")

cnutAll$Treatment <- ordered(cnutAll$Treatment, levels = c("Control","Erie","Maryland","UP"))
cnutAll$Species <- ordered(cnutAll$Species, levels = c("American","Chinese","Hybrid"))

cnutplotAll <- ggplot(cnutAll, aes(x=Treatment, y=Rank, fill=Treatment, 
                               color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+facet_wrap(~Species)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Chestnut Rot Test 2 Combined")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
cnutplotAll
ggsave(cnutplotAll, filename = 'ChestnutRotTestCombined.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()


cnutTrtAll <- read.csv("chestnuttrtsonlyAll.csv")
cnutTrtAll$Treatment <- ordered(cnutTrtAll$Treatment, levels = c("Control","Erie","Maryland","UP"))
TrtAllplot <- ggplot(cnutTrtAll, aes(x=Treatment, y=Rank, fill=Treatment, 
                                 color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Chestnut Rot by Treatment Combined")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
TrtAllplot
ggsave(TrtAllplot, filename = 'ChestnutRotTestCombinedTrt.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

cnutpoolAll <- read.csv("chestnutpooledAll.csv")
cnutpoolAll$Species <- ordered(cnutpoolAll$Species, levels = c("American","Chinese","Hybrid"))
pooledAll <- ggplot(cnutpoolAll, aes(x=Treatment, y=Rank, fill=Treatment, 
                                 color=Treatment))+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  facet_wrap(~Species)+labs(y="Rot Rank",x="Treatment")+
  ggtitle("Chestnut Rot Test 2 Pooled")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
pooledAll
ggsave(pooledAll, filename = 'ChestnutRotAllPool.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()