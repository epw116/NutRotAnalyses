setwd("~/R files")

#Graphing ordinal data from oak nut rot analyses.
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

#Import oak nut rot test 1 pooled treatments data (only considering G. castaneae vs control)
oaklump1 <- read.csv("oaklumped1.csv")
head(oaklump1)

# Data Summary
ftable(xtabs(~Treatment+Species+Rank,data=oaklump1))

#order the levels
oaklump1$Species <- ordered(oaklump1$Species, levels = c("Red","Pin","White"))

ano2 <- ggplot(oaklump1, aes(x=Treatment, y=Rank, fill=Treatment, 
                             color=Treatment))+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1)) #Add data points to boxplots
+facet_wrap(~Species)+labs(y="Rot Rank",x="Treatment") #create separate displays/grids for each species
+ggtitle("Oak Nut Rot Test 1 Lumped Treatments")
+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5) #Add significance asterisks
+theme_bw()+theme(strip.background = element_blank()) #simple black and white theme and remove borders around facet titles
+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #get rid of background grid
+theme(legend.position = "none") #remove legend
ano2

#Save the figure. Can specify width and height if dev.off function follows
ggsave(ano2, filename = 'OakLumped1.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
#dev.off overrides Rs default device and allows height and width arguments for figures
dev.off()

#Import oak nut rot test 2 pooled data (pooled treatments, same as above)
oaklump2 <- read.csv("oaklump2.csv")
oaklump2$Species <- ordered(oaklump2$Species, levels = c("Red","Pin","White"))
ano3 <- ggplot(oaklump2, aes(x=Treatment, y=Rank, fill=Treatment, 
                             color=Treatment))+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  facet_wrap(~Species)+labs(y="Rot Rank",x="Treatment")+
  ggtitle("Oak Nut Rot Test 2 Lumped Treatments")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano3
ggsave(ano3, filename = 'OakLumped2.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Import oak nut rot test 1 data only considering treatments (no species)
only1 <- read.csv("oaktrtonly1.csv")
#Order data by treatment level
only1$Treatment <- ordered(only1$Treatment, levels = c("Control","Erie","Maryland","UP"))
ano4 <- ggplot(only1, aes(x=Treatment, y=Rank, fill=Treatment, 
                          color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Oak Nut Rot by Treatment Test 1")+labs(y="Rot Rank",x="Treatment") #no facet_wrap needed for this because no species analyzed
+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano4
ggsave(ano4, filename = 'OakTreats1.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Import oak nut rot test 2 data only considering treatments (no species)
only2 <- read.csv("oaktrtonly2.csv")
only2$Treatment <- ordered(only2$Treatment, levels = c("Control","Erie","Maryland","UP"))

ano5 <- ggplot(only2, aes(x=Treatment, y=Rank, fill=Treatment, 
                          color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Oak Nut Rot by Treatment Test 2")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano5
ggsave(ano5, filename = 'OakTreats2.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Import oak nut rot test 1 full data set
oak <- read.csv("oakordered1.csv")

#View the first 6 lines of data set
head(oak)

#Order data for treatment and Species
oak$Treatment <- ordered(oak$Treatment, levels = c("Control","Erie","Maryland","UP"))
oak$Species <- ordered(oak$Species, levels = c("Red","Pin","White"))

ano6 <- ggplot(oak, aes(x=Treatment, y=Rank, fill=Treatment, 
                        color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+facet_wrap(~Species)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Oak Nut Rot Test 1")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano6
ggsave(ano6, filename = 'OakFull1.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()


#Import oak nut rot test 2 full data set
oak2 <- read.csv("oakordered2.csv")
oak2$Treatment <- ordered(oak2$Treatment, levels = c("Control","Erie","Maryland","UP"))
oak2$Species <- ordered(oak2$Species, levels = c("Red","Pin","White"))
head(oak2)

ano7 <- ggplot(oak2, aes(x=Treatment, y=Rank, fill=Treatment, 
                         color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+facet_wrap(~Species)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Oak Nut Rot Test 2")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano7
ggsave(ano7, filename = 'OakFull2.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Import full data set of combined test data (test 1 and test 2 combined)
oakAll <- read.csv("oakorderedboth.csv")
oakAll$Treatment <- ordered(oakAll$Treatment, levels = c("Control","Erie","Maryland","UP"))
oakAll$Species <- ordered(oakAll$Species, levels = c("Red","Pin","White"))

ano8 <- ggplot(oakAll, aes(x=Treatment, y=Rank, fill=Treatment, 
                           color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+facet_wrap(~Species)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Oak Nut Rot Test Combined")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano8
ggsave(ano8, filename = 'OakNutTestDataCombined.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Import combined data set considering treatments only (no species)
oakTrtsAll <- read.csv("oaktrtonlyAll.csv")
oakTrtsAll$Treatment <- ordered(oakTrtsAll$Treatment, levels = c("Control","Erie","Maryland","UP"))
ano9 <- ggplot(oakTrtsAll, aes(x=Treatment, y=Rank, fill=Treatment, 
                               color=Treatment),xlab="Nut Species",ylab="Rot Rank")+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  ggtitle("Oak Nut Rot by Treatment Combined Tests")+labs(y="Rot Rank",x="Treatment")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano9
ggsave(ano9, filename = 'OakTreatsCombinedTests.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()

#Import combined data set with pooled treatments (G. castaneae vs control)
oaklumpAll <- read.csv("oaklumpALL.csv")
oaklumpAll$Species <- ordered(oaklumpAll$Species, levels = c("Red","Pin","White"))
ano10 <- ggplot(oaklumpAll, aes(x=Treatment, y=Rank, fill=Treatment, 
                                color=Treatment))+
  geom_boxplot(alpha=0.5)+
  geom_point(alpha=0.5, position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))+
  facet_wrap(~Species)+labs(y="Rot Rank",x="Treatment")+
  ggtitle("Oak Nut Rot Test Pooled Tests Combined")+stat_compare_means(ref.group = "Control", label="p.signif",label.y=3.5)+
  theme_bw()+theme(strip.background = element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+theme(legend.position = "none")
ano10
ggsave(ano10, filename = 'OakPooledDataTestsCombined.png', dpi = 300, type = 'cairo',
       width = 6, height = 4, units = 'in')
dev.off()
