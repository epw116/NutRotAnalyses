#Analyzing ordinal data from an oak nut rot infection assay

setwd("~/R files")
library(tidyr)
library(MASS)
library(emmeans)

#Import oak nut rot test 1 full data set
oak <- read.csv("oakordered1.csv")

#View the first 6 lines of data set
head(oak)

#Change the order of the treatments and species
oak$Treatment <- ordered(oak$Treatment, levels = c("Control","Erie","Maryland","UP"))
oak$Species <- ordered(oak$Species, levels = c("Red","Pin","White"))

# Data Summary
ftable(xtabs(~Treatment+Species+Rank,data=oak))

#set up 1st proportional odds logistic regression model for ordinal data, only considering effect of treatment
res.1 <- polr(factor(Rank)~Treatment,data=oak)
summary(res.1)

#set up next model considering the effects of treatment and species
res.2 <- polr(factor(Rank)~Treatment+Species,data=oak)
summary(res.2)

#compare goodness of fit of model 1 (treatment only) to model 2 (treatment+species)
anova(res.1,res.2)

#Produce table of coefficients from model
ctable2 <- coef(summary(res.2))
ctable2

#Calculate c.d.f (probability distribution) of coefficients (calculates the p-values)
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2
p2

## combined table of probabilities with coefficients. Will have t-value and p-value
(ctable2 <- cbind(ctable2, "p value" = p2))

#Obtain least squares means for treatment
TreatmentL1.emm <-  emmeans(res.2,"Treatment")
TreatmentL1.emm
cld(TreatmentL1.emm,alpha=0.05,Letters=LETTERS) #add letters to denote differences in means

#Obtain least squares means for species
SpeciesL1.emm <-  emmeans(res.2,"Species")
SpeciesL1.emm
cld(SpeciesL1.emm,alpha=0.05,Letters=LETTERS)


#Load oak nut rot test 2 full data set
oak2 <- read.csv("oakordered2.csv")
oak2$Treatment <- ordered(oak2$Treatment, levels = c("Control","Erie","Maryland","UP"))
oak2$Species <- ordered(oak2$Species, levels = c("Red","Pin","White"))
head(oak2)

# Data Summary
ftable(xtabs(~Treatment+Species+Rank,data=oak2))

res.12 <- polr(factor(Rank)~Treatment,data=oak2)
summary(res.12)

res.22 <- polr(factor(Rank)~Treatment+Species,data=oak2)
summary(res.22)

anova(res.12,res.22)

#Produce table of coefficients from model
ctable22 <- coef(summary(res.22))
ctable22

#Calculate c.d.f (probability distribution) of coefficients (calculates the p-values)
p22 <- pnorm(abs(ctable22[, "t value"]), lower.tail = FALSE) * 2
p22

## combined table of probabilities with coefficients. Will have t-value and p-value
(ctable22 <- cbind(ctable22, "p value" = p22))

#Obtain least squares means for treatment
TreatmentL2.emm <-  emmeans(res.22,"Treatment")
TreatmentL2.emm
cld(TreatmentL2.emm,alpha=0.05,Letters=LETTERS) #add letters to denote differences in means

#Obtain least squares means for species
SpeciesL2.emm <-  emmeans(res.22,"Species")
SpeciesL2.emm
cld(SpeciesL2.emm,alpha=0.05,Letters=LETTERS)
