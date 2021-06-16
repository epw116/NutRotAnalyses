#Analyzing ordinal data from a chestnut nut rot infection assay

setwd("~/R files")

#Import chestnut nut rot test 1 data

chestnut <- read.csv("chestnutordered.csv")
head(chestnut)

# Data Summary
ftable(xtabs(~Treatment+Species+Rank,data=chestnut))

library(MASS)
#set up 1st proportional odds logistic regression model for ordinal data, only considering effect of treatment
res.1 <- polr(factor(Rank)~Treatment,data=chestnut)
summary(res.1)

#Set up next model considering effects of treatment and species
res.2 <- polr(factor(Rank)~Treatment+Species,data=chestnut)
summary(res.2)

#Set up final model for data set 1 to only consider effect of species
res.3 <- polr(factor(Rank)~Species, data=chestnut)
summary(res.3)

#Check the goodness of fit of each model(Treatment[model1] only VS Treatment+Species[model 2])
anova(res.1,res.2)

#Check the goodness of fit (Treatment+Species[model 2] VS Species only[model 3])
anova(res.2,res.3)

ctable1 <- coef(summary(res.1))
ctable1
p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
p1
## combined table
(ctable1 <- cbind(ctable1, "p value" = p1))

ctable3 <- coef(summary(res.3))
p3 <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable3 <- cbind(ctable3, "p value" = p3))

ctable2 <- coef(summary(res.2))
p2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable2 <- cbind(ctable2, "p value" = p2))


#Load chestnut nut rot test 2

chestnut2 <- read.csv("chestnutordered2.csv")
head(chestnut2)

# Data Summary
ftable(xtabs(~Treatment+Species+Rank,data=chestnut2))

#Set up 1st model following same methods as above
res.12 <- polr(factor(Rank)~Treatment,data=chestnut2)
summary(res.12)

#Set up 2nd model
res.22 <- polr(factor(Rank)~Treatment+Species,data=chestnut2)
summary(res.22)

#Compare model 1 to model 2
anova(res.12,res.22)

ctable22 <- coef(summary(res.22))
p22 <- pnorm(abs(ctable22[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable22 <- cbind(ctable22, "p value" = p22))

#Import chestnut nut rot test 1 data with pooled fungal strains (treatments are 'G. castaneae vs control')
cnutLump <- read.csv("lumped.csv")
#set up 1st proportional odds logistic regression model for ordinal data, only considering effect of treatment
res.lump <- polr(factor(Rank)~Treatment,data=cnutLump)
summary(res.lump)

#Set up next model considering effects of treatment and species
res.lump2 <- polr(factor(Rank)~Treatment+Species,data=cnutLump)
summary(res.lump2)

#Set up final model for data set 1 to only consider effect of species
res.lump3 <- polr(factor(Rank)~Species, data=cnutLump)
summary(res.lump3)

anova(res.lump,res.lump2)