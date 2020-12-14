############################################################################
### Models for behaviour data from angling selection experiment.
### Jenni Prokkola 2019
### 
############################################################################
library(ggplot2)
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(lme4)
library(lmerTest)
library(coxme)
library(survival)
library(lmfor)
library(nlme)
library(car)
library(tidyverse)
# Install source packages to use ggeffects: 
#library(devtools)
#devtools::install_github("strengejacke/sjlabelled")
#devtools::install_github("strengejacke/sjmisc")
#devtools::install_github("glmmTMB/glmmTMB/glmmTMB")
#devtools::install_github("strengejacke/sjstats")
#devtools::install_github("strengejacke/ggeffects")
library(ggeffects)
library(blmeco) 
library(vegan)

### 1. Import data. See README of data descriptions for details.

Behav_all<-read.table(file="Behavdata_Prokkola_etal.txt", header=T, sep="\t", dec=".")
# Note: Out time min gets value 9.75 when fish did not emerge during trial.
str(Behav_all)
# Beh batch has two levels above 5, but with only 4 fish. These can be combined to batch 5 for the purpose of the analysis.
Behav_all$Beh_batch<-ifelse(Behav_all$Beh_batch >5, 5, Behav_all$Beh_batch)
Behav_all$Beh_batch<-as.factor(Behav_all$Beh_batch)
Behav_all$Arena<-as.factor(Behav_all$Arena)
Behav_all$ID<-as.factor(Behav_all$ID)

# Subset data, as some individuals did not emerge and are not included in the model
Intensity_data<-subset(Behav_all, Intensity >-1)

### 2. Correlate behavior traits with each other for the trials where fish emerged from start box
par(mfcol=c(3,1), mar = c(5, 4, 2, 1))
# Exploration intensity and latency 
plot(Intensity_data$Out_time_min~Intensity_data$Intensity,
     xlab = "Exploration intensity",
     ylab = "Latency")
cor.test(Intensity_data$Out_time_min,Intensity_data$Intensity)

# Exploration intensity and exploration tendency
boxplot(Intensity_data$Intensity~Intensity_data$Explorated,
        xlab = "Exploration tendency",
        ylab = "Exploration intensity")

# Latency and exploration tendency
boxplot(Intensity_data$Out_time_min~Intensity_data$Explorated,
        xlab = "Exploration tendency",
        ylab = "Latency")


### Since the traits can be correlated between the fish that emerged from the box, make PCAs with these variables.
# Make a dataset with scaled variables with NAs removed

Beh_NAomit<- Intensity_data[,c(2:7,9,10,12:17)]
str(Beh_NAomit)
Beh_NAomit<-na.omit(Beh_NAomit)
Beh_NAomit[,12:14]<-scale(Beh_NAomit[,12:14])

# PCA
Beh_RDA <- rda(Beh_NAomit[,12:14])  

summary(Beh_RDA, scaling = 3)  

# Plot PCA
biplot(Beh_RDA, scaling = 3, display="species", type = c("text"),
       cex=c(0.5, 0.5),col="black", xlabs = c("Latency", "Intensity", "Tendency"))
       
# Plot individual observations
#points(Beh_RDA, display="sites", pch=c(1,2,3,4) [as.numeric(Beh_NAomit$Group)], col= c("black"),cex=1) 

# Plot confidence ellipses
#ordiellipse(Beh_RDA,Beh_NAomit$Group, conf = 0.95,
#            kind = "se", lwd = 2, draw = "polygon", alpha = 63)


### 2. Mixed models with Photoperiod, Pop, and Selection, and interactions Photoperiod x Pop and Pop x Selection as fixed effects, Trial repeat as covariate, and ID, arena and batch of fish from the same tank as random effects. Length not included here as ControlVsBurbot indicated not effect of length on behaviour.

### 2.1. GLMM for exploration (1=yes, 0=no), all trials included
glmer_exp<-glmer(Explorated ~ Photoperiod + Pop + Selection + Repeat
                  +  Pop : Selection + Pop : Photoperiod
                  +(1| ID) + (1|Arena) + (1|Beh_batch), 
                 data=Behav_all, family=binomial(link="logit"))

## Convergence warning. Run without the Arena effect.

glmer_exp02<-glmer(Explorated ~ Photoperiod + Pop + Selection + Repeat
                 +  Pop : Selection + Pop : Photoperiod
                 +(1| ID) + (1|Beh_batch), 
                 data=Behav_all, family=binomial(link="logit"))

## Test the two models with likelihood ratio test 
anova(glmer_exp, glmer_exp02) #better fit in 1, keep Arena in model
summary(glmer_exp)
Anova(glmer_exp)

# Assess overdispersion, https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/012632.html
rdev <- sum(residuals(glmer_exp)^2)
mdf <- length(fixef(glmer_exp))
rdf <- nrow(Behav_all)-mdf
rdev/rdf #Not looking overdispersed.

#capture.output(Anova(glmer_exp), file="anova_glmer_exp.txt")
#capture.output(summary(glmer_exp), file="summary_glmer_exp.txt")

### 2.2. Plot predicted exploration with 75% confidence intervals per group (not incl. photoperiod because no significant effect).
exp_pred<-ggpredict(glmer_exp, terms= c("Selection","Pop"),
                    ci.lvl = 0.75, x.as.factor=T, condition = c(Repeat = -1))

pred_exp_plot<-
  ggplot(exp_pred, aes(y= predicted, x=group, group = x))+
# facet_wrap( ~ facet)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= x), stat = "identity",
             size=2)+
  ylab("Predicted probability for exploration (75% CI)")+
  scale_colour_manual(values=c("hotpink", "blue"))+
  theme_bw()+
  theme(panel.spacing.x=unit(0, "lines"),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=10, vjust=0.6),
        axis.title.x = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

## Model for exploration including sex, no photoperiod
glmer_exp03<-glmer(Explorated ~ Pop + Selection + Repeat + Sex
                 +  Pop : Selection +(1| ID) + (1|Arena) + (1|Beh_batch), 
                 data=Behav_all, family=binomial(link="logit"))
summary(glmer_exp03)
Anova(glmer_exp03)

### 2.3. Linear mixed model for exploration intensity measured as proportion of time
# spent  moving during trial

hist(Intensity_data$Intensity)

lmer_int_all<-lmer(Intensity ~  Photoperiod + Pop + Selection 
                   + Photoperiod:Pop+ Pop: Selection + Repeat 
                   + (1| ID) + (1|Arena) + (1|Beh_batch), data=Intensity_data)
summary(lmer_int_all)
anova(lmer_int_all)
plot(fitted(lmer_int_all),resid(lmer_int_all, type="pearson"))
mywhiskers(fitted(lmer_int_all),resid(lmer_int_all, type="pearson"), add=T, se=F)
## Quite heteroscedastic, get weights from a lme model (package nlme)

Intensity_data$fitted_int<-fitted(lmer_int_all)

lmer_int_all<-lme(Intensity~ Photoperiod + Pop + Selection 
                 + Photoperiod:Pop+ Pop: Selection + Repeat,
                 random = ~ 1| ID, data = Intensity_data,
                 na.action = "na.omit", weights = varPower(form=~fitted_int)) 

summary(lmer_int_all)
plot(fitted(lmer_int_all),resid(lmer_int_all, type="pearson"))
#Residuals and fitted values with lines to help visualization:
mywhiskers(fitted(lmer_int_all),resid(lmer_int_all, type="pearson"), add=T, se=F)

# Use var power 1.585213 in full model
lmer_int_all2<-lmer(Intensity ~  Photoperiod + Pop + Selection 
                    + Photoperiod:Pop+ Pop: Selection + Repeat 
                    + (1| ID) + (1|Arena) + (1|Beh_batch), 
                    data=Intensity_data, weight=1/fitted_int^(2*1.585213))

summary(lmer_int_all2)
anova(lmer_int_all2, type="3")


plot(fitted(lmer_int_all2),resid(lmer_int_all2, type="pearson"))

#Residuals and fitted values with lines to help visualization:
mywhiskers(fitted(lmer_int_all2),resid(lmer_int_all2, type="pearson"), add=T, se=F)
qqnorm(resid(lmer_int_all2, type="pearson"))

#capture.output(summary(lmer_int_all2), file="summary_lmer_int_all2.txt")
#capture.output(anova(lmer_int_all2), file="anova_lmer_int_all2.txt")

# Interactions complicate interpreting fixed effects. Run separate hypothesis tests for fixed effects. Function from car package. 

# Test population (use coef() to get names)
coef(lmer_int_all2)

lht(lmer_int_all2, 
    hypothesis.matrix = c("PopWild = 0", "PopWild:SelectionLV = 0", "Photoperiod24:PopWild = 0"), test = "F")

# Test selection line
lht(lmer_int_all2, 
    hypothesis.matrix = c("PopWild:SelectionLV = 0", "SelectionLV = 0"),
    test = "F")

# Test Photoperiod

lht(lmer_int_all2, 
    hypothesis.matrix = c("Photoperiod24:PopWild = 0","Photoperiod24 = 0"),
    test = "F")


### 2.4. Plot predictions of fixed effects for exploration intensity with 75% CI's

int_pred<-ggpredict(lmer_int_all2, terms= c("Selection","Pop", "Photoperiod"), 
                    ci.lvl = 0.75, x.as.factor=T, condition = c(Repeat = -1))

pred_int_plot<-
  ggplot(int_pred, aes(y= predicted, x=group, group = x))+
  facet_wrap( ~ facet)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= x), 
             stat = "identity",size=2)+
  ylab("Predicted exploration intensity (75% CI)")+
  scale_colour_manual(values=c("hotpink", "blue"))+
  theme_bw()+
  theme(panel.spacing.x=unit(0, "lines"),
        axis.text.y = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(size=10, vjust=0.6),
        axis.title.x = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85,0.8))

#Plot activity and exploration together using patchwork (not run)
#pred_act_plot + pred_exp_plot

### 2.5. Exploration intensity model including fish sex, no photoperiod
lmer_int_all_sex<-lmer(Intensity ~   Pop + Selection + Sex 
                    + Pop: Selection + Repeat 
                    + (1| ID) + (1|Arena) + (1|Beh_batch), 
                    data=Intensity_data, weight=1/fitted_int^(2*1.585213))

summary(lmer_int_all_sex)
anova(lmer_int_all_sex, type="3")

### 2.6. Frailty model of boldness (right-censored data)

Behav_all$status<-ifelse(Behav_all$Out_time_min== 9.75, 1, 2)
Behav_all$Surv<-Surv(Behav_all$Out_time_min, Behav_all$status==2)

mixed_cox <- coxme(Surv ~  Photoperiod + Pop + Selection + Repeat
                   +  Pop : Selection + Pop : Photoperiod
                   +(1| ID) + (1|Arena) + (1|Beh_batch), data = Behav_all)

mixed_cox
## Kaplan-Meier estimator. The "log-log" confidence interval is preferred.

km.by.group <- survfit(Surv ~ Group, data = Behav_all, conf.type = "log-log")

summary(km.by.group) # Show all observations by group

## Plot using ascending curves:
par(mai = c(0.6,0.6, 0.2,0.2))
plot(km.by.group, fun ="event", lty=1:4, col=c("hotpink", "blue", "hotpink", "blue"), lwd=1.4, conf.int = F,  yaxt="n", xaxt="n")
axis(side = 1,at=1:9, tck = -.015, labels = NA)
axis(side = 2, tck = -.015, labels = NA)
axis(1, at=1:9, labels= 1:9, tick=F, lwd=0, line=-0.8, cex.axis=0.8) 
axis(2,las=1, lwd=0, line=-0.6, cex.axis = 0.8 )
mtext(side = 2, "Proportion of individuals emerged" , line = 1.5, cex=0.8)
mtext(side = 1, "Time / min" , line = 1.2, cex=0.8)

legend(5.5, .3, c("Hatchery HV","Hatchery LV", "Wild HV", "Wild LV"), lty=1:4, cex = 0.7, bty="n", col=c("hotpink", "blue", "hotpink", "blue"), y.intersp = 1.2) 

### 2.7. Boldness model including fish sex, no photoperiod
mixed_cox2 <- coxme(Surv ~ Pop + Selection + Repeat + Sex
                   +  Pop : Selection +(1| ID) + (1|Arena) + (1|Beh_batch), data = Behav_all)

mixed_cox2

## Generate BLUPS from behavior data for individuals that emerged at least once from the start box. This will be used in correlations with SMR.

# Exploration intensity
# Run model only with ID as random effect
lmer_int_ID <-lmer(Intensity ~ (1| ID), data=Intensity_data)
int_BLUPs<- tibble(ID = row.names(ranef(lmer_int_ID)$ID), 
                   int_BLUP = ranef(lmer_int_ID)$ID[,"(Intercept)"])
write.table(int_BLUPs, file = "intensity_BLUPS.txt", quote = F, dec = ".", sep = "\t")

# Latency
lmer_lat_ID<- lmer(Out_time_min ~ (1| ID), data=Intensity_data)
lat_BLUPs<- tibble(ID = row.names(ranef(lmer_lat_ID)$ID), 
                   lat_BLUP = ranef(lmer_lat_ID)$ID[,"(Intercept)"])
write.table(lat_BLUPs, file = "latency_BLUPS.txt", quote = F, dec = ".", sep = "\t")


