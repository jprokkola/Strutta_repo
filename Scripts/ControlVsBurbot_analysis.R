##################################################################################################
##
## Analysis of brown trout responses to predator olfactory cues
## Jenni Prokkola 2019
##
##################################################################################################
library(ggplot2)
library(lme4)
library(lmerTest)
library(coxme)
library(survival)
library(lmfor)
library(nlme)

### 1. Import data. For details on data see README in Datasets folder of Github repo.
Behavior_data<-read.table(file="ControlVsBurbot_data.txt", header=T, sep="\t", dec=".")
str(Behavior_data)

# Out time min is here given value 9.75 if the fish did not emerge during the trial

### 2. Mixed models with selection line and treatment as fixed effects, ID, Arena, and Batch as random
## Repeat and centred fish length as covariate, except for exploration not length (unidentifiable).
Behavior_data$Length_centr<-Behavior_data$Length - mean(Behavior_data$Length)

### 2.1. GLMM for exploration (1=yes, 0=no), all trials included
glmer_exp<-glmer(Explorated ~ Selection + Treatment + Repeat
                  + (Treatment-1|ID) + (1|Arena) + (1|Batch), 
                  data=Behavior_data, family=binomial(link="logit"))

# Convergence failure. Leave out treatment-specific random intercepts

glmer_exp02<-glmer(Explorated ~ Selection + Treatment + Repeat
                   + (1|ID) + (1|Arena) + (1|Batch), 
                   data=Behavior_data, family=binomial(link="logit"))

# Run LRT
anova(glmer_exp, glmer_exp02) #No difference, use second model.

summary(glmer_exp02)

## write model
#capture.output(summary(glmer_exp02), file="CvsB_summary_glmer_exp.txt")

### 2.2. LMM for activity, ignoring trials where fish did not emerge
Only_activity<-subset(Behavior_data, Activity >-1)
lmer_activity<-lmer(Activity ~ Selection + Treatment + Repeat + Length_centr 
                     + (Treatment-1|ID) + (1|Arena) + (1|Batch),
                     data=Only_activity)
summary(lmer_activity)

plot(lmer_activity)
qqnorm(residuals(lmer_activity, type="pearson"))
qqline(residuals(lmer_activity, type="pearson"))
plot(fitted(lmer_activity),resid(lmer_activity, type="pearson"))
mywhiskers(fitted(lmer_activity),resid(lmer_activity, type="pearson"), add=T, se=F)
## Quite heteroscedastic, get weights from a lme model (package nlme)

Only_activity$fitted_act<-fitted(lmer_activity)
lme_act<-lme(Activity ~ Selection + Treatment + Repeat + Length_centr,
             random = ~ 1| ID, data = Only_activity,
             na.action = "na.omit", weights = varPower(form=~fitted_act)) 

summary(lme_act)
plot(fitted(lme_act),resid(lme_act, type="pearson"))
mywhiskers(fitted(lme_act),resid(lme_act, type="pearson"), add=T, se=F)
#Save variance function power parameter estimate 1.935911 

#Rerun first model and use weights with the power estimate
lmer_activity_new<-lmer(Activity ~ Selection + Treatment + Repeat + Length_centr
                + (Treatment-1|ID) + (1|Arena) + (1|Batch),
                data=Only_activity, weight=1/fitted_act^(2*1.935911))
summary(lmer_activity_new)

#Plot diagnostics
plot(fitted(lmer_activity_new),resid(lmer_activity_new, type="pearson"))
mywhiskers(fitted(lmer_activity_new),resid(lmer_activity_new, type="pearson"), add=T, se=F)
hist(residuals(lmer_activity_new, type="pearson"))
qqnorm(residuals(lmer_activity_new, type="pearson"))
qqline(residuals(lmer_activity_new, type="pearson"))

# Now much better residuals.
# Write model results to a file:
#capture.output(summary(lmer_activity_new), file="CvsB_summary_lmer_activity.txt")

## Test for significant difference in individual variance in control and burbot groups:
## Levene's test:
leveneTest(Activity~Treatment, data = Only_activity)
# no difference

### 2.3. Mixed frailty model for boldness (latency to emerge, i.e., out time min)
# First create survival object based on fish status at the end of trial (not emerged = 1, emerged = 2)
Behavior_data$status<-ifelse(Behavior_data$Out_time_min== 9.75, 1, 2)
Behavior_data$Surv<-Surv(Behavior_data$Out_time_min, Behavior_data$status)

# Mixed cox model
mixed_cox <- coxme(Surv ~ Selection + Repeat + Treatment+ Length_centr
                   +(1 | ID) + (1| Arena) + (1|Batch), data = Behavior_data)
# See results:
mixed_cox 

capture.output(mixed_cox, file="CvsB_mixed_cox_result.txt")

#Plot survival curves for each group.
km.by.group <- survfit(Surv ~ Group, data = Behavior_data, conf.type = "log-log")
summary(km.by.group)

plot(km.by.group, fun ="event", lty=1:2, lwd=1.2, conf.int = F, ylab=NA, yaxt="n", xaxt="n")
axis(side = 1,at=1:9, tck = -.015, labels = NA)
axis(side = 2, tck = -.015, labels = NA)
axis(1, at=1:9, labels= 1:9, tick=F, lwd=0, line=-0.8, cex.axis=0.8) 
axis(2,las=1, lwd=0, line=-0.6, cex.axis = 0.8 )
mtext(side = 2, "Proportion emerged" , line = 1.5, cex=0.8)
mtext(side = 1, "Time / min" , line = 1.2, cex=0.8)
legend(6.5, .1, c("Burbot", "Control"), lty=1:2, cex = 0.8, bty="n") 


