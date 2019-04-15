##################################################################################################
## Statistical analysis of minimum and average oxygen consumption and plasma cortisol level in brown trout
## Jenni Prokkola 2019
##################################################################################################
setwd("set your path here")

## Load packages
library(ggplot2)
library(lme4)
library(car)
library(patchwork)
library(lmfor)
library(lmerTest)
library(ggeffects)

### 1. Import and prepare data
Metabdata<-read.table(file="Metabdata_Prokkola_etal.txt", header=T, dec=".", sep="\t")
Metabdata$Metab_chamber<-as.factor(Metabdata$Metab_chamber)

## Calculate kg fish body mass:
Metabdata$Mass_kg_postMR<-Metabdata$Mass_g_postMR/1000

## Check that log-log mass correction will remove body mass effect:
Metabdata_noNA<-subset(Metabdata, Min_O2_mg_h >0)
plot(log10(Metabdata_noNA$Min_O2_mg_h)~log10(Metabdata_noNA$Mass_kg_postMR))
Metabdata_noNA$MinO2_resid<-resid(lm(log10(Min_O2_mg_h)~log10(Mass_kg_postMR), data= Metabdata_noNA))

#Plot new values with log-mass.
plot(Metabdata_noNA$MinO2_resid~log10(Metabdata_noNA$Mass_kg_postMR),  ylab = "MO2min resid", xlab = "Log10 mass (kg)")

#No correlation but a weird peak. Which values are these?
subset(Metabdata_noNA, MinO2_resid > 0.12)
# most VAA HV and 12:12

subset(Metabdata, subgroups == "Wild HV 12:12")
#Checked four highest values but nothing seems off in measurements, all on different days.

## Calculate log10 body mass for the model:
Metabdata$Log10_mass_kg<-log10(Metabdata$Mass_kg_postMR)

str(Metabdata)

#### 2. Models for minimum metabolic rate
### 2.1. Linear mixed model for minimum metabolic rate using photoperiod, population, selection line, and the interaction between photoperiod and population and population and selection line as the fixed effects and temperature and body mass, and body mass-pop interaction, as covariates, and chamber as the random effect.

lmer_metab_full<-lmer(log10(Min_O2_mg_h) ~ Photoperiod + pop + selection + Temp + Log10_mass_kg
                      + pop:selection + pop:Photoperiod + pop:Log10_mass_kg
                      +(1|Metab_chamber), data= Metabdata, na.action=na.exclude) 

summary(lmer_metab_full)
anova(lmer_metab_full, type = 3)

# Interactions complicate interpreting fixed effects. Run separate linear hypothesis tests for fixed effects. Function from car package. 

# Test population (use coef() to get names)
coef(lmer_metab_full)

lht(lmer_metab_full, 
    hypothesis.matrix = c("popWild = 0", "popWild:selectionLV = 0", "Photoperiod24:popWild = 0", "popWild:Log10_mass_kg = 0"), test = "F")

# Test selection line
lht(lmer_metab_full, 
    hypothesis.matrix = c("popWild:selectionLV = 0", "selectionLV =0"), test = "F")

# Test Photoperiod
lht(lmer_metab_full, 
    hypothesis.matrix = c("Photoperiod24:popWild = 0","Photoperiod24 = 0"), test = "F")

# Test body mass

lht(lmer_metab_full, 
    hypothesis.matrix = c("Log10_mass_kg = 0","popWild:Log10_mass_kg = 0"), test = "F")

# Check residuals from plots:
qqnorm(residuals(lmer_metab_full, type="pearson"))
qqline(residuals(lmer_metab_full, type="pearson"))
#Residuals and fitted values with lines to help visualization:
plot(fitted(lmer_metab_full),resid(lmer_metab_full, type="pearson"))
mywhiskers(fitted(lmer_metab_full),resid(lmer_metab_full, type="pearson"), add=T, se=F)

## Write model to a file
capture.output(summary(lmer_metab_full), file="summary_lmer_metabmin_full.txt")
capture.output(anova(lmer_metab_full), file="anova_lmer_metabmin_full.txt")

### 2.2. Plot mass-corrected MO2min
#Get scaling exponent for mass-correction

power<- coef(lm(log10(Min_O2_mg_h)~log10(Mass_kg_postMR), data=Metabdata))[2]

Metabdata$Min_O2_mg_kg_h<-Metabdata$Min_O2_mg_h / Metabdata$Mass_kg_postMR^power

# Get means and CI's within groups using plyr, from http://www.cookbook-r.com/Manipulating_data/Summarizing_data/

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.75, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

summary_Min_O2_mg_kg_h<- summarySE(Metabdata, measurevar="Min_O2_mg_kg_h", groupvars=c("selection","pop", "Photoperiod"), na.rm = T)

# Plot with ggplot2
plot_MO2min<- ggplot(summary_Min_O2_mg_kg_h, aes(y = Min_O2_mg_kg_h, x = pop, group = selection))+
  facet_wrap( ~ Photoperiod)+
  geom_errorbar(aes(ymin = Min_O2_mg_kg_h-ci, ymax = Min_O2_mg_kg_h+ci), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= selection), stat = "identity",
             size=2)+
  ylab(expression(paste(dot(M), O["2,"][min], " (mg ", O[2], "/", kg^0.928, "/h) (75% CI)"))) +
  scale_colour_manual(values = c("hotpink", "blue")) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "lines"),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, vjust = 0.6),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.85, 0.83)
  )

### 2.3. Linear mixed model on the effect of sex on minimum metabolic rate
# Including all parameters as in the full model, except no photoperiod.
## For sex effect model, subset data (not all individuals were sexed)
Metabdata_sex<-subset(Metabdata, Sex ==1 | Sex == -1)

lmer_metabmin_sex<-lmer(log10(Min_O2_mg_h) ~ pop + selection + Temp + Log10_mass_kg + Sex
                         + pop:selection + pop:Log10_mass_kg
                         +(1|Metab_chamber), data= Metabdata_sex, na.action=na.exclude) 

summary(lmer_metabmin_sex)
anova(lmer_metabmin_sex, type=3)

# Check residuals
qqnorm(residuals(lmer_metabmin_sex, type="pearson"))
qqline(residuals(lmer_metabmin_sex, type="pearson"))
plot(fitted(lmer_metabmin_sex),resid(lmer_metabmin_sex, type="pearson"))
mywhiskers(fitted(lmer_metabmin_sex),resid(lmer_metabmin_sex, type="pearson"), add=T, se=F) 


#### 3. Models for average metabolic rate
#Correlation with body mass?
plot(log10(Metabdata$Ave_O2_mg_h)~log10(Metabdata$Mass_kg_postMR))
#Not strong. Include BM in model anyway

### 3.1. Full model for average metabolic rate
lmer_metab_ave<-lmer(log10(Ave_O2_mg_h) ~ Photoperiod + pop + selection + Temp + Log10_mass_kg
                     + pop:selection + pop:Photoperiod + pop:Log10_mass_kg
                     +(1|Metab_chamber), data= Metabdata, na.action=na.exclude) 

summary(lmer_metab_ave)
anova(lmer_metab_ave, type=3)
# Run separate hypothesis tests for fixed effects, as above.
# Test population (use coef() to get names)
lht(lmer_metab_ave, 
    hypothesis.matrix = c("popWild = 0", "popWild:selectionLV = 0", "Photoperiod24:popWild = 0",
                          "popWild:Log10_mass_kg = 0"), test = "F")

# Test selection line
lht(lmer_metab_ave, 
    hypothesis.matrix = c("popWild:selectionLV = 0", "selectionLV = 0"), test = "F")

# Test Photoperiod
lht(lmer_metab_ave, 
    hypothesis.matrix = c("Photoperiod24:popWild = 0","Photoperiod24 = 0"), test = "F")

# Test body mass
lht(lmer_metab_ave, 
    hypothesis.matrix = c("Log10_mass_kg = 0","popWild:Log10_mass_kg = 0"), test = "F")

#Check assumptions
qqnorm(residuals(lmer_metab_ave, type="pearson"))
qqline(residuals(lmer_metab_ave, type="pearson"))
plot(fitted(lmer_metab_ave),resid(lmer_metab_ave, type="pearson"))
mywhiskers(fitted(lmer_metab_ave),resid(lmer_metab_ave, type="pearson"), add=T, se=F)

## Write model
#capture.output(summary(lmer_metab_ave), file="summary_lmer_metab_ave.txt")
#capture.output(anova(lmer_metab_ave), file="anova_lmer_metab_ave.txt")

### Plot mass-corrected data
plot(resid(lm(log10(Metabdata$Ave_O2_mg_h)~log10(Metabdata$Mass_kg_postMR)))~ log10(Metabdata$Mass_kg_postMR))

# Plot without correction
plot(log10(Metabdata$Ave_O2_mg_kg_h) ~ log10(Metabdata$Mass_kg_postMR))

# get scaling exponent
power_ave<- coef(lm(log10(Ave_O2_mg_h)~log10(Mass_kg_postMR), data=Metabdata))[2]
#Get data for plot
Metabdata$Ave_O2_mg_kgcorr_h<-Metabdata$Ave_O2_mg_h/Metabdata$Mass_kg_postMR^power_ave

#Calculate linear mass-specific data
Metabdata$Ave_O2_mg_kg_h<-Metabdata$Ave_O2_mg_h / Metabdata$Mass_kg_postMR
plot(log10(Metabdata$Ave_O2_mg_kg_h) ~ log10(Metabdata$Mass_kg_postMR))

## Plot mass-specific data
summary_Ave_O2_mg_kg_h<- summarySE(Metabdata, measurevar="Ave_O2_mg_kg_h", groupvars=c("selection","pop", "Photoperiod"), na.rm = T)

plot_MO2ave<- ggplot(summary_Ave_O2_mg_kg_h, aes(y = Ave_O2_mg_kg_h, x = pop, group = selection))+
  facet_wrap( ~ Photoperiod)+
  geom_errorbar(aes(ymin = Ave_O2_mg_kg_h-ci, ymax = Ave_O2_mg_kg_h+ci), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= selection), stat = "identity",
             size=2)+
  ylab(expression(paste(dot(M), O["2,"][ave], " (mg ", O[2], "/kg/h) (75% CI)"))) +
  scale_colour_manual(values = c("hotpink", "blue")) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "lines"),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, vjust = 0.6),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

# Is this different from raw data plot?
summary_Ave_O2_mg_h<- summarySE(Metabdata, measurevar="Ave_O2_mg_h", groupvars=c("selection","pop", "Photoperiod"), na.rm = T)

ggplot(summary_Ave_O2_mg_h, aes(y = Ave_O2_mg_h, x = pop, group = selection))+
  facet_wrap( ~ Photoperiod)+
  geom_errorbar(aes(ymin = Ave_O2_mg_h-ci, ymax = Ave_O2_mg_h+ci), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= selection), stat = "identity",
             size=2)+
  ylab(expression(paste(dot(M), O["2,"][ave], " (mg ", O[2], "/h)"))) +
  scale_colour_manual(values = c("hotpink", "blue")) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "lines"),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, vjust = 0.6),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

# And log-mass corrected data:

summary_Ave_O2_mg_kgcorr_h<- summarySE(Metabdata, measurevar="Ave_O2_mg_kgcorr_h", groupvars=c("selection","pop", "Photoperiod"), na.rm = T)

ggplot(summary_Ave_O2_mg_kgcorr_h, aes(y = Ave_O2_mg_kgcorr_h, x = pop, group = selection))+
  facet_wrap( ~ Photoperiod)+
  geom_errorbar(aes(ymin = Ave_O2_mg_kgcorr_h-ci, ymax = Ave_O2_mg_kgcorr_h+ci), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= selection), stat = "identity",
             size=2)+
  ylab(expression(paste(dot(M), O["2,"][ave], " (mg /kkgcorr", O[2], "/h)"))) +
  scale_colour_manual(values = c("hotpink", "blue")) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "lines"),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8, vjust = 0.6),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

# Scaling exponent is so low that values are misleading, but patterns look the same. Use linear correction.

### 3.3. Sex effect on average MR
lmer_metabave_sex<-lmer(log10(Ave_O2_mg_h) ~ pop + selection + Temp + Log10_mass_kg + Sex
                                      + pop:selection + Log10_mass_kg:pop
                                      +(1|Metab_chamber), data= Metabdata_sex)

summary(lmer_metabave_sex)
anova(lmer_metabave_sex)

#### 4. Analysis for plasma cortisol response to confinement stress
# Import data
cort_data<-read.table(file="Plasma_cort_results.txt", sep="\t", header=T, dec=".")

### 4.1. Separate control and test fish and test for response to confinement
stressgroup<- subset(cort_data, Fish_ID >0)
control<- subset(cort_data, is.na(Fish_ID)) #No ID numbers in controls

#Perform t-test on control vs. stress test fish
t.test(stressgroup$cort_ng_ml, control$cort_ng_ml, alternative = "greater")

#Get mean from the result, and SD:
sd(stressgroup$cort_ng_ml)
sd(control$cort_ng_ml)

### 4.2. Linear model for cortisol response in angling selection lines
# Use fish length (has one more value than mass), temperature and fish serial number from the same tank in the same day as covariates
# Center length 

stressgroup$Length_centr<-stressgroup$Length_mm_end - mean(stressgroup$Length_mm_end, na.rm=T)

cort_model<-lm(cort_ng_ml ~ pop + selection + pop:selection +  Temp_on_stress_test + Length_centr + Stress_test_serial, 
               data= stressgroup)

summary(cort_model)
Anova(cort_model, type = 3)

plot(fitted(cort_model),resid(cort_model, type="pearson"))
mywhiskers(fitted(cort_model),resid(cort_model, type="pearson"), add=T, se=F)
qqnorm(resid(cort_model, type="pearson"))
qqline(resid(cort_model, type="pearson"))
capture.output(summary(cort_model), file="cort_model.txt")
capture.output(Anova(cort_model, type = 3), file="anova_cort_model.txt")

plot(cort_data$cort_ng_ml~cort_data$Stress_test_serial)

# Interactions complicate interpreting fixed effects. Run separate hypothesis tests for fixed effects. Function from car package. 

# Test population (use coef() to get names)
coef(cort_model)

lht(cort_model, 
    hypothesis.matrix = c("popWild = 0", "popWild:selectionlv = 0"),
    test = "F")

#selection line
lht(cort_model, 
    hypothesis.matrix = c("selectionlv = 0", "popWild:selectionlv = 0"),
    test = "F")

### 4.3. Plot cortisol predictions for an average fish at average temperature, fix to serial nr 1

exp_cort<-ggpredict(cort_model, terms= c("selection","pop"), condition = c(Stress_test_serial = 1),
                        ci.lvl = 0.75, x.as.factor=T)

summary(exp_cort)
exp_cort

pred_cort_plot<-ggplot(exp_cort, aes(y = predicted, x = group, group = x))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= x), stat = "identity",
             size=2)+
  ylab("Predicted post-stress cortisol ng/ml (75% CI)")+
  scale_colour_manual(values = c("hotpink", "blue")) +
  theme_bw() +
  theme(
    panel.spacing.x = unit(0, "lines"),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, vjust = 0.6),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = "none")


### 5. Plot Metab figures together using pathwork

plot_MO2min + plot_MO2ave +
  plot_layout(ncol = 2)+
  plot_annotation(tag_levels = "A")
