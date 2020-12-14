##################################################################################################
## Statistical analysis of standard metabolic rate (average of all accepted measurements) in brown trout
## Jenni Prokkola Apr 2020
##################################################################################################
## Load packages
library(ggplot2)
library(lme4)
library(car)
library(patchwork)
library(lmfor)
library(lmerTest)
library(ggeffects)

# Plot data and mass:
plot(Metabdata$ave_MR, Metabdata$Mass)
## Calculate log10 body mass for the model:
Metabdata$Log10_mass_kg<-log10(Metabdata$Mass/1000)
head(Metabdata)

plot(log10(Metabdata$ave_MR), Metabdata$Log10_mass_kg)
# Check for each population
plot_mass_pop <- qplot(data = Metabdata, x = log10(ave_MR), y = Log10_mass_kg) +
  facet_wrap( ~ pop)

# Check individuals across groups
plot_ind <- ggplot(data = Metabdata, aes(y = ave_MR_mgkgh, x = group)) +
  facet_wrap( ~ Photoperiod)+
  geom_point()

####  Linear mixed model
lmer_metab_full_ave<-lmer(log10(ave_MR) ~ Photoperiod + pop + selection + Log10_mass_kg
                           +  Temp + pop:selection + pop:Photoperiod
                           +(1|Chamber.No ), data= Metabdata, na.action=na.exclude) 

summary(lmer_metab_full_ave)
anova(lmer_metab_full_ave, type = 3)

# With sex, without photoperiod
lmer_metab_sex_ave<-lmer(log10(ave_MR) ~ pop + selection + Temp + Log10_mass_kg + Sex
                         + pop:selection 
                          +(1|Chamber.No), data= Metabdata, na.action=na.exclude) 

summary(lmer_metab_sex_ave)
anova(lmer_metab_sex_ave, type = 3)


# Interactions complicate interpreting fixed effects. Run separate linear hypothesis tests for fixed effects. Function from car package. 

# Test population (use coef() to get names)
coef(lmer_metab_full_ave)

lht(lmer_metab_full_ave, 
    hypothesis.matrix = c("popWild = 0", "popWild:selectionLV = 0", "Photoperiod24:popWild = 0"), test = "F")

# Test selection line
lht(lmer_metab_full_ave, 
    hypothesis.matrix = c("popWild:selectionLV = 0", "selectionLV =0"), test = "F")

# Test Photoperiod
lht(lmer_metab_full_ave, 
    hypothesis.matrix = c("Photoperiod24:popWild = 0","Photoperiod24 = 0"), test = "F")

# Check residuals from plots:
qqnorm(residuals(lmer_metab_full_ave, type="pearson"))
qqline(residuals(lmer_metab_full_ave, type="pearson"))
#Residuals and fitted values with lines to help visualization:
plot(fitted(lmer_metab_full_ave),resid(lmer_metab_full_ave, type="pearson"))
mywhiskers(fitted(lmer_metab_full_ave),resid(lmer_metab_full_ave, type="pearson"), add=T, se=F)

## Write model to a file
#capture.output(summary(lmer_metab_full), file="summary_lmer_metabmin_full.txt")
#capture.output(anova(lmer_metab_full), file="anova_lmer_metabmin_full.txt")

### 2.2. Get raw mass-specific metabolic rate

Metabdata$ave_MR_mgkgh<-Metabdata$ave_MR / (Metabdata$Mass/1000)
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

summary_MR_mg_kg_h<- summarySE(Metabdata, measurevar="ave_MR_mgkgh", groupvars=c("Photoperiod", "selection","pop"), na.rm = T)

### 2.3. Get mass-corrected SMR and plot:
# Get predicted values for average size individuals in average chamber and 12:12 photoperiod
metab_pred<-ggpredict(lmer_metab_full_ave, terms= c("selection","pop"),
                    ci.lvl = 0.75, x.as.factor=T, 
                    back.transform = TRUE)

# Back transformation hasn't worked
metab_pred_notlog<-as.data.frame(metab_pred)
metab_pred_notlog[,2:5]<-10^metab_pred_notlog[,2:5]

#Transform from average fish size to kg/h
metab_pred_notlog[,2:5]<-metab_pred_notlog[,2:5]/(10^-1.83)

plot_AveMR<- ggplot(metab_pred_notlog, aes(y = predicted, x=group, group = x))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(.2),
                width=0.2) +
  geom_point(position = position_dodge(.2), aes(col= x), stat = "identity",
             size=2)+
  ylab(expression(paste("Predicted SMR ", "(mg ", O[2], "/kg/h) (75% CI)"))) +
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
    legend.position = c(0.85, 0.23)
  )

### 2.4. Get residual mass-adjusted SMR for correlations:

mass_lm<- lm(log10(ave_MR)~ Log10_mass_kg, data = Metabdata)
summary(mass_lm)
resid_SMR<-resid(mass_lm)
Metabdata_resid<-cbind(Metabdata, resid_SMR)
plot(Metabdata_resid$Log10_mass_kg, log10(Metabdata_resid$ave_MR))
plot(Metabdata$Mass, Metabdata$ave_MR)

cor(Metabdata_resid$Log10_mass_kg, log10(Metabdata_resid$ave_MR))
cor(Metabdata$Mass, Metabdata$ave_MR)

write.table(Metabdata_resid, file= "SMR_wResid.txt", sep = "\t", dec = ".", quote = F)
