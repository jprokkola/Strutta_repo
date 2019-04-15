#####
##### Analysis of cortisol concentration in plasma samples using ELISA and package drc -Jenni Prokkola #####
##### From spectophotometer, exported raw data in excel "Results_cortisoltest_date", calculated input data
#####

rm(list=ls())
library (drc)
library(plyr)

setwd("/Users/jenni/Dropbox/Taimenet/Paltamo2017_data/ELISA_cortisol")

### 1. Import data for standards from txt-files. Plate 1 was run on 11th Oct 2017, plate 2 was run 12th Oct 2017.
# Y-values indicate negative control and blank-corrected A450-A580 nm absorbance (mean of duplicate reactions) for each point on standard curve.
# Conc. indicate concentration of cortisol in pg/mL in the standard curve wells.
stdcrvdata <- read.table(file="Standards_cortisol.txt", sep="\t", header=T, dec=".")
stdcrvdata$logconc_plate1 <-log10(stdcrvdata$conc_plate1)# log10 from conc
stdcrvdata$logconc_plate2 <-log10(stdcrvdata$conc_plate2)# log10 from conc

## 2.1. Plate 1 analysis. Fit a 4-parametric logictic line, equation for example in Ritz & Streibig 2005, Bioassay analysis using R.

# Plot the standard curve
plot(stdcrvdata$logconc_plate1, stdcrvdata$Y_plate1, main="log standard curve", xlab="x=log(conc)", ylab="y=OD")

fit<-drm(formula =   Y_plate1 ~ logconc_plate1 , data = stdcrvdata, fct = LL.4())
x <- seq(2,4.5, length=100) # hypothetical samples from the range of logconc
y <- (fit$coefficients[2]+ (fit$coefficients[3]- fit$coefficients[2])/(1+(x/fit$coefficients[4])^ fit$coefficients[1]))# from OD ~ d + (d - c)/(1 + (logconc/e)^b) where the coefficients correspond to elements 2(c), 3(d), 4(e) and 1(b)
lines(x,y, lty="dotted", col="red") #visualize the slope

summary(fit) #coefficients b,c,d,e are used to infer y for samples' OD values

## 2.2. Import samples and calculate conc

samples <- read.table(file="Sample_OD_plate1.txt", sep="\t", header=T, dec=".") # Absorbance values for samples with sample IDs and dilution coefficients for plasma

str(samples)
## The following equation is from https://www.researchgate.net/post/Anybody_have_any_experience_with_processing_ELISA_data_in_R
# So solve logconc in the above equation when OD is known.

samples$loganswer<- fit$coefficients[4]*(((-1* fit$coefficients[3]+samples$OD)/( fit$coefficients[2]-samples$OD))^(1/ fit$coefficients[1]))

# Solve un-log concentration
samples$conc <- 10^samples$loganswer
# Multiply by dilution factor for each sample and divide by 1000 to get ng/mL concentration
samples$conc_orig<-samples$conc*samples$Dilution/1000

# Calculate means per duplicates
# This plate has extra dilution for all samples. Keep only 20-fold dilutions for samples and 10-fold dilution for controls (as for other plate). 
samples<- samples[-c(1,2,5,6,9,10,15,16),]
samples_means<- ddply(samples, c("Sample"), summarise,
        "cort_ng_ml" = mean(conc_orig),
        "CV_pct"= sd(conc_orig)/mean(conc_orig)*100)
samples_means      
#write.table(samples,file="Cort_conc_11Oct.txt", sep="\t", quote=F, row.names = F)
# Add samples on standard curve
lines(samples$loganswer,samples$OD, type="points", col="blue")
subset(samples, conc < min(stdcrvdata$conc_plate1))
#0 samples
subset(samples, conc > max(stdcrvdata$conc_plate1))
#0 samples
## 3.1. Plate 2 analysis. Similar as above, with more samples and only one dilution per sample. Lower standard curve point 1 than on plate 1.

plot(stdcrvdata$logconc_plate2, stdcrvdata$Y_plate2, main="log standard curve", xlab="x=log(conc)", ylab="y=OD")
fit_2<-drm(formula =   Y_plate2 ~ logconc_plate2 , data = stdcrvdata, fct = LL.4())
x <- seq(1.5,4.5, length=100)
y <- (fit_2$coefficients[2]+ (fit_2$coefficients[3]- fit_2$coefficients[2])/(1+(x/fit_2$coefficients[4])^ fit_2$coefficients[1]))
lines(x,y, lty="dotted", col="red")
summary(fit)

## 3.2. Import samples and calculate conc
samples2 <- read.table(file="Sample_OD_plate2.txt", sep="\t", header=T, dec=".")

samples2$loganswer<- fit_2$coefficients[4]*(((-1* fit_2$coefficients[3]+samples2$OD)/( fit_2$coefficients[2]-samples2$OD))^(1/ fit_2$coefficients[1]))

# Solve un-log concentration
samples2$conc <- 10^samples2$loganswer
# Multiply by dilution factor for each sample and divide by 1000 to get ng/mL concentration
samples2$conc_orig<-samples2$conc*samples2$Dilution/1000
head(samples2)

# Calculate means per duplicates
samples2_means<- ddply(samples2, c("Sample"), summarise,
                      "cort_ng_ml" = mean(conc_orig),
                      "CV_pct"= sd(conc_orig)/mean(conc_orig)*100)

#Plot samples on line
lines(samples2$loganswer,samples2$OD, type="points", col="blue")

# Some samples seem to go over or under standard curve, exclude them
subset(samples2, conc < min(stdcrvdata$conc_plate2))
#0 samples
subset(samples2, conc > max(stdcrvdata$conc_plate2))
# 7 samples where both replicates outside range.
subset(samples2_means, CV_pct > 20)
# one sample besides one that not in range, F69

#Exclude these samples from the means data
excl<-as.vector(unique(subset(samples2, conc > max(stdcrvdata$conc_plate2))$Sample))
excl<-excl[-1] #single duplicates
excl<-excl[-8]
excl<-c(excl, "F69")

samples2_means_final<-subset(samples2_means,samples2_means$Sample %in% excl == FALSE)

rm(excl)

### 4. Combine results from both plates (only three from the first one):

Plasma_cort_results<-rbind(samples_means[,1:2], samples2_means_final[,1:2])
dim(Plasma_cort_results)

### 5. Combine with stress test and group information (not the controls).

fish_info<-read.table(file="Stress_test_fish.txt", sep="\t", header=T, dec=".")
# Merge to cortisol data, controls will be separated later.
cort_data<-merge(fish_info, Plasma_cort_results, by.x = "Sample_ID", by.y = "Sample", all.y = T)
# Write to a file
write.table(cort_data,file="Plasma_cort_results.txt", sep="\t", quote=F, row.names = F)


