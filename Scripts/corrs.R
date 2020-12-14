### 
### Calculate a correlation between SMR and 1. exploration intensity and 2. latency 
# in those fish that emerged from the start box in the behavior test. 
###
library(ggplot2)
# Data files produced in scripts Metabrate_average_analysis.R and Behavdata_analysis.R
# Import data

intensity_BLUPs<- read.table(file="intensity_BLUPs.txt", dec = ".", sep = "\t", header = T)
str(intensity_BLUPs)

latency_BLUPs<- read.table(file="latency_BLUPs.txt", dec = ".", sep = "\t", header = T)
str(latency_BLUPs)

SMR_resid<- read.table(file="SMR_wResid.txt", dec = ".", sep = "\t", header = T)
str(SMR_resid)


# merge BLUPs with SMR data
SMR_resid<-merge(SMR_resid, intensity_BLUPs, by.x = "Fish_ID", by.y = "ID")
SMR_resid<-merge(SMR_resid, latency_BLUPs, by.x = "Fish_ID", by.y = "ID")


# Test Pearson's correlation and plot
cor.test(SMR_resid$int_BLUP,SMR_resid$resid_SMR)

plot_int_SMR<-ggplot(SMR_resid, aes(x=int_BLUP, y=resid_SMR))+
  geom_point(alpha = 0.5)+
  xlab("Exploration intensity BLUP")+
  ylab("rSMR")+
  theme_classic()

cor.test(SMR_resid$lat_BLUP,SMR_resid$resid_SMR)

plot_lat_SMR<-ggplot(SMR_resid, aes(x=lat_BLUP, y=resid_SMR))+
  geom_point(alpha = 0.5)+
  xlab("Latency BLUP")+
  ylab("rSMR")+
  theme_classic()

# Because neither trait is correlated with SMR, not testing further if BLUPs
# produced anti-conservative p-values (Houslay & Wilson 2017)

