################################################
## Metabolic rate analysis from brown trout
## October 2019, Jenni Prokkola
################################################
library(FishResp)
library(dplyr)

### All raw data formatted in "FishResp" format, tab-separated text, decimal ".".

# Analysis overview, continue from Metabrate_dataprep.R:

# 4. Calculate O2 saturation with a correction for bacterial respiration (which will be ignored because negligible).
# 5. Extract slopes and calculate oxygen consumption -remove 150 rows from the beginning of measurements (mixing period), and use slopes with R2>0.9
# 6. Export results with vraious calculations of SMR - use in statistical models (Metabrate_average_analysis.R)


## Batch is imported separately from others because it has measurements with two different lengths
##### Create input info for batch 1
info.Batch1 <- input.info(DO.unit = "mg/L", ID = c("47","NA", "90", "88"),
                   Mass = c(13.10,0,22.10,15.10), Volume = c(386.6,0,375.4,368))

#For empty chamber data, use batch 20 first and last slopes (all chambers, no bacterial consumption)
info.Batch20 <- input.info(DO.unit = "mg/L", ID = c("50","43", "39", "28"),
                           Mass = c(15.4,13.8,24.0,17.7), Volume = c(386.6,390.5,375.4,368))


# Import measurements, start measurement at 16:00 and finish at 11:00 (12-16 may be affected by stress):
# The first cycles:
MR.raw.1 <- import.meas(file = "Batch01/Batch1_set1_raw_fixed_conv.txt", info.data=info.Batch1, 
                       logger="FishResp", n.chamber=c(4), date.format = "DMY", 
                       start.measure = "16:00:00", stop.measure = "11:00:00", meas.to.wait= 150)

# The last cycles:
MR.raw.2 <- import.meas(file = "Batch01/Batch1_set2_raw_new_conv.txt", info.data=info.Batch1, 
                       logger="FishResp", n.chamber=c(4), date.format = "DMY",
                       start.measure = "16:00:00", stop.measure = "11:00:00", meas.to.wait= 150)


### Calculate slopes correcting for an empty chamber (note, negligible slope detected in empty chambers, done because required by the package). 
blank.pre<- import.test(file="Blank_Batch20_pre.txt", info.data = info.Batch20, n.chamber = 4, 
                        logger = "FishResp", meas.to.wait = 150)
blank.post<- import.test(file="Blank_Batch20_post.txt", info.data = info.Batch20, n.chamber = 4, 
                        logger = "FishResp", meas.to.wait = 150)

MR.clean.1 <- correct.meas(info.data=info.Batch1, pre.data = blank.pre, post.data = blank.post, meas.data=MR.raw.1, method="average")
MR.clean.2 <- correct.meas(info.data=info.Batch1, pre.data = blank.pre, post.data = blank.post, meas.data=MR.raw.2, method="average")

### Visualize
QC.meas(MR.clean.1, "Total.O2.phases")# all cycles in each chamber 
QC.meas(MR.clean.1, "Total.O2.chambers") 
QC.meas(MR.clean.2, "Total.O2.phases")
QC.meas(MR.clean.2, "Total.O2.chambers")

# Rename last set of cycles to start from cycle 19 and combine the datasets from first and last cycles.
# For batch1 last cycles, measurement was 7 min (150 rows omitted from each cycle, only M phases left). Chambers are on different rows.
Phase<-c()
n <- nrow(MR.clean.2)/270/4+18
cycles<- c(19:n)

for (i in cycles) {
  cyc<-c(rep(paste("M", i, sep=""), 270))
  Phase<-(c(Phase, cyc))
}

Phase<-rep(Phase, 4)
length(Phase)

MR.clean.2$Phase<- Phase

MR.clean.Batch1<-rbind(MR.clean.1, MR.clean.2) # use output in together with other results
rm(cyc)
rm(cycles)
rm(Phase)
rm(i)
rm(n)

##### Create input info for batches 2 to 32

## Measurement information incl. fish ID and size and chamber volume (before correction with fish size)
# IDs and weights from file Metab_fish_jenni_2, batch ID in original filename
info.Batch2 <- input.info(DO.unit = "mg/L", ID = c("54","NA", "17", "16"),
                          Mass = c(13.20,0,16.80,11.70), Volume = c(386.6,0,375.4,368))

info.Batch3 <- input.info(DO.unit = "mg/L", ID = c("52","NA", "55", "77"),
                     Mass = c(14.60,0,14.10,14.40), Volume = c(386.6,0,375.4,368))

info.Batch4 <- input.info(DO.unit = "mg/L", ID = c("3","NA", "9", "91"),
                     Mass = c(16.40,0,12.70,16.20), Volume = c(386.6,0,375.4,368))

info.Batch5 <- input.info(DO.unit = "mg/L", ID = c("29","NA", "76", "92"),
                     Mass = c(15.40,0,12.40,14.40), Volume = c(386.6,0,375.4,368))

info.Batch6 <- input.info(DO.unit = "mg/L", ID = c("49","NA", "87", "97"),
                          Mass = c(15.80,0,15.30,22.50), Volume = c(386.6,0,375.4,368))

info.Batch7 <- input.info(DO.unit = "mg/L", ID = c("31","NA", "14", "NA"),
                          Mass = c(17.30,0,12.90,0), Volume = c(386.6,0,375.4,368))

info.Batch8 <- input.info(DO.unit = "mg/L", ID = c("84","NA", "6", "8"),
                          Mass = c(14.4,0,16.80,12.60), Volume = c(386.6,0,375.4,368))

info.Batch9 <- input.info(DO.unit = "mg/L", ID = c("95","NA", "46", "72"),
                          Mass = c(8.8,0,15.9,14.20), Volume = c(386.6,0,375.4,368))

info.Batch10 <- input.info(DO.unit = "mg/L", ID = c("25","NA", "58", "94"),
                          Mass = c(16.6,0,21.20,21.20), Volume = c(386.6,0,375.4,368))

info.Batch11 <- input.info(DO.unit = "mg/L", ID = c("60","NA", "80", "23"),
                          Mass = c(18.3,0,14.8,18.50), Volume = c(386.6,0,375.4,368))

# note: in batch 12 measurement was two days, only one day included in analysis
info.Batch12 <- input.info(DO.unit = "mg/L", ID = c("100","NA", "36", "33"),
                           Mass = c(11.6,0,15.10,14.10), Volume = c(386.6,0,375.4,368))

info.Batch13 <- input.info(DO.unit = "mg/L", ID = c("30","NA", "85", "63"),
                           Mass = c(14.70,0,13.40,10.40), Volume = c(386.6,0,375.4,368))

info.Batch14 <- input.info(DO.unit = "mg/L", ID = c("81","NA", "13", "41"),
                           Mass = c(17.0,0,13.3,12.0), Volume = c(386.6,0,375.4,368))

info.Batch15 <- input.info(DO.unit = "mg/L", ID = c("37","NA", "22", "4"),
                           Mass = c(18.90,0,13.10,15.10), Volume = c(386.6,0,375.4,368))

#until batch15 3 chambers, then 4.

info.Batch16 <- input.info(DO.unit = "mg/L", ID = c("78","53", "15", "18"),
                           Mass = c(10.8,14.7,17.3,10.9), Volume = c(386.6,390.5,375.4,368))

info.Batch17 <- input.info(DO.unit = "mg/L", ID = c("74","45", "59", "27"),
                           Mass = c(18.6,15.4,20.3,15.0), Volume = c(386.6,390.5,375.4,368))

info.Batch18 <- input.info(DO.unit = "mg/L", ID = c("66","89", "65", "62"),
                           Mass = c(11.7,12.3,17.4,13.4), Volume = c(386.6,390.5,375.4,368))

info.Batch19 <- input.info(DO.unit = "mg/L", ID = c("70","2", "75", "40"),
                           Mass = c(13.5,15.3,16.9,21.1), Volume = c(386.6,390.5,375.4,368))


#in 21 only 1 chamber good. then only 2 chambers used.
info.Batch21 <- input.info(DO.unit = "mg/L", ID = c("38","NA"),
                           Mass = c(16.0,0), Volume = c(386.6,390.5))

info.Batch22 <- input.info(DO.unit = "mg/L", ID = c("26","69"),
                           Mass = c(17.3,13.1), Volume = c(386.6,390.5))

info.Batch23 <- input.info(DO.unit = "mg/L", ID = c("19","93"),
                           Mass = c(20.9,11.20), Volume = c(386.6,390.5))


info.Batch24 <- input.info(DO.unit = "mg/L", ID = c("44","48"),
                           Mass = c(16.8,20.8), Volume = c(386.6,390.5))

info.Batch25 <- input.info(DO.unit = "mg/L", ID = c("86","51"),
                           Mass = c(12,13.9), Volume = c(386.6,390.5))

info.Batch27 <- input.info(DO.unit = "mg/L", ID = c("67","21"),
                           Mass = c(15.1,11.7), Volume = c(386.6,390.5))

info.Batch28 <- input.info(DO.unit = "mg/L", ID = c("35","42"),
                           Mass = c(11.8,20.0), Volume = c(386.6,390.5))

info.Batch29 <- input.info(DO.unit = "mg/L", ID = c("83","1"),
                           Mass = c(12.0,16.0), Volume = c(386.6,390.5))

info.Batch30 <- input.info(DO.unit = "mg/L", ID = c("64","10"),
                           Mass = c(14.50,13.80), Volume = c(386.6,390.5))

info.Batch31 <- input.info(DO.unit = "mg/L", ID = c("11","57"),
                           Mass = c(12.4,13.3), Volume = c(386.6,390.5))

info.Batch32 <- input.info(DO.unit = "mg/L", ID = c("7","71"),
                           Mass = c(12.90,13.70), Volume = c(386.6,390.5))

# Import raw data in mg/L. For each batch, create MR.raw data and MR.clean data with slopes corrected for bacterial respiration
# To include only the same time period in all calculations, start measurement at 16:00 and finish at 11:00. 
# Previously checked that mixing continued for 2.5 min into the measurement period (ignoring 150 rows). 
# Temperature values are the same for all chambers measured on the same day.

# 3 or 4-channel measurements
batches4cha<-c(paste(c(rep("Batch")), c(2:20), sep = ""))

for (x in batches4cha) {
  assign(paste("MR.raw.", x, sep=""), 
         import.meas(file = paste(x, "/", x, "_raw_fixed_conv.txt", sep=""), 
                     info.data=paste("info.", x, sep=""),
                     logger="FishResp", n.chamber=c(4), 
                     date.format = "DMY", 
                     start.measure = "16:00:00", stop.measure = "11:00:00",
                     meas.to.wait= 150, plot.temperature = FALSE, plot.oxygen = FALSE))
}

#1 or 2 channel measurements
batches2cha<-c(paste(c(rep("Batch")), c(21:32), sep = ""))
batches2cha<-batches2cha[batches2cha != "Batch26"] #no data from this

for (x in batches2cha) {
  assign(paste("MR.raw.", x, sep=""), 
         import.meas(file = paste(x, "/", x, "_raw_fixed_conv.txt", sep=""),
                     info.data=paste("info.", x, sep=""),
                     logger="FishResp", n.chamber=c(2), 
                     date.format = "DMY", 
                     start.measure = "16:00:00", 
                     stop.measure = "11:00:00",
                     meas.to.wait= 150, 
                     plot.temperature = FALSE, plot.oxygen = FALSE))
}



# Calculate corrected slopes into a new data frame. Because almost inexistent bacterial respiration in all measurements, use the same 
# pre and post data in all batches.


MR.clean.Batch2 <- correct.meas(info.data=info.Batch2, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch2, method="average")
MR.clean.Batch3 <- correct.meas(info.data=info.Batch3, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch3, method="average")
MR.clean.Batch4 <- correct.meas(info.data=info.Batch4, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch4, method="average")
MR.clean.Batch5 <- correct.meas(info.data=info.Batch5, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch5, method="average")
MR.clean.Batch6 <- correct.meas(info.data=info.Batch6, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch6, method="average")
MR.clean.Batch7 <- correct.meas(info.data=info.Batch7, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch7, method="average")
MR.clean.Batch8 <- correct.meas(info.data=info.Batch8, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch8, method="average")
MR.clean.Batch9 <- correct.meas(info.data=info.Batch9, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch9, method="average")
MR.clean.Batch10 <- correct.meas(info.data=info.Batch10, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch10, method="average")
MR.clean.Batch11 <- correct.meas(info.data=info.Batch11, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch11, method="average")
MR.clean.Batch12 <- correct.meas(info.data=info.Batch12, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch12, method="average")
MR.clean.Batch13 <- correct.meas(info.data=info.Batch13, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch13, method="average")
MR.clean.Batch14 <- correct.meas(info.data=info.Batch14, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch14, method="average")
MR.clean.Batch15 <- correct.meas(info.data=info.Batch15, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch15, method="average")
MR.clean.Batch16 <- correct.meas(info.data=info.Batch16, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch16, method="average")
MR.clean.Batch17 <- correct.meas(info.data=info.Batch17, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch17, method="average")
MR.clean.Batch18<- correct.meas(info.data=info.Batch18, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch18, method="average")
MR.clean.Batch19<- correct.meas(info.data=info.Batch19, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch19, method="average")

MR.clean.Batch20<- correct.meas(info.data=info.Batch20, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch20, method="average")
MR.clean.Batch21<- correct.meas(info.data=info.Batch21, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch21, method="average")
MR.clean.Batch22<- correct.meas(info.data=info.Batch22, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch22, method="average")
MR.clean.Batch23<- correct.meas(info.data=info.Batch23, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch23, method="average")
MR.clean.Batch24<- correct.meas(info.data=info.Batch24, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch24, method="average")
MR.clean.Batch25<- correct.meas(info.data=info.Batch25, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch25, method="average")
MR.clean.Batch27<- correct.meas(info.data=info.Batch27, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch27, method="average")
MR.clean.Batch28<- correct.meas(info.data=info.Batch28, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch28, method="average")
MR.clean.Batch29<- correct.meas(info.data=info.Batch29, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch29, method="average")
MR.clean.Batch30<- correct.meas(info.data=info.Batch30, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch30, method="average")
MR.clean.Batch31<- correct.meas(info.data=info.Batch31, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch31, method="average")
MR.clean.Batch32<- correct.meas(info.data=info.Batch32, pre.data = blank.pre, post.data = blank.post,
                                meas.data=MR.raw.Batch32, method="average")

# Visualize all phases.
pdf(file = "QC_chambers_16h.pdf")
QC.meas(MR.clean.Batch1, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch2, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch3, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch4, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch5, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch6, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch7, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch8, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch9, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch10, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch11, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch12, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch13, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch14, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch15, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch16, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch17, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch18, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch19, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch20, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch21, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch22, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch23, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch24, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch25, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch27, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch28, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch29, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch30, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch31, QC = "Total.O2.chambers")
QC.meas(MR.clean.Batch32, QC = "Total.O2.chambers")
dev.off()

# Check pdf for obvious outliers. 
# Bath 9 CH1 looks like a technical problem, no variation,
# remove later

#In 24 one value not realistic.
MR.clean.Batch24$O2 <- ifelse(MR.clean.Batch24$O2 > 15, NA, MR.clean.Batch24$O2)
QC.meas(MR.clean.Batch24, QC = "Total.O2.chambers")

# Use minimum R^2 value of 0.9. 
all.slopes.Batch1<- extract.slope(MR.clean.Batch1, method="all", r2=0.9)
all.slopes.Batch2<- extract.slope(MR.clean.Batch2, method="all", r2=0.9)
all.slopes.Batch3<- extract.slope(MR.clean.Batch3, method="all", r2=0.9)
all.slopes.Batch4<- extract.slope(MR.clean.Batch4, method="all", r2=0.9)
all.slopes.Batch5<- extract.slope(MR.clean.Batch5, method="all", r2=0.9)
all.slopes.Batch6<- extract.slope(MR.clean.Batch6, method="all", r2=0.9)
all.slopes.Batch7<- extract.slope(MR.clean.Batch7, method="all", r2=0.9)
all.slopes.Batch8<- extract.slope(MR.clean.Batch8, method="all", r2=0.9)
all.slopes.Batch9<- extract.slope(MR.clean.Batch9, method="all", r2=0.9)
all.slopes.Batch10<- extract.slope(MR.clean.Batch10, method="all", r2=0.9)
all.slopes.Batch11<- extract.slope(MR.clean.Batch11, method="all", r2=0.9)
all.slopes.Batch12<- extract.slope(MR.clean.Batch12, method="all", r2=0.9)
all.slopes.Batch13<- extract.slope(MR.clean.Batch13, method="all", r2=0.9)
all.slopes.Batch14<- extract.slope(MR.clean.Batch14, method="all", r2=0.9)
all.slopes.Batch15<- extract.slope(MR.clean.Batch15, method="all", r2=0.9)
all.slopes.Batch16<- extract.slope(MR.clean.Batch16, method="all", r2=0.9)
all.slopes.Batch17<- extract.slope(MR.clean.Batch17, method="all", r2=0.9)
all.slopes.Batch18<- extract.slope(MR.clean.Batch18, method="all", r2=0.9)
all.slopes.Batch19<- extract.slope(MR.clean.Batch19, method="all", r2=0.9)
all.slopes.Batch20<- extract.slope(MR.clean.Batch20, method="all", r2=0.9)
all.slopes.Batch21<- extract.slope(MR.clean.Batch21, method="all", r2=0.9)
all.slopes.Batch22<- extract.slope(MR.clean.Batch22, method="all", r2=0.9)
all.slopes.Batch23<- extract.slope(MR.clean.Batch23, method="all", r2=0.9)
all.slopes.Batch24<- extract.slope(MR.clean.Batch24, method="all", r2=0.9)
all.slopes.Batch25<- extract.slope(MR.clean.Batch25, method="all", r2=0.9)
all.slopes.Batch27<- extract.slope(MR.clean.Batch27, method="all", r2=0.9)
all.slopes.Batch28<- extract.slope(MR.clean.Batch28, method="all", r2=0.9)
all.slopes.Batch29<- extract.slope(MR.clean.Batch29, method="all", r2=0.9)
all.slopes.Batch30<- extract.slope(MR.clean.Batch30, method="all", r2=0.9)
all.slopes.Batch31<- extract.slope(MR.clean.Batch31, method="all", r2=0.9)
all.slopes.Batch32<- extract.slope(MR.clean.Batch32, method="all", r2=0.9)

#Make a list and remove empty chambers
mylist = mget(ls(pattern = "all.slopes."))

# Remove chambers with no fish IDs and id 95:
mylist<-lapply(mylist, function(x) subset(x, Ind != "NA" & Ind != "95"))

# Calculate mass-specific values
MR.list<-lapply(mylist, function(x) calculate.MR(x, density=1000))

# Export results:

#lapply(MR.list, function(x) write.table(data.frame(x), 'MR.all.slopes.txt', quote = F, append= T, sep="\t", row.names = F, dec = "."))

# Check numbers of cycles included:
head(MR.list$all.slopes.Batch1)

# For each chamber and batch, get the number of measurements included
incl.meas<- lapply(MR.list, function(x) x %>%
                     group_by(Chamber.No) %>%
                     summarise(cycles = length(Phase))) 

# lowest nr of cycles:
cycles<-unlist(unname(sapply(incl.meas, `[[`, 2)))#extract the second column values into vector
summary(cycles)
# min 7, max 70

#exclude sample with 7 cycles (other samples >=12)
incl.meas #Batch 6 CH4

MR.list<-lapply(MR.list, function(x) subset(x, Ind !="97"))

incl.meas<- lapply(MR.list, function(x) x %>%
                     group_by(Chamber.No) %>%
                     summarise(cycles = length(Phase))) 
incl.meas

### Calculate SMR with several methods following Chabot et al. 
# 2 quantiles, 0.1 and 0.2, average, and 10 lowest absolute values

SMR.res.list<- lapply(MR.list, function(x) x %>%
                        group_by(Ind) %>%
                        summarise(
                        low10 = mean(sort(c(MR.abs.with.BR))[1:10]),
                        q0.1 = quantile(MR.abs.with.BR, probs = 0.1),
                        q0.2 = quantile(MR.abs.with.BR, probs = 0.2),
                        ave_MR = mean(MR.abs.with.BR, na.rm = T),
                        Mass = unique(Mass),
                        Chamber.No = unique(Chamber.No),
                        Temp = mean(Temp)))
                        
# Merge data with sample information:
Sampledata<-read.table(file="Metab_sampledata_Prokkola_etal.txt", header=T, dec=".", sep="\t")
Sampledata$Fish_ID<-as.factor(Sampledata$Fish_ID)
str(Sampledata)

SMR.res.sampledat<- lapply(SMR.res.list, function(x) merge(Sampledata, x, by.x = "Fish_ID", by.y = "Ind", all.x = FALSE, all.y = TRUE))

# Check:
SMR.res.sampledat$all.slopes.Batch1

# Combine all into one data frame, the list element names will be in column "Batch":
Metabdata<- bind_rows(SMR.res.sampledat, .id = "Batch")

head(Metabdata)
# write output:
write.table(Metabdata, file = "Metabdata_Fishresp_Feb20.txt", quote = F, dec=".", sep="\t")
# Use in statistical analysis & plots, script "Metabrate_average_analysis.R".


