################################################
## Formatting raw data for metabolic rate analysis using FishResp
## October 2019, Jenni Prokkola
################################################
library(FishResp)

# Overview:
# 1. Create input info data for each batch and fish (separately for batch 1 start and end, batch 2, and the rest, because cycle lengths differ)
# 2. Delete the first and last (i.e. empty) cycles from each batch, rename cycles starting from F1, and re-save data (separately for batch 1 start and end, batch 2, and the rest)
# 3. Convert % atm. pressure to mg/L oxygen using daily air pressure readings (separately for batch 1 start and end, batch 2, batches 3-20, and the rest)
# Continue with Metabrate_slope_analysis.R

########### Batch 1

# Batch 1 was analysed in two stages because the first 19 cycles had longer measurements. Convert and run slope analysis on these 
# separately then combine.

### Delete the first cycle from first cycles of batch1 data and last cycle from last cycles data, rename phases
raw<- read.table(file = "Batch1_1304_1-19_8min_raw_fixed.txt" , sep = "\t",  header=T, dec=".")
raw<-raw[-c(1:1020),]

# Rename phases.
Phase<-c()
# For batch1 first cycles, measurement was 8 min
n <- nrow(raw)/1020
cycles<- c(1:n)

for (i in cycles) {
  cyc<-c(rep(paste("F", i, sep=""), 360), rep(paste("W", i, sep=""), 180), rep(paste("M", i, sep=""), 480))
  Phase<-c(Phase, cyc)
  
}

raw$Phase<- Phase

write.table(raw, file = "Batch1_set1_raw_cycfixed.txt", row.names = FALSE, sep = "\t", quote =F)

rm(raw)
rm(Phase)
rm(cyc)
rm(cycles)

### The last set from batch 1

raw<- read.table(file = "Batch1_raw_fixed.txt" , sep = "\t",  header=T, dec=".")
last<-nrow(raw)-960+1
raw<-raw[-c(last:nrow(raw)),]
head(raw)
write.table(raw, file = "Batch1_set2_raw_cycfixed.txt", row.names = FALSE, sep = "\t", quote =F)

rm(raw)


# Convert % atm. pressure to mg/L oxygen using daily air pressure readings
# The first cycles:
convert.respirometry("Batch1_set1_raw_cycfixed.txt", "Batch1_set1_raw_fixed_conv.txt",
                     n.chamber = 4,
                     logger = c("FishResp"),
                     from = "percent_a.s.", to = "mg_per_l", sal = 0, atm_pres = 1002)

# The last cycles:
convert.respirometry("Batch1_set2_raw_cycfixed.txt", "Batch1_set2_raw_new_conv.txt",
                     n.chamber = 4,
                     logger = c("FishResp"),
                     from = "percent_a.s.", to = "mg_per_l", sal = 0, atm_pres = 1002)

########### Batch 2

# Rename phases to start from 1 and delete first and last cycles.
raw<- read.table(file = "Batch2_raw_fixed.txt", sep = "\t",  header=T, dec=".")
last<-nrow(raw)-960
raw<-raw[961:last,]

Phase<- c()

n <- nrow(raw)/960 #how many cycles
cycles<- c(1:n) #make a vector of new cycle numbers

for (i in cycles) {
  cyc<-c(rep(paste("F", i, sep=""), 360), rep(paste("W", i, sep=""), 180), rep(paste("M", i, sep=""), 420))
  Phase<-c(Phase, cyc) #add in the same vector as previous cycle
  
}

raw$Phase<- Phase #replace old phase names with new

write.table(raw, file = "Batch2_raw_cycfixed.txt", row.names = FALSE, sep = "\t", quote =F)

rm(raw)
rm(cyc)
rm(cycles)
rm(n)
rm(last)
rm(i)
rm(Phase)

########### Batches 3-32 (all have the same cycle length)

# Analyse multiple batches of raw data in FishResp format.
# Make a vector of Batches:
Batches<-c(paste(c(rep("Batch")), c(3:32), sep = ""))
# Batch 26 should be excluded, only a short measurement:
Batches<-Batches[Batches != "Batch26"]


for (x in Batches) {
  # Read file and remove the first and last cycle (were empty)
  raw<- read.table(file = paste(x, "_raw_fixed.txt", sep=""), sep = "\t",  header=T, dec=".")
  last<-nrow(raw)-900
  raw<-raw[901:last,]
  
  # Rename phases.
  Phase<-c()
  # For batches 3-32, measurement was 6 min, total cycle 15 min
  n <- nrow(raw)/900
  cycles<- c(1:n)
  
  for (i in cycles) {
    cyc<-c(rep(paste("F", i, sep=""), 360), rep(paste("W", i, sep=""), 180), rep(paste("M", i, sep=""), 360))
    Phase<-c(Phase, cyc)
    
  }
  
  raw$Phase<- Phase
  
  write.table(raw, file = paste(x, "_raw_cycfixed.txt", sep=""), row.names = FALSE, sep = "\t", quote =F)
  
  rm(raw)
  rm(Phase)
  rm(cyc)
  rm(cycles)
  
}

######## Convert % atm. pressure to mg/L oxygen using daily air pressure readings
## Batches 2-20 with 3 or 4 chambers used
# Vector of batches
batches4cha<-c(paste(c(rep("Batch")), c(2:20), sep = ""))

# Make a table of air pressure readings for each batch 2-32
airpress = data.frame(row.names = batches4cha, airpress = c(1029, 1029,1019,992,992, 989,1009,989,989,1033,1013,1019,1029,1036,1016,1016,1016,1013,1009,1009,1013,1019,1026,1036,1019,1009,1019,1019,1013,1009))

for (x in batches4cha) {
  # Convert % atm. saturation in raw data files into mg per L oxygen
  convert.respirometry(paste(x,"_raw_cycfixed.txt", sep=""), paste(x, "_raw_fixed_conv.txt", sep=""),
                       n.chamber = 4,
                       logger = c("FishResp"),
                       from = "percent_a.s.", to = "mg_per_l", sal = 0, atm_pres = airpress[x,])
}

########### Batches 21-32 with 1-2 chambers

# Convert % atm. pressure to mg/L oxygen using daily air pressure readings
# Vector of all batches
batches2cha<-c(paste(c(rep("Batch")), c(21:32), sep = ""))
batches2cha<-batches4cha[c(1:5, 7:12)]

for (x in batches2cha) {
  # Convert % atm. saturation in raw data files into mg per L oxygen
  convert.respirometry(paste(x,"_raw_cycfixed.txt", sep=""), paste(x, "_raw_fixed_conv.txt", sep=""),
                       n.chamber = 2,
                       logger = c("FishResp"),
                       from = "percent_a.s.", to = "mg_per_l", sal = 0, atm_pres = airpress[x,])
}