############### NEON data exploration for Macrosystems proposal: ##############

rm(list=ls())

#install.packages('geoNEON')
library(neonUtilities)
library(ggplot2)
library(tidyverse)
library(geoNEON)
library(metScanR)

### download data:

# Harvard forest
#zipsByProduct(dpID="DP1.10093.001", site="HARV", package="basic", check.size=T) # ticks
#zipsByProduct(dpID="DP1.10072.001", site="HARV", package="basic", check.size=T) # mammals
#zipsByProduct(dpID="DP1.10092.001", site="HARV", package="basic", check.size=T) # tick pathogens
#zipsByProduct(dpID="DP1.00098.001", site="HARV", package="basic", check.size=T) # relative humidity

#All sites:
zipsByProduct(dpID="DP1.10093.001", site="all", package="basic", check.size=T) # ticks
zipsByProduct(dpID="DP1.10092.001", site="all", package="basic", check.size=T) # tick pathogens
zipsByProduct(dpID="DP1.10072.001", site="all", package="basic", check.size=T) # mammals

### stack data:

stackByTable(filepath="/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack10093", folder=T)
stackByTable(filepath="/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack10072", folder=T)
stackByTable(filepath="/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack10092", folder=T)
#stackByTable(filepath="/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack00098", folder=T)

### pull in csv's from above:

ticks <- read.csv("/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack10093/stackedFiles/tck_fielddata.csv", header=T)
head(ticks)
unique(ticks$siteID)
tail(ticks)
nrow(ticks)

ticks_taxa <- read.csv("/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack10093/stackedFiles/tck_taxonomyProcessed.csv", header=T)
head(ticks_taxa)
nrow(ticks_taxa)

mams <- read.csv("/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack10072/stackedFiles/mam_pertrapnight.csv", header=T)
head(mams)
nrow(mams)

tick_path <- read.csv("/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack10092/stackedFiles/tck_pathogen.csv", header=T)
head(tick_path)
nrow(tick_path)

RH <- read.csv("/Users/andrewmacdonald/GoogleDrive/Projects/NEON_Data/filesToStack00098/stackedFiles/RH_30min.csv", header=T)
head(RH)
nrow(RH)

###################### Try some plots/data manipulation:
unique(ticks_taxa$scientificName)
nrow(ticks_taxa)
ticks_taxa$site <- substr(ticks_taxa$plotID, 1, 4)
unique(ticks_taxa$site)
ticks_taxa_E <- ticks_taxa[which(ticks_taxa$site == 'BART' | ticks_taxa$site == 'HARV' | ticks_taxa$site == 'BLAN' | ticks_taxa$site == 'SCBI' | ticks_taxa$site == 'SERC' | ticks_taxa$site == 'OSBS' | ticks_taxa$site == 'STEI' | ticks_taxa$site == 'TREE' | ticks_taxa$site == 'UNDE' | ticks_taxa$site == 'GRSM' | ticks_taxa$site == 'MLBS' | ticks_taxa$site == 'ORNL' | ticks_taxa$site == 'TALL'),]

DVAR <- ticks_taxa_E[which(ticks_taxa_E$scientificName == 'Dermacentor variabilis'),]
nrow(DVAR)
head(DVAR)
DVAR_L <- DVAR[which(DVAR$sexOrAge == 'Larva'),]
nrow(DVAR_L)
DVAR_N <- DVAR[which(DVAR$sexOrAge == 'Nymph'),]
nrow(DVAR_N)
ISCP <- ticks_taxa_E[which(ticks_taxa_E$scientificName == 'Ixodes scapularis'),]
nrow(ISCP)
AAM <- ticks_taxa_E[which(ticks_taxa_E$scientificName == 'Amblyomma americanum'),]
nrow(AAM)
AAM_L <- AAM[which(AAM$sexOrAge == 'Larva'),]
nrow(AAM_L)
AAM_N <- AAM[which(AAM$sexOrAge == 'Nymph'),]
nrow(AAM_N)

ticks_taxa$plotID <- lapply(ticks_taxa$plotID, as.character)
ticks_taxa$siteID <- substr(ticks_taxa$plotID, 1, nchar(ticks_taxa$plotID)-4)
head(ticks_taxa)

plot(ticks_taxa$collectDate, ticks_taxa$individualCount)

ticks_taxa2 <- ticks_taxa[,c(3,4,8,23,24,33)]
head(ticks_taxa2, n=20)
nrow(ticks_taxa2)

#Nymphs
I_scap_N <- ticks_taxa2[which(ticks_taxa2$sexOrAge == 'Nymph' & ticks_taxa2$scientificName == 'Ixodes scapularis'),]
head(I_scap_N)
nrow(I_scap_N)
sapply(I_scap_N, class)
I_scap_N$collectDate <- as.character(I_scap_N$collectDate)

I_scap_N$collectDate <- substr(I_scap_N$collectDate, 1, nchar(I_scap_N$collectDate)-7)
head(I_scap_N)
I_scap_N$collectDate <- as.factor(I_scap_N$collectDate)
I_scap_N$plotID <- unlist(I_scap_N$plotID)
I_scap_N$collectDate <- as.character(I_scap_N$collectDate)
I_scap_N$Year <- substr(I_scap_N$collectDate, 1, nchar(I_scap_N$collectDate)-6)
head(I_scap_N)

I_scap_N_2017 <- I_scap_N[which(I_scap_N$Year == '2017'),]
head(I_scap_N_2017)

I_scap_N_col <- aggregate(individualCount ~ plotID + collectDate + site + Year, data=I_scap_N, FUN=sum)
head(I_scap_N_col, n=20)
nrow(I_scap_N_col)

#Larvae:
I_scap_L <- ticks_taxa2[which(ticks_taxa2$sexOrAge == 'Larva'),]
head(I_scap_L)
nrow(I_scap_L)
sapply(I_scap_L, class)
I_scap_L$collectDate <- as.character(I_scap_L$collectDate)

I_scap_L$collectDate <- substr(I_scap_L$collectDate, 1, nchar(I_scap_L$collectDate)-7)
head(I_scap_L)
I_scap_L$collectDate <- as.factor(I_scap_L$collectDate)
I_scap_L$plotID <- unlist(I_scap_L$plotID)
I_scap_L$collectDate <- as.character(I_scap_L$collectDate)
I_scap_L$Year <- substr(I_scap_L$collectDate, 1, nchar(I_scap_L$collectDate)-6)
head(I_scap_L)

I_scap_L_2017 <- I_scap_L[which(I_scap_L$Year == '2017'),]
head(I_scap_L_2017)

I_scap_L_col <- aggregate(individualCount ~ plotID + collectDate + site + Year, data=I_scap_L, FUN=sum)
head(I_scap_L_col, n=20)
nrow(I_scap_L_col)

#Adults
I_scap_A <- ticks_taxa2[which(ticks_taxa2$sexOrAge == 'Female' | ticks_taxa2$sexOrAge == 'Male'),]
I_scap_A <- I_scap_A[which(I_scap_A$scientificName == 'Ixodes scapularis'),]
head(I_scap_A)
nrow(I_scap_A)
sapply(I_scap_A, class)
I_scap_A$collectDate <- as.character(I_scap_A$collectDate)

I_scap_A$collectDate <- substr(I_scap_A$collectDate, 1, nchar(I_scap_A$collectDate)-7)
head(I_scap_A)
I_scap_A$collectDate <- as.factor(I_scap_A$collectDate)
I_scap_A$plotID <- unlist(I_scap_A$plotID)
I_scap_A$collectDate <- as.character(I_scap_A$collectDate)
I_scap_A$Year <- substr(I_scap_A$collectDate, 1, nchar(I_scap_A$collectDate)-6)
head(I_scap_A)

I_scap_A_2017 <- I_scap_A[which(I_scap_A$Year == '2017'),]
head(I_scap_A_2017)

I_scap_A_col <- aggregate(individualCount ~ plotID + collectDate + site + Year, data=I_scap_A, FUN=sum)
head(I_scap_A_col, n=20)
nrow(I_scap_A_col)

#plot <- ggplot(I_scap_N, aes(collectDate, individualCount)) + geom_jitter(aes(colour=plotID), size=2)
plotN <- ggplot(I_scap_N, aes(collectDate, individualCount, group=plotID, colour=plotID)) + geom_line(size=1.5) + facet_wrap(~plotID)
plotN

plotL <- ggplot(I_scap_L, aes(collectDate, individualCount, group=plotID, colour=plotID)) + geom_line(size=1.5) + 
  #scale_y_continuous(limits=c(0,100)) +
  facet_wrap(~plotID)
plotL

plotA <- ggplot(I_scap_A, aes(collectDate, individualCount, group=plotID, colour=plotID)) + geom_line(size=1.5) + facet_wrap(~plotID)
plotA

#*should divide by area sampled as it's not always the same...

######### Tick pathogens
tick_path <- tick_path[,c(4,5,7,8,9,14,18,20,23,24)]
head(tick_path, n=20)
unique(tick_path$testPathogenName)
tick_path_Bb <- tick_path[which(tick_path$testPathogenName == 'Borrelia burgdorferi' | tick_path$testPathogenName == 'Borrelia burgdorferi sensu lato' | tick_path$testPathogenName == 'Borrelia sp.'),]
nrow(tick_path_Bb)
tick_path_Bb_pos <- tick_path_Bb[which(tick_path_Bb$testResult == 'Positive'),]
nrow(tick_path_Bb_pos)
unique(tick_path_Bb_pos$siteID)

tick_path_Bb$incidence <- ifelse(tick_path_Bb$testResult == 'Positive', 1, 0)
tick_path_Bb$collectDate <- as.character(tick_path_Bb$collectDate)
tick_path_Bb$Date <- substr(tick_path_Bb$collectDate, 1, nchar(tick_path_Bb$collectDate)-7)
head(tick_path_Bb)
nrow(tick_path_Bb)

tick_path_Bb_col <- aggregate(incidence ~ plotID + Date, data=tick_path_Bb, FUN=mean)
head(tick_path_Bb_col, n=20)
nrow(tick_path_Bb_col)
tick_path_Bb_col$plotID <- as.character(tick_path_Bb_col$plotID)
tick_path_Bb_col$site <- substr(tick_path_Bb_col$plotID, 1, nchar(tick_path_Bb_col$plotID)-4)
head(tick_path_Bb_col)

tick_path_Bb_col_E <- tick_path_Bb_col[which(tick_path_Bb_col$site != 'DELA' & tick_path_Bb_col$site != 'KONA' & tick_path_Bb_col$site != 'KONZ' & tick_path_Bb_col$site != 'LENO' & tick_path_Bb_col$site != 'UKFS'),]
head(tick_path_Bb_col_E)
nrow(tick_path_Bb_col_E)
unique(tick_path_Bb_col_E$site)
#plot:
plotInc <- ggplot(tick_path_Bb_col_E, aes(Date, incidence, group=plotID, colour=plotID)) + geom_line(size=1.5) + facet_wrap(~plotID)
plotInc

### RH:
RH_mean <- RH[,c(2,5,6,7)]
RH_mean <- RH_mean[complete.cases(RH_mean[,4]),]
head(RH_mean)
nrow(RH_mean)
RH_mean$startDateTime <- as.character(RH_mean$startDateTime)
RH_mean$startDate <- substr(RH_mean$startDateTime, 1, nchar(RH_mean$startDateTime)-10)
RH_mean_col <- aggregate(RHMean ~ startDate, data=RH_mean, FUN=mean)
head(RH_mean_col, n=20)

plotRH <- ggplot(RH_mean_col, aes(startDate, RHMean, group=1)) + geom_line()
plotRH


### Mammals:
head(mams)
nrow(mams)
unique(mams$trapStatus)
mamsTrap <- mams[,c(5,6,10,11,16,18,20,21,22,23,29,42,43,44)]
head(mamsTrap, n=20)
mamsTrap$statusCode <- substr(mamsTrap$trapStatus, 0, 1)
head(mamsTrap, n=50)

mamsTrap$capture <- ifelse(mamsTrap$statusCode == '5', 1, ifelse(mamsTrap$trapStatus == '4', 1, 0))
head(mamsTrap)
unique(mamsTrap$nymphalTicksAttached)
mamsTrap$larvaeYN <- ifelse(mamsTrap$larvalTicksAttached == 'Y', 1, 0)
mamsTrap$nymphsYN <- ifelse(mamsTrap$nymphalTicksAttached == 'Y', 1, 0)
mamsTrap$adultsYN <- ifelse(mamsTrap$adultTicksAttached == 'Y', 1, 0)
head(mamsTrap)

mamsTrap_captures <- mamsTrap[which(mamsTrap$capture == 1),]
nrow(mamsTrap_captures)
head(mamsTrap_captures)

mamsTrap_col <- aggregate(capture ~ plotID + collectDate, data=mamsTrap, FUN=mean)
nrow(mamsTrap_col)
head(mamsTrap_col)
mamsTrap_col$site <- substr(mamsTrap_col$plotID, 1, 4)
head(mamsTrap_col)
unique(mamsTrap_col$site)
mamsTrap_col_E <- mamsTrap_col[which(mamsTrap_col$site == 'HARV' | mamsTrap_col$site == 'OSBS' | mamsTrap_col$site == 'BART' | mamsTrap_col$site == 'ORNL' | mamsTrap_col$site == 'TALL' | mamsTrap_col$site == 'SCBI' | mamsTrap_col$site == 'UNDE' | mamsTrap_col$site == 'SERC' | mamsTrap_col$site == 'GRSM' | mamsTrap_col$site == 'BLAN' | mamsTrap_col$site == 'TREE' | mamsTrap_col$site == 'STEI' | mamsTrap_col$site == 'MLBS'),]
unique(mamsTrap_col_E$site)
nrow(mamsTrap_col_E)

mamsTrap_captures_L_col <- aggregate(larvaeYN ~ plotID + collectDate, data=mamsTrap_captures, FUN=mean)
nrow(mamsTrap_captures_L_col)
head(mamsTrap_captures_L_col)
mamsTrap_captures_L_col$site <- substr(mamsTrap_captures_L_col$plotID, 1, 4)
head(mamsTrap_captures_L_col)
unique(mamsTrap_captures_L_col$site)
mamsTrap_captures_L_col_E <- mamsTrap_captures_L_col[which(mamsTrap_captures_L_col$site == 'HARV' | mamsTrap_captures_L_col$site == 'OSBS' | mamsTrap_captures_L_col$site == 'BART' | mamsTrap_captures_L_col$site == 'ORNL' | mamsTrap_captures_L_col$site == 'TALL' | mamsTrap_captures_L_col$site == 'SCBI' | mamsTrap_captures_L_col$site == 'UNDE' | mamsTrap_captures_L_col$site == 'SERC' | mamsTrap_captures_L_col$site == 'GRSM' | mamsTrap_captures_L_col$site == 'BLAN' | mamsTrap_captures_L_col$site == 'TREE' | mamsTrap_captures_L_col$site == 'STEI' | mamsTrap_captures_L_col$site == 'MLBS'),]
unique(mamsTrap_captures_L_col_E$site)
nrow(mamsTrap_captures_L_col_E)

mamsTrap_captures_N_col <- aggregate(nymphsYN ~ plotID + collectDate, data=mamsTrap_captures, FUN=mean)
nrow(mamsTrap_captures_N_col)
head(mamsTrap_captures_N_col)
mamsTrap_captures_N_col$site <- substr(mamsTrap_captures_N_col$plotID, 1, 4)
head(mamsTrap_captures_N_col)
unique(mamsTrap_captures_N_col$site)
mamsTrap_captures_N_col_E <- mamsTrap_captures_N_col[which(mamsTrap_captures_N_col$site == 'HARV' | mamsTrap_captures_N_col$site == 'OSBS' | mamsTrap_captures_N_col$site == 'BART' | mamsTrap_captures_N_col$site == 'ORNL' | mamsTrap_captures_N_col$site == 'TALL' | mamsTrap_captures_N_col$site == 'SCBI' | mamsTrap_captures_N_col$site == 'UNDE' | mamsTrap_captures_N_col$site == 'SERC' | mamsTrap_captures_N_col$site == 'GRSM' | mamsTrap_captures_N_col$site == 'BLAN' | mamsTrap_captures_N_col$site == 'TREE' | mamsTrap_captures_N_col$site == 'STEI' | mamsTrap_captures_N_col$site == 'MLBS'),]
unique(mamsTrap_captures_N_col_E$site)
nrow(mamsTrap_captures_N_col_E)
head(mamsTrap_captures_N_col_E)


plotMams <- ggplot(mamsTrap_col_E, aes(collectDate, capture, group=plotID, colour=plotID)) + geom_line(size=1.5) + facet_wrap(~plotID)
plotMams

plotMamsN <- ggplot(mamsTrap_captures_N_col_E, aes(collectDate, nymphsYN, group=plotID, colour=plotID)) + geom_line(size=1.5) + facet_wrap(~plotID)
plotMamsN

plotMamsL <- ggplot(mamsTrap_captures_L_col_E, aes(collectDate, larvaeYN, group=plotID, colour=plotID)) + geom_line(size=1.5) + facet_wrap(~plotID)
plotMamsL




