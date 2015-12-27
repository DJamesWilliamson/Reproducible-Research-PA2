setwd("C:/Users/he49794/Work/Statistics Resources/Coursera/Reproducible Research/Project 2")

dir("./")

# Libraries
library(R.utils)
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(lattice)
library(reshape2)

# download the data from the website, include date downloaded, and unzip
# note that read.table can read bz2 files directly,
# but this method retains bz2 and download data
dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fileDest <- sprintf("storm_%s.bz2", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))
download.file(dataset_url, fileDest, mode = "wb", method = "libcurl")
bunzip2(filename = fileDest, destname = "storm.csv")

# import data and convert to tbl_df for dplyr
data <- tbl_df(fread("repdata_data_StormData.csv",
                     sep = ",",
                     # nrows = 100,
                     header = TRUE,
                     na.strings = "",
                     stringsAsFactors = FALSE,
                     select = c(1, 2, 5, 7, 8, 23:30, 37),
                     data.table = FALSE,
                     verbose = FALSE))

(nRecords1 <- nrow(data))

# rename and reorder variables; capitalise and trim EVTYPE
storms <- data %>%
        rename(stateCode = STATE__,
               beginDate = BGN_DATE,
               countyCode = COUNTY,
               state = STATE,
               eventType = EVTYPE,
               fatalities = FATALITIES,
               injuries = INJURIES,
               propDamage = PROPDMG,
               propDamageExp = PROPDMGEXP,
               cropDamage = CROPDMG,
               cropDamageExp = CROPDMGEXP,
               wfo = WFO,
               stateOffice = STATEOFFIC,
               refNum = REFNUM) %>%
        select(refNum, beginDate, stateCode, state, countyCode, wfo, stateOffice,
               eventType,
               fatalities, injuries,
               propDamage, propDamageExp,
               cropDamage, cropDamageExp) %>%
        mutate(eventType = toupper(eventType)) %>%
        mutate(eventType = str_trim(eventType, "both"))

# remove original file to reduce impact on memory
rm(data)

# change beginDate to class Date and filter records since 1996 (full data available)
storms$beginDate <- str_replace(storms$beginDate, " 0:00:00", "")
storms$beginDate <- as.Date(storms$beginDate, "%m/%d/%Y")
storms <- storms %>%
        filter(beginDate >= "1996-01-01") %>%
        arrange(beginDate)

# check each variable for class and missing values
(classes <- sapply(storms, class))
(colSums(is.na(storms)))

(nRecords2 <- nrow(storms))

# examine critical variables
table(storms$propDamageExp)
table(storms$cropDamageExp)
storms %>% select(propDamageExp, fatalities, injuries, propDamage, cropDamage) %>%
        group_by(propDamageExp) %>%
        summarise(n = n(), people = sum(fatalities + injuries),
                  damage = sum(propDamage + cropDamage)) %>%
        arrange(desc(n))

# shows propDamageExp restricted to K, B, M, NA and 0  (1 case but no injuries or damage)       
# replace propDamageExp and cropDamageExp with numerical multipliers
storms$propDamageExp <- replace(storms$propDamageExp,
                                which(storms$propDamageExp == "0"), "NA")
storms$propDamageExp <- replace(storms$propDamageExp,
                                which(storms$propDamageExp == "K"), "1000")
storms$propDamageExp <- replace(storms$propDamageExp,
                                which(storms$propDamageExp == "M"), "1000000")
storms$propDamageExp <- replace(storms$propDamageExp,
                                which(storms$propDamageExp == "B"), "1000000000")
storms$propDamageExp <- as.numeric(storms$propDamageExp)

storms$cropDamageExp <- replace(storms$cropDamageExp,
                                which(storms$cropDamageExp == "K"), "1000")
storms$cropDamageExp <- replace(storms$cropDamageExp,
                                which(storms$cropDamageExp == "M"), "1000000")
storms$cropDamageExp <- replace(storms$cropDamageExp,
                                which(storms$cropDamageExp == "B"), "1000000000")
storms$cropDamageExp <- as.numeric(storms$cropDamageExp)

# identify eventTypes where there is no damage to people, property or crops
noDamageTable <- storms %>% select(eventType, fatalities, injuries,
                                   propDamage, propDamageExp,
                                   cropDamage, cropDamageExp) %>%
        group_by(eventType) %>%
        summarise(n = n(),
                  people = as.numeric(sum(fatalities + injuries, na.rm = TRUE)),
                  damage = as.numeric(sum(propDamage + cropDamage, na.rm = TRUE))) %>%
        arrange(desc(n)) %>%
        filter(people == 0 & damage == 0)

noDamageEvents <- noDamageTable$eventType


# exclude eventTypes where no damage to people, property or crops
storms <- storms %>%
        filter(!(eventType %in% noDamageEvents))

(nRecords3 <- nrow(storms))

# correct eventTypes
storms$eventType <- gsub("TSTM", "THUNDERSTORM", storms$eventType)
storms$eventType <- gsub("NON-THUNDERSTORM",  "STRONG", storms$eventType)
storms$eventType <- gsub("NON THUNDERSTORM",  "STRONG", storms$eventType)
storms$eventType <- gsub("CSTL",  "COASTAL", storms$eventType)
storms$eventType <- gsub("  ",  " ", storms$eventType)

# standardise against official codes
event_types_standard <- read.table("event_types.csv",
                                header = FALSE,
                                sep = ",",
                                col.names = c("stdEvType", "designator",
                                              "keyTerm1", "keyTerm2",
                                              "keyTerm3", "keyTerm4"))
event_types_standard <- event_types_standard %>%
        mutate(stdEvType = toupper(stdEvType),
               keyTerm1 = toupper(keyTerm1),
               keyTerm2 = toupper(keyTerm2),
               keyTerm3 = toupper(keyTerm3),
               keyTerm4 = toupper(keyTerm4))

stdEvType <-event_types_standard$stdEvType
keyTerm1 <- event_types_standard$keyTerm1
keyTerm2 <- event_types_standard$keyTerm2
keyTerm3 <- event_types_standard$keyTerm3
keyTerm4 <- event_types_standard$keyTerm4

# ignore exact matches and identify other unique eventTypes to match
events_to_match <- storms %>%
        mutate(eventTypeMatch = eventType %in% stdEvType) %>%
        filter(eventTypeMatch == "FALSE") %>%
        select(eventType)
events_to_match <- sort(unique(events_to_match$eventType))

#####################################################################
# function to map events_to_match (x) against keyTerms (y) and stdEvType (z, default)
matchingGrep <-  function(x,y,z = stdEvType) {
        output <- list(length(y))
        for(i in 1:length(y)) {
                output[[i]] <- x[grep(y[i], x)]
        }
        eventType <- as.vector(unlist(output))
        n <- as.vector(unlist(sapply(output, length)))
        match <- rep(z, times = n)
        replacements <- tbl_df(as.data.frame(cbind(eventType, match)))
        replacements <- replacements %>%
                filter(!duplicated(eventType))
        return(replacements)
}
########################################################################

replacements1 <- matchingGrep(events_to_match, keyTerm1)
remove <- which(events_to_match %in% replacements1$eventType)
events_to_match <- events_to_match[-remove]

replacements2 <- matchingGrep(events_to_match, keyTerm2)
remove <- which(events_to_match %in% replacements2$eventType)
events_to_match <- events_to_match[-remove]

replacements3 <- matchingGrep(events_to_match, keyTerm3)
remove <- which(events_to_match %in% replacements3$eventType)
events_to_match <- events_to_match[-remove]

replacements4 <- matchingGrep(events_to_match, keyTerm4)
remove <- which(events_to_match %in% replacements4$eventType)
events_to_match <- events_to_match[-remove]

replacements5 <- cbind(eventType = events_to_match,
                       match = rep("OTHER", length(events_to_match)))

# nonStandardEvents <- rbind(replacements1, replacements2, replacements3,
#                             replacements4, replacements5)
# write.csv(nonStandardEvents, file = "nonStandard_events.csv")

stdEvents <- event_types_standard %>%
                        select(eventType = stdEvType, match = keyTerm1)
matchingTable <- rbind(replacements1, replacements2, replacements3,
                                replacements4, replacements5,
                                stdEvents)

############################################################################
# standardise the eventTypes in the storms data
storms <- left_join(storms, matchingTable, by = "eventType")

# review the variables
uniqueValues <- sapply(storms, unique)
sapply(uniqueValues, length)
# indicates a problem with the number of states
uniqueValues$state
uniqueValues$stateCode

# read in US state code data
fipsCodes <- read.csv("US State FIPS codes.csv")

includedStates <- which(storms$stateCode %in% fipsCodes$FIPS.Code)
excludedStateCodes <- sort(unique(storms$stateCode[-includedStates]))

excludedEntries <- filter(storms, stateCode %in% excludedStateCodes)
excludedStates <- sort(unique(excludedEntries$state))

# note that some of these are in neither the FIPS codes or supplemental maritime codes
#####################################################################


# select data for analysis
stormsData <- storms %>%
                mutate(propDamageCost = propDamage * propDamageExp,
                        cropDamageCost = cropDamage * cropDamageExp) %>%
                rename(event = match,
                       FIPS.code = stateCode) %>%
                filter(!(FIPS.code %in% excludedStateCodes)) %>%
                select(refNum, beginDate, state, event,
                       fatalities, injuries,
                       propDamageCost, cropDamageCost) %>%
                mutate(state = as.factor(state))


(nRecords4 <- nrow(stormsData))
        
# further restrict to entries where effects on people, property or crops is >0
stormsData <- stormsData %>%
                filter(fatalities > 0 | injuries > 0 | propDamageCost > 0 | cropDamageCost)
# colSums(sapply(stormsData, is.na))

(nRecords5 <- nrow(stormsData))

###############################################################################
Mydotplot <- function(DataSelected){
        P <- dotplot(as.matrix(as.matrix(DataSelected)),
                     groups = FALSE,
                     strip = FALSE,
                     # strip = strip.custom(bg = 'white',
                     # par.strip.text = list(cex = 1.2)),
                     scales = list(x = list(relation = "free", draw = TRUE),
                                   y = list(relation = "free", draw = FALSE)),
                     col=1, cex  = 0.5, pch = 16,
                     xlab = list(label = "Cost($)", cex = 1.5),
                     ylab = list(label = "Order of data", cex = 1.5),
                     main = list(label = "Cleveland Dotplot", cex = 1.75))
        print(P)  
}
###############################################################################
png(filename = "EDA_propDamageCost.png", width = 600, height = 480)
par(mfrow = c(1,1))
Mydotplot(stormsData$propDamageCost)
dev.off()
par(mfrow = c(1, 1))

# outlier identified and changed (million, not billion)
stormsData[which(stormsData$propDamageCost > 1.0e+11), ]
outlier <- storms[which(storms$refNum == 605943), ]
outlier[ c(11:16)]
stormsData[which(stormsData$refNum == 605943), 7] <- 1.15e+8

# potential outlier identified as Hurricane Katrina (see below)
stormsData[which(stormsData$propDamageCost > 2.0e+10), ]
outlier <- storms[which(storms$refNum == 577616), ]
outlier[ c(11:14)]

# Hurricane Katrina caused $81 billion in property damages
# https://www.dosomething.org/us/facts/11-facts-about-hurricane-katrina

################################
png(filename = "EDA_cropDamageCost.png", width = 600, height = 480)
par(mfrow = c(1,1))
Mydotplot(stormsData$cropDamageCost)
dev.off()
par(mfrow = c(1, 1))

# potential outlier identified as Hurricane Katrina (unchanged)
stormsData[which(stormsData$cropDamageCost > 1.5e+9), ]
outlier <- storms[which(storms$refNum == 577616), ]
outlier[ c(11:14)]
#################################
rm(storms)
##################################

# add years
stormsData$date = as.POSIXlt(stormsData$beginDate)
stormsData$year = stormsData$date$year + 1900

stormsData <- stormsData %>%
                select(refNum, beginDate, year, state, event,
                        fatalities, injuries,
                        propDamageCost, cropDamageCost) 

annualHumanData <- group_by(stormsData, year) %>%
                        summarise(Fatalities = sum(fatalities),
                                Injuries = sum(injuries))
par(mfrow = c(2,1))
barplot(height = annualHumanData$Fatalities, names.arg = annualHumanData$year,
        main = "Annual US Storm Fatalities 1996-2011",
        xlab = "Year",
        ylab = "Fatalities")
barplot(height = annualHumanData$Injuries, names.arg = annualHumanData$year,
        main = "Annual US Storm Injuries 1996-2011",
        xlab = "Year",
        ylab = "Injuries")
par(mfrow = c(1,1))

annualEconomicData <- filter(stormsData, year >= 2007) %>%
                        group_by(year) %>%
                        summarise(PropertyDamage = sum(propDamageCost/1.0e+9),
                                CropDamage = sum(cropDamageCost/1.0e+9))
par(mfrow = c(2,1))
barplot(height = annualEconomicData$PropertyDamage, names.arg = annualEconomicData$year,
        main = "Annual US Property Damage 2007-2011",
        xlab = "Year",
        ylab = "Dollars(billions)")
barplot(height = annualEconomicData$CropDamage, names.arg = annualEconomicData$year,
        main = "Annual US Crop Damage 2007-2011",
        xlab = "Year",
        ylab = "Dollars(billions)")
par(mfrow = c(1,1))

annualHumanData_byEvent <- filter(stormsData, year >= 2007) %>%
        group_by(event) %>%
        summarise(Fatalities = sum(fatalities),
                  Injuries = sum(injuries)) %>%
        arrange(desc(Fatalities + Injuries))

annualEconomicData_byEvent <- filter(stormsData, year >= 2007) %>%
        group_by(event) %>%
        summarise(PropertyDamage = sum(propDamageCost/1.0e+9),
                  CropDamage = sum(cropDamageCost/1.0e+9)) %>%
        arrange(desc(PropertyDamage + CropDamage))



rm(list = ls())
ls()
.Last.value %>% View()
