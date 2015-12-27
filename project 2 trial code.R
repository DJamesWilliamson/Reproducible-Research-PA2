setwd("C:/Users/he49794/Work/Statistics Resources/Coursera/Reproducible Research/Project 2")

dir("./")

# Libraries
library(R.utils)
library(dplyr)
library(stringr)
library(data.table)

# create a new directory for the project and set as working directory
if(!file.exists("Storm_Data")) dir.create("Storm_Data")
setwd("./Storm_Data")

# Data source:
# https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2

# download the data from the website, include date downloaded, and unzip
# note that read.table can read bz2 files directly, but this method retains bz2 and download data
dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
fileDest <- sprintf("storm_%s.bz2", format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))
download.file(dataset_url, fileDest, mode = "wb", method = "libcurl")
library(R.utils)
bunzip2(filename = fileDest, destname = "storm.csv")




# OR
# library(downloader)
# dataset_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
# download(dataset_url, 
#                 dest = "storm.csv", 
#                 mode = "wb")

# Initial read of data to facilitate import of full dataset:
initialRead <- read.table("storm.csv",
                header=TRUE,
                sep = ",", 
                na.strings = "NA", 
                stringsAsFactors = FALSE,
                comment.char = "#",
                nrows = 100)
head(initialRead)
classes <- sapply(initialRead, class)
vars <- names(classes)
str(vars)
date_vars <- as.vector(str_match(vars, "_DATE"))
classes[which(!is.na(date_vars))] <- "date"
time_vars <- as.vector(str_match(vars, "_TIME"))
classes[which(!is.na(time_vars))] <- "NULL"
exp_vars <- as.vector(str_match(vars, "EXP"))
classes[which(!is.na(exp_vars))] <- "character"
classes[29:37] <- "NULL"
classes[21] <- "character"
classes
rm(initialRead)
storms <- read.csv("storm.csv", 
                    header=TRUE,
                    na.strings = "NA", 
                    stringsAsFactors = FALSE,
                    comment.char = "",
                    quote = "",
                    # colClasses = classes,
                   # nrows = 100)


                   
                   # data_dt <- fread("repdata_data_StormData.csv",
                   #               sep = ",",
                   #               nrows = 100,
                   #               header = TRUE,
                   #               na.strings = "NA",
                   #               stringsAsFactors = FALSE,
                   #               # select = c(1, 2, 5, 7, 8, 23:30),
                   #               data.table = TRUE)
                   #               # verbose =TRUE)
                   # 
                   # head(data_dt)
                   # tail(data_dt)
                   # names(data_dt)
                   # str(data_dt)                   

# import data
storms <- fread("repdata_data_StormData.csv",
                 sep = ",",
                 # nrows = 100,
                 header = TRUE,
                 na.strings = "",
                 stringsAsFactors = FALSE,
                 select = c(1, 2, 5, 7, 8, 23:30),
                 data.table = FALSE,
                 verbose = FALSE)
# check data
head(storms)
tail(storms)
dim(storms)
names(storms)
str(storms)
summary(storms)

# rename variables (for clarity) with dplyr
storms <- rename(storms,
        stateCode = STATE__,
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
        stateOffice = STATEOFFIC)

# create tbl_df (also creates factors)
storms <- tbl_df(storms)
# names(storms)

# check each variable for NAs
colSums(is.na(storms))

# check classes
(classes <- sapply(storms, class))

# change class to date
storms$beginDate <- str_replace(storms$beginDate, " 0:00:00", "")
storms$beginDate <- as.Date(storms$beginDate, "%m/%d/%Y")
# str(storms)

# restrict dates to periods since 1996 (since full data only available since then)
storms <- filter(storms, beginDate >= "1996-01-01")
storms <- arrange(storms, beginDate)



# examine critical variables
storms %>% select(propDamageExp, fatalities, injuries, propDamage, cropDamage) %>%
        group_by(propDamageExp) %>%
        summarise(n = n(), people = sum(fatalities + injuries), damage = sum(propDamage + cropDamage)) %>%
        arrange(desc(n))
# shows propDamageExp restricted to K, B, M, NA and 0  (1 case but no injuries or damage)       
# replace with numerical multipliers
storms$propDamageExp <- replace(storms$propDamageExp, which(storms$propDamageExp == "K"), "1000")
storms$propDamageExp <- replace(storms$propDamageExp, which(storms$propDamageExp == "M"), "1000000")
storms$propDamageExp <- replace(storms$propDamageExp, which(storms$propDamageExp == "B"), "1000000000")
storms$propDamageExp <- as.numeric(storms$propDamageExp)

# same for crop damage
storms %>% select(cropDamageExp, fatalities, injuries, propDamage, cropDamage) %>%
        group_by(cropDamageExp) %>%
        summarise(n = n(), people = sum(fatalities + injuries), damage = sum(propDamage + cropDamage)) %>%
        arrange(desc(n))
# replace with numerical multipliers
storms$cropDamageExp <- replace(storms$cropDamageExp, which(storms$cropDamageExp == "K"), "1000")
storms$cropDamageExp <- replace(storms$cropDamageExp, which(storms$cropDamageExp == "M"), "1000000")
storms$cropDamageExp <- replace(storms$cropDamageExp, which(storms$cropDamageExp == "B"), "1000000000")
storms$cropDamageExp <- as.numeric(storms$cropDamageExp)

# identify eventTypes where there is no damage to people, property or crops
noDamageTable <- storms %>% select(eventType, fatalities, injuries, propDamage, propDamageExp, cropDamage, cropDamageExp) %>%
        group_by(eventType) %>%
        summarise(n = n(),
                  people = as.numeric(sum(fatalities + injuries, na.rm = TRUE)),
                  damage = as.numeric(sum(propDamage + cropDamage, na.rm = TRUE))) %>%
        arrange(desc(n)) %>%
        filter(people == 0 & damage == 0)

# %>%
        # filter(is.na(propDamageExp) & is.na(cropDamageExp))

noDamageEvents <- noDamageTable$eventType

# check eventTypes where there is no damage
# waterspouts <- filter(storms, eventType == "WATERSPOUTS")
# dim(waterspouts)
# tornados <- filter(storms, eventType == "TORNADO")
# dim(tornados)
# recordWarmth <- filter(storms, eventType == "RECORD WARMTH")
# dim(recordWarmth)
# moderateSnowfall <- filter(storms, eventType == "MODERATE SNOWFALL")
# dim(moderateSnowfall)


# exclude eventTypes where no damage to people, property or crops
storms2 <- storms %>%
                filter(!(eventType %in% noDamageEvents))

# capitalise and trim variable names
storms2 <- storms2 %>%
        mutate(eventType = toupper(eventType)) %>%
        mutate(eventType = str_trim(eventType, "both"))

# standardise against official codes
event_types_standard <- read.table("event_types.txt",
                                   header = FALSE,
                                   sep = ",",
                                   col.names = c("STD_EVENT_NAME", "DESIGNATOR"))
std_event_types <- toupper(event_types_standard$STD_EVENT_NAME)

# ignore exact matches and identify other unique eventTypes to match
events_to_match <- storms2 %>% mutate(eventTypeMatch = eventType %in% std_event_types) %>%
                filter(eventTypeMatch == "FALSE") %>%
                select(eventType)
events_to_match <- events_to_match$eventType
event_types_unique <- unique(events_to_match)

# event_types <- table(storms2$state, storms2$eventType)
# colSums(event_types)
# 
# event_types_unique[grep("MARINE", event_types_unique)]


output <- list()
for(i in 1:length(std_event_types)) {
        output[[i]] <- event_types_unique[grep(std_event_types[i], event_types_unique)]
}        

# matches <- data.frame()
# for(i in 1:length(std_event_types)) {
#         matches[i, 1] <- std_event_types[i]
#         # matches[i, 2] <- output[[i]]
# }  
# 
# length(output)
# for(i in 1:length(output)) {
#         matches[i, 2] <- output[[i]]
# } 


# for(i in seq_len(length(output))) {
#         for(j in seq_len(length(output[[i]]))) {
#                 print(output[[i]][j])
#         }
# }
# 
# for(i in seq_len(length(output))) {
#         for(j in seq_len(length(output[[i]]))) {
#                 print(rep(std_event_types[i], length(output[i])))
#         }
# }

# make pairings for replacement of non-standard eventTypes
pairings <- data.frame()
for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                pairings[i, 1] <- output[[i]][j]
        }
}
for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                pairings[i , 2] <- rep(std_event_types[i], length(output[i]))
        }
}

# remove NAs, check for duplicates, remove the second duplicates (seems reasonable)
pairings <- pairings[which(!is.na(pairings$V1)), ]
pairings <- pairings[which(!duplicated(pairings$V1)), ]

head(storms2)
dim(storms2)
# check each variable for NAs
colSums(is.na(storms2))
# check each variable for unique values (for consideration of conversion to factors)
uniqueValues <- sapply(storms, unique)
sapply(uniqueValues, length)
# indicates a problem with the number of states
uniqueValues$state
uniqueValues$stateCode

fipsCodes <- read.csv("US State FIPS codes.csv")
# fipsCodes$FIPS.Code <- str_pad(fipsCodes$FIPS.Code, width = 2, side = "left", pad = "0")

# storms2 %>% filter(stateCode %in% fipsCodes$FIPS.Code)
OKstates <- which(storms2$stateCode %in% fipsCodes$FIPS.Code)
length(OKstates)
oddStates <- storms2$stateCode[-OKstates]
length(oddStates)
length(OKstates) + length(oddStates)
nrow(storms2)
# identify codes of states with no FIPS
sort(unique(oddStates))

oddEntries <- filter(storms2, stateCode %in% oddStates)
head(oddEntries)
tail(oddEntries)
# note that some of these are in neither the FIPS codes or supplemental maritime codes
#########################

# .Last.value %>% View()


length(output)
output[[3]][2]     
length(output[[3]])

for(i in 1:length(output)) {
        for(j in 1:length(output[[i]])) {
                print(output[[i]],[j])
        }
}

for(i in 1:length(output)) print(output[[i]])

        print(event_types_standard$STD_EVENT_NAME[i])
        # lapply(event_types_unique, grep("event_types_standard[i]"))
length(output[[3]])

?stringr

tables()
