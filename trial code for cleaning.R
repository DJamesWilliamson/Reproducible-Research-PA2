setwd("C:/Users/he49794/Work/Statistics Resources/Coursera/Reproducible Research/Project 2")

dir("./")

library(stringr)
library(dplyr)

# standardise against official codes
event_types_standard <- read.table("event_types.csv",
                                   header = FALSE,
                                   sep = ",",
                                   col.names = c("stdEvType", "designator", "keyTerm1", "keyTerm2", "keyTerm3", "keyTerm4"))


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


# read in events_to_match
events_to_match <- read.csv("events_to_match.csv")
events_to_match <- as.vector(events_to_match[ ,2])
events_to_match <- gsub("NON-THUNDERSTORM",  "STRONG", events_to_match)
events_to_match <- gsub("NON THUNDERSTORM",  "STRONG", events_to_match)
events_to_match <- gsub("CSTL",  "COASTAL", events_to_match)
events_to_match <- gsub("  ",  " ", events_to_match)
###############################################################################
# output <- list(length(stdEvType))
# for(i in 1:length(stdEvType)) {
#         output[[i]] <- events_to_match[grep(stdEvType[i], events_to_match)]
# }
# names(output) <- stdEvType
# original <- as.vector(unlist(output))
# 
# 
# n <- as.vector(unlist(sapply(output, length)))
# match <- rep(stdEvType, times = n)
# 
# replacements <- tbl_df(as.data.frame(cbind(original, match)))
# class(replacements); length(replacements)
# replacements <- replacements %>%
#         filter(!duplicated(original))

#############################################################
# function to map events_to_match (x) against keyTerms (y) and stdEvType (z, default)
matchingGrep <-  function(x,y,z = stdEvType) {
        output <- list(length(y))
        for(i in 1:length(y)) {
        output[[i]] <- x[grep(y[i], x)]
        }
        original <- as.vector(unlist(output))
        n <- as.vector(unlist(sapply(output, length)))
        match <- rep(z, times = n)
        replacements <- tbl_df(as.data.frame(cbind(original, match)))
        replacements <- replacements %>%
                filter(!duplicated(original))
        return(replacements)
}
####################################################################
length(events_to_match)
replacements1 <- matchingGrep(events_to_match, keyTerm1)
remove <- which(events_to_match %in% replacements1$original)
events_to_match <- events_to_match[-remove]
length(events_to_match)
replacements2 <- matchingGrep(events_to_match, keyTerm2)
remove <- which(events_to_match %in% replacements2$original)
events_to_match <- events_to_match[-remove]
length(events_to_match)
replacements3 <- matchingGrep(events_to_match, keyTerm3)
remove <- which(events_to_match %in% replacements3$original)
events_to_match <- events_to_match[-remove]
length(events_to_match)
replacements4 <- matchingGrep(events_to_match, keyTerm4)
remove <- which(events_to_match %in% replacements4$original)
events_to_match <- events_to_match[-remove]
replacements5 <- cbind(original = events_to_match, match = rep("OTHER", length(events_to_match)))

matchingTable <- rbind(replacements1, replacements2, replacements3, replacements4, replacements5)

##########################################################333
rm(list = ls())
.Last.value %>% View()

# make pairings for replacement of non-standard eventTypes
pairings <- data.frame()
for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                pairings[i, 1] <- output[[i]][j]
        }
}
for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                pairings[i , 2] <- rep(stdEvType[i], length(output[i]))
        }
}

# remove NAs, check for duplicates, remove the second duplicates (seems reasonable)
# pairings <- pairings[which(!is.na(pairings$V1)), ]
# pairings <- pairings[which(!duplicated(pairings$V1)), ]


pairings <- pairings %>%
        rename(oldEvType = V1, stdEvType = V2) %>%
        filter(!is.na(oldEvType)) %>%
        filter(!duplicated(oldEvType))
pairings1 <-pairings






























###########################################################################
# cleaning
output <- list()
for(i in 1:length(keyTerm1)) {
        output[[i]] <- events_to_match[str_detect(events_to_match, keyTerm1[i])]
}




for(i in seq_len(length(output))) print(length(output[[i]]))



pairings <- data.frame()
n <- sum(sapply(output, length))
list <- as.vector(dim(n))

test_list <- list(n)
output

for(i in seq_len(length(output))) {
        temp <- as.character()
        for(j in seq_len(length(output[[i]]))) {
                temp <- as.character()
                temp[j] <- output[[i]][j]
        }
        test_list[[i]] <- temp
}
test_list

for(i in seq_len(length(output))) {
        vec <- character()
        for(j in seq_len(length(output[[i]]))) {
                        vec[j] <- output[[i]][j]
                }
        test_list[[i]] <- vec
}
df <- do.call(rbind, test_list)

for(i in 1:5) {
        vec <- numeric(5)
        for(j in 1:6) {
                vec[j] <- j
        }
        test_list[[i]] <- vec
}
df <- do.call(rbind, test_list)
#####################################################################
for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                list[i] <- output[[i]][j]
        }
}
for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                print(rep(stdEvType[i], length(output[i])))
        }
}

pairings <- data.frame()
sum(sapply(output,length))


for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                        list[z] <- as.vector(output[[i]][j])     
        }
}

for(i in seq_len(length(output))) {
        for(j in seq_len(length(output[[i]]))) {
                pairings_new <- rep(stdEvType[i], length(output[i]))
        }
}


rm(pairings)
pairings <- pairings %>%
        rename(oldEvType = V1, stdEvType = V2) %>%
        filter(!is.na(oldEvType)) %>%
        filter(!duplicated(oldEvType))
pairings2 <-pairings