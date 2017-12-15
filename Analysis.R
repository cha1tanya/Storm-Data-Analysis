# download the data file
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2","StormData.csv.bz2")
# Read it into data frame 
StormData <- read.csv("StormData.csv.bz2")

# Only select required columns. This helps to improve performance
library(dplyr)

StormData2 <- select(StormData, BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
# Format the date column as date and filter data from 1996 and later
StormData2$BGN_DATE <- as.Date(StormData2$BGN_DATE, format = "%m/%d/%Y")
StormData2 <- filter(StormData2, format(BGN_DATE, "%Y") >= 1996)


# Clean the variable for typos
# remove leading and traling spaces and Convert all to Upper case
StormData2$EVTYPE <- toupper(trimws(StormData2$EVTYPE))
# Remove multiple spaces
StormData2$EVTYPE <- gsub("[[:space:]]+", " ", StormData2$EVTYPE)
# Other typo substitution
StormData2$EVTYPE <- gsub("EXTREMELY|EXCESSIVE|EXCESSIVELY", "EXTREME", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("FLOODINGINGS*|FLOOODING", "FLOOD", StormData2$EVTYPE)
# StormData2$EVTYPE <- gsub("FLASH FLOODING.*", "FLOODING", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("LIGHTNING\\.|LIGHTING|LIGNTNING", "LIGHTNING", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("NON-", "NON ", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("PROLONG", "PROLONGED", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("RAINS", "RAIN", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("RIP CURRENTS", "RIP CURRENT", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("STORMS", "STORM", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("TORNDAO|TORNADOES", "TORNADO", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("TSTM|TH*UND*ER*[A-Z]*RMW*|THUNDERSTROM|THUDERSTORM",
                    "THUNDERSTORM", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("UNUSUALLY", "UNUSUAL", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("WILD.*FIRE.*|WILD/FOREST.*", "WILD/FOREST FIRES", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("WINDS|WND", "WIND", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("WINTERY", "WINTER", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("WARMTH", "WARM", StormData2$EVTYPE)
# grouping some events
StormData2$EVTYPE <- gsub("^BLIZZARD.*|ICE STORM", "BLIZZARD", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^COASTAL.*|.*/CSTL .*", "COASTAL EROSION/FLOODING/STORM ",
                    StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("EXTREME COLD.*|EXTENDED COLD.*", "EXTREME COLD", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^DRY.*", "DRY CONDITIONS", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^FLOODING.*", "FLOODING", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^FREEZE|^FREEZING.*|^FROST.*",
                    "FREEZING FOG/RAIN/SLEET/SNOW", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("HAIL.*", "HAIL", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("DROUGHT|EXTREME HEAT.*|^HEAT.*", "EXTREME HEAT", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("HEAVY RAIN.*", "HEAVY RAIN", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("HURRICANE.*", "HURRICANE", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("HEAVY SNOW.*|^SNOW.*|EXCESSIVE SNOW", "HEAVY SNOW/ICE",
                    StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("LIGHTNING.*", "LIGHTNING", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^MARINE.*", "MARINE THUNDERSTORM/ACCIDENT", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("RAIN.*|PROLONGEDED RAIN", "RAIN", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("RIP CURRENT.*|HEAVY SURF.*|HIGH SURF.*", "HIGH SURF", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("SLEET.*", "SLEET", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("VOLCANIC.*", "VOLCANIC", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("THUNDERSTORM.*|SEVERE THUNDERSTORM", "THUNDERSTORM", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("TORNADO.*", "TORNADO", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("TROPICAL STORM.*", "TROPICAL STORM", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("UNSEASONAL.*|^UNSEASONABL[EY].*|^^UNUSUAL.*",
                    "UNUSUAL WEATHER CONDITION", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("HIGH WIND.*|STRONG WIND.*|^WIND.*", "HIGH WIND", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^WATERSPOUT.*|WATER SPOUT", "WATERSPOUT", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^WINTER.*", "WINTER STORM/WIND", StormData2$EVTYPE)
StormData2$EVTYPE <- gsub("^NONE|^SUMMARY.*", "?", StormData2$EVTYPE)

Top10FatByEvnt <- select(StormData2, EVTYPE, FATALITIES) %>%
    group_by(EVTYPE) %>%
    summarise_all(funs(sum)) %>%
    arrange(desc(FATALITIES)) %>%
    slice(1:10)

Top10InjByEvnt <- select(StormData2, EVTYPE, INJURIES) %>%
    group_by(EVTYPE) %>%
    summarise_all(funs(sum)) %>%
    arrange(desc(INJURIES)) %>%
    slice(1:10)

# Plots for Human injuries
library(ggplot2)
ggplot(Top10FatByEvnt, aes(x=EVTYPE, y=FATALITIES)) + 
    geom_bar(aes(x=EVTYPE), stat="identity") + 
    xlab("EVTYPE") + ylab("FATALITIES") + ggtitle("Top 10 Events by FATALITIES") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
ggplot(Top10InjByEvnt, aes(x=EVTYPE, y=INJURIES)) + 
    geom_bar(aes(x=EVTYPE), stat="identity") + 
    xlab("EVTYPE") + ylab("INJURIES") + ggtitle("Top 10 Events by INJURIES") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) 

# Plot for economic damage
# Make use of the exponents for Property and Crop damage variables to create the actual damages
# unique(StormData2$PROPDMGEXP)
# unique(StormData2$CROPDMGEXP)

StormData2$PROPDMGEXP <- toupper(trimws(StormData2$PROPDMGEXP))
StormData2$PROP_DMG_K <- StormData2$PROPDMG*ifelse(StormData2$PROPDMGEXP =="K",1,ifelse(StormData2$PROPDMGEXP =="M",1000,ifelse(StormData2$PROPDMGEXP =="B",1e+06,ifelse(StormData2$PROPDMGEXP == "H",0.1,0.001))))
StormData2$CROP_DMG_K <- StormData2$CROPDMG*ifelse(StormData2$CROPDMGEXP =="K",1,ifelse(StormData2$CROPDMGEXP =="M",1000,ifelse(StormData2$CROPDMGEXP =="B",1e+06,ifelse(StormData2$CROPDMGEXP == "H",0.1,0.001))))
StormData2$TOT_DMG_K <- StormData2$PROP_DMG_K + StormData2$CROP_DMG_K

Top10DmgByEvnt <- select(StormData2, EVTYPE, TOT_DMG_K) %>%
    group_by(EVTYPE) %>%
    summarise_all(funs(sum)) %>%
    arrange(desc(TOT_DMG_K)) %>%
    slice(1:10)

ggplot(Top10DmgByEvnt, aes(x=EVTYPE, y=TOT_DMG_K)) + 
    geom_bar(aes(x=EVTYPE), stat="identity") + 
    xlab("EVTYPE") + ylab("TOT_DMG_K") + ggtitle("Top 10 Events by Economic Damage") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) 


