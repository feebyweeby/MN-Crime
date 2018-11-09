library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(microbenchmark)
library(data.table)


setwd("/Users/Phoebe/Desktop/R scripts")
MN <- fread("MN_Crime.csv")

#Cleaning the time columns
MN$ReportedTime <- hms::as.hms(MN$ReportedTime)
MN$`Time of Offense` <- hms::as.hms(MN$`Time of Offense`)
MN$ReportedDate <- mdy(MN$ReportedDate)

#Checking the difference between times, in minutes
MN$Difftime <- difftime(MN$`Time of Offense`,MN$ReportedTime, tz = "UTC", units = "mins")

#Putting all vehicle related offenses under one common code
mn <- tbl_df(MN)

vehicle <- mn %>%
  filter( Offense == "TFMV" | Offense == "TMVP" | Offense == "AUTOTH")

y <- table(MN$Offense)
lut <- c("TFMV" = "VTHFT", "TMVP" = "VTHFT", "AUTOTH" = "VTHFT", "MVTHFT" = "VTHFT",
         "ADLTTN" = "ADLTTN", "ARSON" = "ARSON", "BIKETF" = "BIKETF", "BURGB" = "BURGB", "BURGD" = "BURGD",
         "COINOP" = "COINOP", "COMPUT" = "COMPUT", "DISARM" = "DISARM", "LOOT" = "LOOT", "NOPAY" = "NOPAY", "ONLTHT" = "ONLTHT",
         "POKCET" = "POCKET", "ROBBIZ" = "ROBBIZ", "ROBPAG" = "ROBPAG", "ROBPER" = "ROBPER", "SCRAP" = "SCRAP", "SHOPLF" = "SHOPLF", "TBLDG" = "TBLDG",
         "TFPER" = "TFPER", "THEFT" = "THEFT", "THFTSW" = "THFTSW")
MN$Offense_clean <- lut[MN$Offense]

MNClean <- distinct(MN)
#writing the first version of semi clean data to csv
#write.csv(MNClean,"cleanedMN.csv")

#Loading in the semi clean data
MNC <- fread("cleanedMN.csv")

#Trying to deal precinct 18

describe(MNClean$Precinct)
pre <- MNC %>%
  filter(Precinct == 18)

pre <- tbl_df(pre)

Como <- subset(pre, Neighborhood == "Como")
Como$Precinct <- 2

PP <- subset(pre, Neighborhood == "Prospect Park - East River Road")
PP$Precinct <- 2

UM <- subset(pre, Neighborhood == "University of Minnesota")
UM$Precinct <- 2

MH <- subset(pre, Neighborhood == "Marcy Holmes")
MH$Precinct <- 2

CR <- subset(pre, Neighborhood == "Cedar Riverside")
CR$Precinct <- 3

DE <- subset(pre, Neighborhood == "Downtown East")
DE$Precinct <- 1

DW <- subset(pre, Neighborhood == "Downtown West")
DW$Precinct <- 1

MP <- subset(pre, Neighborhood == "Midtown Phillips")
MP$Precinct <- 3

#combining the fixed files together
Precinctclean <- rbind(Como, PP, UM, MH, CR, DE, DW, MP)

#filtering all non precinct 18
post <- MNC %>%
  filter(Precinct != 18)

#combining the fixed file with the original
MNfinal <- rbind(Precinctclean, post)

#writing second version of semi clean data
write.csv(MNfinal,"finalMN.csv")
