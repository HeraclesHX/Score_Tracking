# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Xi'an Jassen PET
# Purpose:      Read in all the data and do the QC
# programmer:   Xin Huang
# Date:         06-20-2017
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##-- load the required packages
library(openxlsx)
library(dplyr)
library(ggplot2)
library(lubridate)

##-- read in the raw data, including EDA, Meeting and Call
eda_dat <- read.xlsx("./02_Inputs/EDAData201707.xlsx", 
                     sheet = "Raw Data")
colnames(eda_dat)[c(4, 5, 6, 10, 11)] <- 
  c("questionare.id", "most.recent.modify.date", "survey.name", "questions", 
    "answers")
colnames(eda_dat) <- tolower(colnames(eda_dat))
eda_dat$most.recent.modify.date.m <-
  as.Date(eda_dat$most.recent.modify.date, origin = "1899-12-30")


meeting_dat <- read.xlsx("./02_Inputs/iMeetingData.xlsx", 
                         sheet = "iMeeting")
colnames(meeting_dat)[c(2, 3, 4, 5, 6, 7, 10, 11, 12)] <-
  c("imeeting.name", "creator.id", "imeeting.addr.id", "imeeting.addr",
    "imeeting.time", "imeeting.type", "attendee.hosp.id", "attendee.hosp.name",
    "doctor.id")
colnames(meeting_dat) <- tolower(colnames(meeting_dat))

call_dat <- read.xlsx("./02_Inputs/CallData.xlsx")
colnames(call_dat)[c(4, 11)] <- c("call.date", "doctor.tier")
colnames(call_dat) <- tolower(colnames(call_dat))
call_dat$call.date <- as.Date(call_dat$call.date, origin = "1899-12-30")


##-- begin QC process

#- chking the date period

table(year(eda_dat$most.recent.modify.date.m),
      month(eda_dat$most.recent.modify.date.m))


#         1     2     3     4     5     6     7     8     9    10    11    12
# 2016     0     0     0     0     0     0  1275 24541  4757  3463  2120  3278
# 2017  1693   745   934   194   978   124     0     0     0     0     0     0

