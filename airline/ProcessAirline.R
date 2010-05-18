
library(bigmemory)
library(biganalytics)

setwd("/home/jay/Desktop/Data")
xdesc <- dget("airline.desc")
x <- attach.big.matrix(xdesc)
refreshFromMaster <- TRUE

mdesc <- dget("airline.master.desc")
m <- attach.big.matrix(mdesc)

if (refreshFromMaster) {
  for (i in c('DepTime', 'CRSDepTime', 'ArrTime', 'CRSArrTime')) {
    x[,i] <- m[,i]
  }
}

#### WARNING: WE MAY NOT WANT TO CONVERT TO MINUTES AFTER MIDNIGHT!!!!
#### SKIP THE FOLLOWING:


#########################################################################
# Preprocessing takes care of four non-integer variables in the raw data.
# We'll handle a time conversion problem in R (see below).

cbind(m[1:10,'DepTime'],
      x[1:10,'DepTime'])

if (any(m[1:1000,'DepTime']==x[1:1000,'DepTime'], na.rm=TRUE)) {
  cat("NEED TO DO TIME PROCESSING, x and m times are identical.")

#########################################################################
# Do the processing:
ToMinutesAfterMidnight <- function(times) {
  times <- as.character(times)
  nc <- nchar(times)
  minutes <- as.integer(substring(times, nc-1))
  minutes[minutes>59] <- NA
  hour <- as.integer(substring(times, 1, nc-2))
  hour[is.na(hour)] <- as.integer(0)
  hour[hour>23] <- NA
  return(minutes + as.integer(60) * hour)
}

gc.0 <- gc(reset=TRUE)
time.0 <- system.time( {

  for (i in c('DepTime', 'CRSDepTime', 'ArrTime', 'CRSArrTime')) {
    x[,i] <- ToMinutesAfterMidnight(x[,i])
    gc()
  }

} )
gc.1 <- gc()

if (any(m[1:1000,'DepTime']==x[1:1000,'DepTime'], na.rm=TRUE)) {
  stop("NEED TO DO TIME PROCESSING, x and m times are identical.")
} else {
  cat("NOTICE: TIME PROCESSING HAS BEEN COMPLETE: go ahead and use x.\n")
}
#########################################################################

} else {
  cat("NOTICE: TIME PROCESSING HAS BEEN COMPLETE: go ahead and use x.\n")
}



###################################################################################
# Let's trim this down a bit:

# JFK 29
# SFO 2

summary(x[,'DepDelay'])

these <- mwhich(x, c('Year', 'Origin', 'Dest'),
                   list(2005, 2, 29),
                   list('eq', 'eq', 'eq'), op='AND')


these <- mwhich(x, cols=c('DepDelay', 'DepDelay'),
                   vals=list(-100, NA), 
                   comps=list('lt', 'neq'))
length(these)

y <- x[these,]
summary(y[,'DepDelay'])
y[y[,'DepDelay']>400 & !is.na(y[,'DepDelay']),]



# Variables needed:
#
# EventType
# Year
# DayofMonth          # Change this is descriptor??  Tell Mike.
# DayOfWeek
# Time                # Not an airline variable
# Origin
# Dest
# DepTime
# CRSDepTime
# ArrTime
# CRSArrTime

# Filter on Cancelled?  Diverted?  Types of delays?

################################################################################
# GO WITH THIS:
################################################################################
# Convert all times to GMT based on the airport codes and GMT offset information
# from the other file.

#a <- Sys.time() # About 1.2 billion, probably seconds since 1970.
#a <- "2010-05-17 09:16:23"
#b <- as.POSIXct(strptime(a, "%Y-%m-%d %H:%M:%S"))

air <- dget("/home/jay/Desktop/BigmemoryProject/bigmemory/airline/ContinentalUSAirportInfo.txt")
rownames(air) <- air$airport

require(doMC)
registerDoMC(2)

gethourmin <- function(times) {
  times <- as.character(times)
  nc <- nchar(times)
  minutes <- as.integer(substring(times, nc-1))
  minutes[minutes>59] <- NA
  hour <- as.integer(substring(times, 1, nc-2))
  hour[is.na(hour)] <- as.integer(0)
  hour[hour>23] <- NA
  gc()
  return(cbind(hour,minutes))
}

# This will introduce NA values in non-continental US airports (or any
# airports which we aren't considering).
ToSecondsAfter1970 <- function(x, these, i, air) {
  temp <- gethourmin(x[these,i])
  gc()
  a <- ISOdatetime(x[these,'Year'], x[these,'Month'], x[these,'DayofMonth'],
                   temp[,1], temp[,2], sec=0, tz="")
  thiscol <- 'Dest'
  if (i=='DepTime' || i=='CRSDepTime') thiscol <- 'Origin'
  gc()
  a <- as.integer(a) - as.integer((air[as.character(x[these,thiscol]),'GMT']+5)*360)
    # NOW Eastern Standard Time, in seconds after Jan 1, 1970.
  gc()
  return(a)
}

pb <- txtProgressBar(style=3)
for (day in 1:31) {
  these <- mwhich(x, 'DayofMonth', day, 'eq')
  ans <- foreach(i=c('DepTime', 'CRSDepTime', 'ArrTime', 'CRSArrTime'), .combine=cbind) %dopar% {
    z <- ToSecondsAfter1970(x, these, i, air)
    gc()
    return(z)
  }
  x[these, c('DepTime', 'CRSDepTime', 'ArrTime', 'CRSArrTime')] <- ans
  gc()
  setTxtProgressBar(pb, day/31)
}

###
### Cleaning up now
###

# 1. Missing DepTime (or other times) are for airports that are not in our
# ContinentalUSAirportInfo.txt file.  Check this.

#a <- mwhich(x, 'DepTime', NA, 'eq')
#b <- x[a, 'Origin']
#air[air$airport==72,]         # Lots of missing flights, don't have it.

#
# 2. For JFK and SFO, pull one day's flights and compare the ranges, hopefully
# are off by 3 hours.  Check this.

these <- mwhich(x, c('Year', 'Origin', 'Dest'),
                   list(2005, 29, 2),
                   list('eq', 'eq', 'eq'), op='AND')

a <- x[these,]
a[,'ArrTime'] - a[,'DepTime']
hist(a[,'ArrTime'] - a[,'DepTime'], breaks=100) # 5*60*60

#
# 3. Drop any flights leaving more than 30 minutes early.
#
# 4. Focus on 2008 for starters, look at the upper tail of delays, study for problems.
#
# 5. Create new big.matrix with extra columns for EventType and CurrentTime, where
# CurrentTime will be the same as one of the four times, as defined by EventType, or
# possible a "clock event" without flight into.  This will give us ultimate
# synchronous vs asynchronous flexibility I think.  Sort on CurrentTime.  Use all
# variables in case something else is desired.






