setwd("/home/jay/Desktop/Data")
library(bigmemory)
library(biganalytics)

if (FALSE) {
system.time({
x <- read.big.matrix("airline.csv", header=TRUE,
                     backingfile="airlinestreammaster.bin",
                     descriptorfile="airlinestreammaster.desc",
                     type="integer",
                     extraCols=c("EventType", "CurrTime",
                                 "OriginLon", "OriginLat",
                                 "DestLon", "DestLat"))
})
}


xdesc <- dget("airlinestream.desc")
x <- attach.big.matrix(xdesc)
refreshFromMaster <- TRUE

mdesc <- dget("airlinestreammaster.desc")
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
  #cat(i, sum(temp[,1]>23, na.rm=TRUE), "\n")
  #temp[temp[,1]>23,] <- NA
  gc()
  a <- ISOdatetime(x[these,'Year'], x[these,'Month'], x[these,'DayofMonth'],
                   temp[,1], temp[,2], sec=0, tz="")
  thiscol <- 'Dest'
  if (i=='DepTime' || i=='CRSDepTime') thiscol <- 'Origin'
  gc()
  a <- as.integer(a) - as.integer((air[as.character(x[these,thiscol]),'GMT']+5)*3600)
    # NOW Eastern Standard Time, in seconds after Jan 1, 1970.
  gc()
  return(a)
}

system.time({

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

})


###
### Cleaning up now
###

# 1. if ArrTime < DepTime, adjust by 24 hours:

these <- which(x[,'ArrTime'] < x[,'DepTime'])
x[these,'ArrTime'] <- x[these,'ArrTime'] + as.integer(24*3600)

these <- which(x[,'CRSArrTime'] < x[,'CRSDepTime'])
x[these,'CRSArrTime'] <- x[these,'CRSArrTime'] + as.integer(24*3600)

# This might not have taken care of all of them, if some delays were 2 days?
# Odd.



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
                   'eq', op='AND')

a <- x[these,]
hist(a[,'ArrTime'] - a[,'DepTime'], breaks=100) # 5*60*60
plot(a[,'ActualElapsedTime']*60, a[,'ArrTime'] - a[,'DepTime'])
plot(a[,'CRSElapsedTime']*60, a[,'CRSArrTime'] - a[,'CRSDepTime'])
plot(a[,'CRSElapsedTime'], a[,'ActualElapsedTime'])

plot(a[,'CRSArrTime'] - a[,'CRSDepTime'], a[,'ArrTime'] - a[,'DepTime'])
abline(0,1)

hist(a[,'CRSDepTime'] - a[,'DepTime'])
plot(a[,'CRSDepTime'] - a[,'DepTime'], a[,'DepDelay'])

ISOdatetime(a[1,'Year'], a[1,'Month'], a[1,'DayofMonth'], 17, 21, sec=0, tz="")

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
# OH: and originlat, originlon, destlat, destlon
# Will need to multiple lat/lon values by 1000 or something to make them integer.

#################################################################################
# Extract a little subset


air <- dget("/home/jay/Desktop/BigmemoryProject/bigmemory/airline/ContinentalUSAirportInfo.txt")
rownames(air) <- air$airport

these <- mwhich(x, c('Year', 'Month', 'DayofMonth'),
                   list(2005, 1, 2),
                   list('eq', 'eq', 'le'), op='AND')

y <- x[these,]

y <- y[!is.na(y[,'ArrTime']),]
y <- y[!is.na(y[,'DepTime']),]

hist(y[,'ArrTime'] - y[,'DepTime'], breaks=100) # 5*60*60
plot(y[,'ActualElapsedTime']*60, y[,'ArrTime'] - y[,'DepTime'])
sum(y[,'ActualElapsedTime']*60 > y[,'ArrTime'] - y[,'DepTime'])
y <- y[y[,'ActualElapsedTime']*60 == y[,'ArrTime'] - y[,'DepTime'],]
y <- y[y[,'CRSElapsedTime']*60 == y[,'CRSArrTime'] - y[,'CRSDepTime'],]

plot(y[,'CRSElapsedTime']*60, y[,'CRSArrTime'] - y[,'CRSDepTime'])
plot(y[,'CRSElapsedTime'], y[,'ActualElapsedTime'])
abline(0,1)

# Duplicate of previous:
#plot(y[,'CRSArrTime'] - y[,'CRSDepTime'], y[,'ArrTime'] - y[,'DepTime'])
#abline(0,1)

hist(y[,'CRSDepTime'] - y[,'DepTime'])
hist(y[,'CRSArrTime'] - y[,'ArrTime'])
plot(y[,'CRSDepTime'] - y[,'DepTime'], y[,'DepDelay'])

#######################################################################
#######################################################################
# MERGING IN LAT/LON

#y <- attach.big.matrix("airlinestreammaster.desc")
x <- attach.big.matrix("airlinestream.desc")

air <- dget("/home/jay/Desktop/BigmemoryProject/bigmemory/airline/ContinentalUSAirportInfo2.txt")
rownames(air) <- air$airport

these <- mwhich(x, c('Year', 'Month', 'DayofMonth'),
                   list(2005, 1, 2),
                   list('eq', 'eq', 'le'), op='AND')

y <- x[these,]

y <- y[!is.na(y[,'ArrTime']),]
y <- y[!is.na(y[,'DepTime']),]

y <- y[y[,'ActualElapsedTime']*60 == y[,'ArrTime'] - y[,'DepTime'],]
y <- y[y[,'CRSElapsedTime']*60 == y[,'CRSArrTime'] - y[,'CRSDepTime'],]

##################
# Fill in lat/lon:

y[,'OriginLon'] <- air[as.character(y[,'Origin']), 'lon']
y[,'OriginLat'] <- air[as.character(y[,'Origin']), 'lat']
y[,'DestLon'] <- air[as.character(y[,'Dest']), 'lon']
y[,'DestLat'] <- air[as.character(y[,'Dest']), 'lat']

y <- y[!is.na(y[,'OriginLon']) & !is.na(y[,'DestLon']),]

isdup <- duplicated(y)
y <- y[!isdup,]

#######################
# Create event matrix

z <- y                  # z arrival copy
                        # y departure copy
y[,c('ArrTime', 'ActualElapsedTime', 'ArrDelay', 'TaxiIn')] <- NA
y[,'EventType'] <- 1
z[,'EventType'] <- 2
y[,'CurrTime'] <- y[,'DepTime']
z[,'CurrTime'] <- z[,'ArrTime']

y <- rbind(y,z)
y <- y[order(y[,'CurrTime']),]

ticks <- unique(y[,'CurrTime'])
length(ticks)
unique( (ticks - min(ticks)) / 60 )

for (i in c('DepTime', 'ArrTime', 'CurrTime')) {
  y[,i] <- 1 + ( y[,i] - min(ticks) ) / 60
}

ticks <- unique(y[,'CurrTime'])
needthese <- setdiff(1:max(ticks), ticks)
z <- y[1:length(needthese),]
z[,'CurrTime'] <- needthese
z[,'EventType'] <- 3
z[,-c(30:31)] <- NA

y <- rbind(y, z)
y <- y[order(y[,'CurrTime']),]

dput(y, "StreamOfFlights.Jan.1.2.2005NEW.txt")
write.table(y, "StreamOfFlights.Jan.1.2.2005NEW.csv", row.names=F, col.names=T, sep=",")


plot(y[,'OriginLon'], y[,'OriginLat'])








