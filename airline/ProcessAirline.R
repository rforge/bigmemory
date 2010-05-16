
library(bigmemory)

setwd("/home/jay/Desktop/Data")
xdesc <- dget("airline.desc")
x <- attach.big.matrix(xdesc)

mdesc <- dget("airline.master.desc")
m <- attach.big.matrix(mdesc)

#########################################################################
# Preprocessing takes care of four non-integer variables in the raw data.
# We'll handle a time conversion problem in R (see below).

cbind(m[1:10,'DepTime'],
      x[1:10,'DepTime'])

if (any(m[1:1000,'DepTime']==x[1:1000,'DepTime'], na.rm=TRUE)) {
  stop("NEED TO DO TIME PROCESSING, x and m times are identical.")
} else {
  cat("NOTICE: TIME PROCESSING HAS BEEN COMPLETE: go ahead and use x.\n")
}

ToMinutesAfterMidnight <- function(times) {
  times <- as.character(times)
  nc <- nchar(times)
  minutes <- as.integer(substring(times, nc-1))
  hour <- as.integer(substring(times, 1, nc-2))
  hour[is.na(hour)] <- as.integer(0)
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






