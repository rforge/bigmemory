
setwd("/home/jay/Desktop/BigmemoryProject/bigmemory/airline")

x <- dget("ContinentalUSAirportInfo.txt")
states <- read.table("16096.dat", header=FALSE)
intl <- read.table("16109.dat", header=FALSE)
coast <- read.table("16226.dat", header=TRUE)

plotmap <- function(s=TRUE, i=TRUE, c=TRUE) {
  if (s) lines(states[,1], states[,2])
  if (i) lines(intl[,1], intl[,2])
  if (c) lines(coast[,1], coast[,2])
}

plot(x$lon, x$lat, pch=19, cex=0.5)
plotmap(FALSE, TRUE, TRUE)


