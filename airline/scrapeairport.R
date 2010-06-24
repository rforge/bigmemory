
if (FALSE) {

  x <- scan('http://www.world-airport-codes.com/search/?Submit=1&criteria=United+States&searchWhat=countryname', what="", sep="\n", encoding="latin1")

  x <- x[grep("United States", x)]

  y <- substring(x, regexpr("United States", x), regexpr("html", x)+3)
  y <- y[y!=""]
  y <- gsub("United States", "united-states", y)

  baseurl <- 'http://www.world-airport-codes.com/'

  ans <- matrix("", 1, 4)
  for (i in 1:length(y)) {

    thisurl <- paste(baseurl, y[i], sep="")

    z <- scan(thisurl, sep="\n", what="", encoding="latin1")
    z <- z[grep("detail", z)]
    z <- gsub("<[^<>]*>", "", z)
    z <- gsub(" +: ", "", z)

    code <- z[grep("Airport Code", z)+1]
    lon <- z[grep("Longitude", z)+1]
    lat <- z[grep("Latitude", z)+1]
    gmtoffset <- z[grep("GMT Offset", z)+1]

    ans <- rbind(ans, c(code, lon, lat, gmtoffset))
  }

  dput(ans, "airportinfo.txt")

}

##################################################################

x <- dget("airportinfo.txt")
x <- x[-1,]
x[,4] <- gsub(" (?)", "", x[,4], fixed=TRUE)

x <- as.data.frame(x, stringsAsFactors=FALSE)
names(x) <- c("code", "rawlon", "rawlat", "GMToffset")

x$GMToffset <- as.numeric(x$GMToffset)
x$GMToffset <- -1*abs(x$GMT)

for (col in 2:3) {
  x[,col] <- gsub("#176;", "", x[,col])
  x[,col] <- gsub("#8217;", "", x[,col])
  x[,col] <- gsub("&#.*$", "", x[,col])
  x[grep("Unknown", x[,col]),col] <- "NA & NA & NA"
  a <- matrix(as.numeric(unlist(strsplit(x[,col], "&"))), ncol=3, byrow=TRUE)
  if (col==3) x$lat <- a[,1] + (a[,2] + a[,3]/60)/60
  if (col==2) x$lon <- -1*(a[,1] + (a[,2] + a[,3]/60)/60)

}

x <- x[!is.na(x$lon),]
x <- x[x$lon > -130 & x$lon < -65 & x$lat > 23 & x$lat < 50,]

codes <- read.csv("AirportCodes.csv", as.is=TRUE)
codes <- codes[-1,]

x <- x[x$code %in% codes[,1],]
codes <- codes[codes[,1] %in% x$code,]
x <- x[order(x$code),]
codes <- codes[order(codes[,1]),]
x$index <- codes$index
x <- x[,-c(2,3)]
x$GMToffset[x$GMT == 0] <- -5            # Fix up CBM
#x <- x[,-1]

names(x) <- c("code", "GMT", "lon", "lat", "airport")
x <- x[,c(1,5,2,3,4)]
x <- x[order(x$airport),]
rownames(x) <- NULL

dput(x, "ContinentalUSAirportInfo2.txt")# We think we were correct.

#######################################################

x <- dget("ContinentalUSAirportInfo2.txt")
states <- read.table("16096.dat", header=FALSE)
intl <- read.table("16109.dat", header=FALSE)
coast <- read.table("16226.dat", header=TRUE)

plotmap <- function(s=TRUE, i=TRUE, c=TRUE) {
  if (s) lines(states[,1], states[,2])
  if (i) lines(intl[,1], intl[,2])
  if (c) lines(coast[,1], coast[,2])
}

plot(x$lon, x$lat, pch="")
text(x$lon, x$lat, x$code)
plotmap(FALSE, TRUE, TRUE)


















