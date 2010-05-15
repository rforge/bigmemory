
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

x <- dget("airportinfo.txt")
x <- x[-1,]
x[,4] <- gsub(" (?)", "", x[,4], fixed=TRUE)


