library(XML)
# library(esperr)

getEventString=function( xmlString, fieldName ) {
  doc = xmlParse(xmlString, asText=TRUE)
  return(unlist(xpathApply(doc, "//IsTick", xmlValue)))
}

FlightCallback <- function(flightEvent) {
  if (getEventString(flightEvent, 'IsTick') == TRUE) {
    print("No new flights, update the graph")
  } else {
    print("New flights, update the graph")
  }
  
}

# The following code will be replaced with esper.
readCon <- make.socket(port=3333)

while (1) {
  nextEvent <- read.socket(readCon)
  FlightCallback(nextEvent)
}

