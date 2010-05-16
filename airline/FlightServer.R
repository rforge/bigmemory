# R's server sockets cant tell if a client has disconnected.  So,
# each time there is a new session, we'll have to restart the server.

sendCon <- make.socket(port=3333, server=TRUE)

# Attach to the airline data set and other required data.

updateSpan <- 5 # Update (at most) every 5 seconds.

# If no new flights occur in this time period we still have to send the
# client a tick.
tickString <- paste('<?xml version="1.0" encoding="UTF-8"?>',
# The next line will be uncommented when esperr is used.
#  '<Flight xmlns="FlightSchema">',
  '<Flight>',
  '<IsTick>TRUE</IsTick>',
  '</Flight>')

while (1) {
  startTime <- Sys.time()

  # Find the events to send
  # create the xmlEvent strings to send to the client
  xmlEventStrings <- list()
  if (length(xmlEventStrings) == 0) {
    # No new events, send a tick instead.
    write.socket(sendCon, tickString)
  } else {
    for (event in xmlEventStrings)
    {
      write.socket(sendCon, event)    
    }
  }
  timeLeft = updateSpan - as.numeric(difftime(Sys.time(), startTime, 
    units='secs'))
  if (timeLeft > 0) Sys.sleep(timeLeft)
  
}
