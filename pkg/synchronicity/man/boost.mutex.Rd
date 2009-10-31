\name{boost.mutex}
\alias{boost.mutex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
boost.mutex(resourceName = NULL, timeout = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resourceName}{ ~~Describe \code{resourceName} here~~ }
  \item{timeout}{ ~~Describe \code{timeout} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(resourceName=NULL, timeout=NULL)
{
  isRead = TRUE
  if (is.null(resourceName)) 
  {
    resourceName = uuid()
  }
  if (!is.null(timeout) && !is.numeric(timeout))
  {
    stop("The timeout parameter must be numeric.")
  }
  if (!is.null(timeout) && timeout <= 0)
  {
    stop("You must specify a timeout greater than zero.")
  }
  mutexInfoAddr=.Call('CreateBoostMutexInfo', resourceName, as.double(timeout))
  return(new('boost.mutex', isRead=isRead, mutexInfoAddr=mutexInfoAddr))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
