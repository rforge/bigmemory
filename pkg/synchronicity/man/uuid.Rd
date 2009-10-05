\name{uuid}
\Rdversion{1.1}
\alias{uuid}
\title{
Create a universal unique identifier.
}
\description{
This function creates an identifier that will be (with high probability)
unique on a single machine or group of machines
}
\usage{
uuid()
}
\details{
The functions uses the boost uuid functionality.
}
\value{
A unique string.
}
\references{
http://www.boostpro.com/vault/
}
\author{
Michael Kane
}
\examples{
print(uuid())
print(uuid())
}
