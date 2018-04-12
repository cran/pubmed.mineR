\name{Yearwise}
\alias{Yearwise}
\title{To Search abstracts Year wise}
\description{\code{Yearwise} reports the no. of abstracts in a year.}
\usage{
Yearwise(object, year)
}
\arguments{
  \item{object}{An S4 object of class Abstracts.}
  \item{year}{a character vector specifies the year.}
}
\details{Yearwise() is useful to find the no. of abstracts for the given year.}
\value{A text file containing the no. of abstracts for given Year(s)}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{Yearwise(myabs, "2011") or
Yearwise(myabs, c("2011", "2013", "2009")}
## Here myabs is the object containing  PubMed abstracts.
}
\keyword{Function}
