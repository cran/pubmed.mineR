\name{getabsT}
\alias{getabsT}
\title{To get Abstracts for a given term.}
\description{\code{getabsT} will automatically fetch the abstracts containing the query term.}
\usage{
getabsT(object, x, y)
}
\arguments{
  \item{object}{An S4 object of class Abstracts}
  \item{x}{A character string for the term}
  \item{y}{is logical, if set TRUE, search will be case sensitive.}
}
\details{getabsT() is similar to getabs(), but it performs more specific search.}
\value{An object of class 'Abstracts', containing the resulted abstracts for term.}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{getabsT(diabdata, "term")}
}
\keyword{Function}
