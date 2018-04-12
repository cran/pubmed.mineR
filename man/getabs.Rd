\name{getabs}
\alias{getabs}
\title{To get Abstracts for a given term.}
\description{\code{getabs} will automatically fetch the abstracts containing the query term. A base function of the package pubmed.mineR.}
\usage{
getabs(object, x, y)
}
\arguments{
  \item{object}{An S4 object of class Abstracts}
  \item{x}{A character string for the term}
  \item{y}{logical, if TRUE, search will be case sensitive}
}
\details{getabs() is used to find and exctract the abstracts for any given term, from the large a large corpus of abstracts. It uses regexpr based search strategy.}
\value{An S4 object of class 'Abstracts', containing the result abstracts for the given term.}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{getabs(x, "term")}
## x is an S4 obeject of class abstracts containing the abstracts.
}
\keyword{Function}
