\name{searchabsL}
\alias{searchabsL}
\title{To Search the abstracts of term(s) in a combination mode.}
\description{\code{searchabsL} will search for abstracts for the given term(s). Multiple combinations are allowed.}
\usage{
searchabsL(object, yr, include, restrict, exclude)
}
\arguments{
  \item{object}{An S4 object of class Abstracts}
  \item{yr}{character vector specifies the year of search}
  \item{include}{character vector specifies the terms contained in the abstracts.}
  \item{restrict}{character vector specifies the term contained in the abstracts for which search should be restricted.}
  \item{exclude}{character vector specifies the terms contained in the abstracts for excluding these abstracts from the search results.}
}
\details{In the arguments except for the object all other arguments have "NONE" as default. To export or write the result of searchabsL() we use sendabs() function.}
\value{An object of class Abstracts satisfying the term combinations, In addition a text file named "out.txt" reporting the number of abstracts for given query term combinations.}
\author{Dr.S.Ramachandran}
\seealso{\code{\link{searchabsT}}}
\examples{
\dontrun{searchabsL(x, include="term")
searchabsL(x, yr="2013")
searchabsL(x, restrict="term")
searchabsL(x, exclude="term")
searchabsL(x, include="term", exclude="term2")}
## Here x is the object of class Abstracts containing data,
## "term" is the query term to be search.
}
\keyword{Function}
