\name{searchabsT}
\alias{searchabsT}
\title{To Search Abstracts}
\description{\code{searchabsT}It is similar to searchabsL() but performs more specific search. It performs case sensitive search.}
\usage{
searchabsT(object, yr, include, restrict, exclude)
}
\arguments{

\item{object}{An S4 object of class Abstracts}
  \item{yr}{character vector specifies the year(s) of search.}
  \item{include}{character vector specifies the term(s) for which abstracts to be searched.}
  \item{restrict}{character vector specifies the term(s) contained in the abstracts for which search should be restricted.}
  \item{exclude}{character vector specifies the term(s) contained in the abstracts for excluding these abstracts from our search results.}
}
\details{In the arguments except the object all arguments have "NONE" as default. Use sendabs() function to write the results in a tab delimited text file.}
\value{An object of class Abstracts meeting the term and the term combinations. A text file reporting the number of abstracts for the query terms and their combinations is als written with the filename "out.txt".}
\author{Dr.S.Ramachandran}
\seealso{\code{\link{searchabsT}}}
\examples{
\dontrun{searchabsT(x,yr="2013")
searchabsT(x,include="term")
searchabsT(x,restrict="term")
searchabsT(x,exclude="term")
searchabsT(x,yr="2013", include="term")}
## Here x is an S4 object of class Abstracts containing the abstracts to search,
## "term" is the query term to be search. 
}
\keyword{Function}
