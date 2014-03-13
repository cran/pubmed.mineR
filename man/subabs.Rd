\name{subabs}
\alias{subabs}
\title{To find sub-abstracts}
\description{\code{subabs} will automatically extract the sub-abstracts from  large set of abstracts.}
\usage{
subabs(object, start, end)
}
\arguments{
  \item{object}{An S4 object of class Abstracts}
  \item{start}{integer, specifies starting limit of the range to perform search}
  \item{end}{integer, specifies end limit of the range to perform search}
}
\details{From a large number of asbtracts wish to extract a subset of abstracts into a separate object.}
\value{An R object of class 'Abstracts' containing the extracted abstracts meeting a given range.}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{subabs(x,1,5)}
## Here 'x' is an S4 object of class 'Abstracts', 
## 1 and 5 are the start and end point respectively.
}
\keyword{Function}
