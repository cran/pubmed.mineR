\name{cleanabs}
\alias{cleanabs}
\title{To clean the result of searchabsL}
\description{It will remove the 'NONE' abstracts from the result of searchabsL.}
\usage{
cleanabs(object)
}
\arguments{
  \item{object}{an S4 object of class Abstracts.}
}
\value{an S4 object of class Abstracts.}
\author{Jyoti Sharma}
\seealso{
\code{\link{searchabsL}}
}
\examples{\dontrun{test1 = searchabsL(abs, include=c("term1", "term2"))
test2 = cleanabs(test1)}
## here 'abs' is an S4 object of class Abstracts
## 'term1', 'term2' are the searchterms
## test1 is an S4 object containing abstracts for given terms
## and test2 is an S4 object of class Abstracts containing clean abstracts of searchabsL}
\keyword{Function}
