\name{get_DOIs}
\alias{get_DOIs}
\title{
function for extracting Digital Object Identifier (DOIs) of papers
}
\description{
\code{get_DOIs} is used to extract DOIs of papers. 
}
\usage{
get_DOIs(abs)
}
\arguments{
  \item{abs}{
An S4 object of class Abstracts
}
}
\details{
\code{get_DOIs} allow users to get DOIs for individual papers.
}
\value{
It returns a list object containing DOIs. This is useful for further extraction of papers 
}
\author{
S.Ramachandran
}
\examples{
\dontrun{test = get_DOIs(vitiligoabs)}
## 
}
\keyword{function}
