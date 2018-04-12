\name{subsetabs}
\alias{subsetabs}
\title{
To make subsets of large corpus.
}
\description{
It is used to divide the large corpus into a given range. 
}
\usage{
subsetabs(object, indices)
}
\arguments{
  \item{object}{
\code{object} is an S4 object containing Abstracts.
}
  \item{indices}{
\code{indices} a numeric range (e.g. 1:10, c(1,5,7,9,10)). 
}
}
\value{
It returns an S4 obejct of extracted Abstracts.
}
\author{
S. Ramachandran.
}
\examples{
\dontrun{test = subsetabs(diabetesabs, 1:50)}
## here we want to extract the Abstacts ranges from 1 to 50 
## from the large corpus of diabetes. 
}
\keyword{ subsetabs }

