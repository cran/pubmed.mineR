\name{get_PMCtable}
\alias{get_PMCtable}
\title{
To fetch the given PMC article tables. Deprecated
}
\description{
\code{get_PMCtable} is used to extract the full texr article by giving query PMC Id. Deprecated.
}
\usage{
get_PMCtable(url)
}
\arguments{
  \item{url}{
\code{url} is url of query PMC Id.
}
}
\value{
It will return a full text artcle.
}
\references{
http://www.ncbi.nlm.nih.gov/pmc/
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{get_PMCIDS}}
}
\examples{
\dontrun{get_PMCtable("http://www.ncbi.nlm.nih.gov/pmc/?term=4039032")}
}
\keyword{function}

