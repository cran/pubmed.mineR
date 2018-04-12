\name{get_NMids}
\alias{get_NMids}
\title{
To extract NM ids from NCBI.
}
\description{
\code{get_NMids} is to fetch the NM ids from the NCBI for corresponding gene/s to further fetch the sequence of that gene/s.
}
\usage{
get_NMids(x)
}
\arguments{
  \item{x}{
\code{x} an R object containing Locus IDs for genes from NCBI2R package.
}
}
\value{
It returns a list object containing corresponding NM id from NCBI.
}
\references{
http://www.ncbi.nlm.nih.gov/gene
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{get_Sequences}}
}
\examples{
\dontrun{getNMids("5950")}
## 5950 is Locus id of RBP4 gene.
}
\keyword{Function}
