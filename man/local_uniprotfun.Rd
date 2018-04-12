\name{local_uniprotfun}
\alias{local_uniprotfun}
\title{
To Get Information from Uniprot.
}
\description{
It is an auxiliary function for altnamesfun.
}
\usage{
local_uniprotfun(y)
}
\arguments{
  \item{y}{
\code{y} a character value containing HGNC Gene symbol
}
}

\value{
It writes an output file named "x.txt" which will be used as input in altnamesfun().
}
\author{
S.Ramachandran, Jyoti Rani
}
\seealso{
\code{\link{uniprotfun}}
}
\examples{
\dontrun{local_uniprotfun("TLR4")}
## here it will generate an output file named "x.txt" containing 
## result for TLR4.
}
\keyword{local_uniprot_fun}

