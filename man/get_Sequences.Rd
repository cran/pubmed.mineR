\name{get_Sequences}
\alias{get_Sequences}
\title{
To extract the Gene sequence from the NCBI.
}
\description{
\code{get_Sequences} is used to fetch the sequences of genes using NM ids.
}
\usage{
get_Sequences(x, filename)
}
\arguments{
  \item{x}{ list of NM Ids of the sequences.

}
  \item{filename}{
\code{filename} sepcifies the name of output file.
}
}
\value{
It will return a text file containing sequences.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{get_NMids}}, ~~~
}
\examples{
\dontrun{get_Sequences(NM_012238.4, "SIRT1_seq.txt")}
}
\keyword{function}

