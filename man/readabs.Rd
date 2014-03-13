\name{readabs}
\alias{readabs}
\title{To read Abstracts}
\description{\code{readabs} will automatically read the abstracts from the pubmed file.}
\usage{
readabs(x)
}
\arguments{
  \item{x}{Text file of PubMed abstracts. (Abstracts downloaded from PubMed)}
}
\details{The saved file from a general pubmed search as text file is read via readabs().}
\value{An S4 object of class "Abstracts", and a text file with tab delimited headers Journal, Abstract, PMID written with file name "newabs.txt".}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{readabs("pubmed_filename.txt")}
##here x is the text file of abstracts saved from PubMed.
}
\keyword{Function}
