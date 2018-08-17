\name{readabsnew}
\alias{readabsnew}
\title{To read Abstracts}
\description{\code{readabsnew} will automatically read the abstracts from the pubmed text file.}
\usage{
readabsnew(x)
}
\arguments{
  \item{x}{Text file of PubMed abstracts. (Abstracts downloaded from PubMed)}
}
\details{The saved file from a general pubmed search as text file is read via readabsnew().}
\value{An S4 object of class "Abstracts" and a text file with tab delimited headers Journal, Abstract, PMID written with file name "newabs.txt".}
\author{S.Ramachandran}
\examples{
\dontrun{readabsnew("pubmed_result.txt")}
##here pubmed_result.txt is the text file of abstracts saved from PubMed.
}
\keyword{Function}
