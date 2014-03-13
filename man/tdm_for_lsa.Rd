\name{tdm_for_lsa}
\alias{tdm_for_lsa}
\title{create Term Document Matrix for lsa analysis}
\description{lsa package take "Term Document Matrix" as input, so it is needed to create a 'tdm' for Abstracts and \code{tdm_for_lsa} do the same as it find out the frequency of given term in each abstract and each abstract is considered as separate document. It prepares term document matrix of terms in the 'abstracts' corpus}
\usage{
tdm_for_lsa(object, y)
}
\arguments{
  \item{object}{An S4 object of class 'Abstracts'}
  \item{y}{character vector specifying the terms }
}
\value{
a Term Document Matrix (Numerical matrix) containing the raw frequencies of given terms in each abstract.
}
\author{
Jyoti Sharma
}
\examples{
\dontrun{y = c("insulin", "inflammation", "obesity")
tdm_for_lsa(diab_abs,y)}
}
\keyword{ function }
