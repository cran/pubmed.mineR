\name{cos_sim_calc}
\alias{cos_sim_calc}
\title{To calculate the cosine similarity between terms.}
\description{
\code{cos_sim_calc} calculates the cosine measure of similarity between pairs of terms from corpus.}
\usage{
cos_sim_calc(nummatrix)
}
\arguments{
  \item{nummatrix}{A numerical matrix for e.g. a Term Document matrix (output from tdm_for_lsa)}
}
\details{The term document matrix is taken as input and cosine meausures of similarity between all pairs of terms are calculated.}
\value{An R object and a tab delimited text file containing the similarity values between all pairs of terms.}
\author{S. Ramachandran}
\note{This file can be input to cytoscape directly.}
\seealso{\code{\link{tdm_for_lsa}}}
\examples{
\dontrun{x = cos_sim_calc(nummatrix)}
## here nummatrix is the 'Term Document Matrix' generated from tdm_for_lsa()}
\keyword{Function}
