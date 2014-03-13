\name{DAVID_info}
\alias{DAVID_info}
\title{To retreive information from DAVID.}
\description{\code{DAVID_info} function to fetch the information from DAVID (Database for Annotation, Visualization and Integrated Discovery)}
\usage{
DAVID_info(x)
}
\arguments{
  \item{x}{A character string for HGNC approved Gene name.}
}
\details{\code{DAVID_info} takes the gene symbol as input and returns the information about the gene name, its ID and organism name. This function fetches the result from DAVID database.}
\value{A list containing the information of given gene symbol i.e its ID, gene name and organism name respectively.}
\author{Jyoti Sharma}
\examples{
DAVID_info("TLR4")
}
\keyword{Function}
