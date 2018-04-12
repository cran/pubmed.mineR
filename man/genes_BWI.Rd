\name{genes_BWI}
\alias{genes_BWI}
\title{
Function to obtain the Buzz Word Index of Genes from the abstracts.
}
\description{
This function provides the Buzz word index for each gene. The theme is the context in which the gene is studied for e.g. atherosclerosis. Using this function user can identify abstracts with emphasis on a given gene.
}
\usage{
genes_BWI(currentabs, previousabs, theme, genes)
}
\arguments{
  \item{currentabs}{
\code{currentabs} an S4 object containing the Abtracts for the year we want to study. Output from currentabs_fn()
}
  \item{previousabs}{
\code{previousabs} an S4 object containing the Abstracts for years previous than our year of study. Output from previousabs_fn().
}
  \item{theme}{
\code{theme} a character value to categorize our search. For e.g. 'Atherosclerosis' from 'diabetes' Abstracts.
}
  \item{genes}{
\code{genes} list of genes.
}
}
\value{
It returns a dataframe containig Genes with their corresponding BWI values.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{BWI}}
}
\examples{
\dontrun{test = genes_BWI(currentabs, previousabs, theme, genes)}
## currentabs is an S4 object contaning the Abtracts for the year we want to study.
## previousabs is an S4 object contaning the Abtracts for the years previous 
## than our query year for e.g. before 2015
## theme is a character value specifying the search.
## genes is a character vector of gene symbols.
}
\keyword{ function }

