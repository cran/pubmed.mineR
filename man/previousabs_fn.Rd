\name{previousabs_fn}
\alias{previousabs_fn}
\title{
To Retrieve the Abstracts from the large corpus for given years.
}
\description{
This function is used to extract the abstracts from the large corpus excluding the years and under a given theme. Its output is used in other functions like BWI and genes_BWI
}
\usage{
previousabs_fn(yrs_to_exclude, theme, parentabs)
}
\arguments{
  \item{yrs_to_exclude}{
\code{yrs_to_exclude} is abstracts for the list of years we want to exclude from the corpus
}
  \item{theme}{
\code{theme} is a character value specifying the themes for the Abstracts.
}
  \item{parentabs}{
\code{parentabs} an S4 object containing the Abstracts.
}
}
\value{
It returns an S4 object containing the abstracts of the given year.
}
\author{
S.Ramachandran
}
\seealso{
\code{\link{currentabs_fn}}
}
\examples{
\dontrun{test = previousabs_fn(as.character(2015:2010), "atherosclerosis", diabetesabs}
## here we will get the abstracts before 2010 for 'atherosclerosis'
## from the large corpus diabetesabs.
}
\keyword{ previousabs_fn }
