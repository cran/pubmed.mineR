\name{currentabs_fn}
\alias{currentabs_fn}
\title{
To Retrive the Abstracts for year.
}
\description{
This function is used to extract the abstracts for year we want to study. Its output is used as input in other functions like BWI() and genes_BWI()
}
\usage{
currentabs_fn(yr_to_include, theme, parentabs)
}

\arguments{
  \item{yr_to_include}{
\code{yr_to_include} is the year for which we want to extract the Abstracts.
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
\code{\link{previousabs_fn}}
}
\examples{
\dontrun{test = currentabs_fn("2015", "atherosclerosis", diabetesabs)}
## here "2015" is the year for which, we wish to extract the abstracts on theme"Atherosclerosis" 
## from the large corpus of diabetes i.e. diabetesabs.
}
\keyword{ currentabs_fn }
