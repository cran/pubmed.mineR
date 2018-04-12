\name{get_MedlinePlus}
\alias{get_MedlinePlus}
\title{
To Get MedLinePlus Summary
}
\description{
This function is to get the summary from MedLinePlus.
}
\usage{
get_MedlinePlus(x)
}
\arguments{
  \item{x}{
\code{x} is a character input of terms: for examples 'malaria', 'pneumonia', 'chronic diseases'
}
}
\value{
It returns a HTML file with name result_Medline_plus.html to be opened with any browser
}
\references{
www.medlineplus.gov, Conuel T. Finding answers in a beauty shop. NIH MedlinePlus: the magazine [Internet]. 2012 Fall [cited 2013 Feb 9]; 7(3):24-26. Available from: https://medlineplus.gov/magazine/issues/fall12/articles/fall12pg24-26.html
}
\author{
S.Ramachandran
}
\examples{
\dontrun{get_MedlinePlus("malaria")}}
\keyword{function}
