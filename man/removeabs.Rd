\name{removeabs}
\alias{removeabs}
\title{To remove abstracts for the query term.}
\description{\code{removeabs}will report the number of abstracts removed for the given query term.}
\usage{
removeabs(object, x, y)
}
\arguments{
  \item{object}{An S4 object of class Abstracts}
  \item{x}{A character string for the Term}
  \item{y}{is logocal, if set 'TRUE' search will be case specific}
}
\details{removeabs() finds the abstracts for the given term and remove them from the large set of abstracts.A text file of file name "dataout.txt" will be written containing the number of abstracts removed.}
\value{An S4 object of class Abstracts and a text file named "dataout.txt"}
\author{Dr.S.Ramachandran}
\examples{
\dontrun{removeabs(x, "term", TRUE)}
}
\keyword{Function}
