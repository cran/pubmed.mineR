\name{removeabs}
\alias{removeabs}
\title{To remove abstracts for the query term.}
\description{\code{removeabs}will remove the abstracts from a corpus for a given term.}
\usage{
removeabs(object, x, y)
}
\arguments{
  \item{object}{An S4 object of class Abstracts}
  \item{x}{A character value}
  \item{y}{is logical, if set 'TRUE' search will be case specific}
}
\details{removeabs() finds the abstracts for the given term and remove them from the large set of abstracts.A text file of file name "dataout.txt" will be written containing the number of abstracts removed.}
\value{An S4 object of class Abstracts and a text file named "dataout.txt"}
\author{S.Ramachandran, Jyoti Rani}
\examples{
\dontrun{removeabs(myabs, "atherosclerosis", TRUE)}
}
\keyword{Function}
