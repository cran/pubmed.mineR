\name{head_abbrev}
\alias{head_abbrev}
\title{
To extract the abbreviated term.
}
\description{
\code{head_abbrev} is used to find expansion for which abbreviation is used. 
It will help to find the falsely matching abbreviations from the abstracts.
}
\usage{
head_abbrev(limits, term, pmid, abs)
}
\arguments{
  \item{limits}{
\code{limits} specifies the limit up to which expansion should be displayed. Default is 50
}
  \item{term}{
\code{term} is the query term (abbreviation)
}
  \item{pmid}{
\code{pmid} describes the PMID 
}
  \item{abs}{
\code{abs}an S4 object of class Abstracts.
}
}
\value{
It will return a list.
}
\author{
S.Ramachandran
}
\examples{
\dontrun{head_abbrev(50, "AR", "16893912", myabs)}
}
\keyword{function}
