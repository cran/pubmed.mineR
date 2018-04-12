\name{get_original_term2}
\alias{get_original_term2}
\title{
To get the original terms from the corpus.
}
\description{
\code{get_original_term2} is used to get the exact term as it is present in corpus. It takes one term at a time. For multiple terms we can use lapply.
}
\usage{
get_original_term2(x, y)
}
\arguments{
  \item{x}{
\code{x} is a character value specifying the query term.
}
  \item{y}{
\code{y} is an S4 object containing abstracts.
}
}
\value{
It returns a list object containing accurate term.
}
\author{
Jyoti Rani, S.Ramachandran.
}
\seealso{
\code{\link{get_original_term}}
}
\examples{
\dontrun{test = get_original_term("hba1c", diababs)}
## here it will return accurate formation of hba1c i.e. HbA1c from diababs.
}

\keyword{ get_original_term2 }

