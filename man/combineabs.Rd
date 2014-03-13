\name{combineabs}
\alias{combineabs}
\title{To combine the abstracts}
\description{\code{combineabs} will automatically combine two abtracts of two objects.}
\usage{
combineabs(object1, object2)
}
\arguments{
  \item{object1}{An S4 object of class Abstracts}
  \item{object2}{An S4 object of class Abstracts}
}
\details{Two objects of class 'Abstracts' are combined to return non-redundant combined abstracts. It can be used sequentially to combine many objects of class 'Abstracts'.
It will also write the number of combined abstracts into a text file named "data_out.txt"}
\value{ An R object containing the combined abstracts, and a text file named "data_out.txt" containing the number of abstracts combined together }
\author{Dr.S.Ramachandran}
\examples{
\dontrun{res1 = combineabs(x,y)}
## here 'x', 'y' are the S4 objects of class 'Abstracts'.}
\keyword{Function}
