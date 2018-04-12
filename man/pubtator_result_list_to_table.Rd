\name{pubtator_result_list_to_table}
\alias{pubtator_result_list_to_table}
\title{
Function to Convert Pubtator result from list into Table
}
\description{
This function is used to collect the outputs of pubtator_function() after using lapply over multiple PMIDs. This function enables to convert it into table for easy reading and further analysis. 
}
\usage{
pubtator_result_list_to_table(x)
}
\arguments{
  \item{x}{
here x is list output of pubtator_function().
}
}
\value{
It returns table for pubtator_function output.
}
\author{
S.Ramachandran, Jyoti Rani
}
\seealso{
\code{\link{pubtator_function}}
}
\examples{
\dontrun{test = pubtator_result_list_to_table(x)}
##here x is the output of pubtator_function
}
\keyword{ pubtator_result_list_to_table }
