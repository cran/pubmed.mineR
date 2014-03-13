\name{cos_sim_calc_boot}
\alias{cos_sim_calc_boot}
\title{Cosine Similarity Calculation by Boot Strapping}
\description{\code{cos_sim_calc_boot} allows boot strap analysis. This function should be used as argument for 'statistic' in the boot function of 'boot' package.}
\usage{
cos_sim_calc_boot(data, indices)
}
\arguments{
  \item{data}{Term Document Matrix generated from \code{tdm_for_lsa} function of this package. In this matrix, rows are terms and columns are abstracts.}
  \item{indices}{index of matrix.}
}
\details{while calling this function we need to transpose the input tdm and can also set the number of replicates. boot package is required to call this function.}
\value{It will return a matrix containing the cosine similarity of pairs of terms in the abstracts. This object is in same format as returned by the 'boot' function of 'boot' package.}
\author{
Dr.S.Ramachandran
}
\seealso{
\code{\link{tdm_for_lsa}}
}
\examples{
\dontrun{test_boot = boot(data = t(nummatrix), statistic = cos_sim_calc_boot, R = 2)}
## here 'nummatrix' is a Term Document Matrix, boot inbuilt function of boot package, 
## R is number of replicates here it is 2 user can extend this number.
}
\keyword{Function}

