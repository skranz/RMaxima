\name{r.to.mx.matrix}
\alias{r.to.mx.matrix}
\title{An R matrix with individual Maxima strings to a Maxima matrix}
\usage{
  r.to.mx.matrix(mat)
}
\description{
  An R matrix with individual Maxima strings to a Maxima
  matrix
}

\examples{\dontrun{
  mat = matrix(1:8,4,2)
  mat
  r.to.mx.matrix(mat)
  
  
  mx.mat = "matrix([1,5],[2,6],[3,7],[4,8])"
  mx.to.r.matrix(mx.mat)
}}
