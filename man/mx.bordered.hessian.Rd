\name{mx.bordered.hessian}
\alias{mx.bordered.hessian}
\title{Computes the bordered hessian matrix, used for concavity checks}
\usage{
  mx.bordered.hessian(f, var, to.r = TRUE, jacobian = NULL,
    hessian = NULL)
}
\description{
  Computes the bordered hessian matrix, used for concavity
  checks
}

\examples{\dontrun{
  f = "x^2+y^2+x^2*y^2"
  var = c("x","y")
  mat = mx.bordered.hessian(f=f,var=var)
  mat
  mx.determinant(mat)
}}
