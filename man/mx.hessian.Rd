\name{mx.hessian}
\alias{mx.hessian}
\title{Computes the hessian of a maxima expression}
\usage{
  mx.hessian(f, var, to.r = TRUE)
}
\description{
  Computes the hessian of a maxima expression
}

\examples{\dontrun{
  f = "x^2+y^2+x^2*y^2"
  var = c("x","y")
  mx.hessian(f=f,var=var)
}}
