\name{mx.jacobian}
\alias{mx.jacobian}
\title{Computes the jacobian of a maxima expression}
\usage{
  mx.jacobian(f, var, to.r = TRUE)
}
\description{
  Computes the jacobian of a maxima expression
}

\examples{\dontrun{
  f = "x^2+y^2+x^2*y^2"
  var = c("x","y")
  mx.jacobian(f=f,var=var)
}}
