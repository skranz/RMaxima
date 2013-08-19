\name{mx.is.equal}
\alias{mx.is.equal}
\title{Check whether to expressions ar equal}
\usage{
  mx.is.equal(lhs = NULL, rhs = NULL, eq = NULL)
}
\value{
  a string "true","false" or "unknown"
}
\description{
  Check whether to expressions ar equal
}

\examples{\dontrun{
  eq = "x_1+x_1=x_1"
  mx.is.equal(eq=eq)
  eq = "x_1+x_1=2*x_1"
  mx.is.equal(eq=eq)
  
}}
