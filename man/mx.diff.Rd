\name{mx.diff}
\alias{mx.diff}
\title{Differentiate an expression}
\usage{
  mx.diff(expr, var, times = 1, just.code = FALSE,
    declare = NULL, assume = NULL, just.str = TRUE)
}
\description{
  Differentiate an expression
}

\examples{\dontrun{
  mx.diff("x^3+y","x",1)
  mx.diff("(a-b*q)*q-c*q-c2*q^2","q",1)
  mx.diff("(a-b*q)*q-c*q-c2*q^2","q",2)
  
}}
