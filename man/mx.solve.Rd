\name{mx.solve}
\alias{mx.solve}
\title{Solve eq for variables var}
\usage{
  mx.solve(eq, var, radcan = !TRUE, to_poly_solve = !TRUE,
    semi.colon = ";", just.code = FALSE, declare = NULL,
    assume = NULL, real.only = TRUE)
}
\description{
  Solve eq for variables var
}

\examples{\dontrun{
  
  runmx("solve([y=2*x,x=y^2],[x,y])")
  eq = c("y=2*x","x=y^2")
  var = c("x","y")
  mx.solve(eq,var)
}}
