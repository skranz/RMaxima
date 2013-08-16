\name{mx.quasi.concave.cond}
\alias{mx.quasi.concave.cond}
\title{Returns the conditions from the principal minor test
that the function f is jointly quasi-concave in in all variables in var}
\usage{
  mx.quasi.concave.cond(f, var, with.descr = FALSE,
    jacobian = NULL, hessian = NULL)
}
\arguments{
  \item{f}{a string that specifies the Maxima formula of
  the function body}

  \item{var}{a character vector specifying the variables}
}
\description{
  Returns the conditions from the principal minor test that
  the function f is jointly quasi-concave in in all
  variables in var
}

\examples{\dontrun{
  f = "x^(1/2)*y^(1/2)"
  var = c("x","y")
  mx.quasi.concave.cond(f,var)

  f = "x^2+y^(1/2)"
  f = "(x^2)*(y^(1/2))"
  var = c("x","y")
  ret = mx.quasi.concave.cond(f,var)
  ret
  mx.sign(cond,assume="x>=0,y>=0")

  f = "(x^2)*(y^(1/2))"
  f = "x^(1/2)*y^(1/2)"
  
  mx.check.concavity(f,var,assume="x>0,y>0")
  
  pi.i =  "(a*(qi+Q_i)^(-b))*qi-c1*qi"
  dpi.i = mx.diff(pi.i,"qi")
  ddpi.i = mx.diff(dpi.i,"qi")
  
  
  mx.check.concavity(pi.i,"qi", assume="a>0,b>0,b<1,c1>0,Q_i>0,qi>0")
  
}}
