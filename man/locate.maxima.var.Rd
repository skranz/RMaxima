\name{locate.maxima.var}
\alias{locate.maxima.var}
\title{Locate positions (a stringtools pos) of all variables in a maxima expression string}
\usage{
  locate.maxima.var(str, var = extract.maxima.var(str))
}
\description{
  Locate positions (a stringtools pos) of all variables in
  a maxima expression string
}

\examples{\dontrun{
  locate.maxima.var("diff([x^2-y],[x])")  
}}
