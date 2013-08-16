\name{replace.maxima.var}
\alias{replace.maxima.var}
\title{Replace in a maxima expression string variables org by new}
\usage{
  replace.maxima.var(str, org, new)
}
\description{
  Replace in a maxima expression string variables org by
  new
}

\examples{\dontrun{
  replace.maxima.var("diff([x^2-y],[x])",c("x","y"),c("alpha","beta"))  
}}
