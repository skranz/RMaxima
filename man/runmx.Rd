\name{runmx}
\alias{runmx}
\title{Run the maxima code given in str}
\usage{
  runmx(str, clear = TRUE, use.pipe = NULL, show = FALSE,
    time.out = 20,
    answers = rep(c("unknown;", "positive;"), times = 10),
    just.str = FALSE, assume = NULL, declare = NULL)
}
\arguments{
  \item{str}{Character vector with maxima code}

  \item{clear}{if TRUE (default) clears all previous
  assignments by calling kill(all) in Maxima}

  \item{time.out}{number of seconds to wait for result}
}
\value{
  a list with list and character representation of Maxima
  output
}
\description{
  Run the maxima code given in str
}

