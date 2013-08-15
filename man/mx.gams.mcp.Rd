\name{mx.gams.mcp}
\alias{mx.gams.mcp}
\title{Generate Kuhn-Tucker-Conditions (usable in a MCP) for a given objective}
\usage{
  mx.gams.mcp(expr, var, fun.name = "fun",
    has.lo = rep(TRUE, length(var)),
    has.up = rep(TRUE, length(var)),
    lo = paste("lo_", var, sep = ""),
    up = paste("up_", var, sep = ""),
    shadow.lo = paste("mu_lo_", var, sep = ""),
    shadow.up = paste("mu_up_", var, sep = ""),
    constr.lo = paste("constr_lo_", var, sep = ""),
    constr.up = paste("constr_up_", var, sep = ""),
    declare = NULL, assume = NULL, simplify = TRUE)
}
\description{
  Generate Kuhn-Tucker-Conditions (usable in a MCP) for a
  given objective
}

