\name{assign.Rshare}
\alias{assign.Rshare}
\alias{exists.Rshare}
\alias{get.Rshare}
\alias{ls.Rshare}
\alias{ls.str.Rshare}
\alias{lsf.str.Rshare}
\alias{remove.Rshare}
\title{Accessing the Rshare shared environment}
\usage{
  assign.Rshare(x, value, port = 7777, inherits = FALSE)

  get.Rshare(x, port = 7777, timeout = 10L, mode = "any",
    inherits = FALSE)

  remove.Rshare(..., list = character(), port = 7777)

  exists.Rshare(x, port = 7777, mode = "any",
    inherits = FALSE)

  ls.Rshare(port = 7777, all.names = FALSE, ...)

  ls.str.Rshare(port = 7777, all.names = FALSE, ...)

  lsf.str.Rshare(port = 7777, all.names = FALSE, ...)
}
\description{
  Access to the Rshare environment is parameterized by a
  specific port number. Otherwise, the usual R functions
  such as \code{assign}, \code{get},
}

