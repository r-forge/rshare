\name{get.Rshare}
\alias{assign.Rshare}
\alias{exists.Rshare}
\alias{get.Rshare}
\alias{ls.Rshare}
\alias{ls.str.Rshare}
\alias{lsf.str.Rshare}
\alias{remove.Rshare}
\alias{rm.Rshare}
\title{Accessing the Rshare shared environment}
\usage{
  get.Rshare(x, port = 7777, timeout = 10L, mode = "any",
    inherits = FALSE)

  assign.Rshare(x, value, port = 7777, inherits = FALSE)

  remove.Rshare(..., list = character(), port = 7777)

  exists.Rshare(x, port = 7777, mode = "any",
    inherits = FALSE)

  ls.Rshare(port = 7777, all.names = FALSE, ...)

  ls.str.Rshare(port = 7777, all.names = FALSE, ...)

  lsf.str.Rshare(port = 7777, all.names = FALSE, ...)
}
\arguments{
  \item{port}{the Rshare port number.}

  \item{timeout}{number of seconds to wait for a response
  from the Rshare server.}

  \item{x}{a variable name (given as a character string).}

  \item{mode}{the mode or type of object sought: see
  \code{\link{get}}.}

  \item{inherits}{should the enclosing frames of the
  environment be searched?}

  \item{value}{a value to be assigned to x.}

  \item{...}{see \code{\link{remove}} or
  \code{\link{lsf.str}}.}

  \item{list}{a character vector naming objects to be
  removed.}

  \item{all.names}{a logical value. If TRUE, all object
  names are returned. If FALSE, names which begin with a .
  are omitted.}
}
\description{
  Access to the Rshare environment is parameterized by the
  port number on which the Rshare server is run. If Rshare
  has not been initialized in the current \R session, these
  functions will just access the local Rshare environment.
  Other than the lack of any \code{envir} argument and the
  addition of \code{port} and \code{timeout} arguments,
  these functions are analogous to their non-Rshare
  counterparts.
}
\examples{
\dontrun{
# start Rshare server on port 7777 (the default)
startRshare()

# in different R session, start Rshare client, and access shared environment
startRshare()
assign.Rshare("mt",mtcars)
ls.Rshare()
get.Rshare("mt")
}
}
\seealso{
  \code{\link{get}}, \code{\link{assign}},
  \code{\link{remove}}, \code{\link{exists}},
  \code{\link{ls}}, \code{\link{ls.str}},
  \code{\link{lsf.str}}
}

