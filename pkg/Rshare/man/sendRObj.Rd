\name{sendRObj}
\alias{sendRObj}
\title{Send an \R object through a tcl socket connection}
\usage{
  sendRObj(obj, sock, block = FALSE, timeout = 10L)
}
\arguments{
  \item{obj}{the \R object to be sent. May be of any type.}

  \item{sock}{a chacter vector containing the Tcl socket
  identifier.}

  \item{block}{logical; whether to block for a response.}

  \item{timeout}{number of seconds to wait for a response
  if \code{block = TRUE}.}
}
\value{
  invisibly returns \code{TRUE} for successful non-blocking
  sends, otherwise returns the response received for
  blocking sends.
}
\description{
  This is a slightly lower-level method than
  \link{sendRshare} to send data through a Tcl socket. It
  requires the user to know the name of the Tcl channel
  through which the data is to be sent.
}
\seealso{
  \code{\link{sendRshare}}
}

