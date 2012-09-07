\name{sendRObj}
\alias{sendRObj}
\title{Send an R object through a tcl socket connection}
\usage{
  sendRObj(obj, sock, block = FALSE, timeout = 10L)
}
\arguments{
  \item{obj}{the R object to be sent. May be of any type.}

  \item{sock}{a chacter vector containing the tcl socket
  identifier}

  \item{block}{logial; whether to block for a reponse}

  \item{timeout}{number of seconds to wait for a response
  if \code{block} is \code{TRUE}}
}
\value{
  invisibly returns \code{TRUE} for non-blocking sends,
  otherwise returns the response received for blocking
  sends
}
\description{
  Send an R object through a tcl socket connection
}

