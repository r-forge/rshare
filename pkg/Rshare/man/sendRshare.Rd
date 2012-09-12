\name{sendRshare}
\alias{sendRshare}
\title{Send an R object to Rshare server}
\usage{
  sendRshare(obj, port = 7777, block = FALSE,
    timeout = 10L)
}
\arguments{
  \item{obj}{the R object to be sent. May be of any type.}

  \item{port}{the Rshare port number.}

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
  This function is to be used by Rshare clients to send an
  object to the server. The user has the option to block
  for a response with a corresponding timeout.
}
\seealso{
  \code{\link{sendRObj}}
}

