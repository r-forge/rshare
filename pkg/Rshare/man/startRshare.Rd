\name{startRshare}
\alias{startRshare}
\alias{stopRshare}
\title{Start / connect to Rshare}
\usage{
  startRshare(port = 7777, server.only = FALSE,
    verbose = TRUE)

  stopRshare(port = 7777, verbose = TRUE)
}
\arguments{
  \item{port}{the Rshare port number.}

  \item{server.only}{only start in server mode, throwing an
  error if unable. Default \code{FALSE}.}

  \item{verbose}{option to print startup / shutdown
  messages. Default \code{TRUE}.}
}
\description{
  This function starts the Rshare process in the current \R
  session. If it is the first local \R session to start
  Rshare on the specified port, then this \R session will
  take the role of the server on that port. Otherwise, it
  will be a client.
}

