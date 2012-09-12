\name{startRshare}
\alias{startRshare}
\alias{stopRshare}
\title{Start / connect to Rshare}
\usage{
  startRshare(port = 7777)

  stopRshare(port = 7777)
}
\arguments{
  \item{port}{the Rshare port number.}
}
\description{
  This function starts the Rshare process in the current R
  session. If it is the first local R session to start
  Rshare on the specified port, then this R session will
  take the role of the server on that port. Otherwise, it
  will be a client.
}
\author{
  Charlie Friedemann
}

