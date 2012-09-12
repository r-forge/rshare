\name{getStatus}
\alias{getStatus}
\title{Get Rshare status on a particular port}
\usage{
  getStatus(port)
}
\arguments{
  \item{port}{the Rshare port number.}
}
\value{
  one of \code{"server"}, \code{"client"} or
  \code{"closed"} indicating Rshare status.
}
\description{
  This function provides the user with the Rshare status on
  a certain port in the currently running R process. It is
  useful for determining whether the Rshare process is
  running on a particular port as well as whether it is a
  server or client.
}

