\name{getPortEnv}
\alias{getPortEnv}
\title{Fetch a local Rshare environment, creating it if necessary}
\usage{
  getPortEnv(port)
}
\arguments{
  \item{port}{the Rshare port number (\code{NULL})}
}
\value{
  the requested Rshare environment
}
\description{
  This function returns the specific local Rshare
  environment corresponding to the port number provided. If
  \code{port} is \code{NULL}, then the main local Rshare
  environment is returned. All the local port-specific
  Rshare environments are enclosed by the main Rshare
  environment.
}
\details{
  When this function is called, it first checks the search
  path for the ".Rshare" environment. If it cannot be
  found, it is created and added to the search path. If
  \code{port} was provided, it will then try to find the
  port-specific Rshare environment enclosed by the main
  ".Rshare" environment. If this port-specific environment
  is not found, it will be created before it is returned.
}

