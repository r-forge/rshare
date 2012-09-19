\docType{package}
\name{Rshare}
\alias{Rshare}
\alias{Rshare-package}
\title{Rshare: the shared environment package}
\description{
  This is the Rshare package. It is used to create a shared
  environment available to multiple \R sessions. The
  environment is hosted and served by one \R session, and
  other \R sessions may subscribe to this environment as
  clients. Environments are parameterized by a port number.
  Objects may be written to and read from the shared
  environment. For an overview of the functions used to
  access the shared environment, see See
  \code{\link{get.Rshare}}\cr \subsection{Server-side
  Hooks}{ In addition, a simple method of registering
  server-side hooks allows users to create custom
  functionality. These hooks work by dispatching certain
  types of objects received by the server to user-defined
  functions. User-defined functions may do anything, from
  displaying a message in the console to running an
  analysis and returning customized data. See
  \code{\link{registerRshareHook}} for more information. }
}
\section{Implementation}{
  The package works by utilizing Tcl sockets to create a
  lightweight framework for different \R sessions to share
  data. It is written with a mixture of \R and Tcl code to
  allow for portability across mutliple operating systems.
  Be warned, the package is far from complete and
  functionality may change in the future to address current
  limitations.
}

\section{Details}{
  \tabular{ll}{ Package: \tab Rshare\cr Version: \tab
  1.0\cr License: \tab GPL Version 3 or later.\cr }
}
\author{
  Charlie Friedemann
}

