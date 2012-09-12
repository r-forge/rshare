\name{registerRshareHook}
\alias{registerRshareHook}
\title{Register an Rshare Hook}
\usage{
  registerRshareHook(objType, hookFunction, port = 7777)
}
\arguments{
  \item{objType}{name of the type/class of the objects
  which will trigger the hook.}

  \item{hookFunction}{the hook function. See details.}

  \item{port}{the Rshare port number.}
}
\value{
  invisibly returns the number of server hooks currently
  registered for objects of type \code{objType}, including
  the hook registered by calling this function.
}
\description{
  Hooks can be used to trigger certain actions when an
  object of a particular type is sent to the Rshare server.
  These hooks are simple, and can be used for a wide
  variety of purposes allowing for flexible interprocess
  communication and control between R sessions. Hooks may
  be created from both Rshare server sessions and client
  sessions.
}
\section{Details}{
  The hook function may have just two formal arguments:
  \code{obj} and optionally, \code{port}. The object sent
  to the Rshare server will be passed to the \code{obj}
  argument and thus should contain all data (other than the
  Rshare port number) required for the hook function. The
  optional \code{port} argument will be a single integer
  specifying the Rshare port number. If \code{port} is not
  an argument to \code{hookFunction} it will not be passed
  to the hook function.
}
\examples{
\dontrun{
# start Rshare server on port 7777 (the default) and register hook
startRshare()
registerRshareHook("printMsg", function(obj, port) cat("Rshare message on port",port,"-",obj$msg,"\\n"))

# in different R session, start Rshare client, and create an object of type 'printMsg'
startRshare()
obj <- list(msg="Hello World!")
class(obj) <- "printMsg"

# send that object to the Rshare server, and watch the server session console
sendRshare(obj)
}
}

