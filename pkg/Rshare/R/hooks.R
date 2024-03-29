#   Rshare: the shared environment package
#
#   Copyright (C) 2012 Charlie Friedemann (cfriedem at gmail dot com)
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Register an Rshare Hook
#'
#' Hooks can be used to trigger certain actions when an object of a particular type is sent to the Rshare server. 
#' These hooks are simple, and can be used for a wide variety of purposes allowing for flexible interprocess communication and control between \R sessions.
#' Hooks may be created from both Rshare server sessions and client sessions using this function.
#'
#' @section Details: The hook function may have just two formal arguments: \code{obj} and optionally, \code{port}. 
#' The object sent to the Rshare server will be passed to the \code{obj} argument and thus should contain all data (other than the Rshare port number) required for the hook function. 
#' The optional \code{port} argument will be a single integer specifying the Rshare port number. 
#' If \code{port} is not an argument to \code{hookFunction} it will not be passed to the hook function.
#'
#' @param objType name of the type/class of the objects which will trigger the hook.
#' @param hookFunction the hook function. See details.
#' @param port the Rshare port number.
#' @param doResponse logical; whether server hook returns a response to client when executed. Default is \code{FALSE}.
#' @param overwriteExisting logical; should existing \code{objType} hook be overwritten? Default is \code{FALSE}.
#' @return invisibly returns a logical indicating whether hook registration was a success or not.
#' @examples \dontrun{ 
#' # Start Rshare server on port 7777 (the default) and register hook
#' startRshare()
#' registerRshareHook("printMsg", function(obj, port) cat("Rshare message on port",port,"-",obj$msg,"\n")) 
#' 
#' # In a different R session, start Rshare client, and create an object of type 'printMsg'
#' startRshare()
#' obj <- list(msg="Hello World!")
#' class(obj) <- "printMsg"
#'
#' # Send that object to the Rshare server, and watch the server session console
#' sendRshare(obj)
#' }
#' @export
registerRshareHook <- function(objType, hookFunction, port = 7777, doResponse = FALSE, overwriteExisting = FALSE) {
	if (!is.character(objType)) stop("objType must be of type 'character'")
	if (!identical(length(objType),1L)) {
		objType <- objType[1]
		warning("objType had multiple elements, only first element will be used")
	}
	if (!is.function(hookFunction)) stop("hookFunction must be a function")
	hookFunction <- match.fun(hookFunction)
	
	# check hookFunction formals
	hookFormals <- formals(hookFunction)
	if (!"obj" %in% names(hookFormals)) stop("hookFunction must have an 'obj' argument")
	
	status <- getStatus(port)
	
	if (identical(status,"closed")) stop(paste("Rshare must be running on port",port,"in order to register a hook"))
	if (identical(status,"client")) {
		# send hook object to server and wait for response, much like assign.Rshare
		req <- RshareAddHookReq(objType=objType, hookFunction=hookFunction, port=port, doResponse=doResponse, overwriteExisting=overwriteExisting)
		sock <- getClientSocketId(port)
		res <- sendRObj(req, sock, block=TRUE)
		if (!isTRUE(res)) stop(paste("'",objType,"' hook already exists on server. Set overwriteExisting=TRUE to overwrite",sep=""))
	} else { # server
		hooks <- get.Rshare(".hooks",port=port)
		if (is.null(hooks)) hooks <- list()
		
		objHook <- hooks[[objType]]
		if (!is.null(objHook) && !isTRUE(overwriteExisting)) { # hook exists already, don't overwrite
			res <- FALSE
		} else {
			objHook <- list(hookFunction=hookFunction, doResponse=doResponse)
			hooks[[objType]] <- objHook
			assign.Rshare(".hooks",hooks,port=port)
			res <- TRUE
		}
	}
	invisible(res)
}