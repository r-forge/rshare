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

#' Fetch a local Rshare environment, creating it if necessary
#'
#' This function returns the specific local Rshare environment corresponding to the port number provided. If \code{port} is \code{NULL}, then
#' the main local Rshare environment is returned. All the local port-specific Rshare environments are enclosed by the main Rshare environment. 
#'
#' When this function is called, it first checks the search path for the ".Rshare" environment. If it cannot be found, it is created and added to 
#' the search path. If \code{port} was provided, it will then try to find the port-specific Rshare environment enclosed by the main ".Rshare" environment. 
#' If this port-specific environment is not found, it will be created before it is returned.
#'
#' @param port the Rshare port number (\code{NULL})
#' @return the requested Rshare environment
#' @export
# RshareEnv <- function(port = NULL) {
	# pos <- match(".Rshare", search())
	# if (is.na(pos)) {
		# attach(list(), pos = length(search()) - 1,name=".Rshare")
		# pos <- match(".Rshare", search())
	# }
	# if (is.null(port)) {
		# return(pos.to.env(pos))
	# } else {
		# port <- try(as.integer(port),silent=TRUE)
		# if (inherits(port,"try-error") || port <= 0) stop("port must be a positive integer!")
		
		# if (exists("environments",where=pos)) {
			# environments <- get("environments",pos=pos,inherits=FALSE)
		# } else environments <- list()
		# elem <- paste("port",port,sep="_")
		# env <- environments[[elem]]
		# if (is.null(env)) {
			# env <- new.env(hash=TRUE,parent=pos.to.env(pos))
			# environments[[elem]] <- env
			# assign("environments",environments,pos=pos)
		# }
		# return(env)
	# }
# }

#' @export
getPortEnv <- function(port) {
	paste("Rshare",port,sep="_")
}

#' Accessing the Rshare shared environment 
#'
#' Access to the Rshare environment is parameterized by a specific port number. Otherwise, the usual R functions such as \code{assign},
#' \code{get},
#'
#' @rdname assign.Rshare
#' @export
assign.Rshare <- function(x, value, port = 7777, inherits=FALSE) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
		req <- RshareAssignReq(x,value,inherits=inherits)
		res <- sendRObj(req,sock) # do not block for response
		return(invisible(value))
	} else {
		assign(x, value, envir=.Rshare[[getPortEnv(port)]], inherits=inherits)
	}
}

#' @rdname assign.Rshare
#' @export
get.Rshare <- function(x, port = 7777, timeout = 10L, mode = "any", inherits = FALSE) {
	status <- getStatus(port)
	
	if (identical(status,"client")) {
		sock <- as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
		req <- RshareGetReq(x, mode=mode, inherits=inherits)
		res <- sendRObj(req,sock,block=TRUE,timeout=timeout)
		return(res)
	} else { # client
		if (exists(x, envir = .Rshare[[getPortEnv(port)]], mode=mode, inherits = inherits)) {
			return(get(x, envir = .Rshare[[getPortEnv(port)]], mode=mode, inherits = inherits))
		} else return(NULL)
	} 
}

#' @rdname assign.Rshare
#' @export
remove.Rshare <- function(..., list = character(), port = 7777) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
		req <- RshareRemoveReq(..., list = list)
		res <- sendRObj(req,sock,block=TRUE)
		invisible(res)
	} else {
		remove(...,list = list,envir=.Rshare[[getPortEnv(port)]])
	}
}

#' @export
rm.Rshare <- remove.Rshare

#' @rdname assign.Rshare
#' @export
exists.Rshare <- function(x, port = 7777, mode = "any", inherits=FALSE) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
		req <- RshareExistsReq(x=x, mode=mode, inherits=inherits)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		exists(x,where=.Rshare[[getPortEnv(port)]], mode=mode, inherits=inherits)
	}
}

#' @rdname assign.Rshare
#' @export
ls.Rshare <- function(port = 7777, all.names = FALSE, ...) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
		req <- RshareLsReq(all.names = all.names, ...)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		ls(.Rshare[[getPortEnv(port)]], all.names=all.names, ...)
	}
}

#' @rdname assign.Rshare
#' @export
ls.str.Rshare <- function(port = 7777, all.names = FALSE, ...) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
		req <- RshareLsStrReq(all.names = all.names, ...)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		ls.str(.Rshare[[getPortEnv(port)]], all.names=all.names, ...)
	}
}

#' @rdname assign.Rshare
#' @export
lsf.str.Rshare <- function(port = 7777, all.names = FALSE, ...) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
		req <- RshareLsfStrReq(all.names = all.names, ...)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		lsf.str(.Rshare[[getPortEnv(port)]], all.names=all.names, ...)
	}
}