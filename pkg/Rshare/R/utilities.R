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

#' Accessing the Rshare shared environment 
#'
#' Access to the Rshare environment is parameterized by the port number on which the Rshare server is run. 
#' If Rshare has not beeninitialized in the current R session, these functions will just access the local Rshare environment. 
#' Other than the lack of any \code{envir} argument and the addition of a \code{port} argument, these functions are analogous to their standard counterparts.
#' 
#' @param port the Rshare port number
#' @param timeout number of seconds to wait for a response from the Rshare server
#'
#' @seealso \code{\link{get}}, \code{\link{assign}}, \code{\link{remove}}, \code{\link{exists}}, \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}}
#' @author Charlie Friedemann
#' @inheritParams base::get
#' @rdname get.Rshare
#' @export
get.Rshare <- function(x, port = 7777, timeout = 10L, mode = "any", inherits = FALSE) {
	status <- getStatus(port)
	
	if (identical(status,"client")) {
		sock <- getClientSocketId(port)
		req <- RshareGetReq(x, mode=mode, inherits=inherits)
		res <- sendRObj(req,sock,block=TRUE,timeout=timeout)
		return(res)
	} else { # server
		if (exists(x, envir = .Rshare[[getPortEnv(port)]], mode=mode, inherits = inherits)) {
			return(get(x, envir = .Rshare[[getPortEnv(port)]], mode=mode, inherits = inherits))
		} else return(NULL)
	} 
}

#' @inheritParams base::assign
#' @rdname get.Rshare
#' @export
assign.Rshare <- function(x, value, port = 7777, inherits=FALSE) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- getClientSocketId(port)
		req <- RshareAssignReq(x,value,inherits=inherits)
		res <- sendRObj(req,sock) # do not block for response
		return(invisible(value))
	} else {
		assign(x, value, envir=.Rshare[[getPortEnv(port)]], inherits=inherits)
	}
}

#' @inheritParams base::remove
#' @aliases rm.Rshare
#' @rdname get.Rshare
#' @export
remove.Rshare <- function(..., list = character(), port = 7777) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- getClientSocketId(port)
		req <- RshareRemoveReq(..., list = list)
		res <- sendRObj(req,sock,block=TRUE)
		invisible(res)
	} else {
		remove(...,list = list,envir=.Rshare[[getPortEnv(port)]])
	}
}

#' @export
rm.Rshare <- remove.Rshare

#' @inheritParams base::exists
#' @rdname get.Rshare
#' @export
exists.Rshare <- function(x, port = 7777, mode = "any", inherits=FALSE) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- getClientSocketId(port)
		req <- RshareExistsReq(x=x, mode=mode, inherits=inherits)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		exists(x,where=.Rshare[[getPortEnv(port)]], mode=mode, inherits=inherits)
	}
}

#' @inheritParams base::ls
#' @rdname get.Rshare
#' @export
ls.Rshare <- function(port = 7777, all.names = FALSE, ...) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- getClientSocketId(port)
		req <- RshareLsReq(all.names = all.names, ...)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		ls(.Rshare[[getPortEnv(port)]], all.names=all.names, ...)
	}
}

#' @inheritParams utils::ls.str
#' @rdname get.Rshare
#' @export
ls.str.Rshare <- function(port = 7777, all.names = FALSE, ...) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- getClientSocketId(port)
		req <- RshareLsStrReq(all.names = all.names, ...)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		ls.str(.Rshare[[getPortEnv(port)]], all.names=all.names, ...)
	}
}

#' @inheritParams utils::lsf.str
#' @rdname get.Rshare
#' @export
lsf.str.Rshare <- function(port = 7777, all.names = FALSE, ...) {
	status <- getStatus(port)
	if (identical(status,"client")) {
		sock <- getClientSocketId(port)
		req <- RshareLsfStrReq(all.names = all.names, ...)
		res <- sendRObj(req,sock,block=TRUE)
		return(res)
	} else {
		lsf.str(.Rshare[[getPortEnv(port)]], all.names=all.names, ...)
	}
}

getPortEnv <- function(port) {
	paste("Rshare",port,sep="_")
}