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

#' Connect to Rshare
#'
#' This function starts the Rshare process in the current R session. If it is the first local R session to start Rshare on the specified port,
#' then this R session will take the role of the server on that port. Otherwise, it will be a client.
#'
#' @param port
#' @export
startRshare <- function (port = 7777) {
	port <- try(as.integer(port),silent=TRUE)
	if (inherits(port,"try-error") || port <= 0) stop("port must be a positive integer!")
	
	if (getStatus(port) != "closed") stop(paste("Rshare already running as a",getStatus(port),"on port",port,"in this R instance!"))
	
	# Create port-specific environment, set enclosing environment to .Rshare
	if (is.null(.Rshare[[getPortEnv(port)]])) assign(getPortEnv(port),new.env(parent=.Rshare),env=.Rshare)
	
	# Create tcl terminator
	# .Tcl("global sockTerminator")
	# tmp <- tclVar()
	# tclvalue(tmp) <- as.integer(.RshareTerminator())
	# .Tcl(paste("set ::sockTerminator [binary format c* $",as.character(tmp),"]",sep=""))
	
	# When R 2.15.2 comes out, will have patch that makes the following work instead, currenly patch is in R-devel
	# tclvalue(tmp) <- serialize(obj,NULL)
	
	## TODO: allow user to set tcl channel buffer size as option
	
	# Create tcl callback procedures
	createRshareTclCallbacks()
	
	# Load tcl source
	tclsrc <- system.file("Rshare.tcl",package="Rshare")
	tcl("source",tclsrc)
	
	# Start socket server or client
	tcl("sockStart",port)

	status <- as.character(.Tcl(paste("set Rshare_",port,"(status)",sep="")))
	setStatus(port,status)
	
	return(invisible(TRUE))
}

#' @rdname startRshare
#' @export
stopRshare <- function (port = 7777) {
	status <- getStatus(port)
	
	if (!identical(status,"closed")) {
		.Tcl(paste("close $Rshare_",port,"("status,")",sep=""))
		message(paste("Rshare",status,"on port",port,"stopped at",format(Sys.time(),"%H:%M:%S")))
	}
	
	setStatus(port,"closed")
}

#' @export
getClientSocketId <- function(port) {
	if (!identical(getStatus(port),"client")) return(NULL)
	as.character(.Tcl(paste("set Rshare_",port,"(client)",sep="")))
}

#' @export
getSocketIds <- function(port = 7777) {
	get.Rshare(".clientSockets",port=port)
}

addSocketId <- function(serverPort, channelId, channelPort) {
	socks <- get.Rshare(".clientSockets",port=serverPort)
	if (is.null(socks)) socks <- list()
	if (!channelId %in% names(socks)) {
		socks[[channelId]] <- channelPort
	}
	assign.Rshare(".clientSockets",socks,port=serverPort)
}

removeSocketId <- function(serverPort, channelId) {
	socks <- get.Rshare(".clientSockets",port=serverPort)
	if (is.null(socks)) socks <- list()
	if (channelId %in% names(socks)) {
		socks[[channelId]] <- NULL
	}
	assign.Rshare(".clientSockets",socks,port=serverPort)
}

setStatus <- function(port, status) {
	if (!status %in% c("server","client","closed")) stop("status may only be be 'server', 'client', or 'closed'")
	assign(".status", status, envir=.Rshare[[getPortEnv(port)]])
}

#' @export
getStatus <- function(port) {
	status <- try(get(".status", envir=.Rshare[[getPortEnv(port)]]), silent=TRUE) 
	if (inherits(status,"try-error")) status <- "closed"
	status
}

#' Send an R object through a tcl socket connection
#'
#' @param obj the R object to be sent. May be of any type.
#' @param sock a chacter vector containing the tcl socket identifier
#' @param block logial; whether to block for a reponse
#' @param timeout number of seconds to wait for a response if \code{block} is \code{TRUE}
#' @return invisibly returns \code{TRUE} for non-blocking sends, otherwise returns the response received for blocking sends
#' @export
sendRObj <- function(obj, sock, block = FALSE, timeout = 10L) {
	sobj <- serialize(obj,NULL,xdr=FALSE)
	shead <- packBits(intToBits(length(sobj)), type="raw")
	dobj <- c(shead, sobj)
	
	tmp <- tclVar()
	tclvalue(tmp) <- as.integer(dobj)
	.Tcl(paste("set ",as.character(tmp)," [binary format c* $",as.character(tmp),"]",sep=""))
	
	# When R 2.15.2 comes out, will have patch that makes the following work instead, currenly patch is in R-devel
	# tclvalue(tmp) <- serialize(obj,NULL)
	
	if (block) {
		timeout <- timeout * 1000
		.Tcl(paste("set res [sockClientSendBlocking ",sock," $",as.character(tmp)," ",timeout,"]",sep=""))
		
		raw.obj <- as.raw(.Tcl("set res"))
		if (length(raw.obj) == 0) return(invisible(FALSE))
		obj <- unserialize(raw.obj)
		return(obj)
	} else {
		.Tcl(paste("sockSend ",sock," $",as.character(tmp),sep=""))
		return(invisible(TRUE))
	}
}