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

createRshareTclCallbacks <- function() {
	`tclProcExists` <- function(proc) { 
		return(length(as.character(tcl("info", "procs", proc))) > 0)
	}
	
	# Tcl procedure to send message to R
	if (!tclProcExists("Rcat")) {
		res <- .Tcl.callback(TclSocketMsgFun)
		if (length(grep("R_call ", res) > 0)) {
			cmd <- paste(c( "proc Rcat {msg} {", sprintf(res,"$"), "}"),collapse="\n")
			.Tcl(cmd)
		} else stop("Could not create the socket server Rcat tcl callback function")
	}
	
	# Tcl procedure to send error message to R
	# if (!tclProcExists("Rerror")) {
		# res <- .Tcl.callback(TclSocketErrorFun)
		# if (length(grep("R_call ", res) > 0)) {
			# cmd <- paste(c( "proc Rerror {msg} {", sprintf(res,"$"), "}"),collapse="\n")
			# .Tcl(cmd)
		# } else stop("Could not create the socket server Rerror tcl callback function")
	# }
	
	# Tcl procedure to test if connecting IP address is local
	if (!tclProcExists("isLocalIP")) {
		res <- .Tcl.callback(TclSocketLocalIPFun)
		if (length(grep("R_call ", res) > 0)) {
			cmd <- paste(c( "proc isLocalIP {sock ip} {", 
								# First check if peername and sockname ip match so non-local appearing IPs can be verified as local
								"set sockname [fconfigure $sock -sockname]",
								"set peername [fconfigure $sock -peername]",
								"set sockip [string range $sockname 0 [expr [string first \" \" $sockname] -1]]",
								"set peerip [string range $peername 0 [expr [string first \" \" $peername] -1]]",
								"if {[string equal $sockip $peerip]} {return 1}",
								# If sockname and peername ip are not equal, then send to R for further testing
								paste("set res [",sprintf(res,"$"),"]",sep=""),
								"return $res",
							"}"),collapse="\n")
			.Tcl(cmd)
		} else stop("Could not create the socket server isLocalIP tcl callback function")
	}
	
	# Tcl procedure to add a socket connection to list in R
	if (!tclProcExists("addRSocket")) {
		res <- .Tcl.callback(TclSocketAddToR)
		if (length(grep("R_call ", res) > 0)) {
			cmd <- paste(c( "proc addRSocket {port sock chanport} {", sprintf(res,"$","$","$"), "}"),collapse="\n")
			.Tcl(cmd)
		} else stop("Could not create the socket server addRSocket tcl callback function")
	}
	
	# Tcl procedure to remove socket from list in R
	if (!tclProcExists("removeRSocket")) {
		res <- .Tcl.callback(TclSocketRemoveFromR)
		if (length(grep("R_call ", res) > 0)) {
			cmd <- paste(c( "proc removeRSocket {port sock} {", sprintf(res,"$","$"), "}"),collapse="\n")
			.Tcl(cmd)
		} else stop("Could not create the socket server isLocalIP tcl callback function")
	}
	
	# Tcl procedure for reading objects into R server from socket
	if (!tclProcExists("sockServerRead")) {
        res <- .Tcl.callback(TclSocketServerRead)
        if (length(grep("R_call ", res) > 0)) {
            cmd <- paste("proc sockServerRead {port sock} {", sprintf(res,"$","$"), "}", sep = "")
			.Tcl(cmd)
		} else stop("Cannot create the socket server sockServerRead tcl callback function")
	}
	
	# Tcl procedure for reading objects into R client from socket
	# if (!tclProcExists("sockClientRead")) {
        # res <- .Tcl.callback(TclSocketClientRead)
        # if (length(grep("R_call ", res) > 0)) {
            # cmd <- paste("proc sockClientRead {port sock} {", sprintf(res,"$","$"), "}", sep = "")
			# .Tcl(cmd)
		# } else stop("Cannot create the socket server sockClientRead tcl callback function")
	# }
}

# Send messages to R
`TclSocketMsgFun` <- function(smsg) { cat(smsg,"\n") }

# Send error to R
# `TclSocketErrorFun` <- function(smsg) { stop(smsg) }

# Tcl Callbacks
`TclSocketAddToR` <- function(sport, ssock, schanport) {
	if (ssock == "") return(invisible(as.numeric(FALSE)))
	addSocketId(sport, ssock, schanport)
	return(invisible(as.numeric(TRUE)))
}

`TclSocketRemoveFromR` <- function(sport, ssock) {
	if (ssock == "") return(invisible(as.numeric(FALSE)))
	removeSocketId(sport, ssock)
	return(invisible(as.numeric(TRUE)))
}

`TclSocketLocalIPFun` <- function(sip) {
	if (sip == "") tcl("set","res",0)
	if ((length(grep("192.168.1.",sip)) > 0) || (length(grep("127.0.0.1",sip))) || (length(grep("localhost",sip)))) { 
		tcl("set","res",1)
	} else tcl("set","res",0)
}

`TclSocketServerRead` <- function(sport, ssock) {
	# Read object from socket
	port <- sport
	sock <- ssock
	.Tcl(paste("upvar 1 Rshare_",port,"(",sock,",data) obj",sep=""))
	#browser()
	#.Tcl("sockSend $sock $obj")
	
	# tclobj <- as.raw(.Tcl("set obj"))
	# if (length(tclobj) == 0) return(invisible(as.numeric(FALSE)))
	
	obj <- try(unserialize(as.raw(.Tcl("set obj"))),silent=TRUE)
	if (inherits(obj,"try-error")) {
		# Not a valid object
		## TODO: send error to client
		cat(paste("Unable to read data sent to Rshare",getStatus(port),"running on port",port,"\n"))
		return(invisible(as.numeric(FALSE)))
	}
	
	if (inherits(obj,"RshareReq")) {
		doResponse <- TRUE
		obj$port <- port
		if (inherits(obj,"RshareAssignReq")) { 
			# Put request, just assign to server Rshare environment
			do.call("assign.Rshare",obj) 
			doResponse <- FALSE
		} else if (inherits(obj,"RshareGetReq")) {
			# Get request, retrieve object (or NULL) and send back to client
			res <- do.call("get.Rshare",obj)
			obj$obj.found <- !is.null(res)
		} else if (inherits(obj,"RshareExistsReq")) {
			# Exists request
			res <- do.call("exists.Rshare",obj) 
		} else if (inherits(obj,"RshareRemoveReq")) {
			# Remove request
			res <- do.call("remove.Rshare",obj)
		} else if (inherits(obj,"RshareLsReq")) {
			# List request
			res <- do.call("ls.Rshare",obj)
		} else if (inherits(obj,"RshareLsStrReq")) {
			# List request
			res <- do.call("ls.str.Rshare",obj)
		} else if (inherits(obj,"RshareLsfStrReq")) {
			# List request
			res <- do.call("lsf.str.Rshare",obj)
		} else if (inherits(obj,"RshareAddHookReq")) {
			# Add hook request
			res <- do.call("registerRshareHook",obj)
		}
		if (doResponse) sendRObj(res,sock)
		
		obj$sent.time <- Sys.time()
		obj$sock <- sock
		assign.Rshare(".latestRshareReq",obj,port=port)
	} else {
		# User-defined hooks
		objType <- class(obj)
		hooks <- get.Rshare(".hooks",port=port)
		if (is.null(hooks)) hooks <- list()
		if (!is.null(hooks[[objType]])) {
			# object has hooks, execute them in first to last order
			objHooks <- hooks[[objType]]
			for (i in 1:length(objHooks)) {
				if ("port" %in% names(formals(objHooks[[i]]))) do.call(objHooks[[i]], list(obj=obj, port=port)) else do.call(objHooks[[i]], list(obj=obj))
			}
			
		} else {	
			# unrecognized object, throw warning or just give warning message? 
			## TODO: make warning an option
			assign.Rshare(".latestObj",obj,port=port)
			cat("Warning: unrecognized request object type \n")
			return(invisible(as.numeric(FALSE)))
		}
	}
	return(invisible(as.numeric(TRUE)))
}
