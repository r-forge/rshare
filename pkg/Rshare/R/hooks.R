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

#' @export
registerRshareHook <- function(objType, hookFunction, port=7777) {
	if (!is.character(objType)) stop("objType must be of type 'character'")
	if (!is.function(hookFunction)) stop("hookFunction must be a function")
	hookFunction <- match.fun(hookFunction)
	
	# check hookFunction formals
	hookFormals <- formals(hookFunction)
	if (!"obj" %in% names(hookFormals)) stop("hookFunction must have an 'obj' argument")
	
	status <- getStatus(port)
	
	if (identical(status,"closed")) stop(paste("Rshare must be running on port",port,"in order to register a hook"))
	if (identical(status,"client")) {
		# send hook object to server and run it there, much like asssign.Rshare
		res <- "client"
	} else { # server
		hooks <- get.Rshare(".hooks",port=port)
		if (is.null(hooks)) hooks <- list()
		
		objHooks <- hooks[[objType]]
		if (is.null(objHooks)) objHooks <- list()
		
		# Hooks are executed in first to last order of objHooks list, FIFE(xecuted) 
		objHooks[[length(objHooks)+1]] <- hookFunction
		
		hooks[[objType]] <- objHooks
		assign.Rshare(".hooks",hooks,port=port)
		res <- length(objHooks)
	}
	invisible(res)
}