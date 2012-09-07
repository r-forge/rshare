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
RshareAssignReq <- function(x, value, inherits = FALSE) {
	req <- structure(list(x=x, value=value, inherits=inherits))
	class(req) <- c("RshareAssignReq","RshareReq")
	req
}

#' @export
RshareGetReq <- function(x, mode = "any", inherits = FALSE) {
	req <- structure(list(x=x, mode=mode, inherits=inherits))
	class(req) <- c("RshareGetReq","RshareReq")
	req
}

#' @export
RshareExistsReq <- function(x, mode="any", inherits=FALSE) {
	req <- structure(list(x=x, mode=mode, inherits=inherits))
	class(req) <- c("RshareExistsReq","RshareReq")
	req
}

#' @export
RshareRemoveReq <- function(..., list = character()) {
	req <- structure(list(..., list = list))
	class(req) <- c("RshareRemoveReq","RshareReq")
	req
}

#' @export
RshareLsReq <- function(all.names = FALSE, ...) {
	req <- structure(list(all.names=all.names, ...))
	class(req) <- c("RshareLsReq","RshareReq")
	req
}

#' @export
RshareLsStrReq <- function(all.names = FALSE, ...) {
	req <- structure(list(all.names=all.names, ...))
	class(req) <- c("RshareLsStrReq","RshareReq")
	req
}

#' @export
RshareLsfStrReq <- function(all.names = FALSE, ...) {
	req <- structure(list(all.names=all.names, ...))
	class(req) <- c("RshareLsfStrReq","RshareReq")
	req
}

#' @export
RshareAddHookReq <- function(objType, hookFunction, port) {
	req <- structure(list(objType=objType, hookFunction=hookFunction, port=port))
	class(req) <- c("RshareAddHookReq","RshareReq")
	req
}

#' @export
# .RshareTerminator <- function() { serialize(Inf,NULL) }