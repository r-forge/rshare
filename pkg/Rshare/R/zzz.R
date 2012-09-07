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

.onLoad <- function(libname, pkgname) {
	# Initialize environment and add to search path
	#RshareEnv()
	if (!exists(".Rshare")) .Rshare <<- new.env()
	packageStartupMessage("Loaded Rshare version 1.0: See ?Rshare for more information \n")
}

.Last.lib <- function(libpath) {
	stopRshare()
	# close out socket connection if needed
	# if (exists.Rshare("con")) {
		# con <- get.Rshare("con")
		# if (isOpen(con)) close(con)
	# }
}