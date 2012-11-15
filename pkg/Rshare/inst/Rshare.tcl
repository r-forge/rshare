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

proc sockSend {sock data} {
	if {$sock == -1} {return}
	if {[string length $data] == 0} {return}
	puts -nonewline $sock $data
	flush $sock
}

proc sockClientSendBlocking {sock data tout} {
	set port [sockPort $sock client]
	global Rshare_[set port]
	
	# Send request
	sockSend $sock $data
	
	set Rshare_[set port](client,state) waiting
	set cmd [after $tout [list set Rshare_[set port](client,state) timeout]]

	fileevent $sock readable [list sockClientReady $port $sock]
	
	# wait for response or timeout
	vwait Rshare_[set port](client,state)
	after cancel $cmd
	
	if {[string equal [set Rshare_[set port](client,state)] timeout]} {
		set secs [expr $tout / 1000]
		Rcat "Error: client request on port $port timed out after $secs seconds"
	} else {
		set obj [sockClientProcess $port $sock ""]
		return $obj
	}
	
}

proc sockClientReady {port sock} {
	#upvar 1 state state
	global Rshare_[set port]
	set Rshare_[set port](client,state) ready
	fileevent $sock readable ""
}

proc sockClientProcess {port sock buf {left 0}} {
	global Rshare_[set port]
	
	if {$sock == ""} {return}
	
	if {$left == 0} {
		# new read
		# set left value to object length 
		## TODO: deal with endianess here!
		set objlen [read $sock 4]
		binary scan $objlen i* left 
	}
	
	set in [read $sock $left]
	set inlen [string length $in]
	
	# Do we need this?
	# if {$inlen == 0} {
		# fileevent $sock readable ""
		# return $buf
	# }
	
	append buf $in
	if {$inlen < $left} { # didn't read all we need
		set left [expr $left - $inlen]
		return [sockClientProcess $port $sock $buf $left]
	} else { # read entire object
		set left 0
		return $buf
	}
}

proc sockServerProcessDone {port sock args} {
	global Rshare_[set port]
	#set Rshare_[set port]($sock,lastObj) [set Rshare_[set port]($sock,data)]
	#set Rshare_[set port]($sock,data) ""
	if {[set Rshare_[set port]($sock,data)] != ""} {
		sockServerRead $port $sock
		set Rshare_[set port]($sock,data) ""
	}
	set Rshare_[set port]($sock,read_status) 0
	set Rshare_[set port]($sock,data_ready) 0 
}

proc sockServerProcess {port sock buf {left 0}} {
	global Rshare_[set port]
	
	if {$sock == ""} {return}
	
	if [eof $sock] {
		close $sock
		set Rshare_[set port]($sock,data_ready) -1
		removeRSocket $port $sock
		return
	}
	
	if [string equal [set Rshare_[set port]($sock,read_status)] 0] {
		# new thing to read
		set Rshare_[set port]($sock,read_status) 1
		set buf ""
		
		# set left value to object length 
		## TODO: deal with endianess here!
		set objlen [read $sock 4]
		binary scan $objlen i* left 
		if {$left == ""} {set left 0}
	}
	
	if {$left == 0} {
		set in [read -nonewline $sock]
	} else {
		set in [read $sock $left]
	}
	set inlen [string length $in]
	
	append buf $in
	
	if {$inlen < $left} { # didn't read all we need
		set left [expr $left - $inlen]
	} else { # read entire object
		set left 0
		set Rshare_[set port]($sock,data) $buf
		set buf ""
		set Rshare_[set port]($sock,data_ready) 1
	}
	fileevent $sock readable [list sockServerProcess $port $sock $buf $left]
}

proc sockConnected {sock addr sockport} {
	set port [sockPort $sock]
	global Rshare_[set port]
	if {[isLocalIP $sock $addr]} {
		set Rshare_[set port]($sock,meta) [list $addr, $port]
		set Rshare_[set port]($sock,data) ""
		set Rshare_[set port]($sock,data_ready) 1
		set Rshare_[set port]($sock,read_status) 0
		fconfigure $sock -encoding binary -blocking 0 -translation binary -buffering full -buffersize 819200
		trace add variable Rshare_[set port]($sock,data_ready) write [list sockServerProcessDone $port $sock]
		fileevent $sock readable [list sockServerProcess $port $sock ""]
		addRSocket $port $sock $sockport
	} else {
		Rcat "Error: Only local clients allowed. Connection from ip $addr was refused."
		close $sock
		return
	}
}

proc sockPort {sock {role server}} {
	if {[string equal $role server]} {
		set str [fconfigure $sock -sockname]
	} else {
		set str [fconfigure $sock -peername]
	}
	regexp {(\d+)(?!.*\d)} $str port
	return $port
}

proc sockStart {port {serv 0} {clio 0}} {
	global Rshare_[set port]
	
	# Try to create server channel so long as not client-only
	if {$clio == 0} { 
		if {[catch {set Rshare_[set port](server) [socket -server sockConnected $port]}] == 0} {
			# Configure server socket
			fconfigure [set Rshare_[set port](server)] -encoding binary -blocking 0 -translation binary -buffering full -buffersize 819200
			set Rshare_[set port](status) server
			return
		}
		if {$serv != 0} {
			set Rshare_[set port](status) closed
			return
		}
	}
	
	# Server channel creation failed: try client
	if {[catch {set Rshare_[set port](client) [socket [info hostname] $port]}] != 0} {
		# Couldn't create client channel
		# Rcat "Error starting the Rshare client on port $port"
		set Rshare_[set port](client) -1
		set Rshare_[set port](status) closed
		return
	} else {
		# Configure client socket
		fconfigure [set Rshare_[set port](client)] -encoding binary -blocking 0 -translation binary -buffering full -buffersize 819200
		trace add variable Rshare_[set port]([set Rshare_[set port](client)],data_ready) write [list sockServerProcessDone $port [set Rshare_[set port](client)]]
		set Rshare_[set port](status) client
		set Rshare_[set port]([set Rshare_[set port](client)],read_status) 0
		return
	}
}