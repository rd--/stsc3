!ByteArray methodsFor: 'as yet unclassified'!
sendUdpToHost: hostAddress port: portNumber
	"Requires namespace notation, which is not Smalltalk syntax..."
	| socket datagram |
	socket := Sockets.DatagramSocket remote: hostAddress port: portNumber local: nil port: 0.
	datagram := Sockets.Datagram data: self.
	socket nextPut: datagram ; flush.
	socket close!
!
