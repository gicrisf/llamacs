#!/usr/bin/env python3

from epc.server import EPCServer

server = EPCServer(('localhost', 0))

@server.register_function
def query(a):
    return a + "polpetteeee"

server.print_port()
server.serve_forever()
