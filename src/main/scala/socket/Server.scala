package socket

import zio.*

object Server:

  def start(host: String, port: Int): ZIO[Scope, Throwable, Socket] =
    Address.fromHostAndPort(host, port).flatMap(start)

  def start(address: Address): ZIO[Scope, Throwable, Socket] =
    for
      server <- Socket.open
      _      <- server.bind(address)
      _      <- server.listen
    yield server
