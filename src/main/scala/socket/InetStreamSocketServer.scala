package socket

import zio.*

object InetStreamSocketServer:

  def start(host: String, port: Int): ZIO[Scope, Throwable, InetStreamSocket] =
    InetSocketAddress.fromHostAndPort(host, port).flatMap(start)

  def start(address: InetSocketAddress): ZIO[Scope, Throwable, InetStreamSocket] =
    for
      server <- InetStreamSocket.open
      _ <- server.bind(address)
      _ <- server.listen
    yield server
