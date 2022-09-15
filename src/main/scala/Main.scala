import zio.*
import socket.*

object Main extends ZIOAppDefault :

  override def run =
    for
      host <- ZIOAppArgs.getArgs.map(_.headOption).someOrFailException.debug
      port <- ZIOAppArgs.getArgs.map(_.tail.headOption.flatMap(_.toIntOption)).someOrFailException.debug
      server <- ZIO.scoped(InetStreamSocket.open).withFinalizer(_.close.orDie).debug
      address <- InetSocketAddress.fromHostAndPort(host, port).debug
      _ <- server.bind(address).debug
      _ <- server.listen.debug
      client <- server.accept.debug.flatMap(respond).forever
    yield ()

  def respond(client: InetStreamSocket): ZIO[Any, Throwable, Unit] =
    for
      _ <- client.writeLine("que onda soquete").debug
      _ <- client.write("chau ").debug
      _ <- client.writeLine("soquete").debug
      _ <- client.close.debug
    yield ()
