import socket.*
import zio.*

object Main extends ZIOAppDefault:

  // TODO probar con ZLayer scoped para el shutdown. tambien probar ensuring en vez de withFinalizer
  override def run =
    for
      Args(host, port) <- Args.fromCommandLine
//      _                <- socket.test_getAddressInfo_and_getAddressName_identity(host, port)
      _                <- serverApp(host, port)
    yield ()
//    ZLayer.scoped(serverApp).launch // TODO esto es al pedo

  private def serverApp(host: String, port: Int): ZIO[Scope, Throwable, Unit] =
    for
      server <- Server.start(host, port)
      _      <- Console.printLine(s"Listening on $host:$port")
      _      <- ZIO.scoped(server.accept.flatMap(respond)).forever.timeout(10.seconds)
    yield ()

  private def respond(client: Socket) =
    for
      _ <- client.writeLine("que onda soquete")
      _ <- client.write("chau ")
      _ <- client.writeLine("soquete")
    yield ()

case class Args private (host: String, port: Int)
object Args:
  def fromCommandLine: ZIO[ZIOAppArgs, Throwable, Args] =
    ZIOAppArgs.getArgs.map(_.toList).flatMap {
      case host :: StringToInt(port) :: _ => ZIO.succeed(Args(host, port))
      case args                           => ZIO.fail(Exception(s"Invalid command line args: <<$args>>."))
    }

object StringToInt:
  def unapply(input: String): Option[Int] = input.toIntOption
