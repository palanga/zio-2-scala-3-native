import socket.*
import zio.*

object Main extends ZIOAppDefault :

  override def run =
    for
      args <- Args.fromCommandLine
      server <- InetStreamSocketServer.start(args.host, args.port)
      _ <- Console.printLine(s"Listening on ${args.host}:${args.port}")
      _ <- ZIO.scoped(server.accept.flatMap(respond)).forever
    yield ()

  private def respond(client: InetStreamSocket) =
    for
      _ <- client.writeLine("que onda soquete")
      _ <- client.write("chau ")
      _ <- client.writeLine("soquete")
    yield ()

class Args private(val host: String, val port: Int)
object Args:
  def fromCommandLine: ZIO[ZIOAppArgs, Exception, Args] =
    ZIOAppArgs.getArgs.map(_.toList).flatMap {
      case host :: StringToInt(port) :: _ => ZIO.succeed(Args(host, port))
      case args => ZIO.fail(Exception(s"Invalid command line args: <<$args>>."))
    }

object StringToInt:
  def unapply(input: String): Option[Int] = input.toIntOption
