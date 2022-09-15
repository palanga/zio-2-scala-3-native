import zio.*

import scala.scalanative.libc
import scala.scalanative.libc.{errno, stdlib}
import scala.scalanative.libc.stdlib.{EXIT_FAILURE, exit}
import scala.scalanative.libc.string.{strerror, strlen}
import scala.scalanative.posix.netinet.in.sockaddr_in
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{sockaddr, socklen_t}
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.{CString, Ptr, Zone, sizeof, stackalloc, toCString}
import scala.scalanative.unsigned.UInt

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

class InetStreamSocket private(fileDescriptor: Int):

  def close =
    ZIO.attemptBlocking {
      val res = unistd.close(fileDescriptor)
      if res < 0
      then throw Exception(s"Socket closing impossible. File descriptor: $fileDescriptor")
      else ()
    }

  def bind(address: InetSocketAddress) =
    ZIO.attemptBlocking {
      val res = socket.bind(fileDescriptor, address.asSocketAddressPointer, InetSocketAddress.sizeOf)
      if res < 0
      then
        import scalanative.unsafe.CQuote
        libc.stdio.perror(c"bind")
        val e = strerror(errno.errno)
        import scalanative.libc.StdioHelpers
        libc.stdio.printf(e)
//        exit(EXIT_FAILURE)
        throw Exception(s"Cannot bind to address $address: socket.bind errno ${libc.errno.errno}")
      else ()
    }

  def listen = ZIO.attemptBlocking(socket.listen(fileDescriptor, backlog = 2)) // TODO hardcoded

  def accept =
    ZIO
      .attemptBlocking(socket.accept(fileDescriptor, InetSocketAddress.dummy.asSocketAddressPointer, InetSocketAddress.sizeOfPtr))
      .flatMap(InetStreamSocket.fromFileDescriptor)

  def write(input: String) =
    ZIO.attemptBlocking {
      Zone { implicit zone =>
        val text = toCString(input)
        val len = strlen(text)
        unistd.write(fileDescriptor, text, len)
      }
    }

  def writeLine(input: String) = write(input + '\n')

  def connect(address: InetSocketAddress) =
    ZIO.attemptBlocking {
      socket.connect(fileDescriptor, address.asSocketAddressPointer, InetSocketAddress.sizeOf)
    }

  override def toString: String = s"InetStreamSocket($fileDescriptor)"

object InetStreamSocket:

  def open =
    ZIO.attemptBlocking {
      val fileDescriptor = socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0)
      if fileDescriptor < 0
      then throw Exception(s"Socket creation impossible.")
      else InetStreamSocket(fileDescriptor)
    }

  // TODO package private
  def fromFileDescriptor(fileDescriptor: Int) =
    ZIO.attemptBlocking {
      if fileDescriptor < 0
      then throw Exception(s"Socket creation impossible.")
      else InetStreamSocket(fileDescriptor)
    }

class InetSocketAddress(underlying: sockaddr_in):
  // TODO package private
  def asSocketAddressPointer: Ptr[sockaddr] = underlying.toPtr.asInstanceOf[Ptr[sockaddr]]

  override def toString: String =
    import scala.scalanative.posix.netinet.inOps.*
    s"InetSocketAddress(${underlying.toPtr.sin_addr._1})"

object InetSocketAddress:

  def fromHostAndPort(host: String, port: Int) = ZIO.attemptBlocking {
    Zone { implicit z =>
      import scala.scalanative.posix.arpa.inet.{htons, inet_pton}
      import scala.scalanative.posix.netinet.inOps.*
      import scala.scalanative.posix.sys.socket.AF_INET
      import scalanative.unsigned.UnsignedRichInt

      val socketAddress: Ptr[sockaddr_in] = stackalloc[sockaddr_in]()
      socketAddress.sin_family = AF_INET.toUShort
      socketAddress.sin_port = htons(port.toUShort)

      val cHost = toCString(host)
      val res = inet_pton(
        AF_INET,
        //      c"127.0.0.1",
        cHost,
        socketAddress.sin_addr.toPtr.asInstanceOf[Ptr[Byte]]
      )
      if res < 0 then throw Exception("invalid host or port") else ()

      InetSocketAddress(socketAddress)
    }
  }

  // TODO package private
  val dummy =
    import scala.scalanative.posix.arpa.inet.{htons, inet_pton}
    import scala.scalanative.posix.netinet.inOps.*
    import scala.scalanative.posix.sys.socket.AF_INET
    import scalanative.unsafe.CQuote
    import scalanative.unsigned.UnsignedRichInt

    val socketAddress: Ptr[sockaddr_in] = stackalloc[sockaddr_in]()
    socketAddress.sin_family = AF_INET.toUShort
    socketAddress.sin_port = htons(8080.toUShort)

    val res = inet_pton(
      AF_INET,
      c"127.0.0.1",
      socketAddress.sin_addr.toPtr.asInstanceOf[Ptr[Byte]]
    )
    val failure = libc.stdlib.EXIT_FAILURE
    if res < 0 then throw Exception(s"inet_pton errno $failure") else ()

    InetSocketAddress(socketAddress)

  // TODO package private
  val sizeOf: UInt = sizeof[sockaddr_in].toUInt

  // TODO package private
  val sizeOfPtr: Ptr[socklen_t] =
    val len = stdlib.malloc(sizeof[socklen_t]).asInstanceOf[Ptr[socklen_t]]
    !len = sizeof[sockaddr_in].toUInt
    len
