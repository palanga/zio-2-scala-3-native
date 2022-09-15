import zio.*

import scala.scalanative.libc.stdlib
import scala.scalanative.libc.string.strlen
import scala.scalanative.posix.netinet.in.sockaddr_in
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{sockaddr, socklen_t}
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.{CString, Ptr, Zone, sizeof, stackalloc, toCString}
import scala.scalanative.unsigned.UInt

object Main extends ZIOAppDefault :

  override def run =
    for
      port <- ZIOAppArgs.getArgs.map(_.headOption.flatMap(_.toIntOption)).someOrFailException
      server <- InetStreamSocket.open.debug
      address = InetSocketAddress.fromHostAndPort("harcodeado abajo", port)
      _ <- server.bind(address).debug
      _ <- server.listen.debug
      socket2 <- server.accept.debug
      _ <- socket2.write("harcodeado abajo").debug
      _ <- Console.printLine("Hola ZIO 2 en Scala 3 native")
      _ <- server.close.debug
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
      then throw Exception(s"Cannot bind to address $address")
      else ()
    }

  def listen = ZIO.attemptBlocking(socket.listen(fileDescriptor, backlog = 2)) // TODO hardcoded

  def accept =
    ZIO
      .attemptBlocking(socket.accept(fileDescriptor, InetSocketAddress.dummy.asSocketAddressPointer, InetSocketAddress.sizeOfPtr))
      .flatMap(InetStreamSocket.fromFileDescriptor)

  def write(input: String) =
    import scalanative.unsafe.CQuote
    import scalanative.unsigned.UnsignedRichInt
    ZIO.attemptBlocking {
      Zone { implicit z =>
        val text: CString = toCString(input)
        val len = strlen(text)
        unistd.write(fileDescriptor, text, len)
      }
    }

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

object InetSocketAddress:

  def fromHostAndPort(host: String, port: Int): InetSocketAddress =
    import scala.scalanative.posix.arpa.inet.{htons, inet_pton}
    import scala.scalanative.posix.netinet.inOps.*
    import scala.scalanative.posix.sys.socket.AF_INET
    import scalanative.unsafe.CQuote
    import scalanative.unsigned.UnsignedRichInt

    val socketAddress: Ptr[sockaddr_in] = stackalloc[sockaddr_in]()
    socketAddress.sin_family = AF_INET.toUShort
    socketAddress.sin_port = htons(8080.toUShort)
    inet_pton(
      AF_INET,
      //      Zone { implicit z =>
      //        toCString(host)
      //      },
      c"127.0.0.1",
      socketAddress.sin_addr.toPtr.asInstanceOf[Ptr[Byte]]
    )
    socketAddress.sin_port = htons(port.toUShort)
    InetSocketAddress(socketAddress)

  // TODO package private
  val dummy = InetSocketAddress.fromHostAndPort("127.0.0.1", 8080)

  // TODO package private
  val sizeOf: UInt = sizeof[sockaddr_in].toUInt

  // TODO package private
  val sizeOfPtr: Ptr[socklen_t] =
    val len = stdlib.malloc(sizeof[socklen_t]).asInstanceOf[Ptr[socklen_t]]
    !len = sizeof[sockaddr_in].toUInt
    len
