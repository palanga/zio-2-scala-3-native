package socket

import zio.*

import scala.scalanative.libc
import scala.scalanative.libc.errno
import scala.scalanative.libc.string.{strerror, strlen}
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.{Zone, fromCString, sizeof, toCString}

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
        val estring = fromCString(e)
        throw Exception(s"Cannot bind to address $address: socket.bind errno ${libc.errno.errno} message: $estring")
      else ()
    }

  def listen = ZIO.attemptBlocking(socket.listen(fileDescriptor, backlog = 2)) // TODO hardcoded

  def accept =
    ZIO
      .attemptBlocking(socket.accept(fileDescriptor, InetSocketAddress.dummy.asSocketAddressPointer, InetSocketAddress.sizeOfPtr))
      .flatMap(InetStreamSocket.fromFileDescriptor)

  def writeLine(input: String) = write(input + '\n')

  def write(input: String) =
    ZIO.attemptBlocking {
      Zone { implicit zone =>
        val text = toCString(input)
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

  private[socket] def fromFileDescriptor(fileDescriptor: Int) =
    ZIO.attemptBlocking {
      if fileDescriptor < 0
      then throw Exception(s"Socket creation impossible.")
      else InetStreamSocket(fileDescriptor)
    }
