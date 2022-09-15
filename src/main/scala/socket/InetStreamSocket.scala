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
    common.attemptBlocking(unistd.close(fileDescriptor), s"File descriptor number $fileDescriptor.").as(fileDescriptor)

  def bind(address: InetSocketAddress) =
    common.attemptBlocking(socket.bind(fileDescriptor, address.asSocketAddressPointer, InetSocketAddress.sizeOf)).unit

  def listen =
    common.attemptBlocking(socket.listen(fileDescriptor, backlog = 2)).unit // TODO hardcoded

  def accept: ZIO[Scope, Throwable, InetStreamSocket] =
    common
      .attemptBlocking(socket.accept(fileDescriptor, InetSocketAddress.dummy.asSocketAddressPointer, InetSocketAddress.sizeOfPtr))
      .map(InetStreamSocket(_))
      .withFinalizer(_.close.debug("file descriptor closed").orDie)

  def writeLine(input: String) = write(input + '\n')

  def write(input: String) =
    common.attemptBlockingZoned { implicit zone =>
      val text = toCString(input)
      val len = strlen(text)
      unistd.write(fileDescriptor, text, len)
    }

  def connect(address: InetSocketAddress) =
    common
      .attemptBlocking(socket.connect(fileDescriptor, address.asSocketAddressPointer, InetSocketAddress.sizeOf))
      .unit

  override def toString: String = s"InetStreamSocket(fileDescriptor = $fileDescriptor)"

object InetStreamSocket:

  def open: ZIO[Scope, Throwable, InetStreamSocket] =
    common
      .attemptBlocking(socket.socket(socket.AF_INET, socket.SOCK_STREAM, 0))
      .map(InetStreamSocket(_))
      .withFinalizer(_.close.debug("file descriptor closed").orDie)
