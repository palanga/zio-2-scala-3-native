package socket

import zio.*

import scala.scalanative
import scala.scalanative.posix

class InetStreamSocket private(fileDescriptor: Int):

  def close =
    common
      .attemptBlocking(posix.unistd.close(fileDescriptor), s"File descriptor number $fileDescriptor.")
      .as(fileDescriptor)

  def bind(address: InetSocketAddress) =
    common
      .attemptBlocking(posix.sys.socket.bind(fileDescriptor, address.asSocketAddressPointer, InetSocketAddress.sizeOf))
      .unit

  def listen =
    common.attemptBlocking(posix.sys.socket.listen(fileDescriptor, backlog = 2)).unit // TODO hardcoded

  def accept: ZIO[Scope, Throwable, InetStreamSocket] =
    common
      // TODO dummy ?
      .attemptBlocking(posix.sys.socket.accept(fileDescriptor, InetSocketAddress.dummy.asSocketAddressPointer, InetSocketAddress.sizeOfPtr))
      .map(InetStreamSocket(_))
      .withFinalizer(_.close.debug("file descriptor closed").orDie)

  def writeLine(input: String) = write(input + '\n')

  def write(input: String) =
    common.attemptBlockingZoned { implicit zone =>
      val text = scalanative.unsafe.toCString(input)
      val len = scalanative.libc.string.strlen(text)
      posix.unistd.write(fileDescriptor, text, len)
    }

  def connect(address: InetSocketAddress) =
    common
      .attemptBlocking(posix.sys.socket.connect(fileDescriptor, address.asSocketAddressPointer, InetSocketAddress.sizeOf))
      .unit

  override def toString: String = s"InetStreamSocket(fileDescriptor = $fileDescriptor)"

object InetStreamSocket:

  def open: ZIO[Scope, Throwable, InetStreamSocket] =
    common
      .attemptBlocking(posix.sys.socket.socket(posix.sys.socket.AF_INET, posix.sys.socket.SOCK_STREAM, 0))
      .map(InetStreamSocket(_))
      .withFinalizer(_.close.debug("file descriptor closed").orDie)
