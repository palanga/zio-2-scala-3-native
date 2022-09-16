package socket

import zio.*

import scala.scalanative
import scala.scalanative.posix

class Socket private (fileDescriptor: Int):

  def close =
    common
      .attemptBlocking(posix.unistd.close(fileDescriptor), s"File descriptor number $fileDescriptor.")
      .as(fileDescriptor)

  def bind(address: Address) =
    common
      .attemptBlocking(posix.sys.socket.bind(fileDescriptor, address.asSocketAddressPointer, Address.sizeOf))
      .unit

  def listen =
    common.attemptBlocking(posix.sys.socket.listen(fileDescriptor, backlog = 2)).unit // TODO hardcoded

  def accept: ZIO[Scope, Throwable, Socket] =
    common
      // TODO dummy ?
      .attemptBlocking(posix.sys.socket.accept(fileDescriptor, Address.dummy.asSocketAddressPointer, Address.sizeOfPtr))
      .map(Socket(_))
      .withFinalizer(_.close.debug("file descriptor closed").orDie)

  def writeLine(input: String) = write(input + '\n')

  def write(input: String) =
    common.attemptBlockingZoned { implicit zone =>
      val text = scalanative.unsafe.toCString(input)
      val len  = scalanative.libc.string.strlen(text)
      posix.unistd.write(fileDescriptor, text, len)
    }

  def connect(address: Address) =
    common
      .attemptBlocking(posix.sys.socket.connect(fileDescriptor, address.asSocketAddressPointer, Address.sizeOf))
      .unit

  override def toString: String = s"InetStreamSocket(fileDescriptor = $fileDescriptor)"

object Socket:

  def open: ZIO[Scope, Throwable, Socket] =
    common
      .attemptBlocking(posix.sys.socket.socket(posix.sys.socket.AF_INET, posix.sys.socket.SOCK_STREAM, 0))
      .map(Socket(_))
      .withFinalizer(_.close.debug("file descriptor closed").orDie)
