package socket

import zio.*

import scala.scalanative.libc
import scala.scalanative.libc.{errno, stdlib}
import scala.scalanative.libc.stdlib.{EXIT_FAILURE, exit}
import scala.scalanative.libc.string.{strerror, strlen}
import scala.scalanative.posix.netinet.in.sockaddr_in
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{sockaddr, socklen_t}
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.{CString, Ptr, Zone, sizeof, stackalloc, toCString, fromCString}
import scala.scalanative.unsigned.UInt

class InetSocketAddress(underlying: sockaddr_in):

  private[socket] def asSocketAddressPointer: Ptr[sockaddr] = underlying.toPtr.asInstanceOf[Ptr[sockaddr]]

  override def toString: String =
    import scala.scalanative.posix.netinet.inOps.*
    s"InetSocketAddress(${underlying.toPtr.sin_addr._1})" // TODO get the actual host and port

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
        cHost,
        socketAddress.sin_addr.toPtr.asInstanceOf[Ptr[Byte]]
      )
      if res < 0 then throw Exception("invalid host or port") else ()

      InetSocketAddress(socketAddress)
    }
  }

  private[socket] val dummy =
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

  private[socket] val sizeOf: UInt = sizeof[sockaddr_in].toUInt

  private[socket] val sizeOfPtr: Ptr[socklen_t] =
    val len = stdlib.malloc(sizeof[socklen_t]).asInstanceOf[Ptr[socklen_t]]
    !len = sizeof[sockaddr_in].toUInt
    len
