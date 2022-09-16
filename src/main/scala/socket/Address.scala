package socket

import zio.*

import scala.scalanative
import scala.scalanative.posix
import scala.scalanative.libc
import scala.scalanative.libc.stdlib
import scala.scalanative.libc.string.{strerror, strlen}
import scala.scalanative.posix.netinet.in.sockaddr_in
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{sockaddr, socklen_t}
import scala.scalanative.unsafe.{CChar, CString, Ptr, Zone, sizeof, stackalloc, toCString}
import scala.scalanative.unsigned.UInt

class Address private (underlying: sockaddr_in):

  def host: String =
    import scala.scalanative.posix.netinet.inOps.*
    import scalanative.unsigned.UnsignedRichInt

    val destination: CString = scalanative.unsafe.stackalloc[CChar](posix.netinet.in.INET_ADDRSTRLEN.toUInt)

    posix.arpa.inet.inet_ntop(
      posix.sys.socket.AF_INET,
      underlying.toPtr.sin_addr.toPtr.asInstanceOf[Ptr[Byte]],
      destination,
      posix.netinet.in.INET_ADDRSTRLEN.toUInt,
    )

    scalanative.unsafe.fromCString(destination)

  def port: Int = posix.arpa.inet.ntohs(underlying._2).toInt

  override def toString: String = s"$host:$port"

  private[socket] def asSocketAddressPointer: Ptr[sockaddr] = underlying.toPtr.asInstanceOf[Ptr[sockaddr]]

/**
 * {{{
 * #include <stdio.h>
 * #include <stdlib.h>
 * #include <sys/socket.h>
 * #include <netinet/in.h>
 * #include <netdb.h>
 *
 * void init_sockaddr (struct sockaddr_in *name, const char *hostname, uint16_t port) {
 *   struct hostent *hostinfo;
 *
 *   name->sin_family = AF_INET;
 *   name->sin_port = htons (port);
 *
 *   hostinfo = gethostbyname (hostname);
 *   if (hostinfo == NULL) {
 *     fprintf (stderr, "Unknown host %s.\n", hostname);
 *     exit (EXIT_FAILURE);
 *   }
 *
 *   name->sin_addr = *(struct in_addr *) hostinfo->h_addr;
 * }
 * }}}
 */
object Address:

  def fromHostAndPort(host: String, port: Int) = ZIO.attemptBlocking {
    Zone { implicit z =>
      import scala.scalanative.posix.arpa.inet.{ htons, inet_pton }
      import scala.scalanative.posix.netinet.inOps.*
      import scala.scalanative.posix.sys.socket.AF_INET
      import scalanative.unsigned.UnsignedRichInt

      val socketAddress: Ptr[sockaddr_in] = stackalloc[sockaddr_in]()
      socketAddress.sin_family = AF_INET.toUShort
      socketAddress.sin_port = htons(port.toUShort)

      val cHost = toCString(host)
      val res   = inet_pton(
        AF_INET,
        cHost,
        socketAddress.sin_addr.toPtr.asInstanceOf[Ptr[Byte]],
      )
      if res < 0 then throw Exception("invalid host or port") else ()

      Address(socketAddress)
    }
  }.debug

  private def underlyingFromHostAndPort(host: String, port: Int)(using Zone): sockaddr_in =
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
      socketAddress.sin_addr.toPtr.asInstanceOf[Ptr[Byte]],
    )
    if res != 1 then throw Exception("invalid host or port") else ()

    !socketAddress

  private[socket] val sizeOf: UInt = sizeof[sockaddr_in].toUInt

  private[socket] val sizeOfPtr: Ptr[socklen_t] =
    val len = stdlib.malloc(sizeof[socklen_t]).asInstanceOf[Ptr[socklen_t]]
    !len = sizeof[sockaddr_in].toUInt
    len

  private[socket] val dummy =
    import scala.scalanative.posix.arpa.inet.{ htons, inet_pton }
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
      socketAddress.sin_addr.toPtr.asInstanceOf[Ptr[Byte]],
    )
    if res < 0 then throw Exception(s"inet_pton error") else ()

    Address(socketAddress)
