package socket

import zio.*

import scala.scalanative
import scala.scalanative.posix
import scala.scalanative.libc
import scala.scalanative.libc.stdlib
import scala.scalanative.libc.string.{ strerror, strlen }
import scala.scalanative.posix.inttypes.uint16_t
import scala.scalanative.posix.netinet.in.{ in_addr, sockaddr_in }
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{ sockaddr, socklen_t }
import scala.scalanative.unsafe.{ sizeof, stackalloc, toCString, CChar, CInt, CString, Ptr, Zone }
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

  // TODO probably not working
  // 32 bits for AF_INET,  128 bits for AF_INET6
  private def hostStringToNumeric(host: String) =
    common.attemptBlockingZonedDestination[in_addr](
      implicit zone =>
        ptr => posix.arpa.inet.inet_pton(posix.sys.socket.AF_INET, toCString(host), ptr.asInstanceOf[Ptr[Byte]]),
      _ != 1,
    )

//  def fromHostAndPort(host: String, port: Int): Task[Address] =
//    import scalanative.unsigned.UnsignedRichInt
//    import scala.scalanative.posix.netinet.inOps.*
//
//    hostStringToNumeric(host).map { cHost =>
//
//      val cFamily = posix.sys.socket.AF_INET
//      val cPort   = posix.arpa.inet.htons(port.toUShort)
//
//      val cSocketAddress = stackalloc[sockaddr_in]()
//      cSocketAddress.sin_family = cFamily.toUShort
//      cSocketAddress.sin_addr = cHost
//      cSocketAddress.sin_port = cPort
//
//      Address(cSocketAddress)
//    }.debug

  def fromHostAndPort(host: String, port: Int) = ZIO.attemptBlocking {
    Zone { implicit z =>
      import scala.scalanative.posix.arpa.inet.{htons, inet_pton}
      import scalanative.unsigned.UnsignedRichInt

      val cHost = toCString(host)
      val inAddr: Ptr[in_addr] = stackalloc[in_addr]()
      val exitCode = inet_pton(
        posix.sys.socket.AF_INET,
        cHost,
        inAddr.asInstanceOf[Ptr[Byte]],
      )
      if exitCode != 1 then throw Exception("invalid host or port") else ()

      Address(cSocketAddress(inAddr, htons(port.toUShort)))
    }
  }.debug


  // TODO not tested and should be in common package
  def withStackAlloc[A](mutate: Ptr[A] => Any)(using scalanative.unsafe.Tag[A]): Ptr[A] =
    val a = stackalloc[A]()
    mutate(a)
    a

  private def cSocketAddress(host: in_addr, port: uint16_t): Ptr[sockaddr_in] =
    import scalanative.unsigned.UnsignedRichInt
    import scala.scalanative.posix.netinet.inOps.*
    val socket_address = stackalloc[sockaddr_in]()
    socket_address.sin_family = posix.sys.socket.AF_INET.toUShort
    socket_address.sin_addr = host
    socket_address.sin_port = port
    socket_address

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
