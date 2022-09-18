package socket

import zio.*

import scala.scalanative
import scala.scalanative.posix
import scala.scalanative.libc
import scala.scalanative.libc.stdlib
import scala.scalanative.libc.string.{ strerror, strlen }
import scala.scalanative.posix.inttypes.{ uint16_t, uint32_t }
import scala.scalanative.posix.netdb.addrinfo
import scala.scalanative.posix.netinet.in.{ in_addr, in_addr_t, sockaddr_in }
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{ sockaddr, socklen_t }
import scala.scalanative.unsafe.{ fromCString, sizeof, stackalloc, toCString, CChar, CInt, CString, Ptr, Zone }
import scala.scalanative.unsigned.UInt

class Address private (host: String, port: Int, hostTranslated: Long, portTranslated: Int):

  override def toString: String = s"$host:$port"

  private[socket] def asSocketAddressPointer: Ptr[sockaddr] =
    import scalanative.unsigned.UnsignedRichLong
    import scalanative.unsigned.UnsignedRichInt
    Address
      .cSocketAddressPtr(hostTranslated.toUInt, posix.arpa.inet.htons(portTranslated.toUShort))
      .asInstanceOf[Ptr[sockaddr]]

object Address:

  def fromHostAndPort(host: String, port: Int) =
    for
      (hostTranslated, portTranslated) <- getAddressInfo(host, port)
      (host, port)                     <- getAddressName(hostTranslated, portTranslated)
    yield Address(host, port, hostTranslated, portTranslated)

  private[socket] def getAddressInfo(host: String, port: Int): Task[(Long, Int)] = ZIO.attemptBlocking {
    Zone { implicit z =>
      import scalanative.posix.netdbOps.*
      import scala.scalanative.posix.netinet.inOps.*

      val hint: Ptr[addrinfo]              = stackalloc[addrinfo]()
      val addressInfos: Ptr[Ptr[addrinfo]] = stackalloc[Ptr[addrinfo]]()

      hint.ai_family = posix.sys.socket.AF_INET
      hint.ai_protocol = posix.sys.socket.SOCK_STREAM

      val exitCode =
        posix.netdb.getaddrinfo(toCString(host), toCString(port.toString), hint, addressInfos)

      if exitCode != 0
      then
        posix.netdb.freeaddrinfo(addressInfos(0))
        val errorMessage = scalanative.unsafe.fromCString(posix.netdb.gai_strerror(exitCode))
        throw Exception(s"Error number <<$exitCode>>: $errorMessage. Input: <<$host>> <<$port>>")
      else
        val addressInfo: Ptr[addrinfo] = addressInfos(0)
        val hostInt: UInt              = addressInfo.ai_addr.asInstanceOf[Ptr[sockaddr_in]].sin_addr._1
        val result                     = hostInt.toLong -> port
        posix.netdb.freeaddrinfo(addressInfos(0))
        result
    }
  }

  private def ganame(host: Long, port: Int): ZIO[Scope, Nothing, (String, CInt)] =
    import scalanative.unsigned.UnsignedRichLong
    import scalanative.unsigned.UnsignedRichInt
    import scala.scalanative.posix.netinet.inOps.*

    for
      inAddr <- common.ZAllocate.makeAndMutate[in_addr](_._1 = host.toUInt)
      input  <- common.ZAllocate.makeAndMutate[sockaddr_in] { ptr =>
                  ptr.sin_family = posix.sys.socket.AF_INET.toUShort
                  ptr.sin_addr = inAddr
                }
    yield Zone { implicit z =>

      val printHost = toCString("                ")

      val exitCode =
        posix.netdb.getnameinfo(
          input.asInstanceOf[Ptr[sockaddr]],
          sizeof[sockaddr_in].toUInt,
          printHost,
          16.toUShort,
          null,
          0.toUShort,
          0,
        ) // TODO flags doesn't seem to work (tried NUMERICHOST)

      if exitCode != 0
      then
        val errorMessage = scalanative.unsafe.fromCString(posix.netdb.gai_strerror(exitCode))
        throw Exception(s"Error number <<$exitCode>>: $errorMessage. Input: <<$host>> <<$port>>")
      else fromCString(printHost) -> port

    }

  private[socket] def getAddressName(host: Long, port: Int) = ganame(host, port)
//    ZIO.attemptBlocking {
//      Zone { implicit z =>
//        import scalanative.unsigned.UnsignedRichLong
//        import scalanative.unsigned.UnsignedRichInt
//        import scala.scalanative.posix.netinet.inOps.*
//
////      val input: Ptr[sockaddr_in] = stackalloc[sockaddr_in]()
//        val addr: Ptr[in_addr] = stackalloc[in_addr]()
//
//        addr._1 = host.toUInt
//
////      input.sin_family = posix.sys.socket.AF_INET.toUShort
////      input.sin_addr = addr
//
////      val input = withStackAlloc[sockaddr_in] { socketAddress =>
////        socketAddress.sin_family = posix.sys.socket.AF_INET.toUShort
////        socketAddress.sin_addr = addr
////      }
//
//        val input = common.Allocated.make[sockaddr_in].mutate { socketAddress =>
//          socketAddress.sin_family = posix.sys.socket.AF_INET.toUShort
//          socketAddress.sin_addr = addr
//        }
//
//        // TODO use a better buffer
//        val printHost = toCString("                ")
//
//        val exitCode =
//          posix.netdb.getnameinfo(
//            input.get.asInstanceOf[Ptr[sockaddr]],
//            sizeof[sockaddr_in].toUInt,
//            printHost,
//            16.toUShort,
//            null,
//            0.toUShort,
//            0,
//          ) // TODO flags doesn't seem to work (tried NUMERICHOST)
//
//        input.deallocate
//
//        if exitCode != 0
//        then
//          val errorMessage = scalanative.unsafe.fromCString(posix.netdb.gai_strerror(exitCode))
//          throw Exception(s"Error number <<$exitCode>>: $errorMessage. Input: <<$host>> <<$port>>")
//        else fromCString(printHost) -> port
//      }
//    }

  // TODO not tested and should be in common package
  def withStackAlloc[A](mutate: Ptr[A] => Unit)(using scalanative.unsafe.Tag[A]): Ptr[A] =
    val a = stackalloc[A]()
    mutate(a)
    a

  private def cSocketAddressPtr(host: uint32_t, port: uint16_t): Ptr[sockaddr_in] =
    import scalanative.unsigned.UnsignedRichInt
    import scala.scalanative.posix.netinet.inOps.*

    val socket_address = stackalloc[sockaddr_in]()
    socket_address.sin_family = posix.sys.socket.AF_INET.toUShort
    socket_address.sin_addr._1 = host
    socket_address.sin_port = port
    socket_address

  private[socket] val sizeOf: UInt = sizeof[sockaddr_in].toUInt

  private[socket] val sizeOfPtr: Ptr[socklen_t] =
    val len = stdlib.malloc(sizeof[socklen_t]).asInstanceOf[Ptr[socklen_t]]
    !len = sizeof[sockaddr_in].toUInt
    len

  private[socket] val dummy = Address("localhost", 8080, 16777343L, 8080)

// TODO put on a test module
/**
 * If host have a name (like 127.0.0.1 -> localhost), then the name must be provided instead of the numeric form for the
 * identity to work. This is because getAddressName will return the name form and not the numeric one.
 */
def test_getAddressInfo_and_getAddressName_identity(host: String, port: Int) =
  (for
    info                      <- Address.getAddressInfo(host, port).debug("get address info")
    name                      <- Address.getAddressName.tupled(info).debug("get address name")
    nameInfoNameIdentityFiber <- Address.getAddressInfo.tupled(name).flatMap(Address.getAddressName.tupled).fork
    infoNameInfoIdentityFiber <- Address.getAddressName.tupled(info).flatMap(Address.getAddressInfo.tupled).fork
    nameInfoNameIdentity      <- nameInfoNameIdentityFiber.join.debug("name info name identity")
    infoNameInfoIdentity      <- infoNameInfoIdentityFiber.join.debug("info name info identity")
  yield nameInfoNameIdentity == name && infoNameInfoIdentity == info).debug("test result")
