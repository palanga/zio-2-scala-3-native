package socket

import zio.*

import scala.scalanative
import scala.scalanative.posix
import scala.scalanative.libc
import scala.scalanative.libc.stdlib
import scala.scalanative.libc.string.{strerror, strlen}
import scala.scalanative.posix.inttypes.uint16_t
import scala.scalanative.posix.netdb.addrinfo
import scala.scalanative.posix.netinet.in.{in_addr, in_addr_t, sockaddr_in}
import scala.scalanative.posix.sys.socket
import scala.scalanative.posix.sys.socket.{sockaddr, socklen_t}
import scala.scalanative.unsafe.{CChar, CInt, CString, Ptr, Zone, fromCString, sizeof, stackalloc, toCString}
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
 *
       The following programs demonstrate the use of getaddrinfo(),
       gai_strerror(), freeaddrinfo(), and getnameinfo(3).  The programs
       are an echo server and client for UDP datagrams.

   Server program

       #include <sys/types.h>
       #include <stdio.h>
       #include <stdlib.h>
       #include <unistd.h>
       #include <string.h>
       #include <sys/socket.h>
       #include <netdb.h>

       #define BUF_SIZE 500

       int
       main(int argc, char *argv[])
       {
           struct addrinfo hints;
           struct addrinfo *result, *rp;
           int sfd, s;
           struct sockaddr_storage peer_addr;
           socklen_t peer_addr_len;
           ssize_t nread;
           char buf[BUF_SIZE];

           if (argc != 2) {
               fprintf(stderr, "Usage: %s port\n", argv[0]);
               exit(EXIT_FAILURE);
           }

           memset(&hints, 0, sizeof(hints));
           hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6 */
           hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
           hints.ai_flags = AI_PASSIVE;    /* For wildcard IP address */
           hints.ai_protocol = 0;          /* Any protocol */
           hints.ai_canonname = NULL;
           hints.ai_addr = NULL;
           hints.ai_next = NULL;

           s = getaddrinfo(NULL, argv[1], &hints, &result);
           if (s != 0) {
               fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
               exit(EXIT_FAILURE);
           }

           /* getaddrinfo() returns a list of address structures.
              Try each address until we successfully bind(2).
              If socket(2) (or bind(2)) fails, we (close the socket
              and) try the next address. */

           for (rp = result; rp != NULL; rp = rp->ai_next) {
               sfd = socket(rp->ai_family, rp->ai_socktype,
                       rp->ai_protocol);
               if (sfd == -1)
                   continue;

               if (bind(sfd, rp->ai_addr, rp->ai_addrlen) == 0)
                   break;                  /* Success */

               close(sfd);
           }

           freeaddrinfo(result);           /* No longer needed */

           if (rp == NULL) {               /* No address succeeded */
               fprintf(stderr, "Could not bind\n");
               exit(EXIT_FAILURE);
           }

           /* Read datagrams and echo them back to sender. */

           for (;;) {
               peer_addr_len = sizeof(peer_addr);
               nread = recvfrom(sfd, buf, BUF_SIZE, 0,
                       (struct sockaddr *) &peer_addr, &peer_addr_len);
               if (nread == -1)
                   continue;               /* Ignore failed request */

               char host[NI_MAXHOST], service[NI_MAXSERV];

               s = getnameinfo((struct sockaddr *) &peer_addr,
                               peer_addr_len, host, NI_MAXHOST,
                               service, NI_MAXSERV, NI_NUMERICSERV);
               if (s == 0)
                   printf("Received %zd bytes from %s:%s\n",
                           nread, host, service);
               else
                   fprintf(stderr, "getnameinfo: %s\n", gai_strerror(s));

               if (sendto(sfd, buf, nread, 0,
                           (struct sockaddr *) &peer_addr,
                           peer_addr_len) != nread)
                   fprintf(stderr, "Error sending response\n");
           }
       }

   Client program

       #include <sys/types.h>
       #include <sys/socket.h>
       #include <netdb.h>
       #include <stdio.h>
       #include <stdlib.h>
       #include <unistd.h>
       #include <string.h>

       #define BUF_SIZE 500

       int
       main(int argc, char *argv[])
       {
           struct addrinfo hints;
           struct addrinfo *result, *rp;
           int sfd, s;
           size_t len;
           ssize_t nread;
           char buf[BUF_SIZE];

           if (argc < 3) {
               fprintf(stderr, "Usage: %s host port msg...\n", argv[0]);
               exit(EXIT_FAILURE);
           }

           /* Obtain address(es) matching host/port. */

           memset(&hints, 0, sizeof(hints));
           hints.ai_family = AF_UNSPEC;    /* Allow IPv4 or IPv6 */
           hints.ai_socktype = SOCK_DGRAM; /* Datagram socket */
           hints.ai_flags = 0;
           hints.ai_protocol = 0;          /* Any protocol */

           s = getaddrinfo(argv[1], argv[2], &hints, &result);
           if (s != 0) {
               fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
               exit(EXIT_FAILURE);
           }

           /* getaddrinfo() returns a list of address structures.
              Try each address until we successfully connect(2).
              If socket(2) (or connect(2)) fails, we (close the socket
              and) try the next address. */

           for (rp = result; rp != NULL; rp = rp->ai_next) {
               sfd = socket(rp->ai_family, rp->ai_socktype,
                            rp->ai_protocol);
               if (sfd == -1)
                   continue;

               if (connect(sfd, rp->ai_addr, rp->ai_addrlen) != -1)
                   break;                  /* Success */

               close(sfd);
           }

           freeaddrinfo(result);           /* No longer needed */

           if (rp == NULL) {               /* No address succeeded */
               fprintf(stderr, "Could not connect\n");
               exit(EXIT_FAILURE);
           }

           /* Send remaining command-line arguments as separate
              datagrams, and read responses from server. */

           for (int j = 3; j < argc; j++) {
               len = strlen(argv[j]) + 1;
                       /* +1 for terminating null byte */

               if (len > BUF_SIZE) {
                   fprintf(stderr,
                           "Ignoring long message in argument %d\n", j);
                   continue;
               }

               if (write(sfd, argv[j], len) != len) {
                   fprintf(stderr, "partial/failed write\n");
                   exit(EXIT_FAILURE);
               }

               nread = read(sfd, buf, BUF_SIZE);
               if (nread == -1) {
                   perror("read");
                   exit(EXIT_FAILURE);
               }

               printf("Received %zd bytes: %s\n", nread, buf);
           }

           exit(EXIT_SUCCESS);
       }
SEE ALSO         top
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
      import scalanative.posix.netdbOps.*

      val cHost = toCString(host)
      val inAddr: Ptr[in_addr] = stackalloc[in_addr]()
      val exitCode = inet_pton(
        posix.sys.socket.AF_INET,
        cHost,
        inAddr.asInstanceOf[Ptr[Byte]],
      )
      if exitCode != 1 then throw Exception("invalid host or port") else ()


//      val reses: Ptr[Ptr[addrinfo]] = stackalloc[Ptr[addrinfo]]()
//      posix.netdb.getaddrinfo(cHost, toCString(port.toString), null, reses) // TODO don't forget to free address info
//
//      val res: Ptr[addrinfo] = reses(0)
//
//      val printHost = toCString("                ")
//      val pritnPort = toCString("                ") // TODO me transforma el numero del puerto a un nombre en particular (8080 es http-alt)
//      posix.netdb.getnameinfo(res.ai_addr, sizeof[sockaddr_in].toUInt, printHost, 16.toUShort, pritnPort, 16.toUShort, 0)
//
//      println(s"la direccion al final es: ${fromCString(printHost)}:${fromCString(pritnPort)}")
//
//
//      import scala.scalanative.posix.netinet.inOps.*
//      res.ai_family = posix.sys.socket.AF_INET
//      Address(cSocketAddress(res.ai_addr.asInstanceOf[Ptr[sockaddr_in]].sin_addr, htons(port.toUShort))) // TODO me dice address family not supported

      Address(cSocketAddress(inAddr, htons(port.toUShort)))
    }
  }.debug

  def getAddressInfo(host: String, port: Int): Task[(Long, Int)] = ZIO.attemptBlocking { Zone { implicit z =>
      import scalanative.posix.netdbOps.*
      import scala.scalanative.posix.netinet.inOps.*

      val hint: Ptr[addrinfo] = stackalloc[addrinfo]()
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
        val hostInt: UInt = addressInfo.ai_addr.asInstanceOf[Ptr[sockaddr_in]].sin_addr._1
        val result = hostInt.toLong -> port
        posix.netdb.freeaddrinfo(addressInfos(0))
        result
      }
    }

  def getAddressName(host: Long, port: Int): (String, Int) = Zone { implicit z =>
    import scalanative.unsigned.UnsignedRichLong
    import scalanative.unsigned.UnsignedRichInt
    import scala.scalanative.posix.netinet.inOps.*

    val input: Ptr[sockaddr_in] = stackalloc[sockaddr_in]()
    val addr: Ptr[in_addr] = stackalloc[in_addr]()

    addr._1 = host.toUInt

    input.sin_family = posix.sys.socket.AF_INET.toUShort
    input.sin_addr = addr

    val printHost = toCString("                ")

    val exitCode =
      posix.netdb.getnameinfo(input.asInstanceOf[Ptr[sockaddr]], sizeof[sockaddr_in].toUInt, printHost, 16.toUShort, null, 0.toUShort, 0)

    if exitCode != 0
    then throw Exception("nono")
    else fromCString(printHost) -> port
  }


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

// TODO put on a test module
/**
 * If host have a name (like 127.0.0.1 -> localhost), then the name must be provided instead of the numeric form for
 * the identity to work. This is because getAddressName will return the name form and not the numeric one.
 */
def test_getAddressInfo_and_getAddressName_identity(host: String, port: Int) =
    Address
      .getAddressInfo(host, port).debug("get address info")
      .map(Address.getAddressName.tupled).debug("get address name")
      .map(_ == (host -> port)).debug
