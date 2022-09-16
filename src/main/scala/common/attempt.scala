package common

import zio.*

import scala.scalanative
import scala.scalanative.unsafe.CInt

def attemptBlocking(effect: => CInt): Task[CInt] =
  ZIO.attemptBlocking {
    val res = effect
    if res < 0
    then
      val errorNumber  = scalanative.libc.errno.errno
      val errorMessage = scalanative.unsafe.fromCString(scalanative.libc.string.strerror(errorNumber))
      throw Exception(s"Error number <<$errorNumber>>: $errorMessage")
    else res
  }

def attemptBlocking(effect: => CInt, additionalErrorMessage: => String): Task[CInt] =
  ZIO.attemptBlocking {
    val res = effect
    if res < 0
    then
      val errorNumber  = scalanative.libc.errno.errno
      val errorMessage = scalanative.unsafe.fromCString(scalanative.libc.string.strerror(errorNumber))
      throw Exception(s"Error number <<$errorNumber>>: $errorMessage $additionalErrorMessage")
    else res
  }

def attemptBlockingZoned(effect: scalanative.unsafe.Zone => CInt): Task[CInt] =
  ZIO.attemptBlocking {
    scalanative.unsafe.Zone { implicit zone =>
      val res = effect(zone)
      if res < 0
      then
        val errorNumber  = scalanative.libc.errno.errno
        val errorMessage = scalanative.unsafe.fromCString(scalanative.libc.string.strerror(errorNumber))
        throw Exception(s"Error number <<$errorNumber>>: $errorMessage")
      else res
    }
  }
