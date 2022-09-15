package common

import zio.*

import scala.scalanative.libc.errno
import scala.scalanative.libc.string.strerror
import scala.scalanative.unsafe.{CInt, Zone, fromCString}

def attemptBlocking(effect: => CInt): Task[CInt] =
  ZIO.attemptBlocking {
    val res = effect
    if res < 0
    then
      val errorNumber = errno.errno
      val errorMessage = fromCString(strerror(errorNumber))
      throw Exception(s"Error number <<$errorNumber>>: $errorMessage")
    else
      res
  }

def attemptBlocking(effect: => CInt, additionalErrorMessage: String): Task[CInt] =
  ZIO.attemptBlocking {
    val res = effect
    if res < 0
    then
      val errorNumber = errno.errno
      val errorMessage = fromCString(strerror(errorNumber))
      throw Exception(s"Error number <<$errorNumber>>: $errorMessage $additionalErrorMessage")
    else
      res
  }

def attemptBlockingZoned(effect: Zone => CInt): Task[CInt] =
  ZIO.attemptBlocking {
    Zone { implicit zone =>
      val res = effect(zone)
      if res < 0
      then
        val errorNumber = errno.errno
        val errorMessage = fromCString(strerror(errorNumber))
        throw Exception(s"Error number <<$errorNumber>>: $errorMessage")
      else
        res
    }
  }
