package common

import zio.*

import scala.scalanative.unsafe.{ sizeof, Ptr, Tag }
import scala.scalanative.libc.stdlib.{ free, malloc }

object ZAllocate:

  def make[A](using Tag[A]): ZIO[Scope, Nothing, Ptr[A]] =
    ZIO
      .succeed(malloc(sizeof[A]))
      .withFinalizer(ptr => ZIO.succeed(free(ptr)))
      .map(_.asInstanceOf[Ptr[A]])

  def makeAndMutate[A](mutate: Ptr[A] => Unit)(using Tag[A]): ZIO[Scope, Nothing, Ptr[A]] =
    ZIO
      .succeed {
        val ptr = malloc(sizeof[A])
        mutate(ptr.asInstanceOf[Ptr[A]])
        ptr
      }
      .withFinalizer(ptr => ZIO.succeed(free(ptr)).debug("memory freed"))
      .map(_.asInstanceOf[Ptr[A]])
