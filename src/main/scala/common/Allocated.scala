package common

import scala.scalanative.unsafe.{ sizeof, Ptr, Tag }
import scala.scalanative.libc.stdlib.{ free, malloc }

class Allocated[A](underlying: Ptr[Byte]):

  def mutate(f: Ptr[A] => Unit): Allocated[A] =
    f(underlying.asInstanceOf[Ptr[A]])
    this

  def get: Ptr[A] = underlying.asInstanceOf[Ptr[A]]

  def deallocate = free(underlying)

object Allocated:
  def make[A](using Tag[A]): Allocated[A] = Allocated(malloc(sizeof[A]))
