// AwkQdsl.scala

package frontend

import backend.AwkQueryAST

object AwkQuery:
  def apply[T] = new AwkQuery[T]() {}

trait AwkQuery[T]:

  def map[R](f: T => R): AwkQuery[R] =
    throw new IllegalAccessError()