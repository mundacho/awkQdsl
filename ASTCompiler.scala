// ASTCompiler.scala

package backend

import backend._

import scala.annotation.tailrec

object ASTCompiler:

  def compile(awkRootExpr: AwkQueryAST): String = awkRootExpr match {
    case AwkQueryAST(file) =>
      "awk '{print $0 }' " + file
    case AwkQueryAST(file, MapExpr(expr)) =>
      val toPrint = expr.idx.map(x => "$" + x).mkString(",")
      s"""awk '{print $toPrint }' $file"""
  }